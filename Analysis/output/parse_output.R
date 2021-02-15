# Function to parse the results of a series of runs
# Extracts:
#  - stan sampling runtimes
#  - rhats for observations
#  - observations vs modeled cases
#  - WHO country totals and national-level estimates
# Preamble ----------------------------------------------------------------
library(stringr)
library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(sf)
library(abind)
library(rstan)
library(foreach)

# Code options
option_list <- list(
  optparse::make_option(c("-c", "--config"), action = "store", default = Sys.getenv("CHOLERA_CONFIG", "config.yml"),
                        type = "character", help = "Model run configuration file"),
  optparse::make_option(c("-d", "--cholera_directory"), action = "store", default = "./", 
                        type="character", help = "Cholera directory"),
  optparse::make_option(c("-r", "--redo"), action = "store", default = FALSE, 
                        type="logical", help = "REDO")
)

opt <- optparse::OptionParser(option_list = option_list) %>% optparse::parse_args()

# Load config
config <- yaml::read_yaml(opt$config)

# Get filenames
file_names <- taxdat::get_filenames(config, opt$cholera_directory)
preprocessed_data_filename = file_names["data"]
stan_input_filename = file_names["stan_input"]
stan_output_filename = file_names["stan_output"]

# Stan input
stan_input <- taxdat::read_file_of_type(stan_input_filename, "stan_input")
covar_cube_output <- taxdat::read_file_of_type(file_names["covar"], "covar_cube_output")

# Create ouput directory for config files
out_dir <- str_c(opt$cholera_directory, "/Analysis/output/reports/", str_split(opt$config, "/")[[1]] %>% .[-length(.)] %>% last())
cat(out_dir, "\n")
readme_file <- str_c(out_dir, "/README.txt")
dir.create(out_dir, recursive = T)

# What country are we running?
country <- str_extract(opt$config, "[A-Z]{3}")

# Prepare out file names for each output type
obs_stats_filename <- str_c(out_dir, "/", country, "_obs_stats.csv")
runtimes_filename <- str_c(out_dir, "/", country, "_model_runtimes.csv")
case_rast_filename <- str_c(out_dir, "/", country, "_case_rast.rds")
case_comp_filename <- str_c(out_dir, "/", country, "_case_comp.csv")
rhat_filename <- str_c(out_dir, "/", country, "_case_rhats.csv")
who_filename <- str_c(out_dir, "/", country, "_who_comp.csv")
spatial_coverage_filename <- str_c(out_dir, "/", country, "_spatial_coverage.rds")
betas_filename <- str_c(out_dir, "/", country, "_betas.csv")
# Aggregation parameter
negbinom_filename <- str_c(out_dir, "/", country, "_negbinom_k.csv") 
# Get observations --------------------------------------------------------
sf_cases <- taxdat::read_file_of_type(preprocessed_data_filename, "sf_cases")
sf_cases_resized <- taxdat::read_file_of_type(stan_input_filename, "stan_input")$sf_cases_resized

# Get stan object
model.rand <- taxdat::read_file_of_type(stan_output_filename, "model.rand")

# Model runtime
stan_runtimes <- rstan::get_elapsed_time(model.rand) %>% 
  as_tibble() %>% 
  mutate(chain = row_number())

write_csv(stan_runtimes %>% mutate(country = country), path = runtimes_filename)

# Get case data statistics
obs_stats <- taxdat::get_obs_stats(sf_cases_resized)

# Write to file
write_csv(obs_stats %>% mutate(country = country), path = obs_stats_filename)

# Extract the case raster -------------------------------------------------
if (!file.exists(case_rast_filename) | opt$redo) {
  case_raster <- taxdat::get_case_raster(preprocessed_data_filename = file_names["data"],
                                         covar_data_filename = file_names["covar"],
                                         model_output_filenames = file_names["stan_output"])
  
  colnames(case_raster)[8:9] <- c("modeled cases", "modeled rates")
  
  # Write to file
  saveRDS(case_raster %>% mutate(country = country), file = case_rast_filename)
}

# Cases vs. modeled ---------------------------------------------------------

if (!file.exists(case_comp_filename) | opt$redo) {
  # Extract comparison between modeled an observed cases
  data_fidelity <- taxdat::get_data_fidelity(stan_output_filename)[[1]] %>% 
    as_tibble()  %>% 
    mutate(country = country) %>% 
    set_colnames(c("chain", "param", "modeled", "actual", "country"))
  
  write_csv(data_fidelity, 
            path = case_comp_filename)
} else {
  data_fidelity <- read_csv(case_comp_filename)
}

# Rhats -------------------------------------------------------------------

if (!file.exists(rhat_filename) | opt$redo) {
  # Extract the Rhats for modeled cases
  rhat_obs <- rstan::summary(model.rand, pars = "modeled_cases")$summary %>% 
    as_tibble() %>% 
    rename(q025 = `2.5%`,
           q975 = `97.5%`) %>% 
    dplyr::select(mean, q025, q975, Rhat) %>% 
    mutate(obs = row_number()) %>% 
    # Join with the information on the data
    inner_join(
      aggregated_data <- tibble(
        obs = seq_along(stan_input$stan_data$y),
        y = stan_input$stan_data$y,
        censoring = stan_input$stan_data$censoring_inds
      ) %>%  
        inner_join(
          tibble(
            obs = stan_input$stan_data$map_obs_loctime_obs,
            lp = stan_input$stan_data$u_loctime[stan_input$stan_data$map_obs_loctime_loc],
            tfrac = stan_input$stan_data$tfrac
          )
        ) %>%
        inner_join(covar_cube_output$location_periods_dict %>%
                     group_by(location_period_id, loctime_id, t) %>%
                     summarise(n_cell = n()),
                   by = c("lp" = "loctime_id")) %>%
        mutate(location_period_id = as.character(location_period_id)))
  
  # Add mean per chain and maximum diference
  rhat_obs <- rhat_obs %>% 
    inner_join(
      data_fidelity %>% 
        group_by(param) %>% 
        summarise(max_diff_chains = diff(range(modeled))) %>% 
        mutate(obs = str_extract(as.character(param), "[0-9]+") %>% as.numeric()) %>% 
        select(-param)
    )
  
  write_csv(rhat_obs %>% mutate(country = country), path = rhat_filename)
}


# Spatial coverage --------------------------------------------------------
if (!file.exists(spatial_coverage_filename) | opt$redo) {
  sf_lp_unique <- sf_cases_resized %>%
    mutate(obs = row_number()) %>% 
    inner_join(rhat_obs %>% select(obs, t)) %>% 
    group_by(t, locationPeriod_id) %>% 
    mutate(i = row_number(),
           n = n()) %>% 
    filter(i == 1) %>% 
    select(locationPeriod_id, n, t)
  
  sf_cases_grids <- foreach(i = 1:nrow(sf_lp_unique),
                            .combine = rbind) %do%
    {
      sf_lp_unique[i,] %>% 
        st_make_grid(cellsize = 0.1, what = "centers") %>% 
        st_as_sf() %>% 
        mutate(t = sf_lp_unique$t[i],
               n =  sf_lp_unique$n[i],
               lp_id = sf_lp_unique$locationPeriod_id[i],
               cell_id = str_c(lp_id, "-", row_number()))
      
    }
  
  u_coords <- st_coordinates(sf_cases_grids)
  sf_cases_grids <- sf_cases_grids %>% 
    mutate(long = u_coords[,1],
           lat = u_coords[,2])
  
  sf_cases_grids <- sf_cases_grids %>% 
    as_tibble() %>% 
    select(-x) %>% 
    group_by(t, lp_id) %>% 
    group_map(function(x, y) do.call(rbind, map(1:x$n, ~ x)) %>% cbind(y)) %>% 
    bind_rows()
  
  saveRDS(sf_cases_grids %>% mutate(country = country), file = spatial_coverage_filename)
}

# WHO cases ---------------------------------------------------------------
if (!file.exists(who_filename) | opt$redo) {
  who_annual_cases <- sf_cases_resized
  chains <- rstan::extract(model.rand)
  who_annual_cases$modeled <- apply(chains$modeled_cases, 2, mean)
  who_annual_cases$observed <- who_annual_cases$attributes.fields.suspected_cases # fix me
  who_annual_cases <- taxdat::pull_output_by_source(sf_cases = who_annual_cases, 
                                                    source_match = "%WHO Annual Cholera Reports%",
                                                    database_api_key_rfile = str_c(opt$cholera_directory,
                                                                                   "/Analysis/R/database_api_key.R"))
  who_annual_cases %>%
    as.data.frame() %>%
    dplyr::select(OC_UID, TL, TR, observed, modeled) %>% 
    dplyr::mutate(country = country) %>% 
    write_csv(path = who_filename)
}


# Covariate coefficients --------------------------------------------------
if (!file.exists(betas_filename) | opt$redo) {
  betas <- rstan::summary(model.rand, pars = "betas")$summary[, c(1, 4:10)] %>% 
    as.data.frame() %>% 
    mutate(param = rownames(.),
           covar = str_extract(param, "[0-9]+") %>% as.numeric(),
           covar = dimnames(covar_cube_output$covar_cube)[[3]][-1][covar],
           param = str_extract(param, "(.*)(?=\\[)"))
  
  betas %>% 
    mutate(country = country) %>% 
    write_csv(path = betas_filename)
}

# Aggregation --------------------------------------------------
if ("phi" %in% attr(model.rand, "model_pars") &(!file.exists(negbinom_filename) | opt$redo)) {
  
  phi <- rstan::summary(model.rand, pars = "phi")$summary[, c(1, 4:10)] 
  
  phi %>% 
    as.list() %>% 
    as.data.frame() %>% 
    set_colnames(c("mean", "q025", "q25", "q50", "q75", "q975", "neff", "Rhat")) %>% 
    mutate(country = country) %>% 
    write_csv(path = negbinom_filename)
}
