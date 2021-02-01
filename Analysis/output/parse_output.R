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

# Code options
option_list <- list(
  optparse::make_option(c("-c", "--config"), action = "store", default = Sys.getenv("CHOLERA_CONFIG", "config.yml"),
                        type = "character", help = "Model run configuration file"),
  optparse::make_option(c("-d", "--cholera_directory"), action = "store", default = NULL, 
                        type="character", help = "Cholera directory")
)

opt <- optparse::OptionParser(option_list = option_list) %>% optparse::parse_args()
opt$config <- "Analysis/configs/2015_2019_country/config_SSD_2015_2019.yml"
opt$cholera_directory <- "./"

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

# Get observations --------------------------------------------------------
sf_cases <- taxdat::read_file_of_type(preprocessed_data_filename, "sf_cases")
sf_cases_resized <- taxdat::read_file_of_type(stan_input_filename, "stan_input")$sf_cases_resized

# Model runtime
stan_runtimes <- rstan::get_elapsed_time(model.rand) %>% 
  as_tibble() %>% 
  mutate(chain = row_number())

write_csv(stan_runtimes %>% mutate(country = country), path = runtimes_filename)

# Get stan object
model.rand <- taxdat::read_file_of_type(stan_output_filename, "model.rand")

# Get case data statistics
obs_stats <- taxdat::get_obs_stats(sf_cases_resized)

# Write to file
write_csv(obs_stats %>% mutate(country = country), path = obs_stats_filename)

# Extract the case raster -------------------------------------------------
case_raster <- taxdat::get_case_raster(preprocessed_data_filename = file_names["data"],
                                       covar_data_filename = file_names["covar"],
                                       model_output_filenames = file_names["stan_output"])

colnames(case_raster)[8:9] <- c("modeled cases", "modeled rates")

# Write to file
saveRDS(case_raster %>% mutate(country = country), file = case_rast_filename)

# Cases vs. modeled ---------------------------------------------------------
# Extract comparison between modeled an observed cases
data_fidelity <- taxdat::get_data_fidelity(stan_output_filename)[[1]] %>% 
  as_tibble()

write_csv(data_fidelity %>% mutate(country = country) %>% set_colnames(c("chain", "param", "modeled", "actual", "country")), path = case_comp_filename)

# Rhats -------------------------------------------------------------------
# Extract the Rhats for modeled cases
rhat_obs <- rstan::summary(model.rand, pars = "modeled_cases")$summary %>% 
  as_tibble() %>% 
  dplyr::select(mean, Rhat) %>% 
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

write_csv(rhat_obs %>% mutate(country = country), path = rhat_filename)