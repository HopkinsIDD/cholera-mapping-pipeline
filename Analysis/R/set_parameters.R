## Preamble ------------------------------------------------------------------------------------------------------------

### Set Error Handling
if (Sys.getenv("INTERACTIVE_RUN", FALSE)) {
  options(warn = 1, error = recover)
} else {
  options(
    warn = 1,
    error = function(...) {
      quit(..., status = 2)
    }
  )
}

library(magrittr)

### Run options

option_list <- list(
  optparse::make_option(
    c("-c", "--config"),
    action = "store",
    default = Sys.getenv("CHOLERA_CONFIG", "config.yml"),
    type = "character",
    help = "Model run configuration file"
  ),
  optparse::make_option(c("-d", "--cholera_directory"), action = "store", default = NULL, type="character", help = "Cholera directory"),
  optparse::make_option(c("-l", "--layers_directory"), action = "store", default = NULL, type="character", help = "Layers directory"),
  optparse::make_option(c("-v", "--verbose"), action = "store", default = FALSE, type="logical", help = "Print extra messages")
)

opt <- optparse::OptionParser(option_list = option_list) %>% optparse::parse_args()

### Read config file
config <- yaml::read_yaml(opt[["config"]], eval.expr = TRUE)


### Define relevent directories
try({setwd(utils::getSrcDirectory())}, silent = TRUE)
try({setwd(dirname(rstudioapi::getActiveDocumentContext()$path))}, silent = TRUE)
# Where is the repository
cholera_directory <- ifelse(is.null(opt$cholera_directory),
                            rprojroot::find_root(rprojroot::has_file(".choldir")),
                            opt$cholera_directory)


if (as.logical(Sys.getenv("PRODUCTION_RUN", TRUE)) && (nrow(gert::git_status(repo=cholera_directory)) != 0)) {
  print(gert::git_status(repo=cholera_directory))
  stop("There are local changes to the repository.  This is not allowed for a production run. Please revert or commit local changes")
}


# Relative to the repository, where is the layers directory
laydir <- ifelse(is.null(opt$layers_directory),
                 rprojroot::find_root_file("Layers", criterion = rprojroot::has_file(".choldir")),
                 opt$layers_directory)

# s2 has different ideas about geometry validity than postgis does
sf::sf_use_s2(FALSE)

# Admin boundaries are read from a GeoPackage cache (compute nodes are offline)
if (Sys.getenv("CHOLERA_AOI_CACHE_DIR") == "") {
  Sys.setenv(CHOLERA_AOI_CACHE_DIR = file.path(laydir, "admin_units"))
}

## Inputs --------------------------------------------------------------------------------------------------------------
print("---- Reading Parameters ----\n")

# - - - - - - - - - - - - - -
## BASIC MODEL SPECIFICATION
# - - - - - - - - - - - - - -
### Countries over which to run the model
#### For sql, use numeric ids
#### For api, use scoped string names
countries <- config$countries
countries_name <- taxdat::check_countries_name(config$countries_name)
# Area of interest: "raw" (no crop) or an ISO3 code; rasters are cropped and
# masked to that country's boundary plus aoi_buffer_km
aoi <- taxdat::check_aoi(config$aoi)
aoi_buffer_km <- taxdat::check_aoi_buffer_km(config$aoi_buffer_km, aoi = aoi)

# - - - -
### Grid Size
# km by km resolution of analysis
res_space <- taxdat::check_res_space(config$res_space)
# temporal resolution of analysis
res_time <- taxdat::check_res_time(config$res_time)
# number of time slices in spatial random effect
grid_rand_effects_N <- taxdat::check_grid_rand_effects_N(config$grid_rand_effects_N)

# - - - -
# What case definition should be used
case_definition <- taxdat::check_case_definition(config$case_definition)
# Determine the column name that the number of cases is stored in
# TODO check the flag for use_database
cases_column <- taxdat::case_definition_to_column_name(case_definition,
                                                       database = T)

# - - - -
### Get various functions to convert between time units and dates
# Function to convert from date to temporal grid
time_change_func <- taxdat::time_unit_to_aggregate_function(res_time)
# Function to convert from temporal grid to start date
aggregate_to_start <- taxdat::time_unit_to_start_function(res_time)
# Function to convert from temporal grid to end date
aggregate_to_end <- taxdat::time_unit_to_end_function(res_time)

# - - - -
# What range of times should be considered?
start_time <- lubridate::ymd(taxdat::check_time(config$start_time))
end_time <- lubridate::ymd(taxdat::check_time(config$end_time))

taxdat::check_model_date_range(start_time = start_time,
                               end_time = end_time,
                               time_change_func = time_change_func,
                               aggregate_to_start = aggregate_to_start,
                               aggregate_to_end = aggregate_to_end)

# - - - -
# Define modeling time slices (set of time periods at which the data generating process occurs)
time_slices <- taxdat::modeling_time_slices(start_time = start_time,
                                            end_time = end_time,
                                            res_time = res_time,
                                            time_change_func = time_change_func,
                                            aggregate_to_start = aggregate_to_start,
                                            aggregate_to_end = aggregate_to_end)

# - - - -
### Source for cholera data
#### either the taxonomy website of an sql call
data_source <- taxdat::check_data_source(config$data_source)
ovrt_metadata_table <- taxdat::check_ovrt_metadata_table(config$ovrt_metadata_table)

# - - - -
### Optional arguments
# Specific OCs to use (string vector)
OCs <- config$OCs
taxonomy <- taxdat::check_taxonomy(config$taxonomy)

# - - - - - - - - - - - - - -
## MODEL STRUCTURE AND PRIORS
# - - - - - - - - - - - - - -
# Model covariates
# Get the dictionary of covariates
covariate_dict <- yaml::read_yaml(paste0(laydir, "/covariate_dictionary.yml"))
all_covariate_choices <- names(covariate_dict)
short_covariate_choices <- purrr::map_chr(covariate_dict, "abbr")

if (is.null(config$covariate_choices)) {
  # Case when model is run with random effects only
  short_covariates <- NULL
  print("---- Running with no covariates (spatial random effects only)")
} else {
  # User-defined covariates names and abbreviations
  covariate_choices <- taxdat::check_covariate_choices(covar_choices = config$covariate_choices, available_choices = all_covariate_choices)
  short_covariates <- short_covariate_choices[covariate_choices]
}

# - - - -
### Observation model settings
obs_model <- taxdat::check_obs_model(config$obs_model)

# SD of prior of admin0 inverse overdispersion parameter
inv_od_sd_adm0 <- taxdat::check_od_param_sd_prior_adm0(
  inv_od_sd_adm0 = config$inv_od_sd_adm0,
  obs_model = obs_model)

# SD of prior of subnational inverse overdispersion parameters when no pooling
inv_od_sd_nopool <- taxdat::check_od_param_sd_prior_nopooling(
  inv_od_sd_nopool = config$inv_od_sd_nopool,
  obs_model = obs_model)


# mu_alpha and sd_alpha are the mean and sd of the intercept prior, respectively
mu_alpha <- taxdat::check_mu_alpha(config$mu_alpha)
sd_alpha <- taxdat::check_sd_alpha(config$sd_alpha)

# Priors for spatial sd
mu_sd_w <- taxdat::check_mu_sd_w(config$mu_sd_w)
sd_sd_w <- taxdat::check_sd_sd_w(config$sd_sd_w)
do_sd_w_mixture <- taxdat::check_do_sd_w_mixture(config$do_sd_w_mixture)
use_rho_prior <- taxdat::check_use_rho_prior(config$use_rho_prior)

# Intercept structure
time_effect <- taxdat::check_time_effect(config$time_effect)
time_effect_autocorr <- taxdat::check_time_effect_autocorr(config$time_effect_autocorr)
use_intercept <- taxdat::check_use_intercept(config$use_intercept)

# - - - - - - - - - - - - - -
## PRIORS
# - - - - - - - - - - - - - -
sigma_eta_scale <- taxdat::check_sigma_eta_scale(config$sigma_eta_scale)
beta_sigma_scale <- taxdat::check_beta_sigma_scale(config$beta_sigma_scale)
exp_prior <- taxdat::check_exp_prior(config$exp_prior)
do_infer_sd_eta <- taxdat::check_do_infer_sd_eta(config$do_infer_sd_eta)
do_zerosum_cnst <- taxdat::check_do_zerosum_cnst(config$do_zerosum_cnst)
use_weights <- taxdat::check_use_weights(config$use_weights)

# - - - - - - - - - - - - - -
## GAM WARMUP
# - - - - - - - - - - - - - -
warmup <- taxdat::check_warmup(config$warmup)
covar_warmup <- taxdat::check_covar_warmup(config$covar_warmup)

# - - - - - - - - - - - - - -
## OBSERVATION DATA PROCESSING
# - - - - - - - - - - - - - -
# Should observations be aggregated within the modeling time slice?
aggregate <- taxdat::check_aggregate(config$aggregate)
# Is there a threshold on tfrac?
tfrac_thresh <- taxdat::check_tfrac_thresh(config$tfrac_thresh)
# Should observations below censoring_thresh contribute to the likelihood as censored observations?
censoring <- taxdat::check_censoring(config$censoring)
# Set censoring thresh
censoring_thresh <- taxdat::check_censoring_thresh(config$censoring_thresh)
# User-specified value to set tfrac
set_tfrac <- taxdat::check_set_tfrac(config$set_tfrac)
# Tolerance for snap_to_period function
snap_tol <- taxdat::check_snap_tol(snap_tol = config$snap_tol,
                                   res_time = res_time)
ncpus_parallel_prep <- taxdat::check_ncpus_parallel_prep(config$ncpus_parallel_prep)
do_parallel_prep <- taxdat::check_do_parallel_prep(config$do_parallel_prep)

# Drop multi-year data at the national level
drop_multiyear_adm0 <- taxdat::check_drop_multiyear_adm0(config$drop_multiyear_adm0)

# Drop censored amd0-level observations
drop_censored_adm0 <- taxdat::check_drop_censored_adm0(config$drop_censored_adm0)
drop_censored_adm0_thresh <- taxdat::check_drop_censored_adm0_thresh(config$drop_censored_adm0_thresh)

# Drop full amd0-level observations across OCs
drop_full_nat_obs_xOC <- taxdat::check_drop_full_nat_obs_xOC(config$drop_full_nat_obs_xOC)
drop_full_nat_obs_xOC_thresh <- taxdat::check_drop_full_nat_obs_xOC_thresh(config$drop_full_nat_obs_xOC_thresh)

# Drop location periods with population below a specific threshold
drop_low_pop_lps <- taxdat::check_drop_low_pop_lps(config$drop_low_pop_lps)
drop_low_pop_lps_thresh <- taxdat::check_drop_low_pop_lps_thresh(config$drop_low_pop_lps_thresh)

# - - - - - - - - - - - - - -
## SPATIAL GRID SETTINGS
# - - - - - - - - - - - - - -
# turn on subgrid feature
use_pop_weight <- taxdat::check_use_pop_weight(config$use_pop_weight)
# drop cells from master grid if the proportion of spatial overlap (based on pop-weighted area) does not exceed sfrac_thresh_border
sfrac_thresh_border <- taxdat::check_sfrac_thresh_border(config$sfrac_thresh_border)
# drop cells from location period if proportion of spatial overlap does not exceed sfrac_thresh_conn
sfrac_thresh_conn <- taxdat::check_sfrac_thresh_conn(config$sfrac_thresh_conn)

# Whether to include the spatial random effect in the model
spatial_effect <- taxdat::check_spatial_effect(config$spatial_effect)

# - - - - - - - - - - - - - -
## COVARIATE INGESTION
# - - - - - - - - - - - - - -
# ingest covariates to a specific aggregation
ingest_covariates <- taxdat::check_ingest_covariates(config$ingest_covariates)
# create a new metadata table for newly ingested covariates
ingest_new_covariates <- taxdat::check_ingest_new_covariates(config$ingest_new_covariates)

# Whether to adjust the population counts to UN estimates
adjust_pop_UN <- taxdat::check_adjust_pop_UN(config$adjust_pop_UN)

# - - - - - - - - - - - - - -
## STAN PARAMETERS
# - - - - - - - - - - - - - -
debug <- taxdat::check_stan_debug(config$debug)

# Pull default stan model options if not specified in config
stan_params <- taxdat::get_stan_parameters(append(config, config$stan))

# set number of cores and chains
ncores <- stan_params$ncores
nchain <- ncores
if(ncores == 1) {nchain = 2}
rstan::rstan_options(auto_write = FALSE)
options(mc.cores = ncores)
# set stan model
stan_dir <- paste0(cholera_directory, '/Analysis/Stan/')
stan_model <- stan_params$model
stan_model_path <- taxdat::check_stan_model(stan_model_path = paste(stan_dir, stan_model, sep=''),
                                            stan_dir = stan_dir)
# Generated quantities
stan_genquant <- stan_params$genquant
stan_genquant_path <- taxdat::check_stan_model(stan_model_path = paste(stan_dir, stan_genquant, sep=''), stan_dir = stan_dir)
# how many iterations
iter_warmup <- taxdat::check_stan_iter_warmup(config$stan$iter_warmup)
iter_sampling <- taxdat::check_stan_iter_sampling(config$stan$iter_sampling)

# Should the Stan model be recompiled?
recompile <- stan_params$recompile

# Should we be using a lower-triangular adjacency matrix
# (this needs to be the case for the DAGAR model)
lower_triangular_adjacency <- grepl('dagar', stan_model)

# - - - -
# Construct some additional parameters based on the above
# Testing things:
testing = all(grepl("testing",countries))
if(testing){
  # if(length(countries) > 1){
  #   stop("Do not mix testing and countries.  Do not use multiple tests at once")
  # }
  all_test_idx <- as.numeric(gsub('[.][^.]*$','',gsub('testing.','',countries)))
  original_niter <- as.numeric(gsub('.*[.]','',countries))
  if(any(is.na(all_test_idx))){
    stop("Do not mix test cases and countries")
  }
} else {
  # Fix country names
  if (!any(suppressWarnings(is.na(as.numeric(countries))))) {
    countries <- as.numeric(countries)
    message("Treating countries as location periods")
  } else {
    countries <- sapply(countries,taxdat::fix_country_name)
  }
  all_test_idx <- as.numeric(NA)
}

# Set admin levels for which to compute summary statistics
if (is.null(config$summary_admin_levels)) {
  if (testing) {
    config$summary_admin_levels <- NA
  } else {
    cat("-- Did not find specification for summary admin levels, setting to 0-1-2 \n")
    config$summary_admin_levels <- c(0, 1, 2)
  }
}

# - - - -
# cholera_covariates database: connection settings come from the libpq
# environment variables PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD (see
# taxdat::get_db_config). Pre-pulled observations can be supplied with
# CHOLERA_OBSERVATIONS_RDS (see Analysis/R/pull_observations_local.R).
observations_rds <- Sys.getenv("CHOLERA_OBSERVATIONS_RDS", "")
db_used <- FALSE



# Rewrite final config with runtime parameters -----------------------------
# Backup config provided by user
config_user <- config

# Update config parameters
for (param in names(taxdat::get_all_config_options())) {
  if (param != "stan"){
    if (exists(param)){
      config[[param]] <- get(param)
    } else if (!exists(param) & param %in% names(stan_params)){
      config[[param]] <- stan_params[[param]]
    }
  }
}
print("This is the explicit runtime config (printed for debugging).")
print(config)

# Remove environmental variables to keep clean
for (param in names(taxdat::get_all_config_options())) {
  if (param != "stan"){
    if (exists(param)){
      eval(parse(text = stringr::str_glue("rm({param})")))
    }
  }
}
# Pipeline steps ---------------------------------------------------------------

original_countries <- config$countries

for(t_idx in 1:length(all_test_idx)){
  gc()
  test_idx = all_test_idx[t_idx]
  if(!is.na(test_idx)){
    countries = original_countries[t_idx]
    niter = original_niter[t_idx]
  }
  # Name the output file
  if(testing){
    map_name <- paste("testing", test_idx, sep = '.')
  } else {
    if(is.null(config$countries_name)){
      if(length(config$countries) == 1){
        config$countries_name <- config$countries
      }
    }
    warning("We should revisit the way we name maps")
    map_name <- taxdat::make_map_name(config)
  }

  if (is.null(short_covariates)) {
    covariate_name_part <- "nocovar"
  } else {
    covariate_name_part <- paste(short_covariates, collapse = '-')
  }

  setwd(cholera_directory)
  dir.create("Analysis/output", showWarnings = FALSE)

  file_names <- taxdat::get_filenames(config=config, cholera_directory = cholera_directory,
                                      layers_dir = laydir)

  # Check if the file names are valid
  new_file_names<-file_names[!sapply(file_names,file.exists)]
  if(!all(sapply(new_file_names,file.create))){
    stop(paste(names(sapply(new_file_names,file.create)[!sapply(new_file_names,file.create)]),"file name is invalid!"))
  }
  sapply(new_file_names,file.remove)

  ## Step 1: process observation shapefiles and prepare data ##
  print(file_names[["data"]])
  if(file.exists(file_names[["data"]])){
    print("Data already preprocessed, skipping")
    warning("Data already preprocessed, skipping")
    load(file_names[["data"]])
  } else if(!testing){
    db_used <- TRUE
    aoi_obj <- taxdat::get_aoi(config$aoi, buffer_km = config$aoi_buffer_km)

    # First prepare the computation grid
    grid <- taxdat::prepare_grid(res_space = config$res_space, aoi = aoi_obj,
                                 layers_dir = laydir, ingest = config$ingest_covariates)
    full_grid_name <- grid$full_grid_name

    # Observations: pre-pulled file if given, else the taxonomy database
    cases <- if (nzchar(observations_rds)) {
      taxdat::load_observations_rds(observations_rds, config)
    } else {
      taxdat::pull_observations(config)
    }
    map_data <- taxdat::prepare_map_data(cases = cases, config = config,
                                         cases_column = cases_column,
                                         full_grid_name = full_grid_name)
    sf_cases <- map_data$sf_cases
    shapefiles <- map_data$shapefiles
    output_shapefiles <- map_data$output_shapefiles
    rm(cases, map_data)
    save(sf_cases, full_grid_name, shapefiles, output_shapefiles, file = file_names[["data"]])
  } else {
    source(paste(cholera_directory,"Analysis", "R", "create_standardized_testing_data.R",sep='/'))
  }

  ## Step 2: Extract the covariate cube and grid ##
  print(file_names[["covar"]])
  if (file.exists(file_names[["covar"]])) {
    print("Covariate cube already preprocessed, skipping")
    warning("Covariate cube already preprocessed, skipping")
    load(file_names[["covar"]])
  } else if(!testing){
    db_used <- TRUE
    conn_pg <- taxdat::connect_to_db()
    aoi_obj <- taxdat::get_aoi(config$aoi, buffer_km = config$aoi_buffer_km)
    grid <- taxdat::prepare_grid(res_space = config$res_space, aoi = aoi_obj,
                                 layers_dir = laydir, ingest = config$ingest_covariates,
                                 conn = conn_pg)
    if (grid$full_grid_name != full_grid_name) {
      stop("The cached data file was built on ", full_grid_name, " but the database now has ",
           grid$full_grid_name, ". Delete the data file to rebuild it.")
    }
    # The per-run tables are dropped at the end of each run; rebuild them if
    # only the covariate file is being regenerated
    taxdat::ensure_run_tables(conn_pg, config,
                              shapefiles = if (exists("shapefiles")) shapefiles else NULL,
                              output_shapefiles = output_shapefiles,
                              full_grid_name = full_grid_name)

    ## Step 2a: ingest the required covariates (population first)
    covar_list <- taxdat::prepare_covariates(
      covar_abbr = short_covariates,
      covar_dict = covariate_dict,
      layers_dir = laydir,
      res_space = config$res_space,
      res_time = config$res_time,
      grid = grid,
      aoi = aoi_obj,
      mode = taxdat::covariate_mode(config$ingest_covariates, config$ingest_new_covariates),
      conn = conn_pg
    )

    # Population weights use yearly population on the 1 km grid
    taxdat::prepare_population_1km(
      covar_dict = covariate_dict, layers_dir = laydir, aoi = aoi_obj,
      mode = taxdat::covariate_mode(config$ingest_covariates, config$ingest_new_covariates),
      conn = conn_pg)

    ## Step 2b: create the covar cube
    covar_cube_output <- taxdat::prepare_covar_cube(
      covar_list = covar_list,
      config = config,
      full_grid_name = full_grid_name,
      time_slices = time_slices,
      res_space = config$res_space,
      res_time = config$res_time,
      covariate_transformations = config[["covariate_transformations"]],
      sfrac_thresh_border = config$sfrac_thresh_border,
      sfrac_thresh_conn = config$sfrac_thresh_conn,
      conn = conn_pg
    )
    DBI::dbDisconnect(conn_pg)

    # Save results to file
    save(covar_cube_output, file = file_names[["covar"]])
  }

  inject_covar <- Sys.getenv("INJECT_COVAR", FALSE)
  if (as.logical(inject_covar)) {
    stop("Script set_parameter.R stop due to the setting of environment varialbe INJECT_COVAR")
  }

  ## Step 3: Prepare the stan input ##
  print(file_names[["stan_input"]])
  if(!file.exists(file_names[["stan_input"]])){
    source(paste(cholera_directory, "Analysis/R/prepare_stan_input.R", sep = "/"))

    stan_input <-  prepare_stan_input(
      cholera_directory = cholera_directory,
      ncore = ncores,
      res_time = config$res_time,
      res_space = config$res_space,
      time_slices = time_slices,
      grid_rand_effects_N = config$grid_rand_effects_N,
      cases_column = cases_column,
      sf_cases = sf_cases,
      non_na_gridcells = covar_cube_output$non_na_gridcells,
      sf_grid = covar_cube_output$sf_grid,
      location_periods_dict = covar_cube_output$location_periods_dict,
      covar_cube = covar_cube_output$covar_cube,
      opt = opt,
      stan_params = stan_params,
      debug = debug, 
      config = config
    )

    # Save data
    save(stan_input, file = file_names[["stan_input"]])
    sink(gsub('rdata','json', file_names[["stan_input"]]))
    cat(jsonlite::toJSON(stan_input$stan_data, auto_unbox=TRUE,matrix='rowmajor'))
    sink(NULL)

  } else {
    print("Stan input already created, skipping")
    warning("Stan input already created, skipping")
    load(file_names[["stan_input"]])
  }

  stan_data <- stan_input$stan_data
  sf_cases_resized <- stan_input$sf_cases_resized
  sf_grid <- stan_input$sf_grid
  # Cleanup
  rm(stan_input)

  ## Step 4: Prepare the initial conditions
  if(file.exists(file_names[["initial_values"]])){
    print("Initial_values already found, skipping")
    warning("Initial_values already found, skipping")
    load(file_names[["initial_values"]])
  } else {
    source(paste(cholera_directory,'Analysis','R','prepare_initial_values.R',sep='/'))
    recompile <- FALSE
  }

  # Cleanup
  rm(stan_data)
  rm(sf_cases_resized)
  rm(sf_grid)

  ## Step 5: Run the model
  print(file_names[["stan_output"]])
  if(file.exists(file_names[["stan_output"]])){
    print("Data already modeled, skipping")
    warning("Data already modeled, skipping")
    load(file_names[["stan_output"]])
  } else if (Sys.getenv("CHOLERA_SKIP_STAN","FALSE") == "TRUE") {
    print("Skipping stan model in accordance with the environment variable CHOLERA_SKIP_STAN.")
    warning("Skipping stan model in accordance with the environment variable CHOLERA_SKIP_STAN.")
  } else {
    source(paste(cholera_directory,'Analysis','R','run_stan_model.R',sep='/'))
    recompile <- FALSE
  }

  ## Step 6: Run the generated quantities
  print(file_names[["stan_genquant"]])
  if(file.exists(file_names[["stan_genquant"]])){
    print("Data already modeled, skipping")
    warning("Data already modeled, skipping")
    readRDS(file_names[["stan_genquant"]])
  } else if (Sys.getenv("CHOLERA_SKIP_STAN","FALSE") == "TRUE") {
    print("Skipping stan model in accordance with the environment variable CHOLERA_SKIP_STAN.")
    warning("Skipping stan model in accordance with the environment variable CHOLERA_SKIP_STAN.")
  } else {
    source(paste(cholera_directory,'Analysis','R','run_stan_genquant.R',sep='/'))
    recompile <- FALSE
  }

  # Drop this run's per-run tables (they are rebuilt from the data file if a
  # later stage is re-run)
  if (db_used) {
    taxdat::clean_all_tmp(config = config)
  }

}
