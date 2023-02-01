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

if (Sys.getenv("PRODUCTION_RUN", TRUE)) {
  Sys.setenv("REINSTALL_TAXDAT" = TRUE)
}



### Libraries

# Install required packages
package_list <- c(
  "DBI",
  "digest",
  "dplyr",
  "foreach",
  "gdalUtils",
  "glue",
  "geojsonsf",
  "igraph",
  "inline",
  "itertools",
  "jsonlite",
  "lubridate",
  "magrittr",
  "mgcv",
  "ncdf4",
  "odbc",
  "optparse",
  "parallel",
  "posterior",
  "purrr",
  "RCurl",
  "R.utils",
  "raster",
  "rjson",
  "rstan",
  "rpostgis",
  "rts",
  "RPostgres",
  "sf",
  "spdep",
  "stringr",
  "tidync",
  "tibble",
  "zoo"
)

library(magrittr)

# for (package in package_list) {
#   if (!require(package = package, character.only = T)) {
#     install.packages(pkgs = package)
#     library(package = package, character.only = T)
#   }
#   detach(pos = which(grepl(package, search())), force = T)
# }




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


if (!as.logical(Sys.getenv("CHOLERA_ON_MARCC",FALSE))) {
  if (as.logical(Sys.getenv("PRODUCTION_RUN", TRUE)) && (nrow(gert::git_status(repo=cholera_directory)) != 0)) {
    print(gert::git_status(repo=cholera_directory))
    Sys.setenv(REINSTALL_TAXDAT = TRUE)
    stop("There are local changes to the repository.  This is not allowed for a production run. Please revert or commit local changes")
  }

  if (Sys.getenv("REINSTALL_TAXDAT", FALSE)) {
    install.packages(paste0(cholera_directory, "packages/taxdat"), type = "source", repos = NULL)
  } else if (!require(taxdat)) {
    install.packages(paste0(cholera_directory, "packages/taxdat"), type = "source", repos = NULL)
  } else {
    detach("package:taxdat")
  }
}


# Relative to the repository, where is the layers directory
laydir <- ifelse(is.null(opt$layers_directory),
                 rprojroot::find_root_file("Layers", criterion = rprojroot::has_file(".choldir")),
                 opt$layers_directory)

# s2 has different ideas about geometry validity than postgis does
if (!as.logical(Sys.getenv("CHOLERA_ON_MARCC",FALSE))) {
  sf::sf_use_s2(FALSE)
}

## Inputs --------------------------------------------------------------------------------------------------------------
print("---- Reading Parameters ----\n")

# - - - -
### Source for cholera data
#### either the taxonomy website of an sql call
data_source <- config$data_source

# - - - -
### Countries over which to run the model
#### For sql, use numeric ids
#### For api, use scoped string names
countries <- config$countries
countries_name <- config$countries_name
# Specific OCs to use (string vector)
filter_OCs <- config$OCs

# - - - -
### Grid Size
# km by km resolution of analysis
res_space <- as.numeric(config$res_space)
# temporal resolution of analysis
res_time <- taxdat::check_time_res(config$res_time)
# number of time slices in spatial random effect
grid_rand_effects_N <- taxdat::check_grid_rand_effects_N(config$grid_rand_effects_N)
sfrac_thresh <- taxdat::check_sfrac_thresh(config$sfrac_thresh)

# - - - -
### Get various functions to convert between time units and dates
# Function to convert from date to temporal grid
time_change_func <- taxdat::time_unit_to_aggregate_function(res_time)
# Function to convert from temporal grid to start date
aggregate_to_start <- taxdat::time_unit_to_start_function(res_time)
# Function to convert from temporal grid to end date
aggregate_to_end <- taxdat::time_unit_to_end_function(res_time)
# Tolerance for snap_to_period function
snap_tol <- taxdat::check_snap_tol(snap_tol = config$snap_tol, 
                                   res_time = res_time)

# - - - -
# What case definition should be used
suspected_or_confirmed <- taxdat::check_case_definition(config$case_definition)
# Determine the column name that the number of cases is stored in
# TODO check the flag for use_database
cases_column <- taxdat::case_definition_to_column_name(suspected_or_confirmed,
                                                       database = T)
# - - - -
# Is there a threshold on tfrac?
tfrac_thresh <- taxdat::check_tfrac_thresh(config$tfrac_thresh)

# - - - -
# User-specified value to set tfrac
set_tfrac <- taxdat::check_set_tfrac(config$set_tfrac)

# - - - -
# What range of times should be considered?
start_time <- lubridate::ymd(config$start_time)
end_time <- lubridate::ymd(config$end_time)

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
# Data processing
aggregate <- taxdat::check_aggregate(config$aggregate)

# - - - -
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
  covariate_choices <- taxdat::check_covariate_choices(covar_choices = config$covariate_choices,
                                                       available_choices = all_covariate_choices)
  short_covariates <- short_covariate_choices[covariate_choices]
}

# - - - -
# STAN parameters
debug <- taxdat::check_stan_debug(config$debug)
ncore <- config$stan$ncores
nchain <- ncore
if(ncore == 1) {nchain = 2}
rstan::rstan_options(auto_write = FALSE)
options(mc.cores = ncore)
niter <- config$stan$niter
stan_dir <- paste0(cholera_directory, '/Analysis/Stan/')
stan_model <- config$stan$model
stan_model_path <- taxdat::check_stan_model(stan_model_path = paste(stan_dir, stan_model, sep=''),
                                            stan_dir = stan_dir)
# Generated quantities
stan_genquant <- config$stan$genquant
stan_genquant_path <- taxdat::check_stan_model(stan_model_path = paste(stan_dir, stan_genquant, sep=''),
                                               stan_dir = stan_dir)

# Should we be using a lower-triangular adjacency matrix
# (this needs to be the case for the DAGAR model)
lower_triangular_adjacency <- grepl('dagar', stan_model)

# Stan model options
stan_params <- taxdat::get_stan_parameters(config)

# Should the Stan model be recompiled?
recompile <- config$stan$recompile

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
# cholera_covariates database connection settings
# Get username of user (docker doesn't provide username so default to app)
dbuser <- Sys.getenv("USER", "app")



# Rewrite final config with runtime parameters -----------------------------
# Backup config provided by user
config_user <- config

# Update config parameters
for (param in names(taxdat::get_all_config_options())) {
  if (exists(param) & param != "stan") {
    config[[param]] <- get(param)
  }
}


# Pipeline steps ---------------------------------------------------------------

original_countries <- countries

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
      if(length(countries) == 1){
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
  
  # Load dictionary of configuration options
  config_dict <- yaml::read_yaml(paste0(cholera_directory, "/Analysis/configs/config_dictionary.yml"))
  
  file_names <- taxdat::get_filenames(config=config, cholera_directory = cholera_directory)
  
  # Check if the file names are valid
  new_file_names<-file_names[!sapply(file_names,file.exists)]
  if(!all(sapply(new_file_names,file.create))){
  stop(paste(names(sapply(new_file_names,file.create)[!sapply(new_file_names,file.create)]),"file name is invalid!"))
  }
  sapply(new_file_names,file.remove)

  # Preparation: Load auxillary functions
  # source(stringr::str_c(cholera_directory, "/Analysis/R/covariate_helpers.R"))
  
  ## Step 1: process observation shapefiles and prepare data ##
  print(file_names[["data"]])
  if(file.exists(file_names[["data"]])){
    print("Data already preprocessed, skipping")
    warning("Data already preprocessed, skipping")
    load(file_names[["data"]])
  } else if(!testing){
    if (as.logical(Sys.getenv("CHOLERA_ON_MARCC",FALSE))) {
      print(normalizePath(file_names[["data"]]))
      stop("This shouldn't run on marcc")
    }
	 
    source(paste(cholera_directory, 'Analysis', 'R', 'prepare_grid.R', sep='/'))
    
    # First prepare the computation grid and get the grid name
    full_grid_name <- prepare_grid(
      dbuser = dbuser,
      cholera_directory = cholera_directory,
      res_space = res_space,
      ingest = config$ingest_covariates
    )
    
    # Pull data from taxonomy database (either using the API or SQL)
    source(paste(cholera_directory,'Analysis','R','prepare_map_data_revised.R',sep='/'))
    
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
    if (as.logical(Sys.getenv("CHOLERA_ON_MARCC",FALSE))) {
      stop("This shouldn't run on marcc")
    }
    
    # Note: the first covariate is always the population raster
    ## Step 2a: ingest the required covariates ##
    # Load the function
    source(paste(cholera_directory, 'Analysis', 'R', 'prepare_covariates.R', sep='/'))
    
    # Run covariate preparation. The function return the list of covariate names
    # included in the model
    covar_list <- prepare_covariates(
      dbuser = dbuser,
      cholera_covariates_directory = laydir,
      res_space = res_space,
      res_time = res_time,
      ingest = config$ingest_covariates,
      do_parallel = F,
      ovrt_covar = config$ingest_new_covariates,
      ovrt_metadata_table = config$ovrt_metadata_table,
      redo_metadata = config$ingest_new_covariates,
      covar = paste(c('p', short_covariates), collapse = ','),  # add population as first covariate
      full_grid_name = full_grid_name,
      aoi_name = config$aoi
    )
    
    ## Step 2b: create the covar cube
    source(paste(cholera_directory, "Analysis/R/prepare_covar_cube.R", sep = "/"))
    
    covar_cube_output <- prepare_covar_cube(
      covar_list = covar_list,
      dbuser = dbuser,
      map_name = map_name,
      cholera_directory = cholera_directory,
      full_grid_name = full_grid_name,
      start_time = start_time,
      end_time = end_time,
      res_space = res_space,
      res_time = res_time,
      username = dbuser,
      covariate_transformations = config[["covariate_transformations"]],
      sfrac_thresh = sfrac_thresh
    )
    
    # Save results to file
    save(covar_cube_output, file = file_names[["covar"]])
  }
  
  ## Step 3: Prepare the stan input ##
  print(file_names[["stan_input"]])
  if(!file.exists(file_names[["stan_input"]])){
    if (as.logical(Sys.getenv("CHOLERA_ON_MARCC",FALSE))) {
      stop("This shouldn't run on marcc")
    }
    source(paste(cholera_directory, "Analysis/R/prepare_stan_input.R", sep = "/"))
    
    stan_input <-  prepare_stan_input(
      dbuser = dbuser,
      cholera_directory = cholera_directory,
      ncore = ncore,
      res_time = res_time,
      res_space = res_space,
      time_slices = time_slices,
      grid_rand_effects_N = grid_rand_effects_N,
      cases_column = cases_column,
      sf_cases = sf_cases,
      non_na_gridcells = covar_cube_output$non_na_gridcells,
      sf_grid = covar_cube_output$sf_grid,
      location_periods_dict = covar_cube_output$location_periods_dict,
      covar_cube = covar_cube_output$covar_cube,
      set_tfrac = set_tfrac,
      tfrac_thresh = tfrac_thresh,
      snap_tol = snap_tol,
      opt = opt,
      stan_params = stan_params,
      aggregate = aggregate,
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
  }
  load(file_names[["stan_input"]])
  
  stan_data <- stan_input$stan_data
  sf_cases_resized <- stan_input$sf_cases_resized
  sf_grid <- stan_input$sf_grid
  
  ## Step 4: Prepare the initial conditions
  if(file.exists(file_names[["initial_values"]])){
    print("Initial_values already found, skipping")
    warning("Initial_values already found, skipping")
  } else {
    if (as.logical(Sys.getenv("CHOLERA_ON_MARCC",FALSE))) {
      stop("This shouldn't run on marcc")
    }
    source(paste(cholera_directory,'Analysis','R','prepare_initial_values.R',sep='/'))
    recompile <- FALSE
  }
  load(file_names[["initial_values"]])
  
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
  
  if (!as.logical(Sys.getenv("CHOLERA_ON_MARCC",FALSE))) {
    taxdat::clean_all_tmp(dbuser = dbuser, map_name = map_name)
  }
  
}
