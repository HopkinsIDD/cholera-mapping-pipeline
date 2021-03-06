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



### Libraries

if (!require(taxdat)) {
  install.packages("packages/taxdat", type = "source", repos = NULL)
} else {
  detach("package:taxdat")
}
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
  optparse::make_option(c("-l", "--layers_directory"), action = "store", default = NULL, type="character", help = "Layers directory")
)

opt <- optparse::OptionParser(option_list = option_list) %>% optparse::parse_args()

### Read config file
config <- yaml::read_yaml(opt$config)

### Define relevent directories
try({setwd(utils::getSrcDirectory())}, silent = TRUE)
try({setwd(dirname(rstudioapi::getActiveDocumentContext()$path))}, silent = TRUE)
# Where is the repository
cholera_directory <- ifelse(is.null(opt$cholera_directory), 
                            rprojroot::find_root(rprojroot::has_file(".choldir")),
                            opt$cholera_directory)

# Relative to the repository, where is the layers directory
laydir <- ifelse(is.null(opt$layers_directory),
                 rprojroot::find_root_file("Layers", criterion = rprojroot::has_file(".choldir")),
                 opt$layers_directory)  


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

# - - - -
### Grid Size
# km by km resolution of analysis
res_space <- as.numeric(config$res_space)
# temporal resolution of analysis
res_time <- taxdat::check_time_res(config$res_time)
# number of time slices in spatial random effect
smooth_covariate_number_timesteps <- config$smoothing_period

# - - - -
### Get various functions to convert between time units and dates
# Function to convert from date to temporal grid
time_change_func <- taxdat::time_unit_to_aggregate_function(res_time)
# Function to convert from temporal grid to start date
aggregate_to_start <- taxdat::time_unit_to_start_function(res_time)
# Function to convert from temporal grid to end date
aggregate_to_end <- taxdat::time_unit_to_end_function(res_time)

# - - - -
# What case definition should be used
suspected_or_confirmed <- taxdat::check_case_definition(config$case_definition)
# Determine the column name that the number of cases is stored in
# TODO check the flag for use_database
cases_column <- taxdat::case_definition_to_column_name(suspected_or_confirmed,
                                                       database = T)
# - - - -
# Is there a threshold on tfrac?
if (!is.null(config$tfrac_thresh)) {
  cat("---- Running with tfrac threshold of", config$tfrac_thresh, "\n")
} else {
  cat("---- No tfrac thershold used\n")
}

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
# Model covariates
# Get the dictionary of covariates
covariate_dict <- yaml::read_yaml(paste0(laydir, "/covariate_dictionary.yml"))
all_covariate_choices <- names(covariate_dict)
short_covariate_choices <- purrr::map_chr(covariate_dict, "abbr")

# User-defined covariates names and abbreviations
covariate_choices <- taxdat::check_covariate_choices(covar_choices = config$covariate_choices,
                                                     available_choices = all_covariate_choices)
short_covariates <- short_covariate_choices[covariate_choices]

# - - - -
# STAN parameters
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
  print("Path b")
  # Fix country names
  if (!any(suppressWarnings(is.na(as.numeric(countries))))) {
    countries <- as.numeric(countries)
    message("Treating countries as location periods")
  } else {
    countries <- sapply(countries,taxdat::fix_country_name)
  }
  all_test_idx <- as.numeric(NA)
}


# - - - -
# cholera_covariates database connection settings
# Get username of user (docker doesn't provide username so default to app)
dbuser <- Sys.getenv("USER", "app")

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
  
  covariate_name_part <- paste(short_covariates, collapse = '-')
  setwd(cholera_directory)
  dir.create("Analysis/output", showWarnings = FALSE)
  
  # Load dictionary of configuration options
  config_dict <- yaml::read_yaml(paste0(cholera_directory, "/Analysis/configs/config_dictionary.yml"))
  
  file_names <- taxdat::get_filenames(config=config, cholera_directory = cholera_directory)
  
  # Preparation: Load auxillary functions
  # source(stringr::str_c(cholera_directory, "/Analysis/R/covariate_helpers.R"))
  
  ## Step 1: process observation shapefiles and prepare data ##
  print(file_names[["data"]])
  if(file.exists(file_names[["data"]])){
    print("Data already preprocessed, skipping")
    warning("Data already preprocessed, skipping")
    load(file_names[["data"]])
  } else if(!testing){
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
      username = dbuser
    )
    
    # Save results to file
    save(covar_cube_output, file = file_names[["covar"]])
  }
  print("NCOVAR")
  print(length(covariate_choices))
  print("NCOVAR")
  
  ## Step 3: Prepare the stan input ##
  print(file_names[["stan_input"]])
  if(!file.exists(file_names[["stan_input"]])){
    source(paste(cholera_directory, "Analysis/R/prepare_stan_input.R", sep = "/"))
    
    stan_input <-  prepare_stan_input(
      dbuser = dbuser,
      cholera_directory = cholera_directory,
      ncore = ncore,
      res_time = res_time,
      time_slices = time_slices,
      smooth_covariate_number_timesteps = smooth_covariate_number_timesteps,
      cases_column = cases_column,
      sf_cases = sf_cases,
      non_na_gridcells = covar_cube_output$non_na_gridcells,
      sf_grid = covar_cube_output$sf_grid,
      location_periods_dict = covar_cube_output$location_periods_dict,
      covar_cube = covar_cube_output$covar_cube
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
  
  taxdat::clean_all_tmp(dbuser = dbuser, map_name = map_name)
  
}
