# Preamble ---------------------------------------------------------------------
options(warn=1)
options(error=function(...){quit(...,status=2)})
# options(error=recover)
# Libraries
{
  if(!require(taxdat)){
    install.packages('packages/taxdat',type='source',repos=NULL)
  } else {
    detach('package:taxdat')
  }
  # Install required packages 
  package_list <- c('inline', 'parallel', 'rstan', 'sf', 'magrittr', 'RPostgreSQL',
                    'odbc', 'sf', 'raster', 'lubridate', 'tidync', 'ncdf4', 'stringr',
                    'dplyr', 'R.utils', 'ncdf4', 'gdalUtils', 'foreach', 'glue',
                    'spdep', 'igraph', 'itertools', 'purrr', 'optparse', 'RPostgres', 'rjson',
                    'geojsonsf', 'rpostgis', 'mgcv', 'digest')
  
  for (package in package_list) {
    if(!require(package = package, character.only = T)){
      install.packages(pkgs = package, repos = 'http://cran.us.r-project.org')
      library(package = package, character.only = T)
    }
  }
}

# - - - -
# Run options
option_list <- list(
  optparse::make_option(c("-c", "--config"), action = "store", default = Sys.getenv("CHOLERA_CONFIG","config.yml"), type="character", help = "Model run configuration file")
)
opt <- optparse::OptionParser(option_list=option_list) %>% optparse::parse_args()

# - - - -
# Read config file
config <- yaml::read_yaml(opt$config)

# - - - -
# Define relevent directories
try({setwd(utils::getSrcDirectory())},silent = TRUE)
try({setwd(dirname(rstudioapi::getActiveDocumentContext()$path))},silent = TRUE)
# Which taxonomy should we use?
taxonomy <- config$taxonomy #'taxonomy-working/working-entry1'
cholera_directory <- rprojroot::find_root(rprojroot::has_file('.choldir')) # The location of the repository
taxdir <- rprojroot::find_root_file(taxonomy, criterion=rprojroot::has_file('.choldir')) # Relative to the repository, where is the taxonomy
laydir <- rprojroot::find_root_file('Layers',criterion=rprojroot::has_file('.choldir')) # Relative to the repository, where is the layers directory

source(paste0(cholera_directory, "/Analysis/R/setup_helpers.R"))

# Inputs -----------------------------------------------------------------------
print("---- Reading Parameters ----\n")

# - - - - Data source
# Source for cholera data, either the taxonomy website of an sql call
data_source <- config$data_source


# Which countries should be included
# countries <- c("testing.1.1000", "testing.31.1000", "testing.32.1000", "testing.33.1000", "testing.34.1000")
# countries <- c(
#   "Nigeria","Ethiopia","Egypt","Democratic Republic of Congo","Tanzania","South Africa","Kenya","Uganda","Algeria","Sudan","Morocco","Angola","Mozambique","Ghana","Madagascar","Cameroon","Côte d'Ivoire","Niger","Burkina Faso","Mali","Malawi","Zambia","Senegal","Chad",
#   "Somalia","Zimbabwe","Guinea","Rwanda","Benin","Burundi","Tunisia","South Sudan","Togo","Sierra Leone","Libya","Congo","Liberia","Central African Republic","Mauritania","Eritrea","Namibia","Gambia","Botswana","Gabon","Lesotho"
# )

# - - - -
# Countries over which to run the model
countries <- config$countries
countries_name <- config$countries_name


# - - - -
# What size grid should be used
# res_space = 5 # km by km resolution of output map
res_space <- as.numeric(config$res_space) # km by km resolution of analysis
# aggregate_output_map <- 5 # resolution of output (in res_space km x res_space km)
# What is the smallest iso level we should permit
# TODO Is this stil used?
spatial_aggregate_iso_a2_level <- Inf # Aggregate data to ISO_A2_L? use 0 for ISO_A1 level


# - - - -
# What is the time unit to aggregate to?
res_time <- checkTimeRes(config$res_time)
temporal_aggregate_time_unit <- str_split(res_time, " ")[[1]][2]

# Get various functions to convert between time units and dates
time_change_func <- taxdat::time_unit_to_aggregate_function(temporal_aggregate_time_unit)
aggregate_to_start <- taxdat::time_unit_to_start_function(temporal_aggregate_time_unit)
aggregate_to_end <- taxdat::time_unit_to_end_function(temporal_aggregate_time_unit)
# Convert start date and end date to time units
# TODO Check if these are used
# start_step <- time_change_func(start_time)
# end_step <- time_change_func(end_time)


# - - - -
# What is the temporal period of the spatial random effects
smooth_covariate_number_timesteps <- config$smoothing_period


# - - - -
# What case definition should be used
suspected_or_confirmed <- checkCaseDefinition(config$case_definition)
# Determine the column name that the number of cases is stored in
# TODO check the flag for use_database
cases_column <- taxdat::case_definition_to_column_name(suspected_or_confirmed,
                                                       database = T)


# - - - -
# What range of times should be considered?
start_time <- lubridate::ymd(config$start_time)
end_time <- lubridate::ymd(config$end_time)

checkModelDateRange(start_time = start_time, 
                    end_time = end_time,
                    time_change_func = time_change_func,
                    aggregate_to_start = aggregate_to_start,
                    aggregate_to_end = aggregate_to_end)


# - - - -
# Define modeling time slices (set of time periods at which the data generating process occurs)
time_slices <- modelingTimeSlices(start_time = start_time, 
                                  end_time = end_time, 
                                  res_time = res_time,
                                  time_change_func = time_change_func,
                                  aggregate_to_start = aggregate_to_start,
                                  aggregate_to_end = aggregate_to_end)


# - - - -
# Model covariates
# Get the dictionary of covariates
covariate_dict <- yaml::read_yaml(paste0(cholera_directory, "/Layers/covariate_dictionary.yml"))
all_covariate_choices <- names(covariate_dict)
short_covariate_choices <- purrr::map_chr(covariate_dict, "abbr")

# User-defined covariates names and abbreviations
covariate_choices <- checkCovariateChoices(covar_choices = config$covariate_choices,
                                           available_choices = all_covariate_choices)
short_covariates <- short_covariate_choices[covariate_choices]


# - - - -
# STAN parameters
ncore <- config$stan$ncores
nchain <- ncore
if(ncore == 1) {nchain = 2}
rstan_options(auto_write = FALSE)
options(mc.cores = ncore)
niter <- config$stan$niter
stan_dir <- paste0(cholera_directory, '/Analysis/Stan/')
stan_model <- config$stan$model
stan_model_path <- checkStanModel(stan_model_path = paste(stan_dir, stan_model, sep=''),
                                  stan_dir = stan_dir)

# Should we be using a lower-triangular adjacency matrix 
# (this needs to be the case for the DAGAR model)
lower_triangular_adjacency <- grepl('dagar', stan_model)

# Initial Values for each stan parameter.
stan_params <- c('tau_theta','tau_phi','beta0','beta')
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
  if(!any(is.na(as.numeric(countries)))){
    countries <- as.numeric(countries)
    warning("Treating countries as location periods")
  } else {
    countries = sapply(countries,taxdat::fix_country_name)
  }
  all_test_idx = as.numeric(NA)
}


# - - - -
# cholera_covariates database connection settings
dbuser <- Sys.getenv("USER")

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
    map_name <- paste(paste(config$countries_name, collapse = '-'),
                      temporal_aggregate_time_unit,
                      paste(start_time, end_time, sep = '-'),
                      spatial_aggregate_iso_a2_level,
                      paste(res_space, 'km', sep = ''),
                      suspected_or_confirmed, 
                      str_replace(last(str_split(opt$config, "/")[[1]]), "\\.yml", ""),
                      sep = '_')
  }
  
  covariate_name_part <- paste(short_covariates, collapse = '-')
  
  setwd(cholera_directory)
  dir.create("Analysis/output", showWarnings = FALSE)
  
  preprocessed_data_fname <- makeObservationsFilename(cholera_directory, map_name) 
  preprocessed_covar_fname <- makeCovarFilename(cholera_directory, map_name, covariate_name_part)
  stan_input_fname <- makeStanInputFilename(cholera_directory, map_name, covariate_name_part, stan_model, niter)
  stan_output_fname <- makeStanOutputFilename(cholera_directory, map_name, covariate_name_part, stan_model, niter)
  map_output_fname <- makeMapOutputFilename(cholera_directory, map_name, covariate_name_part, stan_model, niter)
  
  # Preparation: Load auxillary functions
  source(str_c(cholera_directory, "/Analysis/R/covariate_helpers.R"))
  
  ## Step 1: process observation shapefiles and prepare data ##
  print(preprocessed_data_fname)
  if(file.exists(preprocessed_data_fname)){
    print("Data already preprocessed, skipping")
    warning("Data already preprocessed, skipping")
    load(preprocessed_data_fname)
  } else if(!testing){
    source(paste(cholera_directory, 'Analysis', 'R', 'prepare_grid.R', sep='/'))
    
    # First prepare the computation grid and get the grid name
    full_grid_name <- prepare_grid(
      dbuser = dbuser,
      cholera_directory = cholera_directory,
      res_space = res_space,
      ingest = F
    )
    
    # Pull data from taxonomy database (either using the API or SQL)
    source(paste(cholera_directory,'Analysis','R','prepare_map_data_revised.R',sep='/'))
    
  } else {
    source(paste(cholera_directory,"Analysis", "R", "create_standardized_testing_data.R",sep='/'))
  }
  
  ## Step 2: Extract the covariate cube and grid ##
  print(preprocessed_covar_fname)
  if(file.exists(preprocessed_covar_fname)){
    print("Covariate cube already preprocessed, skipping")
    warning("Covariate cube already preprocessed, skipping")
    load(preprocessed_covar_fname)
  } else if(!testing){
    
    # Note: the first covariate is always the population raster
    ## Step 2a: ingest the required covariates ##
    # Load the function
    source(paste(cholera_directory, 'Analysis', 'R', 'prepare_covariates.R', sep='/'))
    
    # Run covariate preparation. The function return the list of covariate names
    # included in the model
    covar_list <- prepare_covariates(
      dbuser = dbuser,
      cholera_directory = cholera_directory,
      res_space = res_space,
      res_time = res_time,
      ingest = F,
      do_parallel = F,
      ovrt_covar = F,
      ovrt_metadata_table = F,
      redo_metadata = F,
      covar = paste(c('p', short_covariates), collapse = ','),  # add population as first covariate
      full_grid_name = full_grid_name,
      aoi_name = "raw"
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
      temporal_aggregate_time_unit = temporal_aggregate_time_unit,
      res_space = res_space,
      res_time = res_time
    ) 
    
    # Save results to file
    save(covar_cube_output, file = preprocessed_covar_fname)
  }
  print("NCOVAR")
  print(length(covariate_choices))
  print("NCOVAR")
  
  ## Step 3: Prepare the stan input ##
  print(stan_input_fname)
  if(!file.exists(stan_input_fname)){
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
    save(stan_input, file = stan_input_fname)
    sink(gsub('rdata','json', stan_input_fname))
    cat(jsonlite::toJSON(stan_input$stan_data, auto_unbox=TRUE,matrix='rowmajor'))
    sink(NULL)
    
  } else {
    print("Stan input already created, skipping")
    warning("Stan input already created, skipping")
  }
  load(stan_input_fname)
  
  stan_data <- stan_input$stan_data
  sf_cases_resized <- stan_input$sf_cases_resized
  sf_grid <- stan_input$sf_grid
    
  ## Step 4: Run the model
  print(stan_output_fname)
  if(file.exists(stan_output_fname)){
    print("Data already modeled, skipping")
    warning("Data already modeled, skipping")
    load(stan_output_fname)
  } else {
    source(paste(cholera_directory,'Analysis','R','run_stan_model.R',sep='/'))
    recompile <- FALSE
  }
  
  cleanAllTmp(dbuser = dbuser, map_name = map_name)
  
  ## Step 6: Make maps
  # source(paste(cholera_directory,'Analysis','R','make_cholera_map.R',sep='/'))
}
