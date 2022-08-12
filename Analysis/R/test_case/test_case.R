#!/usr/bin/Rscript
# This scripts purpose is to simulate testing data and run that data through# the cholera mapping pipeline.

# Control Variables: Preamble
# ------------------------------------------------------------------------------------------------------------
### Set Error Handling
taxdat::set_error_handling(is_interactive = Sys.getenv("INTERACTIVE_RUN", "FALSE"))

### Libraries TODO : Update this list
taxdat::update_libraries(perform = Sys.getenv("CHOLERA_CHECK_LIBRARIES", TRUE), package_list = c(
  "optparse", "DBI", "RPostgres", "sf", "magrittr", "dplyr",
  "rstan", "xfun", "kableExtra", "MCMCvis"
))

library(magrittr)

option_list <- list(
  optparse::make_option(c("-c", "--config"),
    action = "store",
    default = Sys.getenv("CHOLERA_CONFIG", "config.yml"), type = "character", help = "Model run configuration file"
  ),
  optparse::make_option(c("-d", "--cholera_pipeline_directory"),
    action = "store",
    default = Sys.getenv("CHOLERA_PIPELINE_DIRECTORY", "."), type = "character",
    help = "Pipeline directory"
  ), optparse::make_option(c("-o", "--cholera_output_directory"),
    action = "store", default = Sys.getenv("CHOLERA_OUTPUT_DIRECTORY", "Analysis/data"),
    type = "character", help = "Output directory"
  ), optparse::make_option(c(
    "-D",
    "--postgres_database_name"
  ), action = "store", default = Sys.getenv(
    "CHOLERA_POSTGRES_DATABASE",
    "cholera_covariates"
  ), type = "character", help = "Postgres database name"),
  optparse::make_option(c("-p", "--postgres_database_port"),
    action = "store",
    default = Sys.getenv("CHOLERA_POSTGRES_PORT", 5435), type = "character",
    help = "Postgres database port"
  ), optparse::make_option(c("-u", "--postgres_database_user"),
    action = "store", default = Sys.getenv("USER", "app"), type = "character",
    help = "Postgres database user"
  ), optparse::make_option(c("--testing_run"),
    action = "store", default = FALSE, type = "logical", help = "Is this run a testing run or a production run"
  )
)

opt <- optparse::parse_args((optparse::OptionParser(option_list = option_list)))

### Config Options
config <- yaml::read_yaml(opt[["config"]])
config <- taxdat::complete_config(config)
if (!opt[["testing_run"]]) {
  yaml::write_yaml(config, file = paste0(opt[["config"]], ".complete"))
}
if (!taxdat::check_config(config)) {
  stop("Could not validate the config")
}


dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

conn_pg <- taxdat::connect_to_db(
  dbname = opt[["postgres_database_name"]], dbuser = opt[["postgres_database_user"]],
  port = opt[["postgres_database_port"]]
)

DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

# This is a difference
# TODO : change the way seeding interacts with the config
global_seed <- config[["seeds"]][["global_seed"]] %>%
  as.integer()
cov3_seed <- config[["seeds"]][["cov3_seed"]] %>%
  as.integer()

query_time_left <- lubridate::ymd(config[["general"]][["start_date"]])
query_time_right <- lubridate::ymd(config[["general"]][["end_date"]])

## Pull data frames needed to create testing database from the api This doesn't
## pull covariates, but does pull everything else tryCatch({ all_dfs <-
## taxdat::create_testing_dfs_from_api( username
## =Sys.getenv('CHOLERA_API_USERNAME'), api_key =
## Sys.getenv('CHOLERA_API_KEY'), locations = 'AFR::KEN', time_left =
## query_time_left, time_right = query_time_right, uids = NULL, website =
## 'https://api.cholera-taxonomy.middle-distance.com/' ) }, error = function(e)
## { })
## load(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "all_dfs_object.rdata"))

all_dfs <- list()
## ------------------------------------------------------------------------------------------------------------------------

## Change polygons

## Pulled from Kenya
test_extent <- sf::st_bbox(c(xmin = 33.9101, ymin = -4.7199, xmax = 41.9262, ymax = 5.0612))
# This is a difference
test_raster <- taxdat::create_test_raster(
  test_extent = test_extent,
  nrow = config[["test_metadata"]][["raster"]][["nrow"]],
  ncol = config[["test_metadata"]][["raster"]][["ncol"]],
  nlayer = config[["test_metadata"]][["raster"]][["nlayer"]]
)

test_polygons <- NULL
if (config[["test_metadata"]][["polygons"]][["template"]] == "full and grid") {
  test_polygons <- sf::st_make_valid(taxdat::create_test_layered_polygons(
    test_raster = test_raster,
    base_number = 1, n_layers = 2,
    factor = config[["test_metadata"]][["raster"]][["nrow"]] * config[["test_metadata"]][["raster"]][["ncol"]], snap = FALSE, randomize = FALSE,
    seed = global_seed
  ))
} else {
  stop(paste("Polygon template", config[["polygons"]][["template"]], "is not supported"))
}
global_seed <- .GlobalEnv$.Random.seed

## Make this into a function:
all_dfs$shapes_df <- test_polygons %>%
  dplyr::mutate(
    qualified_name = location, start_date = config[["general"]][["start_date"]],
    end_date = config[["general"]][["end_date"]]
  )
names(all_dfs$shapes_df)[names(all_dfs$shapes_df) == "geometry"] <- "geom"
sf::st_geometry(all_dfs$shapes_df) <- "geom"

all_dfs$location_period_df <- all_dfs$shapes_df %>%
  sf::st_drop_geometry()
all_dfs$location_df <- all_dfs$shapes_df %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(qualified_name) %>%
  dplyr::summarize()

## ------------------------------------------------------------------------------------------------------------------------
## Change covariates

## TODO : Add template code to config checks and defaults
covariates_table <- lapply(config[["test_metadata"]][["covariates"]], function(covariate_spec) {
  return(tibble::tibble(
    nonspatial = covariate_spec[["nonspatial"]],
    nontemporal = covariate_spec[["nontemporal"]],
    spatially_smooth = covariate_spec[["spatially_smooth"]],
    temporally_smooth = covariate_spec[["temporally_smooth"]],
    polygonal = covariate_spec[["polygonal"]],
    radiating = covariate_spec[["radiating"]],
    constant = covariate_spec[["constant"]],
    Data_simulation_covariates = covariate_spec[["include_in_simulation"]],
    Model_covariates = covariate_spec[["include_in_model"]],
    seed_name = covariate_spec[["seed"]]
  ))
}) %>%
  do.call(what = dplyr::bind_rows)

warning("Currently not respecting different covariate seeds.")
test_covariates <- taxdat::create_multiple_test_covariates(
  test_raster = test_raster, ncovariates = nrow(covariates_table),
  nonspatial = covariates_table$nonspatial, nontemporal = covariates_table$nontemporal,
  spatially_smooth = covariates_table$spatially_smooth, temporally_smooth = covariates_table$temporally_smooth,
  polygonal = covariates_table$polygonal, radiating = covariates_table$radiating,
  constant = covariates_table$constant, seed = global_seed
)
global_seed <- .GlobalEnv$.Random.seed

test_covariates_simulation <- test_covariates[covariates_table$Data_simulation_covariates]
test_covariates_modeling <- test_covariates[covariates_table$Model_covariates]

min_time_left <- query_time_left
max_time_right <- query_time_right
covariate_raster_funs_simulation <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
  test_covariates_simulation,
  min_time_left, max_time_right
)

global_seed <- .GlobalEnv$.Random.seed
min_time_left <- query_time_left
max_time_right <- query_time_right
covariate_raster_funs_modeling <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
  test_covariates_modeling,
  min_time_left, max_time_right
)

## save additional covariates in the data generation process for country data
## report
# This is a difference
rds_file <- config[["test_metadata"]][["file_names"]][["simulation_covariates"]]
if (!dir.exists(dirname(rds_file))) {
  dir.create(dirname(rds_file))
}
saveRDS(test_covariates_modeling, rds_file)

## ------------------------------------------------------------------------------------------------------------------------
## Change observations
raster_df <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs_simulation)

test_underlying_distribution <- taxdat::create_underlying_distribution(
  covariates = raster_df,
  seed = global_seed
)
global_seed <- .GlobalEnv$.Random.seed

test_observed_grid <- taxdat::observe_gridcells(
  underlying_distribution = test_underlying_distribution,
  proportion_observed = config[["test_metadata"]][["grid_observation"]][["proportion_observed"]],
  number_draws = config[["test_metadata"]][["grid_observation"]][["number_draws"]],
  spatial_observation_bias = config[["test_metadata"]][["grid_observation"]][["spatial_observation_bias"]],
  temporal_observation_bias = config[["test_metadata"]][["grid_observation"]][["temporal_observation_bias"]],
  value_observation_bias = config[["test_metadata"]][["grid_observation"]][["value_observation_bias"]],
  noise = config[["test_metadata"]][["grid_observation"]][["noise"]],
  seed = global_seed
)
global_seed <- .GlobalEnv$.Random.seed

counter <- 0
test_observations <- lapply(config[["test_metadata"]][["observations"]], function(spec) {
  counter <<- counter + 1
  rc <- taxdat::observe_polygons(
    test_polygons = dplyr::mutate(all_dfs$shapes_df, location = qualified_name, geometry = geom),
    test_covariates = raster_df,
    underlying_distribution = test_underlying_distribution,
    observed_grid = test_observed_grid,
    number_draws = config[["test_metadata"]][["grid_observation"]][["number_draws"]],
    polygon_proportion_observed = spec[["proportion_observed"]],
    min_time_left = lubridate::ymd(config[["general"]][["start_date"]]),
    max_time_right = lubridate::ymd(config[["general"]][["end_date"]]),
    observation_time_left = lubridate::ymd(spec[["start_date"]]),
    observation_time_right = lubridate::ymd(spec[["end_date"]]),
    seed = global_seed
  )
  if (grepl("inflated", spec[["template"]])) {
    rc <- rc %>%
      dplyr::mutate(cases = ifelse(qualified_name == config[["general"]][["location"]], cases, cases * spec[["inflation_factor"]]))
  }
  if (grepl("filtered", spec[["template"]])) {
    rc <- rc %>%
      dplyr::filter(qualified_name %in% spec[["kept_location_periods"]])
  }
  return(rc)
}) %>%
  do.call(what = dplyr::bind_rows)
global_seed <- .GlobalEnv$.Random.seed

all_dfs$observations_df <- test_observations %>%
  dplyr::mutate(
    observation_collection_id = draw, time_left = time_left, time_right = time_right,
    qualified_name = location, primary = TRUE, phantom = FALSE, suspected_cases = cases,
    deaths = NA, confirmed_cases = NA
  )

buffer <- min(c(
  (sf::st_bbox(test_extent)$xmax - sf::st_bbox(test_extent)$xmin) / config[["test_metadata"]][["raster"]][["ncol"]],
  (sf::st_bbox(test_extent)$ymax - sf::st_bbox(test_extent)$ymin) / config[["test_metadata"]][["raster"]][["nrow"]]
)) * 100 * 1000 * .5
lhs <- t(sf::st_contains(sf::st_buffer(test_observations, buffer), test_observed_grid))
rhs <- sf::st_contains(sf::st_buffer(test_observed_grid, buffer), test_observations)
test_observed_grid$observed <- mapply(
  idx = seq_len(length(lhs)),
  lhs,
  rhs,
  FUN = function(idx, x, y) {
    rc <- intersect(x, y)
    rc <- rc[test_observed_grid$t[idx] == test_observations$tmin[rc]]
    rc <- rc[test_observed_grid$t[idx] == test_observations$tmax[rc]]
    return(ifelse(length(rc) > 0, "Observed grid cells", "Unobserved grid cells"))
  }
)

rds_file <- config[["test_metadata"]][["file_names"]][["true_grid_cases"]]
if (!dir.exists(dirname(rds_file))) {
  dir.create(dirname(rds_file))
}
saveRDS(test_observed_grid, rds_file)

## ------------------------------------------------------------------------------------------------------------------------
## Create Database
taxdat::setup_testing_database(conn_pg, drop = TRUE)
taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs_modeling)

# Sys.setenv(CHOLERA_CONFIG = config_filename)
source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", "execute_pipeline.R"))

## Through here in editing
rmarkdown::render(
  rprojroot::find_root_file(
    criterion = ".choldir", "Analysis", "output",
    "country_data_report.Rmd"
  ),
  params = list(
    cholera_directory = rprojroot::find_root(criterion = ".choldir"),
    config = opt$config,
    drop_nodata_years = TRUE
  ),
  # This is a difference
  output_file = "test_case_1_country_data_report"
)
