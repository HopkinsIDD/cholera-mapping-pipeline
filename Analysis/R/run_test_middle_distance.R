## Basic test setup starting from real data
library(taxdat)

dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
option_list <- list(optparse::make_option(c("-d", "--cholera_pipeline_directory"),
    action = "store", default = Sys.getenv("CHOLERA_PIPELINE_DIRECTORY", rprojroot::find_root(".choldir")),
    type = "character", help = "Pipeline directory"), optparse::make_option(c("-d",
    "--cholera_output_directory"), action = "store", default = Sys.getenv("CHOLERA_OUTPUT_DIRECTORY",
    paste0(rprojroot::find_root(".choldir"), "/Analysis/data")), type = "character",
    help = "Output directory"), optparse::make_option(c("-p", "--postgres_database_name"),
    action = "store", default = Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates"),
    type = "character", help = "Postgres database name"), optparse::make_option(c("--postgres_database_user"),
    action = "store", default = Sys.getenv("USER", "app"), type = "character", help = "Postgres database user"))

opt <- optparse::parse_args((optparse::OptionParser(option_list = option_list)))

dbname <- opt$postgres_database_name
dbuser <- opt$postgres_database_user
pipeline_dir <- normalizePath(opt$cholera_pipeline_directory)
output_dir <- normalizePath(opt$cholera_output_directory)

conn_pg <- taxdat::connect_to_db(dbuser, dbname)
DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

query_time_left <- lubridate::ymd("2000-01-01")
query_time_right <- lubridate::ymd("2001-12-31")

## Pull data frames needed to create testing database from the api This doesn't
## pull covariates, but does pull everything else tryCatch({ all_dfs <-
## taxdat::create_testing_dfs_from_api( username
## =Sys.getenv('CHOLERA_API_USERNAME'), api_key =
## Sys.getenv('CHOLERA_API_KEY'), locations = 'AFR::KEN', time_left =
## query_time_left, time_right = query_time_right, uids = NULL, website =
## 'https://api.cholera-taxonomy.middle-distance.com/' ) }, error = function(e)
## { })
load(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "all_dfs_object.rdata"))


## ------------------------------------------------------------------------------------------------------------------------
## Change polygons
test_extent <- sf::st_bbox(all_dfs$shapes_df)
test_raster <- create_test_raster(nrows = 10, ncols = 10, nlayers = 24, test_extent = test_extent)
# Create 3 layers of testing polygons starting with a single country, and
# splitting each polygon into 4 sub-polygons
test_polygons <- sf::st_make_valid(create_test_layered_polygons(test_raster = test_raster,
    base_number = 1, n_layers = 2, factor = 10 * 10, snap = FALSE, randomize = FALSE,
    seed = my_seed))
my_seed <- .GlobalEnv$.Random.seed

all_dfs$shapes_df <- test_polygons %>%
    dplyr::mutate(qualified_name = location, start_date = min(all_dfs$shapes_df$start_date),
        end_date = max(all_dfs$shapes_df$end_date))
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
test_extent <- sf::st_bbox(all_dfs$shapes_df)
test_raster <- create_test_raster(nrows = 10, ncols = 10, nlayers = 24, test_extent = test_extent)
test_covariates <- create_multiple_test_covariates(test_raster = test_raster, ncovariates = 2,
    nonspatial = c(FALSE, FALSE), nontemporal = c(FALSE, FALSE), spatially_smooth = c(TRUE,
        TRUE), temporally_smooth = c(FALSE, TRUE), polygonal = c(TRUE, FALSE), radiating = c(FALSE,
        FALSE), seed = my_seed)
my_seed <- .GlobalEnv$.Random.seed
min_time_left <- query_time_left
max_time_right <- query_time_right
covariate_raster_funs <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(test_covariates,
    min_time_left, max_time_right)

## ------------------------------------------------------------------------------------------------------------------------
## Change observations
raster_df <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs)

test_underlying_distribution <- create_underlying_distribution(covariates = raster_df,
    seed = my_seed)
my_seed <- .GlobalEnv$.Random.seed

observation_time_lefts <- seq.Date(query_time_left, query_time_right, 200)
observation_time_rights <- c(observation_time_lefts[-1] - 1, query_time_right) -
    100
observation_time_lefts <- c(observation_time_lefts, query_time_left)
observation_time_rights <- c(observation_time_rights, query_time_left + lubridate::years(1) -
    1)

test_observations <- observe_polygons(test_polygons = dplyr::mutate(all_dfs$shapes_df,
    location = qualified_name, geometry = geom), test_covariates = raster_df, underlying_distribution = test_underlying_distribution,
    noise = FALSE, number_draws = 1, grid_proportion_observed = 1, polygon_proportion_observed = 1,
    min_time_left = query_time_left, max_time_right = query_time_right - 100, seed = my_seed)
for (date_idx in seq_len(length(observation_time_lefts))) {
    if (is.null(test_observations)) {
        test_observations <- observe_polygons(test_polygons = dplyr::mutate(all_dfs$shapes_df,
            location = qualified_name, geometry = geom), test_covariates = raster_df,
            underlying_distribution = test_underlying_distribution, noise = FALSE,
            number_draws = 1, grid_proportion_observed = 1, polygon_proportion_observed = 1,
            min_time_left = observation_time_lefts[date_idx], max_time_right = observation_time_rights[date_idx],
            seed = my_seed)
    } else {
        test_observations <- rbind(test_observations, observe_polygons(test_polygons = dplyr::mutate(all_dfs$shapes_df,
            location = qualified_name, geometry = geom), test_covariates = raster_df,
            underlying_distribution = test_underlying_distribution, noise = FALSE,
            number_draws = 1, grid_proportion_observed = 1, polygon_proportion_observed = 1,
            min_time_left = observation_time_lefts[date_idx], max_time_right = observation_time_rights[date_idx],
            seed = my_seed))
    }
}
my_seed <- .GlobalEnv$.Random.seed

all_dfs$observations_df <- test_observations %>%
    dplyr::mutate(observation_collection_id = draw, time_left = time_left, time_right = time_right,
        qualified_name = location, primary = TRUE, phantom = FALSE, suspected_cases = cases,
        deaths = NA, confirmed_cases = NA)


## ------------------------------------------------------------------------------------------------------------------------
## Create Database
setup_testing_database(conn_pg, drop = TRUE)
taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs)

## NOTE: Change me if you want to run the report locally config_filename <-
## paste(tempfile(), 'yml', sep = '.')
config_filename <- paste0(pipeline_dir, "/Analysis/R/test_config.yml")

## Put your config stuff in here
config <- list(general = list(location_name = all_dfs$location_df$qualified_name[[1]],
    start_date = as.character(min_time_left), end_date = as.character(max_time_right),
    width_in_km = 2, height_in_km = 2, time_scale = "year"), stan = list(directory = rprojroot::find_root_file(criterion = ".choldir",
    "Analysis", "Stan"), ncores = 1, model = "dagar_seasonal_flexible.stan", niter = 10000,
    recompile = TRUE), name = "test_???", taxonomy = "taxonomy-working/working-entry1",
    smoothing_period = 1, case_definition = "suspected", covariate_choices = raster_df$name,
    data_source = "sql", file_names = list(stan_output = rprojroot::find_root_file(criterion = ".choldir",
        "Analysis", "output", "test.stan_output.rdata"), stan_input = rprojroot::find_root_file(criterion = ".choldir",
        "Analysis", "output", "test.stan_input.rdata")), processing = list(aggregate = TRUE,
        remove_overlaps = TRUE))

yaml::write_yaml(x = config, file = config_filename)

Sys.setenv(CHOLERA_CONFIG = config_filename)
source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", "execute_pipeline.R"))
print(config_filename)
print(pipeline_dir)
print(TRUE)
rmarkdown::render(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "output",
    "country_data_report.Rmd"), params = list(config_filename = config_filename,
    cholera_directory = pipeline_dir, drop_nodata_years = TRUE))


## Actually do something with the groundtruth and output
