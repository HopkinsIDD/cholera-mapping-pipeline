## Basic test setup starting from real data
library(taxdat)

dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

conn_pg <- taxdat::connect_to_db(dbuser, dbname)
DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

query_time_left <- lubridate::ymd("2000-01-01")
query_time_right <- lubridate::ymd("2000-12-31")
## Pull data frames needed to create testing database from the api This doesn't
## pull covariates, but does pull everything else tryCatch({ all_dfs <-
## taxdat::create_testing_dfs_from_api(username =
## Sys.getenv('CHOLERA_API_USERNAME'), api_key = Sys.getenv('CHOLERA_API_KEY'),
## locations = 'AFR::KEN', time_left = query_time_left, time_right =
## query_time_right, uids = NULL, website =
## 'https://api.cholera-taxonomy.middle-distance.com/') }, error = function(e) {
## })
load(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "all_dfs_object.rdata"))


## ------------------------------------------------------------------------------------------------------------------------
## Change polygons
test_extent <- sf::st_bbox(all_dfs$shapes_df)
test_raster <- create_test_raster(nrows = 10, ncols = 10, nlayers = 2, test_extent)
# Create 3 layers of testing polygons starting with a single country, and
# splitting each polygon into 4 sub-polygons
test_polygons <- sf::st_make_valid(create_test_layered_polygons(test_raster = test_raster, 
                                                                base_number = 1, n_layers = 2, factor = 10 * 10, snap = FALSE, randomize = FALSE))

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
test_raster <- create_test_raster(nrows = 10, ncols = 10, nlayers = 2, test_extent)
min_time_left <- query_time_left
max_time_right <- query_time_right

test_covariates1 <- create_multiple_test_covariates(test_raster = test_raster, ncovariates = 2, 
                                                   nonspatial = c(FALSE, FALSE), 
                                                   nontemporal = c(FALSE, FALSE), 
                                                   spatially_smooth = c(TRUE,FALSE), 
                                                   temporally_smooth = c(FALSE, FALSE), 
                                                   polygonal = c(TRUE, TRUE), 
                                                   radiating = c(FALSE,FALSE))
covariate_raster_funs1 <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(test_covariates1,min_time_left, max_time_right)

test_covariates2 <- create_multiple_test_covariates(test_raster = test_raster, ncovariates = 2, 
                                                    nonspatial = c(TRUE, TRUE), 
                                                    nontemporal = c(TRUE,TRUE), 
                                                    spatially_smooth = c(FALSE,TRUE), 
                                                    temporally_smooth = c(TRUE, TRUE), 
                                                    polygonal = c(FALSE, FALSE), 
                                                    radiating = c(FALSE,FALSE))
covariate_raster_funs2 <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(test_covariates2,min_time_left, max_time_right)

## ------------------------------------------------------------------------------------------------------------------------
## Change observations
test_polygons <- dplyr::mutate(all_dfs$shapes_df, location = qualified_name, geometry = geom)
#first test observation
raster_df1 <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs1)
sf::st_crs(test_polygons)<-sf::st_crs(raster_df1[[1]])
test_underlying_distribution1 <- create_underlying_distribution(covariates = raster_df1)
test_observations1 <- observe_polygons(test_polygons = test_polygons, test_covariates = raster_df1$covar, 
                                      underlying_distribution = test_underlying_distribution1, noise = FALSE, number_draws = 100, 
                                      grid_proportion_observed = 1, polygon_proportion_observed = 1, min_time_left = query_time_left, 
                                      max_time_right = query_time_right)
#second test observation
raster_df2 <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs2)
test_underlying_distribution2 <- create_underlying_distribution(covariates = raster_df2)
test_observations2 <- observe_polygons(test_polygons = test_polygons, test_covariates = raster_df2$covar, 
                                       underlying_distribution = test_underlying_distribution2, noise = FALSE, number_draws = 100, 
                                       grid_proportion_observed = 1, polygon_proportion_observed = 1, min_time_left = query_time_left, 
                                       max_time_right = query_time_right)

test_observations=dplyr::bind_rows(test_observations1,test_observations2)

all_dfs$observations_df <- test_observations %>%
  dplyr::mutate(observation_collection_id = draw, time_left = time_left, time_right = time_right, 
                qualified_name = location, primary = TRUE, phantom = FALSE, suspected_cases = cases, 
                deaths = NA, confirmed_cases = NA)

## ------------------------------------------------------------------------------------------------------------------------
## Create Database
setup_testing_database(conn_pg, drop = TRUE)
taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs1)

## NOTE: Change me if you want to run the report locally config_filename <-
## paste(tempfile(), 'yml', sep = '.')
config_filename <- "/home/app/cmp/Analysis/R/test_config.yml"

## Put your config stuff in here
config <- list(general = list(location_name = all_dfs$location_df$qualified_name[[1]], 
                              start_date = as.character(min_time_left), end_date = as.character(max_time_right), 
                              width_in_km = 1, height_in_km = 1, time_scale = "month"), stan = list(directory = rprojroot::find_root_file(criterion = ".choldir", 
                                                                                                                                          "Analysis", "Stan"), ncores = 1, model = "dagar_seasonal.stan", niter = 1000, 
                                                                                                    recompile = TRUE), name = "test_???", taxonomy = "taxonomy-working/working-entry1", 
               smoothing_period = 1, case_definition = "suspected", covariate_choices = raster_df1$name, 
               data_source = "sql", file_names = list(stan_output = rprojroot::find_root_file(criterion = ".choldir", 
                                                                                              "Analysis", "output", "test.stan_output.rdata"), stan_input = rprojroot::find_root_file(criterion = ".choldir", 
                                                                                                                                                                                      "Analysis", "output", "test.stan_input.rdata")))

yaml::write_yaml(x = config, file = config_filename)

Sys.setenv(CHOLERA_CONFIG = config_filename)
source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", "execute_pipeline.R"))
rmarkdown::render(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "output", 
                                            "country_data_report.Rmd"), params = list(config_filename = config_filename, 
                                                                                      cholera_directory = "~/cmp/", drop_nodata_years = TRUE))
