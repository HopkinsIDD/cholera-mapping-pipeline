## Basic test setup starting from real data
library(taxdat)

dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

conn_pg <- taxdat::connect_to_db(dbuser,dbname)
DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

## Pull data frames needed to create testing database from the api This doesn't
## pull covariates, but does pull everything else
all_dfs <- taxdat::create_testing_dfs_from_api(username = Sys.getenv("CHOLERA_API_USERNAME"), 
                                               api_key = Sys.getenv("CHOLERA_API_KEY"), locations = "AFR::KEN", time_left = lubridate::ymd("2000-01-01"), 
                                               time_right = lubridate::ymd("2000-12-31"), uids = NULL, website = "https://api.cholera-taxonomy.middle-distance.com/")
all_dfs_obs=data.frame(names=names(all_dfs$observations_df),class=class(all_dfs_obs))
write.csv(all_dfs_obs,'/home/app/cmp/all_dfs_obs.csv')

#all_dfs=all_dfs[which(all_dfs$observations_df["attributes.fields.suspected_cases"]>0),]


# ## ------------------------------------------------------------------------------------------------------------------------
# ## Change polygons
# test_extent <- sf::st_bbox(all_dfs$shapes_df)
# test_raster <- create_test_raster(nrows = 10, ncols = 10, nlayers = 2, test_extent)
# 
# # Create 3 layers of testing polygons starting with a single country, and
# test_polygons <- create_test_layered_polygons(test_raster = test_raster, base_number = 1,
#                                               n_layers = 2, factor = 10, snap = FALSE, randomize = FALSE)
# 
# all_dfs$shapes_df <- test_polygons %>%
#   dplyr::mutate(qualified_name = location, start_date = min(all_dfs$shapes_df$start_date),
#                 end_date = max(all_dfs$shapes_df$end_date))
# names(all_dfs$shapes_df)[names(all_dfs$shapes_df) == "geometry"] <- "geom"
# sf::st_geometry(all_dfs$shapes_df) <- "geom"
# 
# all_dfs$location_period_df <- all_dfs$shapes_df %>%
#   sf::st_drop_geometry()
# all_dfs$location_df <- all_dfs$shapes_df %>%
#   sf::st_drop_geometry() %>%
#   dplyr::group_by(qualified_name) %>%
#   dplyr::summarize()
# 
# ## ------------------------------------------------------------------------------------------------------------------------
# ## Change covariates
# test_extent <- sf::st_bbox(all_dfs$shapes_df)
# test_raster <- create_test_raster(nrows = 10, ncols = 10, nlayers = 2, test_extent)
# test_covariates <- create_multiple_test_covariates(test_raster = test_raster)
# test_covariates[[1]]$covariate <- 1 + 10^test_covariates[[1]][["covariate"]]
# 
# min_time_left <- min(all_dfs$observations_df$time_left)
# max_time_right <- max(all_dfs$observations_df$time_right)
# covariate_raster_funs <- lapply(seq_len(length(test_covariates)), function(covariate_idx) {
#   covariate <- test_covariates[[covariate_idx]]
#   min_time_index <- min(covariate$t)
#   max_time_index <- max(covariate$t)
#   lapply(unique(covariate$t), function(time_index) {
#     return(list(name = ifelse(covariate_idx == 1, "population", paste("covariate",
#                                                                       covariate_idx, sep = "")), start_date = min_time_left + ((time_index -
#                                                                                                                                   1) - min_time_index)/(max_time_index - min_time_index) * (max_time_right -
#                                                                                                                                                                                               min_time_left), end_date = min_time_left + (time_index - min_time_index)/(max_time_index -
#                                                                                                                                                                                                                                                                           min_time_index) * (max_time_right - min_time_left), fun = function(psql_connection) {
#                                                                                                                                                                                                                                                                             covariate %>%
#                                                                                                                                                                                                                                                                               dplyr::filter(t == time_index) %>%
#                                                                                                                                                                                                                                                                               dplyr::select(covariate) %>%
#                                                                                                                                                                                                                                                                               stars::st_rasterize(nx = max(test_raster$row), ny = max(test_raster$col)) %>%
#                                                                                                                                                                                                                                                                               stars:::st_as_raster() %>%
#                                                                                                                                                                                                                                                                               return()
#                                                                                                                                                                                                                                                                           }))
#   })
# }) %>%
#   unlist(recursive = FALSE)
# 
# ## ------------------------------------------------------------------------------------------------------------------------
# ## Change observations
# raster_df <- lapply(covariate_raster_funs, function(x) {
#   tibble::tibble(name = x$name, start_date = x$start_date, end_date = x$end_date,
#                  covar = list(x$fun()))
# }) %>%
#   do.call(what = bind_rows) %>%
#   dplyr::group_by(name) %>%
#   summarize(covar = list(do.call(what = bind_rows, mapply(SIMPLIFY = FALSE, covar,
#                                                           start_date, FUN = function(covar, time_left) {
#                                                             tmp <- reshape2::melt(array(raster::values(covar), dim(covar[[1]])))
#                                                             tmp$geometry <- sf::st_geometry(sf::st_as_sf(raster::rasterToPolygons(covar)))
#                                                             tmp$id <- seq_len(nrow(tmp))
#                                                             tmp$row <- tmp$Var1
#                                                             tmp$col <- tmp$Var2
#                                                             tmp$t <- which(start_date == time_left)
#                                                             tmp$covariate <- tmp$value
#                                                             return(sf::st_as_sf(tmp[, c("id", "row", "col", "t", "covariate", "geometry")]))
#                                                           }))), .groups = "drop") %>%
#   dplyr::arrange(as.numeric(gsub("covariate", "", gsub("population", "0", name))))
# 
# raster_df$covar[[1]]$covariate <- log(raster_df$covar[[1]]$covariate - 1)/log(10)
# 
# test_underlying_distribution <- create_underlying_distribution(covariates = raster_df$covar)
# 
# test_observations <- observe_polygons(test_polygons = dplyr::mutate(all_dfs$shapes_df,
#                                                                     location = qualified_name, geometry = geom), test_covariates = raster_df$covar,
#                                       underlying_distribution = test_underlying_distribution, noise = FALSE)
# 
# #
# all_dfs$observations_df <- test_observations %>%
#   dplyr::mutate(observation_collection_id = draw, time_left = time_left, time_right = time_right,
#                 qualified_name = location, primary = TRUE, phantom = FALSE, suspected_cases = cases,
#                 deaths = NA, confirmed_cases = NA)
# 
# ## ------------------------------------------------------------------------------------------------------------------------
# ## Create Database
# setup_testing_database(conn_pg, drop = TRUE)
# taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs)
# config_filename <- paste(tempfile(), "yml", sep = ".")
# 
# # ## Put your config stuff in here
# config <- list(general = list(location_name = all_dfs$location_df$qualified_name[[1]],
#                               start_date = as.character(min_time_left), end_date = as.character(max_time_right),
#                               width_in_km = 1, height_in_km = 1, time_scale = "month"), stan = list(directory = rprojroot::find_root_file(criterion = ".choldir",
#                                                                                                                                           "Analysis", "Stan"), ncores = 1, model = "dagar_seasonal.stan", niter = 20, recompile = TRUE),
#                name = "test_???", taxonomy = "taxonomy-working/working-entry1", smoothing_period = 1,
#                case_definition = "suspected", covariate_choices = raster_df$name, data_source = "sql",
#                file_names = list(stan_output = "stan_output.Rdata"))
# 
# yaml::write_yaml(x = config, file = config_filename)
# 
# Sys.setenv(CHOLERA_CONFIG = config_filename)
# source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", "execute_pipeline.R"))

