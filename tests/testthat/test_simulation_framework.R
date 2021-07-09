context("Test 4 : Simulation Framework")
library(magrittr)
all_data <- list()
seed <- 12345

nc <- 2  ## Number of covariates
min_time_left <- lubridate::ymd("2000-01-01")
max_time_right <- lubridate::ymd("2000-12-31")

#### testing.1 ####
data_fun <- function() {
    taxdat:::create_standardized_test_data(nrows = 20, ncols = 20, nlayers = 12, 
        base_number = 1, n_layers = 4, factor = 2, snap = FALSE, randomize = FALSE, 
        ncovariates = nc, nonspatial = c(FALSE, FALSE), nontemporal = c(FALSE, FALSE), 
        spatially_smooth = c(FALSE, FALSE), temporally_smooth = c(FALSE, FALSE), 
        polygonal = c(FALSE, FALSE), radiating = c(FALSE, TRUE), constant = c(TRUE, 
            FALSE), rho = rep(0.999999, times = nc), radiating_polygons = list(NA, 
            sf::st_union(sf::st_sfc(sf::st_point(c(0.25, 0.25)), sf::st_point(c(0.75, 
                0.75)), crs = sf::st_crs(taxdat::create_test_polygons())))), radiation_function = rep(list(function(x, 
            mu) {
            mu * RandomFieldsUtils::matern(x/1000, 3/4)
        }), nc), radiating_means = list(NA, c(1)), smoothing_function = rep(list(function(n, 
            mu, covariance, centers) {
            return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
        }), nc), family = "Gaussian", magnitude = c(6, 1), normalization = function(x) {
            if (length(unique(x[])) == 1) {
                return(x * 0 + 1)
            }
            x <- exp(x)
            x[] <- 0.1 * ((x[] - min(x[]))/(max(x[]) - min(x[])))
            return(x)
        }, grid_proportion_observed = 1, number_draws = 1, grid_spatial_observation_bias = FALSE, 
        grid_temporal_observation_bias = FALSE, grid_value_observation_bias = FALSE, 
        noise = FALSE, polygon_proportion_observed = 1, polygon_observation_rates = exp(rnorm(nrow(test_polygons), 
            -1)), polygon_size_bias = FALSE, nonlinear_covariates = FALSE, min_time_left = min_time_left, 
        max_time_right = max_time_right, seed = seed)
}

full_data <- data_fun()

all_dfs <- list()
all_dfs$location_df <- data.frame(qualified_name = sort(unique(full_data$observed_polygons$location)))
all_dfs$location_period_df <- full_data$observed_polygons %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(qualified_name = location) %>%
    dplyr::group_by(qualified_name) %>%
    dplyr::summarize(start_date = min(time_left), end_date = max(time_right), .groups = "drop")
all_dfs$shapes_df <- full_data$observed_polygons %>%
    dplyr::mutate(qualified_name = location) %>%
    dplyr::group_by(qualified_name) %>%
    dplyr::summarize(start_date = min(time_left), end_date = max(time_right), .groups = "drop")
names(all_dfs$shapes_df)[4] <- "geom"
sf::st_geometry(all_dfs$shapes_df) <- "geom"
all_dfs$observations_df <- full_data$observed_polygons %>%
    dplyr::mutate(observation_collection_id = draw, time_left = time_left, time_right = time_right, 
        qualified_name = location, primary = TRUE, phantom = FALSE, suspected_cases = cases, 
        deaths = NA, confirmed_cases = NA)

covariate_raster_funs <- lapply(seq_len(length(full_data$covariates)), function(covariate_idx) {
    covariate = full_data$covariates[[covariate_idx]]
    min_time_index <- min(covariate$t)
    max_time_index <- max(covariate$t)
    lapply(unique(covariate$t), function(time_index) {
        return(list(name = ifelse(covariate_idx == 1, "population", paste("covariate", 
            covariate_idx, sep = "")), start_date = min_time_left + ((time_index - 
            1) - min_time_index)/(max_time_index - min_time_index) * (max_time_right - 
            min_time_left), end_date = min_time_left + (time_index - min_time_index)/(max_time_index - 
            min_time_index) * (max_time_right - min_time_left), fun = function(psql_connection) {
            covariate %>%
                dplyr::filter(t == time_index) %>%
                dplyr::select(covariate) %>%
                stars::st_rasterize() %>%
                stars:::st_as_raster() %>%
                return()
        }))
    })
}) %>%
    unlist(recursive = FALSE)

dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
conn_pg <- taxdat::connect_to_db(dbuser, dbname)
DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs)

test_that("execute_pipeline.R runs successfully", {
    Sys.setenv(CHOLERA_CONFIG = rprojroot::find_root_file(criterion = ".choldir", 
        "tests", "testthat", "config_simulation.yml"))
    expect_error({
        source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", 
            "execute_pipeline.R"))
    }, NA)
})
