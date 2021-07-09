## Basic test to make sure that testing setup works
context("Test 1 : Real data from api")


dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

all_dfs <- taxdat::create_testing_dfs_from_api(username = Sys.getenv("CHOLERA_API_USERNAME"), 
    api_key = Sys.getenv("CHOLERA_API_KEY"), locations = "AFR::KEN", time_left = lubridate::ymd("2000-01-01"), 
    time_right = lubridate::ymd("2000-12-31"), uids = NULL, website = "https://api.cholera-taxonomy.middle-distance.com/")


conn_pg <- taxdat::connect_to_db(dbuser, dbname)
DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

## Stopgap measure for covariates for now, eventually also pull from the api


test_that("setup works", {
    pop_raster_funs <- list(population = list(name = "population", start_date = lubridate::ymd("2000-01-01"), 
        end_date = lubridate::ymd("2000-12-31"), fun = function(psql_connection) {
            return(rpostgis::pgGetRast(psql_connection, c("grids", "master_spatial_grid")))
        }))

    expect_error(taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, 
        pop_raster_funs), NA)
})

test_that("execute_pipeline.R runs successfully", {
    Sys.setenv(CHOLERA_CONFIG = rprojroot::find_root_file(criterion = ".choldir", 
        "tests", "testthat", "config_data_pull.yml"))
    expect_error({
        source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", 
            "execute_pipeline.R"))
    }, NA)
})
