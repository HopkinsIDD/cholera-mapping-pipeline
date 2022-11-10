## Basic test to make sure that testing setup works
context("Test 1 : Real data from api")

test_that("Data pull works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

  skip_if_not(dbuser == "app")
  expect_error(
    {
      all_dfs <- taxdat::create_testing_dfs_from_api(
        username = Sys.getenv("CHOLERA_API_USERNAME"),
        api_key = Sys.getenv("CHOLERA_API_KEY"), locations = "CT-World::AFR::KEN", time_left = lubridate::ymd("2000-01-01"),
        time_right = lubridate::ymd("2000-12-31"), uids = NULL, website = "https://api.cholera-taxonomy.middle-distance.com/"
      )
    },
    NA
  )


  conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
})

test_that("Database connection works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
  skip_if_not(dbuser == "app")

  expect_error(
    {
      conn_pg <- taxdat::connect_to_db(dbuser, dbname)
      DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
    },
    NA
  )
})

## Stopgap measure for covariates for now, eventually also pull from the api


test_that("setup works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
  skip_if_not(dbuser == "app")

  all_dfs <- taxdat::create_testing_dfs_from_api(
    username = Sys.getenv("CHOLERA_API_USERNAME"),
    api_key = Sys.getenv("CHOLERA_API_KEY"), locations = "CT-World::AFR::KEN", time_left = lubridate::ymd("2000-01-01"),
    time_right = lubridate::ymd("2000-12-31"), uids = NULL, website = "https://api.cholera-taxonomy.middle-distance.com/"
  )

  conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

  # pop_raster_funs <- list(population = list(
  #   name = "population", start_date = lubridate::ymd("2000-01-01"),
  #   end_date = lubridate::ymd("2000-12-31"), fun = function(psql_connection) {
  #     return(rpostgis::pgGetRast(psql_connection, c("grids", "master_spatial_grid")))
  #   }
  # ))
  test_extent <- sf::st_bbox(all_dfs$shapes_df)
  test_raster <- taxdat::create_test_raster(test_extent = test_extent)
  test_covariates <- list(taxdat::create_test_covariate(test_raster = test_raster))
  pop_raster_funs <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
    test_covariates,
    lubridate::ymd("2000-01-01"), lubridate::ymd("2000-12-31")
  )

  expect_error(taxdat::setup_testing_database_from_dataframes(
    conn_pg, all_dfs,
    pop_raster_funs
  ), NA)
})

test_that("execute_pipeline.R runs successfully", {
  Sys.setenv(CHOLERA_CONFIG = rprojroot::find_root_file(
    criterion = ".choldir",
    "tests", "tests", "testthat", "config_data_pull.yml"
  ))
  expect_error(
    {
      source(rprojroot::find_root_file(
        criterion = ".choldir", "Analysis", "R",
        "execute_pipeline.R"
      ))
    },
    NA
  )
})
