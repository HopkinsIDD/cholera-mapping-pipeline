## Basic test to make sure that testing setup works
context("Test 3 : Simple Pipeline Run")

dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

location_df <- data.frame(qualified_name = c("testlocation"))

location_period_df <- data.frame(
  qualified_name = location_df$qualified_name, start_date = lubridate::ymd("2000-01-01"),
  end_date = lubridate::ymd("2001-12-31")
)

shapes_df <- sf::st_sf(
  qualified_name = location_df$qualified_name, start_date = location_period_df$start_date,
  end_date = location_period_df$end_date, geom = sf::st_sfc(sf::st_polygon(list(matrix(c(
    0,
    0, 0, 1, 1, 1, 1, 0, 0, 0
  ), byrow = TRUE, ncol = 2))))
)
names(shapes_df)[[4]] <- "geom"

test_that("establishing postgres connection works", {
  expect_error(
    {
      conn_pg <- taxdat::connect_to_db(dbuser, dbname)
      DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
    },
    NA
  )
})

observations_df <- data.frame(
  observation_collection_id = c(1, 1, 2, 2), time_left = lubridate::ymd(c(
    "2000-01-01",
    "2000-06-15", "2001-01-01", "2001-06-15"
  )), time_right = lubridate::ymd(c(
    "2000-06-14",
    "2000-12-31", "2001-06-14", "2001-12-31"
  )), qualified_name = c(
    "testlocation",
    "testlocation", "testlocation", "testlocation"
  ), primary = rep(TRUE, times = 4),
  phantom = rep(FALSE, times = 4), suspected_cases = 9:12, confirmed_cases = 5:8,
  deaths = 1:4
)

conn_pg <- taxdat::connect_to_db(dbuser, dbname)
DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
pop_raster <- rpostgis::pgGetRast(conn_pg, c("grids", "master_spatial_grid"))


test_that("setup works", {
  taxdat::setup_testing_database(conn_pg, drop = TRUE)
  taxdat::insert_testing_locations(conn_pg, location_df)
  taxdat::insert_testing_location_periods(conn_pg, location_period_df)
  taxdat::insert_testing_shapefiles(conn_pg, shapes_df)
  taxdat::refresh_materialized_views(conn_pg)
  taxdat::ingest_spatial_grid(conn_pg, width = 1, height = 1)
  taxdat::refresh_materialized_views(conn_pg)
  taxdat::insert_testing_observations(conn_pg, observations_df)
  taxdat::ingest_covariate_from_raster(
    conn_pg, "population", pop_raster, lubridate::ymd("2000-01-01"),
    lubridate::ymd("2001-12-31")
  )
  taxdat::refresh_materialized_views(conn_pg)
})

test_that("pull_observation_data runs successfully", {
  conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  expect_error(
    {
      taxdat::pull_observation_data(
        conn_pg, "testlocation", lubridate::ymd("2000-01-01"),
        lubridate::ymd("2001-12-31")
      )
    },
    NA
  )
})

test_that("pull_observation_data has the right size", {
  conn_pg <- taxdat::connect_to_db(dbuser, dbname)

  expect_error(
    {
      nrow(taxdat::pull_observation_data(
        conn_pg, "testlocation", lubridate::ymd("2000-01-01"),
        lubridate::ymd("2000-12-31")
      ))
    },
    NA
  )
  expect_equal(
    {
      nrow(taxdat::pull_observation_data(
        conn_pg, "testlocation", lubridate::ymd("2000-01-01"),
        lubridate::ymd("2000-12-31")
      ))
    },
    2
  )

  expect_error(
    {
      nrow(taxdat::pull_observation_data(
        conn_pg, "fakelocation", lubridate::ymd("2000-01-01"),
        lubridate::ymd("2000-12-31")
      ))
    },
    NA
  )
  expect_equal(
    {
      nrow(taxdat::pull_observation_data(
        conn_pg, "fakelocation", lubridate::ymd("2000-01-01"),
        lubridate::ymd("2000-12-31")
      ))
    },
    0
  )

  expect_error(
    {
      nrow(taxdat::pull_observation_data(
        conn_pg, "testlocation", lubridate::ymd("2010-01-01"),
        lubridate::ymd("2011-12-31")
      ))
    },
    NA
  )
  expect_equal(
    {
      nrow(taxdat::pull_observation_data(
        conn_pg, "testlocation", lubridate::ymd("2010-01-01"),
        lubridate::ymd("2011-12-31")
      ))
    },
    0
  )

  expect_error(
    {
      nrow(taxdat::pull_observation_data(
        conn_pg, "testlocation", lubridate::ymd("1000-01-01"),
        lubridate::ymd("3000-12-31")
      ))
    },
    NA
  )
  expect_equal(
    {
      nrow(taxdat::pull_observation_data(
        conn_pg, "testlocation", lubridate::ymd("1000-01-01"),
        lubridate::ymd("3000-12-31")
      ))
    },
    4
  )
})

test_that("execute_pipeline.R runs successfully", {
  Sys.setenv(CHOLERA_CONFIG = rprojroot::find_root_file(
    criterion = ".choldir",
    "tests", "testthat", "config.yml"
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
