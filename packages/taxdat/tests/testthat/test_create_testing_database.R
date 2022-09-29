## All tests
test_that("Create locations table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker

  expect_error(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    NA
  )

  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)

      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_locations_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_locations_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_locations_table(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_locations_table(conn_pg, FALSE)
    },
    NA
  )
})

test_that("Create location_periods table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message, "with message", e$message))
    }
  )

  expect_error(
    {
      create_location_periods_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_location_periods_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_location_periods_table(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_location_periods_table(conn_pg, FALSE)
    },
    NA
  )
})

test_that("Create shapes table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      create_location_periods_table(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_shapes_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_shapes_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_shapes_table(conn_pg)
    }
  )
  expect_error(
    {
      create_shapes_table(conn_pg, FALSE)
    }
  )
})

test_that("Create location_hierarchies table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_location_hierarchies_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_location_hierarchies_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_location_hierarchies_table(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_location_hierarchies_table(conn_pg, FALSE)
    },
    NA
  )
})

test_that("Create observation table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      create_location_periods_table(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_observations_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_observations_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_observations_table(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_observations_table(conn_pg, FALSE)
    },
    NA
  )
})

test_that("Create master_spatial_grid table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      create_location_periods_table(conn_pg)
      create_shapes_table(conn_pg)
      create_all_covariates_table(conn_pg)
      create_raster_covariate_collections_table(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_master_spatial_grid_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_master_spatial_grid_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_master_spatial_grid_table(conn_pg)
    }
  )
  expect_error(
    {
      create_master_spatial_grid_table(conn_pg, FALSE)
    }
  )
})

test_that("Create resized_spatial_grids table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      create_location_periods_table(conn_pg)
      create_shapes_table(conn_pg)
      create_all_covariates_table(conn_pg)
      create_raster_covariate_collections_table(conn_pg)
      create_master_spatial_grid_table(conn_pg)
      create_spatial_resolutions_table(conn_pg)
      create_resize_spatial_grid_function(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_resized_spatial_grids_view(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_resized_spatial_grids_view(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_resized_spatial_grids_view(conn_pg)
    }
  )
  expect_error(
    {
      create_resized_spatial_grids_view(conn_pg, FALSE)
    }
  )
})

test_that("Create all_covariates table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_all_covariates_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_all_covariates_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_all_covariates_table(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_all_covariates_table(conn_pg, FALSE)
    },
    NA
  )
})

test_that("Create time_bounds table works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_time_bounds_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_time_bounds_table(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_time_bounds_table(conn_pg)
    }
  )
  expect_error(
    {
      create_time_bounds_table(conn_pg, FALSE)
    }
  )
})

test_that("Create master_temporal_grid_view works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_time_bounds_table(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_master_temporal_grid_view(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_master_temporal_grid_view(conn_pg)
    }
  )
})

test_that("Create resized_spatial_grid_pixels_view works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      create_location_periods_table(conn_pg)
      create_shapes_table(conn_pg)
      create_all_covariates_table(conn_pg)
      create_raster_covariate_collections_table(conn_pg)
      create_master_spatial_grid_table(conn_pg)
      create_spatial_resolutions_table(conn_pg)
      create_resize_spatial_grid_function(conn_pg)
      create_resized_spatial_grids_view(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_resized_spatial_grid_pixels_view(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_resized_spatial_grid_pixels_view(conn_pg)
    }
  )
})

test_that("Create location_period_raster_map view works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      create_location_periods_table(conn_pg)
      create_shapes_table(conn_pg)
      create_all_covariates_table(conn_pg)
      create_raster_covariate_collections_table(conn_pg)
      create_master_spatial_grid_table(conn_pg)
      create_spatial_resolutions_table(conn_pg)
      create_resize_spatial_grid_function(conn_pg)
      create_resized_spatial_grids_view(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_location_period_raster_map_view(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_location_period_raster_map_view(conn_pg)
    }
  )
})

test_that("Create covariate_grid_map view works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_locations_table(conn_pg)
      create_location_periods_table(conn_pg)
      create_shapes_table(conn_pg)
      create_all_covariates_table(conn_pg)
      create_raster_covariate_collections_table(conn_pg)
      create_master_spatial_grid_table(conn_pg)
      create_spatial_resolutions_table(conn_pg)
      create_resize_spatial_grid_function(conn_pg)
      create_resized_spatial_grids_view(conn_pg)
      create_all_covariates_table(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_covariate_grid_map_view(conn_pg)
    },
    NA
  )
  expect_error(
    {
      create_covariate_grid_map_view(conn_pg)
    }
  )
})

test_that("create_testing_base_database works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_testing_base_database(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      create_testing_base_database(conn_pg, TRUE)
    },
    NA
  )
})

test_that("create_testing_additional_database works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      create_testing_base_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      create_testing_additional_database(conn_pg, FALSE)
    },
    NA
  )
  expect_error(
    {
      create_testing_additional_database(conn_pg, TRUE)
    },
    NA
  )
})

test_that("refresh_materialized_views works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )

  expect_error(
    {
      setup_testing_database(conn_pg, TRUE)
    },
    NA
  )
  expect_error(
    {
      refresh_materialized_views(conn_pg)
    },
    NA
  )
  expect_error(
    {
      refresh_materialized_views(conn_pg)
    },
    NA
  )
})

test_that("destroy_testing_database works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )
  expect_error(
    {
      destroy_testing_database(conn_pg)
    },
    NA
  )
  expect_error(
    {
      destroy_testing_database(conn_pg)
    },
    NA
  )
})

test_that("We can add locations to testing database", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )
  destroy_testing_database(conn_pg)
  setup_testing_database(conn_pg, FALSE)
  expect_error(
    {
      insert_testing_locations(conn_pg, location_df)
    },
    NA
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM locations")[["qualified_name"]]
    },
    location_df[["qualified_name"]]
  )
})

test_that("We can add location_periods to testing database", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )
  destroy_testing_database(conn_pg)
  setup_testing_database(conn_pg, FALSE)
  insert_testing_locations(conn_pg, location_df)
  expect_error(
    {
      insert_testing_location_periods(conn_pg, location_period_df)
    },
    NA
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM location_periods")[["start_date"]]
    },
    location_period_df[["start_date"]]
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM location_periods")[["end_date"]]
    },
    location_period_df[["end_date"]]
  )
})

test_that("We can add shapefiles to testing database", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )
  destroy_testing_database(conn_pg)
  setup_testing_database(conn_pg, FALSE)
  insert_testing_locations(conn_pg, location_df)
  insert_testing_location_periods(conn_pg, location_period_df)
  expect_error(
    {
      insert_testing_shapefiles(conn_pg, shapes_df)
    },
    NA
  )
  if (is.na(sf::st_crs(shapes_df))) {
    sf::st_crs(shapes_df) <- 4326
  }
  expect_equal(
    {
      sf::st_as_sfc(DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM shapes")[["shape"]])
    },
    shapes_df[["geom"]]
  )

  expect_true({
    all(
      sf::st_as_sfc(DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM shapes")[["box"]]) ==
        rep(sf::st_as_sfc(sf::st_bbox(shapes_df[["geom"]])), times = 2)
    )
  })
})

test_that("We can add observations to testing database", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )
  destroy_testing_database(conn_pg)
  setup_testing_database(conn_pg, FALSE)
  insert_testing_locations(conn_pg, location_df)
  insert_testing_location_periods(conn_pg, location_period_df)
  expect_error(
    {
      insert_testing_observations(conn_pg, observations_df)
    },
    NA
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["time_left"]]
    },
    observations_df[["time_left"]]
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["time_right"]]
    },
    observations_df[["time_right"]]
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["primary"]]
    },
    observations_df[["primary"]]
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["phantom"]]
    },
    observations_df[["phantom"]]
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["suspected_cases"]]
    },
    observations_df[["suspected_cases"]]
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["confirmed_cases"]]
    },
    observations_df[["confirmed_cases"]]
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["deaths"]]
    },
    observations_df[["deaths"]]
  )
})

test_that("We can ingest spatial grids", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )
  destroy_testing_database(conn_pg)
  setup_testing_database(conn_pg, FALSE)
  insert_testing_locations(conn_pg, location_df)
  insert_testing_location_periods(conn_pg, location_period_df)
  insert_testing_shapefiles(conn_pg, shapes_df)
  insert_testing_observations(conn_pg, observations_df)
  expect_error(
    {
      ingest_spatial_grid(conn_pg, 1, 1)
    },
    NA
  )
  expect_equal(
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT COUNT(*) FROM grids.master_spatial_grid")
    },
    {
      DBI::dbGetQuery(conn = conn_pg, "SELECT COUNT(*) FROM grids.resized_spatial_grids WHERE width = 1 AND height = 1")
    },
  )
  expect_error(
    {
      ingest_spatial_grid(conn_pg, 5, 5)
    },
    NA
  )
})



test_that("Conversion between simulation framework formats and testing framework formats works", {
  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_POSTGRES_DATABASE", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      destroy_testing_database(conn_pg)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )
  destroy_testing_database(conn_pg)
  setup_testing_database(conn_pg, FALSE)
  tryCatch(
    {
      full_simulation_data <- create_standardized_test_data(
        nrow = 4, ncol = 4,
        nlayers = 2, base_number = 1, n_layers = 2, factor = 4, seed = my_seed_1,
        polygon_proportion_observed = 1
      )
    },
    error = function(e) {
      expect_error(
        {
          stop(paste("Could not setup initial data for simulation data"))
        },
        NA
      )
      skip(paste("Could not setup initial data for simulation data"))
    }
  )
  tryCatch(
    {
      full_dfs_and_covar_funs <- list(
        dataframes = list(
          location_df = location_df,
          location_period_df = location_period_df, shapes_df = shapes_df, observations_df = observations_df
        ),
        covariate_function_list = list(list(
          name = "test covariate", start_date = lubridate::ymd("2000-01-01"),
          end_date = lubridate::ymd("2001-12-31"), fun = function(psql_connection) {
            return(raster::raster(nrow = 3, ncol = 3, vals = 1))
          }
        ))
      )
    },
    error = function(e) {
      expect_error(
        stop(paste("Could not setup initial data for testing data")),
        NA
      )
      skip(paste("Could not setup initial data for testing data"))
    }
  )

  ## Direction: simulated covariates -> test_covariates
  expect_error(
    {
      test_covar_fun_list <- convert_simulated_covariates_to_test_covariate_funs(full_simulation_data$covariates,
        min_time_left = min(full_simulation_data$observed_polygons$time_left),
        max_time_right = max(full_simulation_data$observed_polygons$time_right)
      )
      lhs <- convert_test_covariate_funs_to_simulation_covariates(test_covar_fun_list)
    },
    NA
  )

  expect_error(
    {
      all_dfs_and_covar_funs <- convert_simulated_data_to_test_dataframes(full_simulation_data)
      setup_testing_database_from_dataframes(
        conn_pg, all_dfs_and_covar_funs$dataframes,
        all_dfs_and_covar_funs$covariate_function_list
      )
    },
    NA
  )

  expect_error(
    {
      all_dfs_and_covar_funs <- convert_simulated_data_to_test_dataframes(full_simulation_data)
      convert_test_dfs_to_simulation_observed_polygons(
        all_dfs_and_covar_funs$dataframes$shapes_df,
        all_dfs_and_covar_funs$dataframes$observations_df
      )
    },
    NA
  )

  expect_true({
    test_covar_fun_list <- convert_simulated_covariates_to_test_covariate_funs(full_simulation_data$covariates,
      min_time_left = min(full_simulation_data$observed_polygons$time_left),
      max_time_right = max(full_simulation_data$observed_polygons$time_right)
    )
    lhs <- convert_test_covariate_funs_to_simulation_covariates(test_covar_fun_list)
    rhs <- full_simulation_data$covariates
    first_covariate <- TRUE
    all(mapply(x = lhs, y = rhs, function(x, y) {
      ## Geometries are equal
      rc <- all(sf::st_geometry(x) == sf::st_geometry(y))
      x <- sf::st_drop_geometry(x)
      if (first_covariate) {
        y$covariate[y$covariate > log(2^32 - 1) / log(10)] <- log(2^32 -
          1) / log(10)
        y$covariate[y$covariate < log(2^(-31)) / log(10)] <- log(2^(-31)) / log(10)
        first_covariate <<- FALSE
      }

      y <- sf::st_drop_geometry(y)
      rc <- all(x[, c("id", "row", "col", "t")] == x[, c(
        "id", "row", "col",
        "t"
      )])
      rc <- rc && all(abs(x[, "covariate"] - y[, "covariate"]) < 1e-06)
      return(rc)
    }))
  })


  ## Direction: test_covariates -> simulated covariates
  expect_error(
    {
      simulation_covariates <- convert_test_covariate_funs_to_simulation_covariates(full_dfs_and_covar_funs$covariate_function_list)
      convert_simulated_covariates_to_test_covariate_funs(simulation_covariates,
        min_time_left = min(full_dfs_and_covar_funs$dataframes$observations_df$time_left),
        max_time_right = max(full_dfs_and_covar_funs$dataframes$observations_df$time_right)
      )
    },
    NA
  )

  expect_error(
    {
      simulation_polygons <- convert_test_dfs_to_simulation_observed_polygons(
        full_dfs_and_covar_funs$dataframes$shapes_df,
        full_dfs_and_covar_funs$dataframes$observations_df
      )
      convert_simulated_polygons_to_test_dataframes(simulation_polygons)
    },
    NA
  )


  expect_true({
    all_dfs_and_covar_funs <- convert_simulated_data_to_test_dataframes(full_simulation_data)
    lhs <- convert_test_dfs_to_simulation_observed_polygons(
      all_dfs_and_covar_funs$dataframes$shapes_df,
      all_dfs_and_covar_funs$dataframes$observations_df
    )
    rhs <- full_simulation_data$observed_polygons
    attributes(rhs)[["seed"]] <- NULL
    all.equal(lhs, rhs)
  })


  ## Direction 2
  expect_true(
    {
      simulation_covariates <- convert_test_covariate_funs_to_simulation_covariates(full_dfs_and_covar_funs$covariate_function_list)
      lhs <- convert_simulated_covariates_to_test_covariate_funs(simulation_covariates,
        min_time_left = min(full_dfs_and_covar_funs$dataframes$observations_df$time_left),
        max_time_right = max(full_dfs_and_covar_funs$dataframes$observations_df$time_right)
      )
      rhs <- full_dfs_and_covar_funs$covariate_function_list
      all(c(
        mapply(lhs = lhs, rhs = rhs, function(lhs, rhs) {
          return(all(c(
            isTRUE(all.equal(lhs[2:3], rhs[2:3])),
            isTRUE(all.equal(lhs$fun(), rhs$fun()))
          )))
        })
      ))
    },
    full_dfs_and_covar_funs$dataframes$covariate_function_list
  )

  expect_true({
    simulation_polygons <- convert_test_dfs_to_simulation_observed_polygons(
      full_dfs_and_covar_funs$dataframes$shapes_df,
      full_dfs_and_covar_funs$dataframes$observations_df
    )
    lhs <- convert_simulated_polygons_to_test_dataframes(simulation_polygons)
    rhs <- full_dfs_and_covar_funs$dataframes[c("location_df", "location_period_df", "shapes_df")]
    all(c(
      isTRUE(all.equal(lhs[[1]], rhs[[1]])),
      isTRUE(all.equal(lhs[[2]], rhs[[2]])),
      isTRUE(all.equal(lhs[[1]], rhs[[1]]))
    ))
  })
})
