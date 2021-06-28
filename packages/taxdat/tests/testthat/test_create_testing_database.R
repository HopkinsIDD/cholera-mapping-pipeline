test_that("Create observation table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_observations_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_observations_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_observations_table(conn_pg)
    })
    expect_error({
        create_observations_table(conn_pg, FALSE)
    })

})

test_that("Create locations table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_locations_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_locations_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_locations_table(conn_pg)
    })
    expect_error({
        create_locations_table(conn_pg, FALSE)
    })

})

test_that("Create location_periods table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_location_periods_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_location_periods_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_location_periods_table(conn_pg)
    })
    expect_error({
        create_location_periods_table(conn_pg, FALSE)
    })

})

test_that("Create shapes table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_shapes_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_shapes_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_shapes_table(conn_pg)
    })
    expect_error({
        create_shapes_table(conn_pg, FALSE)
    })

})

test_that("Create location_hierarchies table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_location_hierarchies_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_location_hierarchies_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_location_hierarchies_table(conn_pg)
    })
    expect_error({
        create_location_hierarchies_table(conn_pg, FALSE)
    })

})

test_that("Create master_spatial_grid table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_master_spatial_grid_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_master_spatial_grid_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_master_spatial_grid_table(conn_pg)
    })
    expect_error({
        create_master_spatial_grid_table(conn_pg, FALSE)
    })

})

test_that("Create resized_spatial_grids table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_resized_spatial_grids_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_resized_spatial_grids_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_resized_spatial_grids_table(conn_pg)
    })
    expect_error({
        create_resized_spatial_grids_table(conn_pg, FALSE)
    })

})

test_that("Create all_covariates table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_all_covariates_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_all_covariates_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_all_covariates_table(conn_pg)
    })
    expect_error({
        create_all_covariates_table(conn_pg, FALSE)
    })

})

test_that("Create time_bounds table works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_time_bounds_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_time_bounds_table(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_time_bounds_table(conn_pg)
    })
    expect_error({
        create_time_bounds_table(conn_pg, FALSE)
    })

})

test_that("Create master_temporal_grid_view works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_master_temporal_grid_view(conn_pg)
    }, NA)
    expect_error({
        create_master_temporal_grid_view(conn_pg)
    }, NA)
})

test_that("Create resized_spatial_grid_polygons_view works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_resized_spatial_grid_polygons_view(conn_pg)
    }, NA)
    expect_error({
        create_resized_spatial_grid_polygons_view(conn_pg)
    }, NA)
})

test_that("Create resized_spatial_grid_centroids_view works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_resized_spatial_grid_centroids_view(conn_pg)
    }, NA)
    expect_error({
        create_resized_spatial_grid_centroids_view(conn_pg)
    }, NA)
})

test_that("Create resized_spatial_grid_polygons view works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_resized_spatial_grid_polygons_view(conn_pg)
    }, NA)
    expect_error({
        create_resized_spatial_grid_polygons_view(conn_pg)
    }, NA)
})

test_that("Create location_period_raster_map view works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_location_period_raster_map_view(conn_pg)
    }, NA)
    expect_error({
        create_location_period_raster_map_view(conn_pg)
    }, NA)
})

test_that("Create covariate_grid_map view works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_covariate_grid_map_view(conn_pg)
    }, NA)
    expect_error({
        create_covariate_grid_map_view(conn_pg)
    }, NA)
})

test_that("create_testing_base_database works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_testing_base_database(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_testing_base_database(conn_pg, TRUE)
    }, NA)
})

test_that("create_testing_additional_database works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        create_testing_additional_database(conn_pg, TRUE)
    }, NA)
    expect_error({
        create_testing_additional_database(conn_pg, TRUE)
    }, NA)
})

test_that("refresh_materialized_views works", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })

    expect_error({
        setup_testing_database(conn_pg, TRUE)
    }, NA)
    expect_error({
        refresh_materialized_views(conn_pg)
    }, NA)
    expect_error({
        refresh_materialized_views(conn_pg)
    }, NA)
})


test_that("destroy_testing_database works", {

    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })
    expect_error({
        destroy_testing_database(conn_pg)
    }, NA)
    expect_error({
        destroy_testing_database(conn_pg)
    }, NA)
})

test_that("We can add locations to testing database", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })
    destroy_testing_database(conn_pg)
    setup_testing_database(conn_pg, FALSE)
    expect_error({
        insert_testing_locations(conn_pg, location_df)
    }, NA)
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM locations")[["qualified_name"]]
    }, location_df[["qualified_name"]])
})

test_that("We can add location_periods to testing database", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })
    destroy_testing_database(conn_pg)
    setup_testing_database(conn_pg, FALSE)
    insert_testing_locations(conn_pg, location_df)
    expect_error({
        insert_testing_location_periods(conn_pg, location_period_df)
    }, NA)
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM location_periods")[["start_date"]]
    }, location_period_df[["start_date"]])
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM location_periods")[["end_date"]]
    }, location_period_df[["end_date"]])
})

test_that("We can add shapefiles to testing database", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })
    destroy_testing_database(conn_pg)
    setup_testing_database(conn_pg, FALSE)
    insert_testing_locations(conn_pg, location_df)
    insert_testing_location_periods(conn_pg, location_period_df)
    expect_error({
        insert_testing_shapefiles(conn_pg, shapes_df)
    }, NA)
    expect_equal({
        sf::st_as_sfc(DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM shapes")[["shape"]])
    }, shapes_df[["geom"]])
    expect_equal({
        sf::st_as_sfc(DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM shapes")[["box"]])
    }, sf::st_as_sfc(sf::st_bbox(shapes_df[["geom"]])))
})

test_that("We can add observations to testing database", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })
    destroy_testing_database(conn_pg)
    setup_testing_database(conn_pg, FALSE)
    insert_testing_locations(conn_pg, location_df)
    insert_testing_location_periods(conn_pg, location_period_df)
    expect_error({
        insert_testing_observations(conn_pg, observations_df)
    }, NA)
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["time_left"]]
    }, observations_df[["time_left"]])
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["time_right"]]
    }, observations_df[["time_right"]])
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["primary"]]
    }, observations_df[["primary"]])
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["phantom"]]
    }, observations_df[["phantom"]])
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["suspected_cases"]]
    }, observations_df[["suspected_cases"]])
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["confirmed_cases"]]
    }, observations_df[["confirmed_cases"]])
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT * FROM observations")[["deaths"]]
    }, observations_df[["deaths"]])
})

test_that("We can ingest spatial grids", {
    dbuser <- Sys.getenv("USER", "app")
    dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
    skip_if_not(dbuser == "app")  ## Check for on docker
    tryCatch({
        conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
        database_working <- TRUE
    }, error = function(e) {
        skip(paste("Could not connect to database", dbname, "as user", dbuser))
    })
    destroy_testing_database(conn_pg)
    setup_testing_database(conn_pg, FALSE)
    insert_testing_locations(conn_pg, location_df)
    insert_testing_location_periods(conn_pg, location_period_df)
    insert_testing_observations(conn_pg, observations_df)
    expect_error({
        ingest_spatial_grid(conn_pg, 1, 1)
    }, NA)
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT COUNT(*) FROM grids.master_spatial_grid")
    }, {
        DBI::dbGetQuery(conn = conn_pg, "SELECT COUNT(*) FROM grids.resized_spatial_grids WHERE width = 1 AND height = 1")
    }, )
    expect_error({
        ingest_spatial_grid(conn_pg, 5, 5)
    }, NA)
    expect_equal({
        DBI::dbGetQuery(conn = conn_pg, "SELECT COUNT(*)/5/5 FROM grids.master_spatial_grid")
    }, {
        DBI::dbGetQuery(conn = conn_pg, "SELECT COUNT(*) FROM grids.resized_spatial_grids WHERE width = 5 AND height = 5")
    }, )
})
