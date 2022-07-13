test_that("Writing covariates to postgres preserves dimension", {
  # covariate_file <- "data/resizing_raster.tif"
  covariate_file <- "data/resizing_raster.tif"

  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
  skip_if_not(dbuser == "app") ## Check for on docker
  tryCatch(
    {
      conn_pg <- connect_to_db(dbname = dbname, dbuser = dbuser)
      database_working <- TRUE
    },
    error = function(e) {
      skip(paste("Could not connect to database", dbname, "as user", dbuser, "with message", e$message))
    }
  )
  expect_true(file.exists(covariate_file))
  expect_error(
    {
      write_raster_to_postgres(
        psql_connection = conn_pg, raster_to_write = raster::raster(covariate_file),
        table_name = "covariates.test_covariate", overwrite = TRUE
      )
    },
    NA
  )
  expect_equal(
    {
      raster::values(rpostgis::pgGetRast(conn_pg, c("covariates", "test_covariate")))
    },
    raster::values(raster::raster(covariate_file))
  )

  expect_true({
    sf::st_crs(rpostgis::pgGetRast(conn_pg, c("covariates", "test_covariate"))) ==
      sf::st_crs(raster::raster(covariate_file))
  })
})
