#' @title Prepare grid
#' @description Prepares the projection of the 3D spatio-temporal modeling grid
#' onto the 2D spatial coordinates.
#'
#' @param dbuser the username for the cholera_covariates postgres database
#' @param cholera_directory the directory from which the analysis is run
#' @param res_space the spatial resolution of the grid in km
#'
#' @return the name of the grid
#'
prepare_grid <- function(
    dbuser,
    cholera_directory,
    res_space,
    ingest = T,
    aoi_name = 'raw'
) {

  # Preamble ---------------------------------------------------------------------


  # Inputs ------------------------------------------------------------------

  # User-defined parameters
  # Spatial resolution
  res_x <- res_space    # longitude resolution in km
  res_y <- res_space    # latitude resolution in km

  # Other parameters
  km_to_deg <- 1 / 110.57    # how many degrees is a km at the equator

  # Other objects
  conn_pg <- taxdat::connect_to_db(dbuser)

  # Area of interest (for cropping rasters pre-ingestion)
  # This is mostly for testing purposes
  aois <- list(
    list(name = "raw", extent = NULL),
    list(name = "SSD", extent = raster::extent(23, 37, 3, 13)),   # SSD
    list(name = "KEN", extent = raster::extent(33, 42, -5.2, 5)),   # Kenya
    list(name = "SSA", extent = raster::extent(-18.8, 52.6, -35.4, 28))   # SSA
  )
  aoi_names <- purrr::map_chr(aois, "name")
  if (!(aoi_name %in% aoi_names)) {
    stop("Area of interest ", aoi_name, " not among pre-defined areas (", stringr::str_c(aoi_names, collapse = ","), ")")
  } else {
    aoi <- aois[[which(aoi_name == aoi_names)]]
  }

  # Process grid ------------------------------------------------------------------

  cat("**** PROCESSING GRID ****\n")

  # Check if the master grid is set
  if (DBI::dbExistsTable(conn = conn_pg, DBI::Id(schema = "grids", table = "master_grid"))) {

    cat("---- Found master grid \n")

  } else {

    if (!ingest)
      stop("Couldn't find master grid in database. This needs to be ingested by authorized users.")

    # make sure this exists
    master_grid_url <- paste0(
      "ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020/",
      2020,
      "/0_Mosaicked/ppp_",
      2020,
      "_1km_Aggregated.tif"
    )

    if (!dir.exists("Layers/pop_old")) {
      dir.create("Layers/pop_old")
    }

    master_grid_filename <- paste0("Layers/pop_old/ppp_", 2020, "_1km_Aggregated.tif")

    cat("---- Couldn't find master grid, importing it from", master_grid_filename, "\n")

    if (!all(file.exists(master_grid_filename))) {
      missing_indices <- which(!file.exists(master_grid_filename))
      for (missing_index in missing_indices) {
        utils::download.file(url = master_grid_url[missing_index], destfile = master_grid_filename[missing_index])
      }
    }
    if (!all(file.exists(master_grid_filename))) {
      stop("The file doesn't exists, try pulling it with git-lfs")
    }

    ## Adding cropping to aoi:
    r <- raster::stack(master_grid_filename)
    if (!is.null(aoi$extent)) {
      cat(paste("Cropping",master_grid_filename, "to", "[", stringr::str_c(c("xmin:", ", xmax:", ", ymin:", ", ymax:"), as.vector(aoi$extent)), "]\n"))
      r <- raster::crop(r, aoi$extent)
    }
    master_grid_filename <- gsub('.tif$', paste0('.cropped.',aoi_name, '.tif'), master_grid_filename)
    raster::writeRaster(x = r, filename = master_grid_filename)

    conn_string <- taxdat::get_covariate_conn_string(dbuser)

    r2psql_cmd <- glue::glue("raster2pgsql -s EPSG:4326 -I -t auto -d {master_grid_filename}  grids.master_grid |
                     psql {conn_string}")
    err <- system(r2psql_cmd)
    if (err != 0) {
      stop(paste("System command", r2psql_cmd, "failed"))
    }

    update_query <- "UPDATE grids.master_grid SET rast = ST_Reclass(rast, 1, '[0-1000000]:1', '32BF', 0);"
    DBI::dbClearResult(DBI::dbSendStatement(conn_pg, update_query))
    DBI::dbClearResult(DBI::dbSendStatement(conn_pg, "UPDATE grids.master_grid SET rast = ST_SetBandNoDataValue(rast,1, NULL);"))
    DBI::dbClearResult(DBI::dbSendStatement(conn_pg, "SELECT AddRasterConstraints('grids'::name, 'master_grid'::name, 'rast'::name);"))
  }

  # Set the desired reference grid name
  grid_name <- stringr::str_c("grid_", res_x, "_", res_y)
  grid_in_db <- taxdat::db_exists_table_multi(conn_pg, c("public", "grids"), grid_name)

  if (sum(grid_in_db) == 0) {

    if (!ingest)
      stop(glue::glue(paste(
        "Couldn't find reference grid at spatial resolution {res_x}x{res_y}km in database.",
        "This needs to be ingested by authorized users."
      )))

    cat("Couldn't find grid at", res_x, "x", res_y, "[km] resolution, computing it.\n")

    tmp_rast <- stringr::str_c(raster::tmpDir(), "grid_resampled.tif")
    cholera_password <- Sys.getenv("COVARIATE_DATABASE_PASSWORD", "")
    ref_grid <- glue::glue(
      "PG:\"host=localhost dbname=cholera_covariates schema=grids table=master_grid user={dbuser} password={cholera_password} mode=2\""
    )

    # Aggregate
    taxdat::gdalwarp2(ref_grid, tmp_rast,
                      tr = c(res_x, res_y) * km_to_deg,
                      t_srs = "EPSG:4326",
                      s_srs = "EPSG:4326")
    # Write to database
    # set host as localhost, this part can't run in docker right now.
    conn_string <- taxdat::get_covariate_conn_string(dbuser)
    r2psql_cmd <- glue::glue(
      "raster2pgsql -s EPSGS:4326 -I -C -t auto -d {tmp_rast} {grid_name} | psql {conn_string}"
    )
    err <- system(r2psql_cmd)
    if (err != 0) {
      stop(paste("System command", r2psql_cmd, "failed"))
    }

    # Create centroids table and spatial index
    taxdat::build_geoms_query(conn_pg, schema = "public", grid_name, "centroids")
    taxdat::build_geoms_query(conn_pg, schema = "public", grid_name, "polygons")

    # Set the schema to public
    grid_schema <- "public"

  } else {
    grid_schema <- names(grid_in_db)[grid_in_db][1]

    cat("---- Found grid at ", res_x, "x", res_y, " km resolution in schema '", grid_schema, "'\n", sep = "")
  }

  full_grid_name <- stringr::str_c(grid_schema, grid_name, sep = ".")

  cat("**** DONE GRID ****\n")

  return(full_grid_name)
}
