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
  ingest = T
) {
  
  # Preamble ---------------------------------------------------------------------
  library(tidyverse)
  library(magrittr)
  library(DBI)
  library(foreach)
  library(raster)
  library(sf)
  library(rgdal)
  library(gdalUtils)
  library(glue)
  library(ncdf4)
  
  # Get files
  # source(stringr::str_c(cholera_directory, "/Analysis/R/covariate_helpers.R"))
  
  # Inputs ------------------------------------------------------------------
  
  # User-defined parameters
  # Spatial resolution
  res_x <- res_space    # longitude resolution in km
  res_y <- res_space    # latitude resolution in km
  
  # Other parameters
  km_to_deg <- 1/110.57    # how many degrees is a km at the equator
  
  # Other objects
  conn_pg <- taxdat::connect_to_db(dbuser)
  
  # Process grid ------------------------------------------------------------------
  
  cat("**** PROCESSING GRID ****\n")
  
  # Check if the master grid is set
  if (DBI::dbExistsTable(conn=conn_pg, DBI::Id(schema = "grids", table="master_grid"))) {
    
    cat("---- Found master grid \n")
    
  } else {
    
    if (!ingest)
      stop("Couldn't find master grid in database. This needs to be ingested by authorized users.")
    
    # make sure this exists
    master_grid_file <- "/home/big_cholera_data/Layers/pop/WORLD/ppp_2000_1km_Aggregated.tif" 
    
    cat("---- Couldn't find master grid, importing it from", master_grid_file, "\n")
    
    if (!file.exists(master_grid_file))
      stop("The file doesn't exists, try pulling it with git-lfs")
    
    r2psql_cmd <-glue::glue("raster2pgsql -s EPSGS:4326 -I -t auto -d {master_grid_file}  grids.master_grid | 
                     psql -w -h localhost -d cholera_covariates")
    system(r2psql_cmd)
    
    DBI::dbSendStatement(conn_pg, "UPDATE grids.master_grid SET rast = ST_Reclass(rast, 1, '[0-1000000]:1', '32BF', 0);")
    DBI::dbSendStatement(conn_pg, "SELECT AddRasterConstraints('grids'::name, 'master_grid'::name, 'rast'::name);")
  }
  
  # Set the desired reference grid name
  grid_name <- stringr::str_c("grid_", res_x, "_", res_y)
  grid_in_db <- taxdat::db_exists_table_multi(conn_pg, c("public", "grids"), grid_name)
  
  if (sum(grid_in_db) == 0) {
    
    if (!ingest)
      stop(glue::glue("Couldn't find reference grid at spatial resolution {res_x}x{res_y}km in database. This needs to be ingested by authorized users."))
    
    cat("Couldn't find grid at", res_x, "x", res_y, "[km] resolution, computing it.\n")
    
    tmp_rast <- stringr::str_c(raster::tmpDir(), "grid_resampled.tif")
    ref_grid <- glue::glue("PG:\"host=localhost dbname=cholera_covariates schema=grids table=master_grid user={dbuser} mode=2\"")
    
    # Aggregate
    taxdat::gdalwarp2(ref_grid, tmp_rast,
              tr = c(res_x, res_y) * km_to_deg, 
              t_srs = "EPSG:4326",
              s_srs = "EPSG:4326")
    # Write to database
    r2psql_cmd <- glue::glue("raster2pgsql -s EPSGS:4326 -I -C -t auto -d {tmp_rast} {grid_name} | psql -w -h localhost -d cholera_covariates")
    system(r2psql_cmd)
    
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