# Pipeline step: master grid and modelling grids in the covariates database.

# Degrees per km at the equator. Applied to both axes, so a "20 km" cell is
# 20 km north-south and 20 * cos(latitude) km east-west.
km_to_deg <- 1 / 110.57

#' @title Grid file path
#' @name grid_file_path
#' @description GeoTIFF copy of a modelling grid. Covariate processing warps
#' onto this file, so it does not need database access.
#' @param layers_dir Layers directory
#' @param res_space grid resolution in km
#' @param aoi_name AOI tag ("raw" for none)
#' @return file path
#' @export
grid_file_path <- function(layers_dir, res_space, aoi_name = "raw") {
  file.path(layers_dir, "grids", sprintf("grid_%s_%s_%s.tif", res_space, res_space, aoi_name))
}

#' @title Master grid file path
#' @name master_grid_file_path
#' @param layers_dir Layers directory
#' @param aoi_name AOI tag
#' @return path of the cropped and masked 1 km land mask
#' @export
master_grid_file_path <- function(layers_dir, aoi_name = "raw") {
  file.path(layers_dir, "grids", sprintf("master_grid_%s.tif", aoi_name))
}

#' @title Default WorldPop source for the master grid
#' @name default_master_grid_source
#' @param layers_dir Layers directory
#' @return `CHOLERA_MASTER_GRID_FILE` if set, else the WorldPop 2020 1 km mosaic
#'   under `<layers_dir>/pop_old/`
#' @export
default_master_grid_source <- function(layers_dir) {
  Sys.getenv("CHOLERA_MASTER_GRID_FILE",
             file.path(layers_dir, "pop_old", "ppp_2020_1km_Aggregated.tif"))
}

grid_metadata_row <- function(conn, grid) {
  if (!DBI::dbExistsTable(conn, DBI::Id(schema = "grids", table = "metadata"))) {
    return(NULL)
  }
  row <- DBI::dbGetQuery(conn, glue::glue_sql(
    "SELECT * FROM grids.metadata WHERE grid = {grid};", .con = conn))
  if (nrow(row) == 0) NULL else row
}

write_grid_metadata <- function(conn, grid, aoi, res_km, bbox) {
  am <- aoi_metadata(aoi)
  db_exec(conn, glue::glue_sql("
    INSERT INTO grids.metadata (grid, aoi_name, aoi_buffer_km, bbox_xmin, bbox_xmax,
                                bbox_ymin, bbox_ymax, res_km, built_at)
    VALUES ({grid}, {am$aoi_name}, {am$aoi_buffer_km}, {bbox[1]}, {bbox[3]},
            {bbox[2]}, {bbox[4]}, {res_km}, now())
    ON CONFLICT (grid) DO UPDATE SET
      aoi_name = EXCLUDED.aoi_name, aoi_buffer_km = EXCLUDED.aoi_buffer_km,
      bbox_xmin = EXCLUDED.bbox_xmin, bbox_xmax = EXCLUDED.bbox_xmax,
      bbox_ymin = EXCLUDED.bbox_ymin, bbox_ymax = EXCLUDED.bbox_ymax,
      res_km = EXCLUDED.res_km, built_at = now();", .con = conn))
}

check_grid_aoi <- function(conn, grid, aoi) {
  row <- grid_metadata_row(conn, grid)
  want <- aoi_metadata(aoi)$aoi_name
  if (!is.null(row) && !identical(row$aoi_name, want)) {
    stop("grids.", grid, " was built for area of interest '", row$aoi_name,
         "' but this run asks for '", want, "'. Use a database per area of interest ",
         "(PGDATABASE), or drop the grid tables to rebuild them.")
  }
  invisible(row)
}

#' @title Build the master grid
#' @name build_master_grid
#' @description Crops and masks the WorldPop 1 km raster to the area of
#' interest, turns it into a land mask (1 = populated land, NoData elsewhere),
#' writes it as a GeoTIFF and loads it into `grids.master_grid`.
#'
#' @param conn DBI connection
#' @param source_file WorldPop 1 km GeoTIFF
#' @param aoi output of `get_aoi`, or NULL
#' @param layers_dir Layers directory
#' @return path of the master grid GeoTIFF
#' @export
build_master_grid <- function(conn, source_file, aoi, layers_dir) {
  if (!file.exists(source_file)) {
    stop("Master grid source not found: ", source_file, ". Stage the WorldPop 2020 1 km ",
         "mosaic there or set CHOLERA_MASTER_GRID_FILE (compute nodes cannot download it).")
  }
  aoi_name <- aoi_metadata(aoi)$aoi_name
  out <- master_grid_file_path(layers_dir, aoi_name)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

  message("-- Building master grid for ", aoi_name, " from ", basename(source_file))
  r <- crop_mask_to_aoi(terra::rast(source_file), aoi, mask = TRUE)
  mask <- terra::ifel(!is.na(r) & r >= 0, 1, NA)
  terra::writeRaster(mask, out, datatype = "INT1U", NAflag = 255, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES"))

  raster2pgsql_pipe(out, "grids.master_grid", mode = "create", index = TRUE, constraints = TRUE)
  write_grid_metadata(conn, "master_grid", aoi, res_km = 1, bbox = as.vector(terra::ext(mask))[c(1, 3, 2, 4)])
  out
}

#' @title Prepare grid
#' @name prepare_grid
#' @description Ensures the master grid and the modelling grid at `res_space`
#' exist in the database (schema `grids`), with centroid and polygon tables,
#' and that the modelling grid's GeoTIFF exists for covariate processing.
#'
#' A cell of the modelling grid is valid when at least one 1 km master-grid
#' pixel inside it is valid, so cells outside the area of interest's mask
#' never become grid cells.
#'
#' @param res_space grid resolution in km
#' @param aoi output of `get_aoi`, or NULL for the full master grid
#' @param layers_dir Layers directory
#' @param ingest build missing grids (FALSE stops instead)
#' @param master_grid_source WorldPop 1 km GeoTIFF used to build the master grid
#' @param conn optional DBI connection
#' @return list(full_grid_name, grid_file)
#' @export
prepare_grid <- function(res_space, aoi = NULL, layers_dir, ingest = TRUE,
                         master_grid_source = default_master_grid_source(layers_dir),
                         conn = NULL) {
  if (is.null(conn)) {
    conn <- connect_to_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  aoi_name <- aoi_metadata(aoi)$aoi_name
  cat("**** PROCESSING GRID (", res_space, " km, area of interest: ", aoi_name, ") ****\n", sep = "")

  # Master grid ---------------------------------------------------------------
  master_file <- master_grid_file_path(layers_dir, aoi_name)
  master_in_db <- DBI::dbExistsTable(conn, DBI::Id(schema = "grids", table = "master_grid"))
  if (master_in_db) {
    check_grid_aoi(conn, "master_grid", aoi)
    cat("---- Found master grid\n")
    if (!file.exists(master_file)) {
      if (!ingest) stop("Master grid file ", master_file, " is missing.")
      master_file <- build_master_grid(conn, master_grid_source, aoi, layers_dir)
    }
  } else {
    if (!ingest) {
      stop("Couldn't find grids.master_grid. It needs to be ingested by authorized users.")
    }
    master_file <- build_master_grid(conn, master_grid_source, aoi, layers_dir)
  }

  # Modelling grid ------------------------------------------------------------
  grid_name <- paste0("grid_", res_space, "_", res_space)
  grid_file <- grid_file_path(layers_dir, res_space, aoi_name)
  in_db <- db_exists_table_multi(conn, c("grids", "public"), grid_name)
  schema <- if (any(in_db)) names(in_db)[in_db][1] else "grids"

  if (!file.exists(grid_file) || !any(in_db)) {
    if (!ingest) {
      stop("Couldn't find the ", res_space, " km grid. It needs to be ingested by authorized users.")
    }
    cat("---- Computing ", res_space, " km grid\n", sep = "")
    # Same origin as the master grid, extent rounded outwards to whole cells so
    # the area of interest's buffer is never trimmed.
    tr <- res_space * km_to_deg
    ms <- gdal_grid_spec(master_file)
    te <- c(ms$xmin, ms$ymax - ceiling((ms$ymax - ms$ymin) / tr) * tr,
            ms$xmin + ceiling((ms$xmax - ms$xmin) / tr) * tr, ms$ymax)
    gdal_warp(master_file, grid_file, te = te, tr = c(tr, tr),
              r = "max", srcnodata = 255, dstnodata = 255, ot = "Byte",
              t_srs = "EPSG:4326", s_srs = "EPSG:4326")
  }

  if (!any(in_db)) {
    raster2pgsql_pipe(grid_file, paste0("grids.", grid_name), mode = "create",
                      index = TRUE, constraints = TRUE)
    build_geoms_query(conn, schema = "grids", table_name = grid_name, type = "centroids")
    build_geoms_query(conn, schema = "grids", table_name = grid_name, type = "polygons")
    spec <- gdal_grid_spec(grid_file)
    write_grid_metadata(conn, grid_name, aoi, res_km = res_space, bbox = spec$te)
    schema <- "grids"
  } else {
    check_grid_aoi(conn, grid_name, aoi)
    cat("---- Found ", res_space, " km grid in schema '", schema, "'\n", sep = "")
  }

  # The file and the table must describe the same cells.
  n_db <- DBI::dbGetQuery(conn, glue::glue_sql(
    "SELECT count(*) AS n FROM {`schema`}.{`paste0(grid_name, '_centroids')`};", .con = conn))$n
  n_file <- terra::global(!is.na(terra::rast(grid_file)), "sum")$sum
  if (as.numeric(n_db) != n_file) {
    stop("Grid file ", grid_file, " has ", n_file, " valid cells but ", schema, ".",
         grid_name, "_centroids has ", n_db, ". Rebuild one from the other.")
  }

  cat("**** DONE GRID: ", schema, ".", grid_name, " (", n_file, " cells) ****\n", sep = "")
  list(full_grid_name = paste(schema, grid_name, sep = "."), grid_file = grid_file)
}
