# Pipeline step: extract the covariate cube and the location-period dictionary.

#' @title Prepare covariate cube
#' @name prepare_covar_cube
#' @description Extracts every covariate at each grid cell and time slice of the
#' run, builds the space-time grid, the location-period dictionary with
#' population weights, and applies the spatial-fraction filters.
#'
#' @param covar_list schema-qualified covariate tables, population first
#' @param config the run's config (names the per-run tables)
#' @param full_grid_name schema-qualified modelling grid
#' @param time_slices model time slices (TL, TR)
#' @param res_space,res_time model resolutions
#' @param covariate_transformations transformations, see `transform_covariates`
#' @param sfrac_thresh_border drop cells whose largest population fraction in a
#'   bordering location period is below this
#' @param sfrac_thresh_conn drop cell-to-location-period connections below this
#' @param conn optional DBI connection
#' @return list(covar_cube, sf_grid, non_na_gridcells, location_periods_dict)
#' @export
prepare_covar_cube <- function(covar_list, config, full_grid_name, time_slices,
                               res_space, res_time, covariate_transformations = NULL,
                               sfrac_thresh_border, sfrac_thresh_conn, conn = NULL) {
  if (is.null(conn)) {
    conn <- connect_to_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  n_time_slices <- nrow(time_slices)
  n_covar <- length(covar_list)
  cntrd_table <- run_table_name(config, "grid_cntrds")
  n_grid_cells <- as.numeric(DBI::dbGetQuery(conn, glue::glue_sql(
    "SELECT COUNT(*) AS n FROM {sql_table(conn, cntrd_table)};", .con = conn))$n)

  covar_cube <- array(0, c(n_grid_cells, n_time_slices, n_covar))
  dimnames(covar_cube)[[3]] <- sub("^.*\\.", "", covar_list)

  for (j in seq_along(covar_list)) {
    vals <- get_covariate_values(covar_name = covar_list[j], cntrd_table = cntrd_table,
                                 time_slices = time_slices, res_time = res_time,
                                 conn_pg = conn)
    if (nrow(vals) == 0) {
      stop("Couldn't find data to extract from covariate ", covar_list[j])
    }
    # Arrange by pixel id for consistency with the dictionary
    dat <- vals %>%
      dplyr::arrange(rid, x, y) %>%
      dplyr::select(dplyr::starts_with("values_")) %>%
      as.matrix()
    if (ncol(dat) == 1 && n_time_slices > 1) {
      # static covariate: same value at every time slice
      dat <- matrix(rep(dat, n_time_slices), nrow = nrow(dat))
    }
    if (!identical(dim(dat), dim(covar_cube)[1:2])) {
      stop("Dimensions of extraction for covariate ", covar_list[j], " (",
           paste(dim(dat), collapse = " x "), ") do not match the cube (",
           paste(dim(covar_cube)[1:2], collapse = " x "), ")")
    }
    covar_cube[, , j] <- dat
    cat("---- Done ", covar_list[j], "\n")
  }

  # Cells with population >= 1 and every covariate present
  non_na_gridcells <- which(apply(covar_cube, 1:2, function(x) (x[1] >= 1) && !anyNA(x)))

  # Space-time grid
  sf_grid <- sf::st_read(conn, query = glue::glue_sql(
    "SELECT p.* FROM {sql_table(conn, full_grid_name, '_polys')} p
     INNER JOIN {sql_table(conn, cntrd_table)} c
     ON p.rid = c.rid AND p.x = c.x AND p.y = c.y;", .con = conn), quiet = TRUE) %>%
    dplyr::arrange(rid, x, y) %>%
    dplyr::mutate(id = dplyr::row_number())
  if (nrow(sf_grid) != n_grid_cells || anyDuplicated(sf::st_drop_geometry(sf_grid)[, c("rid", "x", "y")])) {
    stop("Grid polygons (", nrow(sf_grid), ") do not match the run's centroids (", n_grid_cells,
         ") one to one; cell ids would be misaligned.")
  }
  sf_grid <- do.call(rbind, lapply(seq_len(n_time_slices), function(t) {
    sf_grid$t <- t
    sf_grid
  })) %>%
    dplyr::mutate(long_id = dplyr::row_number())  # overall cell id, 1 .. n_space * n_time

  lp_name <- run_table_name(config, "location_periods")
  intersections_table <- run_table_name(config, "grid_intersections")
  location_periods_dict <- make_location_periods_dict(
    conn_pg = conn, lp_name = lp_name, intersections_table = intersections_table,
    cntrd_table = cntrd_table, res_space = res_space, sf_grid = sf_grid,
    grid_changer = make_changer(x = non_na_gridcells), res_time = res_time,
    time_slices = time_slices)

  # Drop cells outside the output summary shapefiles
  output_cells <- DBI::dbGetQuery(conn, glue::glue_sql(
    "SELECT DISTINCT rid, x, y FROM {sql_table(conn, run_table_name(config, 'grid_cntrds', output = TRUE))};",
    .con = conn))
  sf_grid_drop <- sf_grid %>%
    dplyr::left_join(dplyr::mutate(output_cells, include = TRUE), by = c("rid", "x", "y")) %>%
    dplyr::filter(is.na(include))
  if (nrow(sf_grid_drop) > 0) {
    cat("---- Dropping", nrow(sf_grid_drop), "space-time cells that do not overlap the output shapefiles.\n")
    non_na_gridcells <- setdiff(non_na_gridcells, sf_grid_drop$long_id)
  } else {
    cat("---- All cells within output summary shapefiles.\n")
  }
  location_periods_dict <- dplyr::inner_join(location_periods_dict, output_cells, by = c("rid", "x", "y"))

  # Cells whose largest population fraction in a bordering location period is low
  low_sfrac <- location_periods_dict %>%
    dplyr::group_by(rid, x, y) %>%
    dplyr::slice_max(pop_weight) %>%
    dplyr::filter(pop_weight < sfrac_thresh_border & !lp_covered & area_ratio > 2) %>%
    dplyr::select(rid, x, y) %>%
    dplyr::inner_join(sf::st_drop_geometry(sf_grid), by = c("rid", "x", "y"))
  if (nrow(low_sfrac) > 0) {
    cat("---- Dropping", nrow(low_sfrac), "space grid cells because the max sfrac is below",
        sfrac_thresh_border, ".\n")
    non_na_gridcells <- setdiff(non_na_gridcells, low_sfrac$long_id)
  } else {
    cat("---- No cells with sfrac below", sfrac_thresh_border, ".\n")
  }

  # Temporal consistency of the kept cells
  non_na_gridcells <- make_temporal_grid_consistency(sf_grid = sf_grid,
                                                     non_na_gridcells = non_na_gridcells)
  grid_changer <- make_changer(x = non_na_gridcells)

  location_periods_dict <- location_periods_dict %>%
    dplyr::filter(!(long_id %in% low_sfrac$long_id)) %>%
    # Reset upd_long_id with the new grid_changer
    dplyr::mutate(upd_long_id = grid_changer[as.character(long_id)]) %>%
    dplyr::filter(!is.na(upd_long_id)) %>%
    dplyr::mutate(connect_id = dplyr::row_number())

  low_sfrac_connections <- location_periods_dict %>%
    dplyr::filter(pop_weight < sfrac_thresh_conn & !lp_covered & area_ratio > 2)
  cat("Dropping", nrow(low_sfrac_connections), "/", nrow(location_periods_dict),
      "connections between grid cells and location periods which have sfrac <",
      sfrac_thresh_conn, "\n")
  location_periods_dict <- dplyr::filter(location_periods_dict,
                                         !(connect_id %in% low_sfrac_connections$connect_id))

  cat("**** FINISHED EXTRACTING COVARIATE CUBE OF DIMENSIONS",
      paste0(dim(covar_cube), collapse = "x"), "[n_pix x n_time x n_covar]\n")

  covar_cube <- transform_covariates(covar_cube, covariate_transformations)

  list(covar_cube = covar_cube, sf_grid = sf_grid, non_na_gridcells = non_na_gridcells,
       location_periods_dict = location_periods_dict)
}
