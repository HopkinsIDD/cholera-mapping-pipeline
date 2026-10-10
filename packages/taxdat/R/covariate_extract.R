# Reading covariate values back from the database for a mapping run.

#' @title Get covariate metadata
#' @name get_covariate_metadata
#' @param conn_pg DBI connection
#' @param covar covariate table name, with or without "covariates."
#' @return one-row data frame (src_res_time, res_time, first_tl, last_tl)
#' @export
get_covariate_metadata <- function(conn_pg, covar) {
  covar <- sub("^.*\\.", "", covar)
  DBI::dbGetQuery(conn_pg, glue::glue_sql(
    "SELECT src_res_time, res_time, first_tl, last_tl
     FROM covariates.metadata WHERE covariate = {covar};", .con = conn_pg))
}

#' @title Band-to-date map of a covariate
#' @name get_covariate_bands
#' @description Reads covariates.bands. For databases built before that table
#' existed, reconstructs it from the metadata date range, assuming contiguous
#' bands at the model's resolution.
#'
#' @param conn_pg DBI connection
#' @param covar covariate table name
#' @param res_time model time resolution
#' @return data frame (band, tl, tr)
#' @export
get_covariate_bands <- function(conn_pg, covar, res_time) {
  covar <- sub("^.*\\.", "", covar)
  has_bands <- DBI::dbExistsTable(conn_pg, DBI::Id(schema = "covariates", table = "bands"))
  if (has_bands) {
    b <- DBI::dbGetQuery(conn_pg, glue::glue_sql(
      "SELECT band, tl, tr FROM covariates.bands WHERE covariate = {covar} ORDER BY band;",
      .con = conn_pg))
    if (nrow(b) > 0) {
      return(b)
    }
  }
  meta <- get_covariate_metadata(conn_pg, covar)
  if (nrow(meta) == 0) {
    stop("Couldn't find covariate ", covar, " in covariates.metadata")
  }
  if (meta$src_res_time == "static") {
    return(data.frame(band = 1L, tl = as.Date(NA), tr = as.Date(NA)))
  }
  warning("No covariates.bands rows for ", covar, "; assuming contiguous bands from metadata")
  tl <- seq.Date(as.Date(meta$first_tl), as.Date(meta$last_tl), by = res_time)
  data.frame(band = seq_along(tl), tl = tl,
             tr = time_unit_to_end_function(res_time)(time_unit_to_aggregate_function(res_time)(tl)))
}

#' @title Get temporal bands
#' @name get_temporal_bands
#' @description Band indices of a covariate matching each model time slice.
#'
#' @param model_time_slices data frame of model time slices (TL, TR)
#' @param covar_TL_seq left bounds of the covariate's bands
#' @param covar_TR_seq right bounds of the covariate's bands
#' @return list(ind, tl, tr), one element per model time slice
#' @export
get_temporal_bands <- function(model_time_slices, covar_TL_seq, covar_TR_seq) {
  ind <- vapply(seq_len(nrow(model_time_slices)), function(i) {
    w <- which(covar_TL_seq == model_time_slices$TL[i] & covar_TR_seq == model_time_slices$TR[i])
    if (length(w) != 1) {
      stop("Covariate not found for modelling time slice ", i, " (",
           model_time_slices$TL[i], " to ", model_time_slices$TR[i], ")")
    }
    w
  }, integer(1))
  list(ind = ind, tl = covar_TL_seq[ind], tr = covar_TR_seq[ind])
}

covariate_band_index <- function(conn_pg, covar_name, time_slices, res_time) {
  bands <- get_covariate_bands(conn_pg, covar_name, res_time)
  if (all(is.na(bands$tl))) {
    return(list(ind = 1L, static = TRUE))
  }
  b <- get_temporal_bands(time_slices, as.Date(bands$tl), as.Date(bands$tr))
  list(ind = bands$band[b$ind], static = FALSE)
}

#' Get covariate values
#'
#' Covariate value at each grid centroid of the run, one column per model time
#' slice (`values_<band>`), or a single column for static covariates.
#'
#' @param covar_name schema-qualified covariate table
#' @param cntrd_table the run's grid centroids table
#' @param time_slices model time slices (TL, TR)
#' @param res_time model time resolution
#' @param conn_pg DBI connection
#' @return data frame with rid, x, y and the value columns
#' @export
get_covariate_values <- function(covar_name, cntrd_table, time_slices, res_time, conn_pg) {
  bands <- covariate_band_index(conn_pg, covar_name, time_slices, res_time)
  cat("---- Extracting ", covar_name, "\n")
  cols <- DBI::SQL(paste(sprintf("ST_Value(r.rast, %d, g.geom) AS values_%d", bands$ind, bands$ind),
                         collapse = ", "))
  # Left join: every centroid gets a row. Tiles that are NoData in every band
  # are not stored (load_raster_to_db(skip_empty = TRUE)), so a centroid there
  # reads NA, exactly as it would from a stored all-NoData tile.
  DBI::dbGetQuery(conn_pg, glue::glue_sql("
    SELECT g.rid, g.x, g.y, {cols}
    FROM {sql_table(conn_pg, cntrd_table)} g
    LEFT JOIN {sql_table(conn_pg, covar_name)} r ON ST_Intersects(r.rast, g.geom);", .con = conn_pg))
}

#' Get population fractions
#'
#' Share of each grid cell's population (at the model resolution) living in
#' its intersection with each location period, computed from the 1 km
#' population.
#'
#' @param res_space model spatial resolution in km
#' @param cntrd_table the run's grid centroids table
#' @param intersections_table the run's grid intersections table
#' @param conn_pg DBI connection
#' @param res_time model time resolution
#' @param time_slices model time slices (TL, TR)
#' @param pop_grid_covar population table at the model resolution
#' @param pop_1km_covar population table at 1 km
#' @return data frame (location_period_id, rid, x, y, lp_covered, area_ratio,
#'   t, pop_weight)
#' @export
get_pop_weights <- function(res_space, cntrd_table, intersections_table, conn_pg, res_time,
                            time_slices,
                            pop_grid_covar = sprintf("covariates.pop_1_years_%s_%s", res_space, res_space),
                            pop_1km_covar = "covariates.pop_1_years_1_1") {
  # Population of each modelling cell, by time slice
  grid_bands <- covariate_band_index(conn_pg, pop_grid_covar, time_slices, res_time)
  pop_grid <- get_covariate_values(pop_grid_covar, cntrd_table, time_slices, res_time, conn_pg) %>%
    tidyr::pivot_longer(dplyr::starts_with("values_"), names_to = "band", values_to = "pop_grid") %>%
    dplyr::mutate(t = match(as.integer(sub("values_", "", band)), grid_bands$ind)) %>%
    dplyr::select(rid, x, y, t, pop_grid)

  # 1 km population inside each cell-by-location-period intersection
  km_bands <- covariate_band_index(conn_pg, pop_1km_covar, time_slices, res_time)
  # ST_Band first, then clip band 1: ST_Clip(rast, n, ...) with n > 1 crashes
  # the server (segfault) in PostGIS 3.6.4 on multi-band rasters.
  cols <- DBI::SQL(paste(sprintf(
    "sum((ST_SummaryStats(ST_Clip(ST_Band(r.rast, %d), 1, g.geom, true), 1, true)).sum) AS values_%d",
    km_bands$ind, km_bands$ind), collapse = ", "))
  # Left join: an intersection over tiles that were not stored (NoData in
  # every band, see load_raster_to_db) keeps its row with 0 population. An
  # inner join would drop it, and make_location_periods_dict reads a missing
  # weight as 1 (cell fully inside the location period).
  pop_1km <- DBI::dbGetQuery(conn_pg, glue::glue_sql("
    SELECT g.location_period_id, g.rid, g.x, g.y, g.lp_covered, g.area_ratio, {cols}
    FROM {sql_table(conn_pg, intersections_table)} g
    LEFT JOIN {sql_table(conn_pg, pop_1km_covar)} r ON ST_Intersects(r.rast, g.geom)
    GROUP BY g.location_period_id, g.rid, g.x, g.y, g.lp_covered, g.area_ratio;", .con = conn_pg)) %>%
    tidyr::pivot_longer(dplyr::starts_with("values_"), names_to = "band", values_to = "pop_1km") %>%
    dplyr::mutate(t = match(as.integer(sub("values_", "", band)), km_bands$ind),
                  # Very small overlaps can return NA
                  pop_1km = ifelse(is.na(pop_1km), 0, pop_1km)) %>%
    dplyr::select(-band)

  pop_1km %>%
    dplyr::left_join(pop_grid, by = c("rid", "x", "y", "t")) %>%
    dplyr::mutate(pop_weight = pop_1km / pop_grid,
                  # Cells fully inside a location period are not in the
                  # intersections table by design; missing ratios mean full weight
                  pop_weight = ifelse(is.na(pop_weight), 1, pop_weight)) %>%
    dplyr::select(location_period_id, rid, x, y, lp_covered, area_ratio, t, pop_weight)
}

#' Make location periods dictionary
#'
#' Dictionary between location periods and grid cells, with population
#' weighted fractions for partial coverage.
#'
#' @param conn_pg DBI connection
#' @param lp_name the run's location periods table
#' @param intersections_table the run's grid intersections table
#' @param cntrd_table the run's grid centroids table
#' @param res_space model spatial resolution
#' @param sf_grid space-time grid (rid, x, y, t, long_id)
#' @param grid_changer map from long_id to updated id
#' @param res_time model time resolution
#' @param time_slices model time slices (TL, TR)
#' @export
make_location_periods_dict <- function(conn_pg, lp_name, intersections_table, cntrd_table,
                                       res_space, sf_grid, grid_changer, res_time, time_slices) {
  location_periods_dict <- DBI::dbReadTable(conn_pg, paste0(lp_name, "_dict")) %>%
    dplyr::inner_join(sf::st_drop_geometry(sf_grid), by = c("rid", "x", "y")) %>%
    dplyr::mutate(upd_long_id = grid_changer[as.character(long_id)]) %>%
    dplyr::filter(!is.na(upd_long_id))

  # Unique location-period-by-time-slice id; ordering by location period id
  # keeps it consistent throughout the pipeline
  location_periods_dict <- location_periods_dict %>%
    dplyr::distinct(location_period_id, t) %>%
    dplyr::arrange(location_period_id, t) %>%
    dplyr::mutate(loctime_id = dplyr::row_number()) %>%
    dplyr::inner_join(location_periods_dict, by = c("location_period_id", "t"))

  pop_weights <- get_pop_weights(res_space = res_space, cntrd_table = cntrd_table,
                                 intersections_table = intersections_table, conn_pg = conn_pg,
                                 res_time = res_time, time_slices = time_slices)

  location_periods_dict <- location_periods_dict %>%
    dplyr::left_join(pop_weights, by = c("location_period_id", "rid", "x", "y", "t")) %>%
    dplyr::mutate(pop_weight = ifelse(is.na(pop_weight), 1, pop_weight)) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct()

  location_periods_dict
}
