# Pipeline step: observations, location-period shapes and per-run tables.

report_drop <- function(step, n_before, n_after) {
  if (n_after < n_before) {
    message(sprintf("-- %s: dropped %d of %d observations (%d kept)",
                    step, n_before - n_after, n_before, n_after))
  }
  invisible(n_after)
}

#' @title Prepare map data
#' @name prepare_map_data
#' @description Cleans the pulled observations, clips their shapes to the
#' national boundary, writes the observed and output location-period tables
#' for the run, and returns the observations as an sf object snapped to the
#' model's time slices. Prints how many observations each step drops.
#'
#' @param cases observations from `pull_observations` or `load_observations_rds`
#' @param config the run's config
#' @param cases_column column holding the case counts
#' @param full_grid_name schema-qualified modelling grid
#' @param cache_dir admin-units cache directory
#' @param conn optional DBI connection
#' @return list(sf_cases, shapefiles, output_shapefiles)
#' @export
prepare_map_data <- function(cases, config, cases_column, full_grid_name,
                             cache_dir = admin_units_cache_dir(), conn = NULL) {
  if (is.null(conn)) {
    conn <- connect_to_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  n0 <- nrow(cases)
  message("-- ", n0, " observations pulled")

  # NA cases are missing observations; only primary (space-time stratified)
  # observations are modelled
  cases <- dplyr::filter(cases, !is.na(.data[[cases_column]]))
  report_drop("NA cases", n0, nrow(cases))
  n1 <- nrow(cases)
  cases <- dplyr::filter(cases, is_primary)
  report_drop("non-primary", n1, nrow(cases))

  n2 <- nrow(cases)
  cases <- drop_missing_shapefiles(cases = cases, cases_column = cases_column)
  report_drop("missing shapefile", n2, nrow(cases))
  if (nrow(cases) == 0) {
    stop("No primary, non-NA observations were found.")
  }

  shapefiles <- get_valid_shapefiles(cases)
  iso_code <- get_country_isocode(config)
  shapefiles <- clip_shapefiles_to_adm0(iso_code = iso_code, shapefiles = shapefiles,
                                        cache_dir = cache_dir)

  cat("-- Creating tables for observed location periods in the database\n")
  build_lp_tables(conn, shapefiles, full_grid_name, config, output = FALSE)

  cat("-- Creating tables for output summary location periods in the database\n")
  output_shapefiles <- get_multi_country_admin_units(
    iso_code = iso_code, admin_levels = config$summary_admin_levels,
    lps = shapefiles, source = "cache", cache_dir = cache_dir)
  build_lp_tables(conn, output_shapefiles, full_grid_name, config, output = TRUE)

  # Observations with their (clipped) shapes
  n3 <- nrow(cases)
  sf::st_geometry(cases) <- NULL
  sf_cases <- sf::st_as_sf(dplyr::inner_join(
    cases, shapefiles,
    by = c("attributes.location_period_id" = "location_period_id",
           "location_name" = "location_name")))
  report_drop("shape outside the country or failed reprojection", n3, nrow(sf_cases))

  empty <- sf::st_is_empty(sf_cases)
  if (any(empty)) {
    warning("Missing shapefiles for ", sum(empty), " observations; location period IDs: ",
            paste(unique(sf_cases$attributes.location_period_id[empty]), collapse = ", "))
    sf_cases <- sf_cases[!empty, ]
  }

  sf_cases$TL <- lubridate::ymd(sf_cases$TL)
  sf_cases$TR <- lubridate::ymd(sf_cases$TR)
  sf_cases <- snap_to_time_period_df(df = sf_cases, TL_col = "TL", TR_col = "TR",
                                     res_time = config$res_time, tol = config$snap_tol)
  sf_cases <- dplyr::mutate(sf_cases,
                            admin_level = purrr::map_dbl(location_name, ~ get_admin_level(.)))

  if (isTRUE(config$drop_multiyear_adm0)) {
    n4 <- nrow(sf_cases)
    sf_cases <- drop_multiyear(df = sf_cases, admin_levels = 0)
    report_drop("multi-year national", n4, nrow(sf_cases))
  }
  message("-- ", nrow(sf_cases), " of ", n0, " observations kept for the model")

  list(sf_cases = sf_cases, shapefiles = shapefiles, output_shapefiles = output_shapefiles)
}

#' @title Make sure a run's per-run tables exist
#' @name ensure_run_tables
#' @description The per-run tables are dropped at the end of every run. When a
#' later stage is re-run from cached files, they are rebuilt from the shapes
#' saved with the observations.
#'
#' @param conn DBI connection
#' @param config the run's config
#' @param shapefiles observed location-period shapes
#' @param output_shapefiles output summary shapes
#' @param full_grid_name schema-qualified modelling grid
#' @return TRUE if the tables had to be rebuilt, invisibly
#' @export
ensure_run_tables <- function(conn, config, shapefiles, output_shapefiles, full_grid_name) {
  if (run_tables_exist(conn, config)) {
    return(invisible(FALSE))
  }
  if (is.null(shapefiles)) {
    stop("The run's per-run tables are gone and the cached data file has no shapefiles. ",
         "Delete the data file to rebuild it.")
  }
  message("-- Rebuilding per-run tables from the cached data file")
  build_lp_tables(conn, shapefiles, full_grid_name, config, output = FALSE)
  build_lp_tables(conn, output_shapefiles, full_grid_name, config, output = TRUE)
  invisible(TRUE)
}
