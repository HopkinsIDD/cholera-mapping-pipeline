# Names and cleanup of the per-run tables a mapping run creates in the
# covariates database (work schema, see db_connection.R).
#
# Every name derives from one hash of the run's config, so all steps of a run
# agree on the same tables and two runs never collide.

run_table_kinds <- c("location_periods", "location_periods_dict",
                     "grid_cntrds", "grid_intersections")

#' @title Config hash
#' @name config_hash
#' @param config the run's (validated) config list
#' @return md5 hash string
#' @export
config_hash <- function(config) {
  digest::digest(config, algo = "md5")
}

#' @title Per-run table name
#' @name run_table_name
#' @description Name of one of the per-run tables. Names match the historical
#' ones (`location_periods_<hash>`, `location_periods_<hash>_dict`,
#' `grid_cntrds_<hash>`, `grid_intersections_<hash>`, and the `_output_`
#' variants for the summary admin units).
#'
#' @param config the run's config list
#' @param kind one of location_periods, location_periods_dict, grid_cntrds,
#'   grid_intersections
#' @param output TRUE for the tables built on the output summary shapefiles
#' @return table name (unqualified; it lives in the work schema)
#' @export
run_table_name <- function(config, kind = run_table_kinds, output = FALSE) {
  kind <- match.arg(kind)
  h <- config_hash(config)
  infix <- if (output) "output_" else ""
  switch(kind,
         location_periods = glue::glue("location_periods_{infix}{h}"),
         location_periods_dict = glue::glue("location_periods_{infix}{h}_dict"),
         grid_cntrds = glue::glue("grid_cntrds_{infix}{h}"),
         grid_intersections = glue::glue("grid_intersections_{infix}{h}"))
}

#' @title All per-run table names
#' @name run_table_names
#' @param config the run's config list
#' @return character vector of the eight per-run table names
#' @export
run_table_names <- function(config) {
  unlist(lapply(c(FALSE, TRUE), function(o) {
    vapply(run_table_kinds, function(k) as.character(run_table_name(config, k, o)), "")
  }), use.names = FALSE)
}

#' @title make location periods table name
#' @name make_locationperiods_table_name
#' @param config the run's config list
#' @return the table name
#' @export
make_locationperiods_table_name <- function(config) {
  run_table_name(config, "location_periods")
}

#' @title make grid centroids table name
#' @name make_grid_centroids_table_name
#' @param config the run's config list
#' @return the table name
#' @export
make_grid_centroids_table_name <- function(config) {
  run_table_name(config, "grid_cntrds")
}

#' @title make grid intersections table name
#' @name make_grid_intersections_table_name
#' @param config the run's config list
#' @return the table name
#' @export
make_grid_intersections_table_name <- function(config) {
  run_table_name(config, "grid_intersections")
}

#' @title make output location periods table name
#' @name make_output_locationperiods_table_name
#' @param config the run's config list
#' @return the table name
#' @export
make_output_locationperiods_table_name <- function(config) {
  run_table_name(config, "location_periods", output = TRUE)
}

#' @title make output grid centroids table name
#' @name make_output_grid_centroids_table_name
#' @param config the run's config list
#' @return the table name
#' @export
make_output_grid_centroids_table_name <- function(config) {
  run_table_name(config, "grid_cntrds", output = TRUE)
}

#' @title make output grid intersections table name
#' @name make_output_grid_intersections_table_name
#' @param config the run's config list
#' @return the table name
#' @export
make_output_grid_intersections_table_name <- function(config) {
  run_table_name(config, "grid_intersections", output = TRUE)
}

#' @title Check that a run's tables exist
#' @name run_tables_exist
#' @param conn DBI connection
#' @param config the run's config list
#' @return TRUE if all eight per-run tables exist in the work schema
#' @export
run_tables_exist <- function(conn, config) {
  schema <- get_db_config()$work_schema
  all(vapply(run_table_names(config), function(t) {
    DBI::dbExistsTable(conn, DBI::Id(schema = schema, table = t))
  }, logical(1)))
}

#' @title clean all tmp
#' @name clean_all_tmp
#' @description Drops the per-run tables of one run (all eight of them).
#' @param config the run's config list
#' @param conn optional DBI connection; one is opened if NULL
#' @export
clean_all_tmp <- function(config, conn = NULL) {
  if (is.null(conn)) {
    conn <- connect_to_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  schema <- get_db_config()$work_schema
  message("-- Dropping per-run tables for config ", config_hash(config))
  for (t in run_table_names(config)) {
    db_exec(conn, glue::glue_sql("DROP TABLE IF EXISTS {`schema`}.{`t`};", .con = conn))
  }
  invisible(NULL)
}
