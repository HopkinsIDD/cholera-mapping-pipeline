#' @export
#' @name is_shapefile_valid
#' @title is_shapeilfe_valid
#' @description Determine whether shapefiles are valid for use in our modeling
#'   pipeline
#' @param shapefiles_to_check An sf object
#' @param location_column_name The column containing the unique id for the
#' @return a data frame with one row for each row of \code{shapefiles_to_check}
#'   and 4 columns \code{id} The id from the original shapefile \code{valid} is
#'   a boolean containing whether the shapefile in this position is valid,
#'   \code{reason} is an explanation for why the shapefile in question is not
#'   valid, and \code{fixed_value} an attempt to fix the shapefile in question.
is_shapefile_valid <- function(
  shapefiles,
  location_column_name = "id",
  quiet = FALSE
) {
  if (!("sf" %in% class(shapefiles))) {
    stop("This function only works on 'sf' objects")
  }
  rc <- data.frame(valid = rep(TRUE, times = nrow(shapefiles)), reason = "")
  rc$id <- shapefiles[[location_column_name]]
  rc$geometry <- sf::st_geometry(shapefiles)
  rc <- sf::st_as_sf(rc)

  shapefiles$valid <- sf::st_is_valid(shapefiles)
  if (!all(shapefiles$valid)) {
    if (!quiet) {
      warning("At least one topology error occurred.  See output for details")
      print(paste(sum(!shapefiles$valid), "shapefiles were invalid."))
      print(paste(
        "The following location periods were affected:",
        paste(shapefiles$location_period_id[!shapefiles$valid], collapse = ", ")
      ))
    }

    rc$valid[!shapefiles$valid] <- FALSE
    rc$reason[!shapefiles$valid] <- paste(
      rc$reason[!shapefiles$valid],
      "Shapefile has topology error."
    )
    sf::st_geometry(rc)[!shapefiles$valid] <-sf::st_make_valid(
      sf::st_geometry(shapefiles)[!shapefiles$valid]
    )
    shapefiles$valid[!shapefiles$valid] <- sf::st_is_valid(
      rc[!shapefiles$valid]
    )
    rc$reason[!shapefiles$valid] <- paste(
      rc$reason[!shapefiles$valid],
      "Automatic fix failed to fix topology error."
    )
  }

  shapefiles$valid <- sf::st_dimension(shapefiles) == 2
  if (any(!shapefiles$valid)) {
    if (!quiet) {
      warning("At least one location period is not of appropriate dimension")
      print(paste(
        "The following location periods are affected:",
        paste(
          shapefiles[!shapefiles$valid, ][[location_column_name]],
          collapse = ", "
        )
      ))
    }

    rc$valid[!shapefiles$valid] <- FALSE
    rc$reason[!shapefiles$valid] <- paste(
      rc$reason[!shapefiles$valid],
      "Shapefile is not of appropriate dimension:",
      sf::st_dimension(shapefiles),
      "vs 2"
    )
  }
  shapefiles$valid <- !grepl(
    "GEOMETRYCOLLECTION",
    sf::st_geometry_type(shapefiles)
  )
  ## This is not a long term solution in any capacity
  if (any(!shapefiles$valid)) {
    if (!quiet) {
      warning(
        "Geometry collections present in locations.  See output for details"
      )
      print(paste(
        "The following location periods are affected:",
        paste(
          shapefiles[!shapefiles$valid][[location_column_name]],
          collapse = ", "
        )
      ))
    }
    rc$valid[!shapefiles$valid] <- FALSE
    rc$reason[!shapefiles$valid] <- paste(
      rc$reason[!shapefiles$valid],
      "Shapefile is a geometry collection"
    )
    if (!quiet) {
    }
    tmp2 <- do.call(
      sf:::rbind.sf,
      lapply(
        sf::st_geometry(shapefiles)[!shapefiles$valid],
        function(x) {
          sf::st_sf(sf::st_sfc(x[[1]]))
        }
      )
    )
    sf::st_geometry(rc)[!shapefiles$valid] <- sf::st_geometry(tmp2)
  }

  return(rc)
}


create_table_from_sf <- function(df, user_name, table_name, overwrite = TRUE) {
  drop_query <- "DROP TABLE IF EXISTS {table_name} CASCADE"
  table_index_name <- paste(table_name, "idx", sep = "_")
  df_copy <- df
  df_copy <- sf::st_set_geometry(df_copy, NULL)
  geometry_name <- names(df)[!(names(df) %in% names(df_copy))]
  index_query <- paste(
    "CREATE INDEX {table_index_name} ON {table_name}",
    "USING GIST({geometry_name})"
  )
  vacuum_query <- "VACUUM ANALYZE {table_name}"
  conn_pg <- taxdat::connect_to_db(user_name)
  if (overwrite) {
    DBI::dbClearResult(DBI::dbSendQuery(
      conn_pg,
      glue::glue_sql(drop_query, .con = conn_pg)
    ))
  }
  DBI::dbWriteTable(conn_pg, table_name, df)
  warning("We are not currently setting the projection to 4326.  We used to do that")
  DBI::dbClearResult(DBI::dbSendQuery(conn_pg, glue::glue_sql(index_query)))
  DBI::dbClearResult(DBI::dbSendQuery(conn_pg, glue::glue_sql(vacuum_query)))
  DBI::dbDisconnect(conn_pg)
}

create_table_from_data_frame <- function(df, user_name, table_name, overwrite = TRUE) {
  drop_query <- "DROP TABLE IF EXISTS {table_name} CASCADE"
  table_index_name <- paste(table_name, "idx", sep = "_")
  conn_pg <- taxdat::connect_to_db(user_name)
  if (overwrite) {
    DBI::dbClearResult(DBI::dbSendQuery(
      conn_pg,
      glue::glue_sql(drop_query, .con = conn_pg)
    ))
  }
  DBI::dbWriteTable(conn_pg, table_name, df)
  DBI::dbDisconnect(conn_pg)
}
