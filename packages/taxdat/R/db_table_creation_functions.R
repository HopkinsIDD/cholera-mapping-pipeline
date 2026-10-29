# Builders for the vector tables a mapping run needs in the covariates
# database: grid geometries, location-period shapes, and their intersections.

#' @title Quote a possibly schema-qualified table name
#' @name sql_table
#' @param conn DBI connection
#' @param name "table" or "schema.table"
#' @param suffix optional suffix appended to the table part (e.g. "_polys")
#' @return a quoted SQL identifier
#' @export
sql_table <- function(conn, name, suffix = "") {
  parts <- strsplit(as.character(name), ".", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    DBI::dbQuoteIdentifier(conn, DBI::Id(schema = parts[1], table = paste0(parts[2], suffix)))
  } else if (length(parts) == 1) {
    DBI::dbQuoteIdentifier(conn, paste0(parts[1], suffix))
  } else {
    stop("Invalid table name: ", name)
  }
}

# Unquoted table part of "schema.table", for building index names.
table_part <- function(name) {
  parts <- strsplit(as.character(name), ".", fixed = TRUE)[[1]]
  parts[length(parts)]
}

#' @title Build grid geometry tables
#' @name build_geoms_query
#' @description Builds the centroids or the polygons of the valid (non-NoData)
#' pixels of a grid raster, as `<schema>.<table>_centroids` or `_polys`.
#'
#' @param conn DBI connection
#' @param schema schema of the grid raster
#' @param table_name grid raster table
#' @param type "centroids" or "polygons"
#' @return NULL, invisibly
#' @export
build_geoms_query <- function(conn, schema = "grids", table_name, type = c("centroids", "polygons")) {
  type <- match.arg(type)
  suffix <- c(centroids = "_centroids", polygons = "_polys")[[type]]
  pix_fun <- DBI::SQL(c(centroids = "ST_PixelAsCentroids", polygons = "ST_PixelAsPolygons")[[type]])
  grid <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = schema, table = table_name))
  out <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = schema, table = paste0(table_name, suffix)))
  gidx <- DBI::dbQuoteIdentifier(conn, paste0(table_name, suffix, "_gidx"))
  idx <- DBI::dbQuoteIdentifier(conn, paste0(table_name, suffix, "_idx"))

  db_exec(conn, glue::glue_sql("DROP TABLE IF EXISTS {out};", .con = conn))
  # exclude_nodata_value = TRUE: masked pixels do not become grid cells
  db_exec(conn, glue::glue_sql("
    CREATE TABLE {out} AS
    SELECT rid, (dp).x AS x, (dp).y AS y, (dp).geom AS geom
    FROM (SELECT rid, {pix_fun}(rast, 1, TRUE) AS dp FROM {grid}) foo;", .con = conn))
  db_exec(conn, glue::glue_sql("CREATE INDEX {gidx} ON {out} USING GIST(geom);", .con = conn))
  db_exec(conn, glue::glue_sql("CREATE INDEX {idx} ON {out} (rid, x, y);", .con = conn))
  db_exec(conn, glue::glue_sql("ANALYZE {out};", .con = conn))
  invisible(NULL)
}

#' Write shapefiles to the database
#'
#' @param conn_pg connection to a postgis database
#' @param shapefiles sf object with a `geom` geometry column
#' @param table_name table to (re)create, unqualified (work schema) or qualified
#' @export
write_shapefiles_table <- function(conn_pg, shapefiles, table_name) {
  tbl <- sql_table(conn_pg, table_name)
  db_exec(conn_pg, glue::glue_sql("DROP TABLE IF EXISTS {tbl};", .con = conn_pg))

  parts <- strsplit(as.character(table_name), ".", fixed = TRUE)[[1]]
  layer <- if (length(parts) == 2) DBI::Id(schema = parts[1], table = parts[2]) else parts[1]
  sf::st_write(obj = shapefiles, dsn = conn_pg, layer = layer,
               append = FALSE, delete_layer = TRUE, quiet = TRUE)

  geom_col <- attr(shapefiles, "sf_column")
  gcol <- DBI::dbQuoteIdentifier(conn_pg, geom_col)
  gidx <- DBI::dbQuoteIdentifier(conn_pg, paste0(table_part(table_name), "_gidx"))
  db_exec(conn_pg, glue::glue_sql("UPDATE {tbl} SET {gcol} = ST_SetSRID({gcol}, 4326);", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("CREATE INDEX {gidx} ON {tbl} USING GIST({gcol});", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("ANALYZE {tbl};", .con = conn_pg))
  invisible(NULL)
}

#' Make grid location periods mapping
#'
#' Table of correspondence between location periods and grid cells
#' (`<lp_name>_dict`).
#'
#' @param conn_pg DBI connection
#' @param lp_name location periods table
#' @param full_grid_name schema-qualified grid name (e.g. "grids.grid_20_20")
#' @export
make_grid_lp_mapping_table <- function(conn_pg, lp_name, full_grid_name) {
  dict <- sql_table(conn_pg, lp_name, "_dict")
  lps <- sql_table(conn_pg, lp_name)
  polys <- sql_table(conn_pg, full_grid_name, "_polys")
  db_exec(conn_pg, glue::glue_sql("DROP TABLE IF EXISTS {dict};", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("
    CREATE TABLE {dict} AS (
      SELECT location_period_id, b.rid, b.x, b.y
      FROM {lps} a
      JOIN {polys} b ON ST_Intersects(b.geom, a.geom)
    );", .con = conn_pg))
  invisible(NULL)
}

#' Make grid intersections table
#'
#' Intersection geometries between location periods and the grid cells that
#' either touch a location-period border or fully cover it. Used to compute
#' population-weighted spatial fractions.
#'
#' @param conn_pg DBI connection
#' @param full_grid_name schema-qualified grid name
#' @param lp_name location periods table
#' @param intersections_table table to create
#' @export
make_grid_intersections_table <- function(conn_pg, full_grid_name, lp_name, intersections_table) {
  out <- sql_table(conn_pg, intersections_table)
  lps <- sql_table(conn_pg, lp_name)
  polys <- sql_table(conn_pg, full_grid_name, "_polys")
  cntrds <- sql_table(conn_pg, full_grid_name, "_centroids")
  gidx <- DBI::dbQuoteIdentifier(conn_pg, paste0(table_part(intersections_table), "_gidx"))

  db_exec(conn_pg, glue::glue_sql("DROP TABLE IF EXISTS {out};", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("
    CREATE TABLE {out} AS (
      SELECT location_period_id, b.rid, b.x, b.y,
             ST_CoveredBy(a.geom, b.geom) AS lp_covered,
             ST_Area(a.geom) / ST_Area(b.geom) AS area_ratio,
             ST_CollectionExtract(ST_Intersection(b.geom, a.geom), 3) AS geom,
             g.geom AS grid_centroid
      FROM {lps} a
      JOIN {polys} b
        ON ST_Intersects(b.geom, ST_Boundary(a.geom)) OR ST_CoveredBy(a.geom, b.geom)
      JOIN {cntrds} g
        ON b.rid = g.rid AND b.x = g.x AND b.y = g.y
    );", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("CREATE INDEX {gidx} ON {out} USING GIST(geom);", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("ANALYZE {out};", .con = conn_pg))
  invisible(NULL)
}

#' Make grid location period centroids
#'
#' Grid centroids of every cell intersecting at least one location period.
#'
#' @param conn_pg DBI connection
#' @param full_grid_name schema-qualified grid name
#' @param lp_name location periods table
#' @param cntrd_table table to create
#' @export
make_grid_lp_centroids_table <- function(conn_pg, full_grid_name, lp_name, cntrd_table) {
  out <- sql_table(conn_pg, cntrd_table)
  lps <- sql_table(conn_pg, lp_name)
  polys <- sql_table(conn_pg, full_grid_name, "_polys")
  cntrds <- sql_table(conn_pg, full_grid_name, "_centroids")
  gidx <- DBI::dbQuoteIdentifier(conn_pg, paste0(table_part(cntrd_table), "_gidx"))

  db_exec(conn_pg, glue::glue_sql("DROP TABLE IF EXISTS {out};", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("
    CREATE TABLE {out} AS (
      SELECT DISTINCT g.*
      FROM {polys} p
      JOIN {lps} l ON ST_Intersects(p.geom, l.geom)
      JOIN {cntrds} g ON p.rid = g.rid AND p.x = g.x AND p.y = g.y
    );", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("CREATE INDEX {gidx} ON {out} USING GIST(geom);", .con = conn_pg))
  db_exec(conn_pg, glue::glue_sql("ANALYZE {out};", .con = conn_pg))
  invisible(NULL)
}

#' Build all location-period tables for a run
#'
#' Writes the shapes and builds the grid mapping, intersections and centroid
#' tables, for either the observed location periods or the output summary
#' admin units.
#'
#' @param conn_pg DBI connection
#' @param shapefiles sf object with `location_period_id` and a `geom` column
#' @param full_grid_name schema-qualified grid name
#' @param config the run's config (names the tables)
#' @param output TRUE for the output summary admin units
#' @export
build_lp_tables <- function(conn_pg, shapefiles, full_grid_name, config, output = FALSE) {
  lp_name <- run_table_name(config, "location_periods", output)
  write_shapefiles_table(conn_pg, shapefiles, lp_name)
  make_grid_lp_mapping_table(conn_pg, lp_name, full_grid_name)
  make_grid_intersections_table(conn_pg, full_grid_name, lp_name,
                                run_table_name(config, "grid_intersections", output))
  make_grid_lp_centroids_table(conn_pg, full_grid_name, lp_name,
                               run_table_name(config, "grid_cntrds", output))
  invisible(NULL)
}

#' Clean temporary tables
#'
#' Drops every per-run and scratch table in the work schema. Do not call while
#' a mapping run is in progress: it removes that run's tables too.
#'
#' @param conn optional DBI connection
#' @param pattern regular expression on table names
#' @export
clean_tmp_tables <- function(conn = NULL,
                             pattern = "^(tmp|location_periods|grid_cntrds|grid_intersections)") {
  if (is.null(conn)) {
    conn <- connect_to_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  schema <- get_db_config()$work_schema
  tables <- DBI::dbGetQuery(conn, glue::glue_sql(
    "SELECT tablename FROM pg_tables WHERE schemaname = {schema};", .con = conn))$tablename
  tables <- tables[grepl(pattern, tables)]
  message("-- Deleting ", length(tables), " temporary tables from schema ", schema)
  for (t in tables) {
    db_exec(conn, glue::glue_sql("DROP TABLE IF EXISTS {`schema`}.{`t`};", .con = conn))
  }
  invisible(tables)
}
