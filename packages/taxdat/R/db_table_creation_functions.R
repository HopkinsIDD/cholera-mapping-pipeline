
#' Write Shapefiles to taxdat
#'
#' @param conn_pg connection to a postgis database
#' @param shapefiles set of shapefiles to write
#' @param table_name name of table to write the shapefiles to
#'
#' @export
#'
write_shapefiles_table <- function(conn_pg,
                                   shapefiles,
                                   table_name) {
  
  DBI::dbClearResult(
    DBI::dbSendStatement(
      conn_pg,  
      glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(table_name)}`};", 
                     .con = conn_pg)))
  
  # Write to database
  sf::st_write(obj = shapefiles,
               dsn = conn_pg,
               layer = table_name,
               append = F,
               delete_layer = T)
  
  # Creat spatial index
  DBI::dbClearResult(DBI::dbSendStatement(conn_pg, glue::glue_sql("UPDATE {`{DBI::SQL(table_name)}`} SET geom = ST_SetSRID(geom, 4326);", .con = conn_pg)))
  DBI::dbClearResult(DBI::dbSendStatement(conn_pg, glue::glue_sql("CREATE INDEX  {`{DBI::SQL(paste0(table_name, '_idx'))}`} ON  {`{DBI::SQL(table_name)}`} USING GIST(geom);", .con = conn_pg)))
  DBI::dbClearResult(DBI::dbSendStatement(conn_pg, glue::glue_sql("VACUUM ANALYZE {`{DBI::SQL(table_name)}`};", .con = conn_pg)))
  
}

#' Make grid location periods mapping
#'
#' @param conn_pg 
#' @param lp_name 
#'
#' @export
#'
make_grid_lp_mapping_table <- function(conn_pg,
                                       lp_name) {
  
  # Table of correspondence between location periods and grid cells
  location_periods_table <- paste0(lp_name, "_dict")
  
  DBI::dbClearResult(DBI::dbSendStatement(
    conn_pg,
    glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(location_periods_table)}`};",
                   .con = conn_pg)
  ))
  DBI::dbClearResult(DBI::dbSendStatement(
    conn_pg,
    glue::glue_sql("CREATE TABLE {`{DBI::SQL(location_periods_table)}`} AS (
                  SELECT location_period_id , b.rid, b.x, b.y
                  FROM {`{DBI::SQL(lp_name)}`} a
                  JOIN {`{DBI::SQL(paste0(full_grid_name, '_polys'))}`} b
                  ON ST_Intersects(b.geom, a.geom)
                );",
                   .con = conn_pg
    )
  ))
  
}

#' Make grid intersections table
#' The aim of this function is to provide the intersection geometries for all 
#' location period shapefiles that either touch a grid cell border, or that are
#' completely covered by a grid cell. These intersections will then be used to
#' compute population-weighted spatial fractions for the mapping.
#'
#' @param conn_pg 
#' @param full_grid_name 
#' @param lp_name 
#' @param intersections_table 
#'
#' @return
#' @export
#'
make_grid_intersections_table <- function(conn_pg,
                                          full_grid_name,
                                          lp_name,
                                          intersections_table) {
  
  DBI::dbClearResult(DBI::dbSendStatement(
    conn_pg,
    glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(intersections_table)}`};",
                   .con = conn_pg)
  ))
  DBI::dbClearResult(DBI::dbSendStatement(
    conn_pg,
    glue::glue_sql("CREATE TABLE {`{DBI::SQL(intersections_table)}`} AS (
                  SELECT location_period_id , b.rid, b.x, b.y, 
                  ST_CoveredBy(a.geom, b.geom) as lp_covered,
                  ST_Area(a.geom)/ST_Area(b.geom) as area_ratio,
                  ST_CollectionExtract(ST_Intersection(b.geom, a.geom),3) as geom, g.geom as grid_centroid
                  FROM {`{DBI::SQL(lp_name)}`} a
                  JOIN {`{DBI::SQL(paste0(full_grid_name, '_polys'))}`} b
                  ON ST_Intersects(b.geom, ST_Boundary(a.geom)) OR ST_CoveredBy(a.geom, b.geom)
                  JOIN {`{DBI::SQL(paste0(full_grid_name, '_centroids'))}`} g
                  ON b.rid = g.rid AND b.x = g.x AND b.y = g.y
                );",
                   .con = conn_pg
    )
  ))
  
  # Create spatial index
  DBI::dbClearResult(DBI::dbSendStatement(conn_pg, glue::glue_sql("CREATE INDEX  {`{DBI::SQL(paste0(lp_name, 'intersections__idx'))}`} ON  {`{DBI::SQL(intersections_table)}`} USING GIST(geom);", .con = conn_pg)))
  DBI::dbClearResult(DBI::dbSendStatement(conn_pg, glue::glue_sql("VACUUM ANALYZE {`{DBI::SQL(intersections_table)}`};", .con = conn_pg)))
  
}


#' Make grid location period centroids
#'
#' @param conn_pg 
#' @param full_grid_name 
#' @param lp_name 
#' @param cntrd_table 
#'
#' @return
#' @export
#'
make_grid_lp_centroids_table <- function(conn_pg,
                                         full_grid_name,
                                         lp_name,
                                         cntrd_table) {
  
  # Create table of grid centroids included in the model
  DBI::dbClearResult(DBI::dbSendStatement(
    conn_pg,
    glue::glue_sql(
      "DROP TABLE IF EXISTS {`{DBI::SQL(cntrd_table)}`};", .con = conn_pg)))
  DBI::dbClearResult(DBI::dbSendStatement(
    conn_pg,
    glue::glue_sql(
      "CREATE TABLE {`{DBI::SQL(cntrd_table)}`} AS (
        SELECT DISTINCT g.*
        FROM {`{DBI::SQL(paste0(full_grid_name, '_polys'))}`} p
        JOIN {`{DBI::SQL(lp_name)}`} l
        ON ST_Intersects(p.geom, l.geom)
        JOIN {`{DBI::SQL(paste0(full_grid_name, '_centroids'))}`} g
        ON p.rid = g.rid AND p.x = g.x AND p.y = g.y
      );", .con = conn_pg
    )
  ))
  
  DBI::dbClearResult(DBI::dbSendStatement(
    conn_pg,
    glue::glue_sql(
      "CREATE INDEX {`{DBI::SQL(paste0(cntrd_table, '_gidx'))}`} on {`{DBI::SQL(cntrd_table)}`} USING GIST(geom);",
      .con = conn_pg)))
  
  DBI::dbSendStatement(conn_pg, glue::glue_sql("VACUUM ANALYZE {`{DBI::SQL(cntrd_table)}`};", .con = conn_pg))
}


#' clean_tmp_tables
#' Query found in: https://dba.stackexchange.com/questions/30061/how-do-i-list-all-tables-in-all-schemas-owned-by-the-current-user-in-postgresql
#' 
#' Note that this function will delete tables that are used while the pipeline is running for a given config, so it should not be called while the pipeline is running for the config of interest. 
#'
#' @return
#'
#' @examples
clean_tmp_tables <- function(dbuser) {
  
  conn <- connect_to_db(dbuser)
  
  query <- "
  select nsp.nspname as object_schema,
       cls.relname as object_name, 
       rol.rolname as owner, 
       case cls.relkind
         when 'r' then 'TABLE'
         when 'm' then 'MATERIALIZED_VIEW'
         when 'i' then 'INDEX'
         when 'S' then 'SEQUENCE'
         when 'v' then 'VIEW'
         when 'c' then 'TYPE'
         else cls.relkind::text
       end as object_type
       from pg_class cls
       join pg_roles rol on rol.oid = cls.relowner
       join pg_namespace nsp on nsp.oid = cls.relnamespace
       where nsp.nspname not in ('information_schema', 'pg_catalog')
       and nsp.nspname not like 'pg_toast%'
       and rol.rolname = current_user  --- remove this if you want to see all objects
       order by nsp.nspname, cls.relname;
  "
  
  # Get all tables owned by user
  owned_tables <- DBI::dbGetQuery(conn, statement = query) %>% 
    dplyr::as_tibble()
  
  # Filter temporary tables
  tmp_tables <- owned_tables %>% 
    dplyr::filter(
      object_type == "TABLE",
      stringr::str_detect(
        object_name, 
        "tmp|location_periods|grid_cntrds|grid_intersections"))
  
  cat("-- Deleting", nrow(tmp_tables), "temporary tables from DB. \n")
  
  # Loop over tables and delete
  purrr::walk(tmp_tables$object_name,
              function(x) {
                try(DBI::dbSendStatement(
                  conn, 
                  glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(x)}`};",
                                 .con = conn)))
              })
  
  cat("-- Done deleting temporary tables. \n")
}
