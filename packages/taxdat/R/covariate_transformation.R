#' Transform covariate
#' This function takes a covariate table in the database and transforms its values
#' with a user-supplied function
#'
#' @param source_table the source table in the database
#' @param dest_table   the destination table in the database  
#' @param transform    the transformation to apply
#' @param overwrite    flag to overwrite the destination table
#'
#' @return NULL
#' @export
#'
transform_covariate <- function(dbuser = Sys.getenv("USER"),
                                source_schema = "covariates",
                                source_table,
                                dest_schema = "covariates",
                                dest_table,
                                transform,
                                transform_inner = "",
                                overwrite = F) {
  # Connect to database
  conn_pg <- connect_to_db(dbuser)
  
  # Check if source table exists
  source_table_in_db <- db_exists_table_multi(conn_pg, source_schema, source_table)
  
  if (!any(source_table_in_db)) {
    stop("Could not find source covariate ", source_table, " in database when trying to transform.")
  }
  
  # Check if destination table exists
  dest_table_in_db <- db_exists_table_multi(conn_pg, dest_schema, dest_table)
  
  if (any(dest_table_in_db) & !overwrite) {
    stop("Destination table ", dest_table, " already exists in schema ", dest_schema, ". If you want to overwrite set overwrite=TRUE")
  }
  
  # Modify values
  stab <- DBI::SQL(paste0(source_schema, ".", source_table))
  dtab <- DBI::SQL(paste0(dest_schema, ".", dest_table))
  
  # Get number of bands to modify
  n_bands <- DBI::dbGetQuery(conn_pg, 
                             glue::glue_sql("SELECT ST_NumBands(rast) n
                   FROM {`stab`}
                   LIMIT 1", .con = conn_pg)) %>% 
    unlist()
  
  # Create table
  DBI::dbSendStatement(conn_pg, glue::glue_sql("DROP TABLE IF EXISTS {`dtab`}", .con = conn_pg))
  DBI::dbSendStatement(conn_pg,
                       glue::glue_sql("CREATE TABLE {`dtab`} AS (SELECT * FROM {`stab`});", .con = conn_pg))
  
  # Modify bands
  for (band in 1:n_bands) {
    t1 <- Sys.time()
    
    DBI::dbSendStatement(conn_pg, glue::str_glue("DROP TABLE IF EXISTS tmptrans_{band}"));
    DBI::dbSendStatement(conn_pg,
                         glue::glue_sql("CREATE TABLE tmptrans_{band} AS (
                    SELECT rid, (gval).x, (gval).y, (gval).geom as geom, ST_Centroid(
                    (gval).geom) as centroid, (gval).val
                    FROM (
                    SELECT rid, ST_PixelAsPolygons(rast, {band}) as gval
                    FROM {`stab`}) foo);", .con = conn_pg)
    )
    
    DBI::dbSendStatement(conn_pg, glue::glue_sql(
      "UPDATE {`dtab`} r
                     SET rast = ST_SetValues(rast, {band}, (SELECT ARRAY(
                     SELECT (a.centroid, {`DBI::SQL(transform)`}(a.val{`DBI::SQL(transform_inner)`}))::geomval
                     FROM tmptrans_{band} a)
                     ))
                     ;", .con = conn_pg))
    
    DBI::dbSendStatement(conn_pg, str_glue("DROP TABLE IF EXISTS tmptrans_{band}"));
    t2 <- Sys.time()
    
    cat("--- Done transformation of band ", band, " in ", formatC(difftime(t2, t1, units= "min"), digits = 2), " min \n")
    
  }
}