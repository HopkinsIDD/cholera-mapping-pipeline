library(taxdat)
library(raster)
library(sf)
library(optparse)
library(tidyverse)

option_list <- list(
  make_option(c("-b", "--band"), default = 1, action ="store", type = "numeric", help = "band to process"),
  make_option(c("-s", "--set"), default = FALSE, action ="store", type = "logical", help = "Set data in raster"),
  make_option(c("-e", "--extract"), default = TRUE, action ="store", type = "logical", help = "Set data in raster")
)

opt <- parse_args(OptionParser(option_list = option_list))

# Number of bands to process
# num_bands <- DBI::dbGetQuery(conn_pg, "SELECT ST_NumBands(rast) FROM covariates.pop_1_years_1_1 LIMIT 1") %>% 
#   unlist()

# Loop over bands and get values where is non 0

conn_pg <- taxdat::connect_to_db(Sys.getenv("perez"))
band <- opt$band

cat("-- STARTING band", band, "\n")

t1 <- Sys.time()

if (opt$extract) {
  # Dump pixels as polygons
  DBI::dbSendStatement(conn_pg, str_glue("DROP TABLE IF EXISTS tmppop_{band}"));
  DBI::dbSendStatement(conn_pg,
                       glue::glue_sql("CREATE TABLE tmppop_{band} AS (
                    SELECT rid, (gval).x, (gval).y, (gval).geom as geom, ST_Centroid(
                    (gval).geom) as centroid, (gval).val
                    FROM (
                    SELECT rid, ST_PixelAsPolygons(rast, {band}) as gval
                    FROM covariates.pop_1_years_20_20) foo);", .con = conn_pg)
  )
  
  t2 <- Sys.time()
  cat("--- Done Pix2Poly in ", formatC(difftime(t2, t1, units= "min"), digits = 2), " min \n")
  
  # Compute sum of 1km population
  DBI::dbSendStatement(conn_pg, str_glue("DROP TABLE IF EXISTS tmppop2_{band}"));
  DBI::dbSendStatement(conn_pg, 
                       glue::glue_sql("
                CREATE TABLE tmppop2_{band} AS (
                SELECT a.rid, a.x, a.y, a.centroid, (ST_SummaryStats(ST_Union(ST_Clip(rast, {band}, geom, true)))).sum as pop1km
                FROM covariates.pop_1_years_1_1, tmppop a
                WHERE ST_Intersects(rast, geom)
                );
                ", .con = conn_pg))
  
  t3 <- Sys.time()
  cat("--- Done 1km sum in ", formatC(difftime(t3, t2, units= "min"), digits = 2), " min \n")
  
} else {
  t3 <- t1
}

# Set new values
if (opt$set) {
  DBI::dbSendStatement(conn_pg, glue::glue_sql(
    "UPDATE covariates.pop_1_years_20_20 r
                     SET rast = ST_SetValues(rast, {band}, (SELECT ARRAY(
                     SELECT (a.centroid, a.pop1km)::geomval
                     FROM tmppop2_{band} a)
                     ))
                     ;", .con = conn_pg))
  t4 <- Sys.time()
  cat("--- Done replacement in ", formatC(difftime(t4, t3, units= "min"), digits = 2), " min \n")
}  else {
  t4 <- t3
}

cat("--- DONE band ", band, "in", formatC(difftime(t4, t1, units= "hours"), digits = 2), " hr \n")

DBI::dbDisconnect(conn_pg)