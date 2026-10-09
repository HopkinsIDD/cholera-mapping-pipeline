#' Get the population of one year at grid cells
#' @name get_pop2017
#' @description Adds the population of `year` (from the covariates database)
#' at the centroid of each cell of `sf_grid`, as column `pop<year>`.
#' @param sf_grid the sf_grid object from the stan_input file
#' @param year year to extract (default 2017)
#' @param covar population covariate table
#' @return sf_grid with the population column added
#' @export
get_pop2017 <- function(sf_grid, year = 2017, covar = "covariates.pop_1_years_20_20") {
  conn <- connect_to_db()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  bands <- get_covariate_bands(conn, covar, res_time = "1 years")
  band <- bands$band[!is.na(bands$tl) & lubridate::year(as.Date(bands$tl)) == year]
  if (length(band) != 1) {
    stop("No band for year ", year, " in ", covar)
  }
  parts <- strsplit(covar, ".", fixed = TRUE)[[1]]
  pop <- read_pg_raster(parts[1], parts[2], bands = band)
  cntr <- terra::vect(sf::st_centroid(sf::st_geometry(sf_grid)))
  sf_grid[[paste0("pop", year)]] <- terra::extract(pop, cntr, ID = FALSE)[[1]]
  sf_grid
}
