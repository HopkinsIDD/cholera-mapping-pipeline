#' Get 2017 population
#' @name get_pop2017
#' @param sf_grid the sf_grid object from the stan_input file
#'
#' @return a new sf_grid object with a column for the 2017 population
#'
get_pop2017 <- function(sf_grid) {
  # Connect to database
  conn <- taxdat::connect_to_db(Sys.getenv("USER"))
  
  # Pul the 2017 population data
  pop2017 <- rpostgis::pgGetRast(conn, 
                                 name = c("covariates", "pop_1_years_20_20"), 
                                 band =  which(2000:2020 == 2017))
  
  # Extract the values at the centroids of sf_grid
  sf_grid$pop2017 <- raster::extract(pop2017, sf::st_centroid(sf_grid))
  
  DBI::dbDisconnect(conn)
  
  return(sf_grid)
}
