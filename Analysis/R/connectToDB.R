# This could be put in a separate file
connectToDB <- function() {
  #' @title Connect to database
  #' @description Connects to the postgres/postgis cholera_covariates database
  #' @return db connection object
  RPostgres::dbConnect(RPostgres::Postgres(), 
                         dbname = "cholera_covariates")
}
