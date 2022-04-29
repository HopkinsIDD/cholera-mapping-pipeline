#' @include plot_cache_function.R

#' @name get_non_na_gridcells
#' @title get_non_na_gridcells
#' @description add
#' @param cache covariates rdata filename
#' @param config
#' @param cholera_directory
#' @return
#' @export
get_non_na_gridcells <- function(cache, config,cholera_directory){
  get_covar (name="covar_cube_output", config = config,cache=cache,cholera_directory = cholera_directory)
  covar_cube_output <- cache[["covar_cube_output"]]
  non_na_gridcells <- covar_cube_output$non_na_gridcells
  return(non_na_gridcells)
}

#' @export
#' @name get_case_raster
#' @title get_case_raster
#' @description add
#' @param cache
#' @param config
#' @param cholera_directory
#' @return
get_case_raster <- function(cache,cholera_directory,config
) {
  get_covar (name="covar_cube_output", config = config,cache=cache,cholera_directory = cholera_directory)
  get_covar_cube (name="covar_cube", config = config,cache=cache,cholera_directory = cholera_directory)
  get_sf_grid(name="sf_grid",config = config,cache=cache,cholera_directory = cholera_directory)
  
  covar_cube_output <- cache[["covar_cube_output"]]
  case_raster <- cache[["sf_grid"]]
  
  non_na_gridcells <- get_non_na_gridcells(cache=cache,config=config,cholera_directory = cholera_directory)

  # Get cases and rates
  cache[["aggregated_modeled_cases_by_gridtime"]]<-aggregate_modeled_cases_by_gridtime_no_cache(cache = cache,config=config,cholera_directory = cholera_directory)
  modeled_cases_mean<-cache[["aggregated_modeled_cases_by_gridtime"]]
  
  cache[["aggregated_modeled_rates_by_gridtime"]]<-aggregate_modeled_rates_by_gridtime_no_cache(cache = cache,config=config,cholera_directory = cholera_directory)
  modeled_rates_mean<-  cache[["aggregated_modeled_rates_by_gridtime"]]
  
  case_raster <- dplyr::mutate(case_raster,
                               modeled_cases_mean = modeled_cases_mean,
                               modeled_rates_mean = modeled_rates_mean)
  return(case_raster)
}