#' @include plot_cache_function.R

#' @export
#' @name plot_unique_db_shps
#' @param config
#' @param cholera_directory
#' @param cache
plot_unique_db_shps <- function(cache,config,cholera_directory){
  taxdat::get_sf_cases_resized(name="sf_cases_resized",cache=cache,config = config,cholera_directory = cholera_directory)
  unique_db_shps <- taxdat::get_unique_db_shps(cache=cache,config=config,cholera_directory=cholera_directory)
  plot<-ggplot2::ggplot()+
    ggplot2::geom_sf(data=unique_db_shps,
                     alpha=0)
  return(plot)
}

#' @export
#' @name plot_unique_db_shps_by_time
#' @param config
#' @param cholera_directory
#' @param cache
plot_unique_db_shps_by_time <- function(cache,config,cholera_directory){
  taxdat::get_sf_cases_resized(name="sf_cases_resized",cache=cache,config = config,cholera_directory = cholera_directory)
  unique_db_shps <- taxdat::get_unique_db_shps(cache=cache,config=config,cholera_directory=cholera_directory)
  plot<-ggplot2::ggplot()+
    ggplot2::geom_sf(data=unique_db_shps,
                     alpha=0) +
    ggplot2::facet_wrap(~time_slice)
  return(plot)
}