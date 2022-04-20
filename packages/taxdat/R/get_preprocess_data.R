#' @include plot_cache_function.R

#' @export
#' @name get_sf_cases_no_cache
#' @title get_sf_cases_no_cache
#' @description load sf cases based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return sf cases from preproces data
get_sf_cases_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  sf_cases <- taxdat::read_file_of_type(file_names[["data"]], "sf_cases")
  require(bit64)
  require(sf)
  return(sf_cases)
}

get_sf_cases <- cache_fun_results(name = "sf_cases", fun = get_sf_cases_no_cache,
                                    overwrite = T,cholera_directory=cholera_directory)