#' @include plot_cache_function.R

#' @export
#' @name get_stan_input_no_cache
#' @title get_stan_input_no_cache
#' @description load stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return stan_input
get_stan_input_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  stan_input <- taxdat::read_file_of_type(file_names[["stan_input"]], "stan_input")
  require(bit64)
  require(sf)
  return(stan_input)
}

get_stan_input <- cache_fun_results(name = "stan_input", fun = get_stan_input_no_cache,
                                    overwrite = T,cholera_directory=cholera_directory)

#' @name get_sf_cases_resized_no_cache
#' @title get_sf_cases_resized_no_cache
#' @description load sf object (i.e.,sf_cases_resized) from stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return sf_cases_resized object
get_sf_cases_resized_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config, cache, cholera_direcotry)
  return(cache[["stan_input"]][["sf_cases_resized"]])
}

get_sf_cases_resized <- cache_fun_results("sf_cases_resized", get_sf_cases_resized_no_cache)

#' @name get_stan_data_no_cache
#' @title get_stan_data_no_cache
#' @description load sf object (i.e.,sf_cases_resized) from stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return stan_data object
get_stan_data_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config, cache, cholera_direcotry)
  return(cache[["stan_input"]][["stan_data"]])
}

get_stan_data <- cache_fun_results("stan_data", get_sf_cases_resized_no_cache)

#' @name get_smooth_grid_no_cache
#' @title get_smooth_grid_no_cache
#' @description load sf object (i.e.,sf_cases_resized) from stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return smooth_grid object
get_smooth_grid_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config, cache, cholera_direcotry)
  return(cache[["stan_input"]][["smooth_grid"]])
}

get_smooth_grid <- cache_fun_results("smooth_grid", get_smooth_grid_no_cache)