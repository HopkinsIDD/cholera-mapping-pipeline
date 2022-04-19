#' @name get_covar_no_cache
#' @description load covariate output data
#' @param config 
#' @param cache 
#' @return covar cube
get_covar_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config_filename)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  covar_cube_output <- taxdat::read_file_of_type(file_names["covar"], "covar_cube_output")
  require(bit64)
  require(sf)
  return(covar_cube_output)
}
# cache the results
get_covar <- cache_fun_results(name = "covar_cube_output", fun = get_covar_no_cache,
                               overwrite = T, config = config)

#' @name get_covar_cube_no_cache
#' @title get_covar_cube_no_cache
#' @description extrac covar_cube from covar_cube_output
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  covar_cube
get_covar_cube_no_cache <- function(config, cache, cholera_directory, ...) {
  get_covar(config, cache, cholera_directory, ...)
  covar_cube <- cache[["covar_cube_output"]]$covar_cube
  return(covar_cube)
}
# cache the results
get_covar_cube <- cache_fun_results("covar_cube", get_covar_cube_no_cache,
                                    overwrite = T, config = config)

#' @name get_sf_grid_no_cache
#' @title get_sf_grid_no_cache
#' @description extrac sf_grid from covar_cube_output
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  sf_grid
get_sf_grid_no_cache <- function(config, cache, cholera_directory, ...) {
  get_covar(config, cache, cholera_directory, ...)
  sf_grid <- cache[["covar_cube_output"]]$sf_grid
  return(sf_grid)
}
# cache the results
get_sf_grid <- cache_fun_results("sf_grid", get_sf_grid_no_cache,
                                 overwrite = T, config = config)
