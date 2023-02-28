#' @include plot_cache_function.R

#' @export
#' @name get_sf_cases_no_cache
#' @title get_sf_cases_no_cache
#' @description load sf cases based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return sf cases from preproces data
get_sf_cases_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory,"/",config))
  file_names <- taxdat::get_filenames(config, cholera_directory)
  sf_cases <- taxdat::read_file_of_type(file_names[["data"]], "sf_cases")
  require(bit64)
  require(sf)
  return(sf_cases)
}
#' @export
#' @name get_sf_cases
get_sf_cases <- cache_fun_results(name = "sf_cases", fun = get_sf_cases_no_cache,
                                    overwrite = F,cholera_directory=cholera_directory)


#' @export
#' @name get_preprocessed_stan_no_cache
#' @title get_preprocessed_stan_no_cache
#' @description load sf cases based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return sf cases from preproces data
get_preprocessed_stan_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory,config))
  file_names <- taxdat::get_filenames(config, cholera_directory)
  preprocessed_stan_data <- taxdat::read_file_of_type(file_names[["data"]], "stan_data")
  require(bit64)
  require(sf)
  return(preprocessed_stan_data)
}
#' @export
#' @name get_preprocessed_stan
get_preprocessed_stan <- cache_fun_results(name = "preprocessed_stan_data", fun = get_preprocessed_stan_no_cache,
                                  overwrite = F,cholera_directory=cholera_directory)

#' @export
#' @name get_output_shapefiles_no_cache
#' @title get_output_shapefiles_no_cache
#' @description load output shapefiles based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return output shapefiles from preprocessed data
get_output_shapefiles_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory,config))
  file_names <- taxdat::get_filenames(config, "/",cholera_directory)
  output_shapefiles <- taxdat::read_file_of_type(file_names[["data"]], "output_shapefiles")
  require(bit64)
  require(sf)
  return(output_shapefiles)
}
#' @export
#' @name get_output_shapefiles
get_output_shapefiles <- cache_fun_results(name = "output_shapefiles", fun = get_output_shapefiles_no_cache,
                                           overwrite = F,cholera_directory=cholera_directory)