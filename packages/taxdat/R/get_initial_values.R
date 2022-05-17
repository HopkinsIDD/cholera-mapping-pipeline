#' @include plot_cache_function.R

#' @export
#' @name get_initial_values_no_cache
#' @description load initial values rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return initicial values
get_initial_values_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory, config))
  file_names <- taxdat::get_filenames(config, cholera_directory)
  initial_values_data <- read_file_of_type(file_names["initial_values"], "initial_values_data")
  require(bit64)
  require(sf)
  return(initial_values_data)
}
# cache the results
#' @export
#' @name get_initial_values
get_initial_values <- cache_fun_results(name = "initial_values_data", fun = get_initial_values_no_cache,
                                    overwrite = T, config = config)
