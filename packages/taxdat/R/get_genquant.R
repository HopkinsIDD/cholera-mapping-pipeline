#' @include plot_cache_function.R

#' @export
#' @name get_genquant_no_cache
#' @description load stan output
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return genquant
get_genquant_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory, "/", config))
  file_names <- taxdat::get_filenames(config, cholera_directory)
  genquant <- taxdat::read_file_of_type(file_names[["stan_genquant"]], "chol_gen")
  require(bit64)
  require(sf)
  return(genquant)
}
# cache the results
#' @export
#' @name get_genquant
get_genquant <- cache_fun_results(name = "genquant", fun = get_genquant_no_cache,
                                    overwrite = F, config = config)