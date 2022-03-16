#' @include plot_cache_function.R

#' @name get_model_rand_no_cache
#' @description load stan output
#' @param config 
#' @param cache 
#' @return covar cube
get_model_rand_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config_filename)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  model.rand <- taxdat::read_file_of_type(file_names[["stan_output"]], "model.rand")
  require(bit64)
  require(sf)
  return(model.rand)
}
# cache the results
get_model_rand <- cache_fun_results(name = "model.rand", fun = get_model_rand_no_cache,
                                    overwrite = T, config = config)

#' @name get_sf_cases_resized_no_cache
#' @description load stan output
#' @param config 
#' @param cache 
#' @return covar cube
get_sf_cases_resized_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config_filename)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  sf_cases_resized <- taxdat::read_file_of_type(file_names[["stan_input"]], "sf_cases_resized")
  require(bit64)
  require(sf)
  return(sf_cases_resized)
}
# cache the results
get_sf_cases_resized <- cache_fun_results(name = "sf_cases_resized", fun = get_sf_cases_resized_no_cache,
                                    overwrite = T, config = config)

#' @name plot_WHOcomparison_table
#' @title plot_WHOcomparison_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return table with who comparison statistics
plot_WHOcomparison_table <- function(config, cache, cholera_directory) {
  get_sf_cases_resized(config, cache, cholera_directory)
  get_model_rand(config, cache, cholera_directory)
  
  who_annual_cases <- sf_cases_resized
  chains <- rstan::extract(model.rand)
  who_annual_cases$modeled <- apply(chains$modeled_cases,2,mean)
  who_annual_cases$observed <- who_annual_cases$attributes.fields.suspected_cases # fix me
  who_annual_cases_from_db <- NULL
  
  try({
    who_annual_cases_from_db <- taxdat::pull_output_by_source(who_annual_cases, "%WHO Annual Cholera Reports%",
                                                              database_api_key_rfile = stringr::str_c(cholera_directory, "Analysis/R/database_api_key.R"))
  })
  
  if(!is.null(who_annual_cases_from_db)) {
    who_comparison_table <- who_annual_cases_from_db %>%
      as.data.frame() %>%
      dplyr::select(OC_UID, TL, TR, observed, modeled) %>%
      dplyr::mutate_if(is.numeric, function(x) {format(round(x) , big.mark=",")}) %>%
      dplyr::rename(col.names = c("OC id", "start time", "end time", "# Observed cases", "# Modeled Cases"))
      dplyr::rename( `OC id`=OC_UID, `start time`=TL, `end time`=TR, `Observed cases`=observed, `Modeled Cases`=modeled)
  }
  
  return(who_comparison_table)
}