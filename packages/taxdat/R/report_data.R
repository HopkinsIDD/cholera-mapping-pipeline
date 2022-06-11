#' @include report_cache.R

#' @name get_config_no_cache
#' @description load config.rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return config
get_config_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory, config))
  return(config)
}

## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_config <- cache_fun_results(name = "config", fun = get_config_no_cache)


#' @name get_stan_input_no_cache
#' @description load stan_input.rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return stan_input
get_stan_input_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  file_names <- taxdat::get_filenames(cache[["config"]], cholera_directory)
  stan_input <- read_file_of_type(file_names[["stan_input"]], "stan_input")
  require(bit64)
  require(sf)
  return(stan_input)
}

## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_stan_input <- cache_fun_results(name = "stan_input", fun = get_stan_input_no_cache)

#' @name get_initial_values_df_no_cache
#' @description load initial_values_dfrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return initial_values_df
get_initial_values_df_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config = config, cache = cache, cholera_directory = cholera_directory)
  cache[["stan_input"]][["initial_values_df"]] %>%
    dplyr::mutate(t = taxdat::cast_to_int32(t)) %>%
    return()
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_initial_values_df <- cache_fun_results(name = "initial_values_df", fun = get_initial_values_df_no_cache)

#' @name get_covar_cube_no_cache
#' @description load covar_cuberdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return covar_cube
get_covar_cube_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config = config, cache = cache, cholera_directory = cholera_directory)
  cache[["stan_input"]][["covar_cube"]] %>%
    dplyr::mutate(t = taxdat::cast_to_int32(t)) %>%
    return()
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_covar_cube <- cache_fun_results(name = "covar_cube", fun = get_covar_cube_no_cache)

#' @name get_observation_data_no_cache
#' @description load observation_datardata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return observation_data
get_observation_data_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config = config, cache = cache, cholera_directory = cholera_directory)
  cache[["stan_input"]][["observation_data"]] %>%
    return()
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_observation_data <- cache_fun_results(name = "observation_data", fun = get_observation_data_no_cache)

#' @name get_obs_year_no_cache
#' @description load obs_yearrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return obs_year
get_obs_year_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  return(
    lubridate::year(lubridate::ymd(cache[["config"]][["general"]][["start_date"]])):
    lubridate::year(lubridate::ymd(cache[["config"]][["general"]][["end_date"]]))
  )
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_obs_year <- cache_fun_results(name = "obs_year", fun = get_obs_year_no_cache)

#' @name get_cmdstan_fit_no_cache
#' @description load cmdstan_fitrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return cmdstan_fit
get_cmdstan_fit_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  file_names <- taxdat::get_filenames(cache[["config"]], cholera_directory)
  cmdstan_fit <- read_file_of_type(file_names[["stan_output"]], "cmdstan_fit")
  return(cmdstan_fit)
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_cmdstan_fit <- cache_fun_results(name = "cmdstan_fit", fun = get_cmdstan_fit_no_cache)

#' @name get_elapsed_time_no_cache
#' @description load elapsed_timerdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return elapsed_time
get_elapsed_time_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  file_names <- taxdat::get_filenames(cache[["config"]], cholera_directory)
  elapsed_time <- read_file_of_type(file_names[["stan_output"]], "elapsed_time")
  return(elapsed_time)
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_elapsed_time <- cache_fun_results(name = "elapsed_time", fun = get_elapsed_time_no_cache)
