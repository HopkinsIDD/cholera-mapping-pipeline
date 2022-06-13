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

#' @name get_observation_temporal_location_mapping_no_cache
#' @description load observation_temporal_location_mappingrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return observation_temporal_location_mapping
get_observation_temporal_location_mapping_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config = config, cache = cache, cholera_directory = cholera_directory)
  cache[["stan_input"]][["observation_temporal_location_mapping"]] %>%
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
get_observation_temporal_location_mapping <- cache_fun_results(name = "observation_temporal_location_mapping", fun = get_observation_temporal_location_mapping_no_cache)

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

#' @name get_observed_years_no_cache
#' @description load observed_yearsrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return observed_years
get_observed_years_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  get_observation_temporal_location_mapping(config = config, cache = cache, cholera_directory = cholera_directory)
  return(
    lubridate::ymd(cache[["config"]][["general"]][["start_date"]]) +
      do.call(
        args = lapply(
          unique(cache[["observation_temporal_location_mapping"]][["t"]] - 1),
          lubridate::period,
          units = cache[["config"]][["general"]][["time_scale"]]
        ),
        what = c
      )
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
get_observed_years <- cache_fun_results(name = "observed_years", fun = get_observed_years_no_cache)

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

#' @name get_modeled_years_no_cache
#' @description load modeled_yearsrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled_years
get_modeled_years_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  get_covar_cube(config = config, cache = cache, cholera_directory = cholera_directory)
  return(
    lubridate::ymd(cache[["config"]][["general"]][["start_date"]]) +
      do.call(
        args = lapply(
          unique(cache[["covar_cube"]][["t"]] - 1),
          lubridate::period,
          units = cache[["config"]][["general"]][["time_scale"]]
        ),
        what = c
      )
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
get_modeled_years <- cache_fun_results(name = "modeled_years", fun = get_modeled_years_no_cache)

#' @name get_analysis_years_no_cache
#' @description load analysis_yearsrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return analysis_years
get_analysis_years_no_cache <- function(config, cache, cholera_directory) {
  get_modeled_years(config = config, cache = cache, cholera_directory = cholera_directory)
  get_observed_years(config = config, cache = cache, cholera_directory = cholera_directory)
  return(cache[["observed_years"]][cache[["observed_years"]] %in% cache[["modeled_years"]]])
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_analysis_years <- cache_fun_results(name = "analysis_years", fun = get_analysis_years_no_cache)

#' @name get_dropped_years_no_cache
#' @description load dropped_yearsrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return dropped_years
get_dropped_years_no_cache <- function(config, cache, cholera_directory) {
  get_modeled_years(config = config, cache = cache, cholera_directory = cholera_directory)
  get_analysis_years(config = config, cache = cache, cholera_directory = cholera_directory)
  return(cache[["modeled_years"]][!(cache[["modeled_years"]] %in% cache[["analysis_years"]])])
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_dropped_years <- cache_fun_results(name = "dropped_years", fun = get_dropped_years_no_cache)

#' @name get_boundary_polygon_no_cache
#' @description load boundary_polygonrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return boundary_polygon
get_boundary_polygon_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config = config, cache = cache, cholera_directory = cholera_directory)
  return
  return(cache[["stan_input"]][["boundary_polygon"]])
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_boundary_polygon <- cache_fun_results(name = "boundary_polygon", fun = get_boundary_polygon_no_cache)

#' @name get_minimal_grid_population_no_cache
#' @description load minimal_grid_populationrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return minimal_grid_population
get_minimal_grid_population_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config = config, cache = cache, cholera_directory = cholera_directory)
  return
  return(cache[["stan_input"]][["minimal_grid_population"]])
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_minimal_grid_population <- cache_fun_results(name = "minimal_grid_population", fun = get_minimal_grid_population_no_cache)
