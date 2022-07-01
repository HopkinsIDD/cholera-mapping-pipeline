#' @include report_cache.R
#' @include setup_helpers.R

#' @name get_config_no_cache
#' @description load config.rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return config
get_config_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config)
  config <- complete_config(config)
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

#' @name get_model.rand_no_cache
#' @description load model.randrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return model.rand
get_model.rand_no_cache <- function(config, cache, cholera_directory) {
  get_cmdstan_fit(config = config, cache = cache, cholera_directory = cholera_directory)
  return(rstan::read_stan_csv(cache[["cmdstan_fit"]]$output_files()))
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_model.rand <- cache_fun_results(name = "model.rand", fun = get_model.rand_no_cache)

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
  rc <- cache[["stan_input"]][["minimal_grid_population"]]
  rc$t <- cast_to_int32(rc$t)
  return(rc)
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

#' @name get_grid_cases_no_cache
#' @description load grid_casesrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return grid_cases
get_grid_cases_no_cache <- function(config, cache, cholera_directory) {
  get_cmdstan_fit(config = config, cache = cache, cholera_directory = cholera_directory)
  return(cache[["cmdstan_fit"]]$draws(variables = "grid_cases"))
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_grid_cases <- cache_fun_results(name = "grid_cases", fun = get_grid_cases_no_cache)


#' @name get_sf_grid_data_no_cache
#' @description load sf_grid_cases/incidence
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return grid_cases
get_sf_grid_data_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  true_grid_data<-readRDS(cache[["config"]][["test_metadata"]][["test_true_grid_case_filename"]])
  return(true_grid_data)
}
## cache the results
#' @export
#' @name get_sf_grid_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_sf_grid_data <- cache_fun_results(name = "true_grid_data", fun = get_sf_grid_data_no_cache)


#' @name get_modeled_cases_no_cache
#' @description load modeled_casesrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled_cases
get_modeled_cases_no_cache <- function(config, cache, cholera_directory) {
  get_cmdstan_fit(config = config, cache = cache, cholera_directory = cholera_directory)
  return(cache[["cmdstan_fit"]]$draws(variables = "modeled_cases"))
}
## cache the results
#' @export
#' @name get_stan_data_data
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_modeled_cases <- cache_fun_results(name = "modeled_cases", fun = get_modeled_cases_no_cache)

#' @name get_mean_rates_sf_no_cache
#' @description load mean_rates_sfrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return mean_rates_sf
get_mean_rates_sf_no_cache <- function(config, cache, cholera_directory) {
  get_minimal_grid_population(config = config, cache = cache, cholera_directory = cholera_directory)
  aggregate_grid_cases_mean(config = config, cache = cache, cholera_directory = cholera_directory)
  mean_rates_sf <- sf::st_sf(
    rates = cache[["grid_cases_mean"]] / cache[["covar_cube"]][["population"]],
    t = cache[["covar_cube"]][["t"]],
    geometry = sf::st_geometry(cache[["covar_cube"]])
  )
  return(return(mean_rates_sf))
}
## cache the results
#' @export
#' @name get_mean_rates_sf
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_mean_rates_sf <- cache_fun_results(name = "mean_rates_sf", fun = get_mean_rates_sf_no_cache)

#' @name get_data_fidelity_df_no_cache
#' @description load data_fidelity_dfrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return data_fidelity_df
get_data_fidelity_df_no_cache <- function(config, cache, cholera_directory) {
  aggregate_modeled_cases_mean_by_chain(config = config, cache = cache, cholera_directory = cholera_directory)
  get_stan_input(config = config, cache = cache, cholera_directory = cholera_directory)
  sCh_national= data.frame(cache[["stan_input"]][["observation_data"]]%>%filter(location_name==1))%>%dplyr::select(suspected_cases)
  cache[["modeled_cases_mean_by_chain"]] %>%
    dplyr::mutate(
      observed_cases = cache[["stan_input"]][["stan_data"]][["y"]][as.numeric(updated_observation_id)],
      tfrac = cache[["stan_input"]][["stan_data"]][["tfrac"]][as.numeric(updated_observation_id)],
      spatial_scale=ifelse(observed_cases==sCh_national$suspected_cases,"National level","Sub-national level"),
      censored = ifelse(
        updated_observation_id %in% cache[["stan_input"]][["stan_data"]][["ind_full"]],
        "uncensored",
        ifelse(
          updated_observation_id %in% cache[["stan_input"]][["stan_data"]][["ind_right"]],
          "right",
          ifelse(
            updated_observation_id %in% cache[["stan_input"]][["stan_data"]][["ind_left"]],
            "left",
            as.character(NA)
          )
        )
      )
    ) %>%
    return()
}
## cache the results
#' @export
#' @name get_data_fidelity_df
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_data_fidelity_df <- cache_fun_results(name = "data_fidelity_df", fun = get_data_fidelity_df_no_cache)

#' @name get_stan_parameters_of_interest_no_cache
#' @description load stan_parameters_of_interestrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return stan_parameters_of_interest
get_stan_parameters_of_interest_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  warning("This function should do something smarter")
  return(c("rho", "betas", "eta", "log_std_dev_w"))
}
## cache the results
#' @export
#' @name get_stan_parameters_of_interest
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_stan_parameters_of_interest <- cache_fun_results(name = "stan_parameters_of_interest", fun = get_stan_parameters_of_interest_no_cache)

#' @name get_stan_parameter_draws_no_cache
#' @description load stan_parameter_drawsrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return stan_parameter_draws
get_stan_parameter_draws_no_cache <- function(config, cache, cholera_directory) {
  get_stan_parameters_of_interest(config = config, cache = cache, cholera_directory = cholera_directory)
  get_cmdstan_fit(config = config, cache = cache, cholera_directory = cholera_directory)
  variable_names <- sapply(
    X = cache[["stan_parameters_of_interest"]],
    FUN = function(x) {
      return(
        cache[["cmdstan_fit"]]$metadata()$variables[
          grepl(
            paste0("^", x, "\\[?[1234567890,]*]?"),
            cache[["cmdstan_fit"]]$metadata()$variables
          )
        ]
      )
    }
  )
  return(cache[["cmdstan_fit"]]$draws()[, , unlist(variable_names)])
}
## cache the results
#' @export
#' @name get_stan_parameter_draws
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_stan_parameter_draws <- cache_fun_results(name = "stan_parameter_draws", fun = get_stan_parameter_draws_no_cache)


#' @name get_rhat_threshold_no_cache
#' @description load rhat_thresholdrdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return rhat_threshold
get_rhat_threshold_no_cache <- function(config, cache, cholera_directory) {
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  return(1.05)
}
## cache the results
#' @export
#' @name get_rhat_threshold
#' @description load stan data rdata
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return None. Instead stores the value in the provided cache
get_rhat_threshold <- cache_fun_results(name = "rhat_threshold", fun = get_rhat_threshold_no_cache)
