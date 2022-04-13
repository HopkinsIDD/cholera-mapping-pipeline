#' @include plot_cache_function.R

#' @name get_observed_polygon_cases_disjoint_no_cache
#' @title get_observed_polygon_cases_disjoint_no_cache
#' @description get observed cases by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return observed cases location periods
aggregate_observed_polygon_cases_disjoint_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config, cache, cholera_directory)
  get_sf_cases_resized(config, cache, cholera_directory)
  observed_polygon_cases_disjoint <- separate_by_overlap(cache[["sf_cases_resized"]],
                                                         name_column = "locationPeriod_id")
  return(observed_polygon_cases_disjoint)
}

get_observed_polygon_cases_disjoint <- cache_fun_results("observed_polygon_cases_disjoint",
                                                         get_observed_polygon_cases_disjoint_no_cache, overwrite = T)

#' @name aggregate_to_location_period
#' @title aggregate_to_location_period
#' @description aggregated cases to location periods
#' @param sf_object sf object
#' @param aggregation_function the function used to aggregate cases by location periods
#' @param group_columns the column name of location period ids
#' @return observed cases by sets (location periods)
aggregate_to_location_period <- function(sf_object, aggregation_function, grouping_columns = "locationPeriod_id",
                                         case_column = "attributes.fields.suspected_cases") {
  sf_object %>%
    dplyr::group_by(!!!rlang::syms(grouping_columns)) %>%
    dplyr::group_modify(function(.x, .y) {
      rc <- sf::st_sf(x = aggregation_function(cases = .x[[case_column]], time_left = .x[["TL"]],
                                               time_right = .x[["TR"]]), geom = sf::st_geometry(.x)[1])
      names(rc)[[1]] <- case_column
      return(rc)
    }) %>%
    sf::st_as_sf() %>%
    return()
}


#' @name aggregate_observed_polygon_cases_disjoint_no_cache
#' @title aggregate_observed_polygon_cases_disjoint_no_cache
#' @description get observed cases by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return observed cases location periods
aggregate_observed_polygon_cases_disjoint_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config, cache, cholera_directory)
  get_sf_cases_resized(config, cache, cholera_directory)
  observed_polygon_cases_disjoint <- separate_by_overlap(cache[["sf_cases_resized"]],
                                                         name_column = "locationPeriod_id")
  return(observed_polygon_cases_disjoint)
}

aggregate_observed_polygon_cases_disjoint <- cache_fun_results("observed_polygon_cases_disjoint",
                                                               aggregate_observed_polygon_cases_disjoint_no_cache, overwrite = T)