#' @include plot_cache_function.R

#' @export
#' @name separate_by_overlap
#' @title separate_by_overlap
#' @description group observations by non-overlapping location periods
#' @param sf_object sf object
#' @param name_column the column name of the location periods
#' @return updated sf object by set (location periods)
separate_by_overlap <- function(sf_object, name_column = "location_period_id") {
  unique_geometries <- sf_object %>%
    dplyr::group_by(!!!rlang::syms(name_column)) %>%
    dplyr::summarize(.groups = "drop") %>%
    sf::st_as_sf()
  unique_geometries[["area"]] <- sf::st_area(unique_geometries)
  unique_geometries <- unique_geometries %>%
    dplyr::arrange(-area)
  overlaps <- sf::st_relate(unique_geometries, unique_geometries, "2********")
  
  non_overlaps <- lapply(overlaps, setdiff, x = seq_len(nrow(unique_geometries)))
  
  unique_geometries[["set"]] <- NA
  set_index <- 0
  unassigned_elements <- which(is.na(unique_geometries[["set"]]))
  while (length(unassigned_elements) > 0) {
    set_index <- set_index + 1
    unique_geometries[["set"]][unassigned_elements[[1]]] <- set_index
    compatible_things <- non_overlaps[[unassigned_elements[[1]]]]
    while (length(compatible_things) > 0) {
      unique_geometries[["set"]][compatible_things[[1]]] <- set_index
      compatible_things <- intersect(compatible_things, non_overlaps[[compatible_things[[1]]]])
    }
    unassigned_elements <- which(is.na(unique_geometries[["set"]]))
  }
  
  sf_object[["set"]] <- unique_geometries[["set"]][match(sf_object[[name_column]],
                                                         unique_geometries[[name_column]])]
  
  return(sf_object)
}

#' @export
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

#' @export
#' @name aggregate_observed_polygon_cases_disjoint_no_cache
#' @title aggregate_observed_polygon_cases_disjoint_no_cache
#' @description get observed cases by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return observed cases location periods
aggregate_observed_polygon_cases_disjoint_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(name="stan_input",config=config, cache=cache, cholera_directory=cholera_directory)
  get_sf_cases_resized(name="sf_cases_resized",config=config, cache=cache, cholera_directory=cholera_directory)
  observed_polygon_cases_disjoint <- separate_by_overlap(cache[["sf_cases_resized"]],
                                                         name_column = "locationPeriod_id")
  return(observed_polygon_cases_disjoint)
}

#' @export
#' @name aggregate_observed_polygon_cases_disjoint
aggregate_observed_polygon_cases_disjoint <- cache_fun_results("observed_polygon_cases_disjoint",
                                                               aggregate_observed_polygon_cases_disjoint_no_cache, 
                                                               overwrite = T,
                                                               cofig,
                                                               cholera_directory)


#' @export
#' @name aggregate_observed_polygon_cases_disjoint_aggregated_no_cache
#' @title aggregate_observed_polygon_cases_disjoint_aggregated_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return modeled cases mean by location periods
aggregate_observed_polygon_cases_disjoint_aggregated_no_cache <- function(config, cache,
                                                                          cholera_directory) {
  aggregate_observed_polygon_cases_disjoint(name="observed_polygon_cases_disjoint",
                                            cache=cache,
                                            config=config,
                                            cholera_directory=cholera_directory)

  observed_polygon_cases_disjoint_aggregated = aggregate_to_location_period(cache[["observed_polygon_cases_disjoint"]],
                                                                            aggregation_function = function(...) {
                                                                              mean(normalize_cases_by_time(...))
                                                                            }, grouping_columns = c("locationPeriod_id", "set"), case_column = "attributes.fields.suspected_cases")
  return(observed_polygon_cases_disjoint_aggregated)
}

#' @export
#' @name aggregate_observed_polygon_cases_disjoint_aggregated
aggregate_observed_polygon_cases_disjoint_aggregated <- cache_fun_results("observed_polygon_cases_disjoint_aggregated",
                                                                          aggregate_observed_polygon_cases_disjoint_aggregated_no_cache, overwrite = T,config,cholera_directory)
