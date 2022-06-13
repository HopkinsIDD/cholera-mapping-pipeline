#' @export
#' @name normalize_cases_by_time
#' @title normalize_cases_by_time
#' @description normalize the cases reported in location periods to yearly data
#' @param cases cholera cases
#' @param time_left the lower bound of the location period
#' @param time_right the upper bound of the location periods
#' @return annual cholera cases
normalize_cases_by_time <- function(cases, time_left, time_right) {
  return(cases / as.numeric(time_right - time_left + 1) * 365)
}

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

  sf_object[["set"]] <- unique_geometries[["set"]][match(
    sf_object[[name_column]],
    unique_geometries[[name_column]]
  )]

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
aggregate_to_location_period <- function(sf_object, aggregation_function, grouping_columns = "location_period_id",
                                         case_column = "suspected_cases", time_columns = c("time_left", "time_right")) {
  sf_object %>%
    dplyr::group_by(!!!rlang::syms(grouping_columns)) %>%
    dplyr::group_modify(function(.x, .y) {
      rc <- sf::st_sf(x = aggregation_function(
        cases = .x[[case_column]], time_left = .x[[time_columns[[1]]]],
        time_right = .x[[time_columns[[2]]]]
      ), geom = sf::st_geometry(.x)[1])
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
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folders
#' @return observed cases location periods
aggregate_observed_polygon_cases_disjoint_no_cache <- function(config, cache, cholera_directory) {
  get_observation_data(config = config, cache = cache, cholera_directory = cholera_directory)
  observed_polygon_cases_disjoint <- separate_by_overlap(cache[["observation_data"]],
    name_column = "location_period_id"
  )
  return(observed_polygon_cases_disjoint)
}

#' @export
#' @name aggregate_observed_polygon_cases_disjoint
aggregate_observed_polygon_cases_disjoint <- cache_fun_results(
  "observed_polygon_cases_disjoint",
  aggregate_observed_polygon_cases_disjoint_no_cache
)

#' @export
#' @name aggregate_observed_polygon_cases_disjoint_aggregated_no_cache
#' @title aggregate_observed_polygon_cases_disjoint_aggregated_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases mean by location periods
aggregate_observed_polygon_cases_disjoint_aggregated_no_cache <- function(config, cache, cholera_directory) {
  aggregate_observed_polygon_cases_disjoint(
    cache = cache,
    config = config,
    cholera_directory = cholera_directory
  )

  observed_polygon_cases_disjoint_aggregated <- aggregate_to_location_period(
    cache[["observed_polygon_cases_disjoint"]],
    aggregation_function = function(...) {
      mean(normalize_cases_by_time(...))
    },
    grouping_columns = c("location_period_id", "set"),
    case_column = "suspected_cases"
  )
  return(observed_polygon_cases_disjoint_aggregated)
}

#' @export
#' @name aggregate_observed_polygon_cases_disjoint_aggregated
aggregate_observed_polygon_cases_disjoint_aggregated <- cache_fun_results(
  "observed_polygon_cases_disjoint_aggregated",
  aggregate_observed_polygon_cases_disjoint_aggregated_no_cache
)


#' @export
#' @name aggregate_observed_polygon_cases_disjoint_counted_no_cache
#' @title aggregate_observed_polygon_cases_disjoint_counted_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases mean by location periods
aggregate_observed_polygon_cases_disjoint_counted_no_cache <- function(config, cache, cholera_directory) {
  aggregate_observed_polygon_cases_disjoint(
    cache = cache,
    config = config,
    cholera_directory = cholera_directory
  )

  observed_polygon_cases_disjoint_counted <- aggregate_to_location_period(
    cache[["observed_polygon_cases_disjoint"]],
    aggregation_function = function(cases, time_left, time_right) {
      length(cases)
    },
    grouping_columns = c("location_period_id", "set"),
    case_column = "suspected_cases"
  )
  return(observed_polygon_cases_disjoint_counted)
}

#' @export
#' @name aggregate_observed_polygon_cases_disjoint_counted
aggregate_observed_polygon_cases_disjoint_counted <- cache_fun_results(
  "observed_polygon_cases_disjoint_counted",
  aggregate_observed_polygon_cases_disjoint_counted_no_cache
)

#' @export
#' @name aggregate_covar_cube_covariates_no_cache
#' @title aggregate_covar_cube_covariates_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases mean by location periods
aggregate_covar_cube_covariates_no_cache <- function(config, cache, cholera_directory) {
  get_covar_cube(config = config, cache = cache, cholera_directory = cholera_directory)
  get_config(config = params$config, cache = cache, cholera_directory = params$cholera_directory)

  return(aggregate_to_location_period(
    sf_object = tidyr::pivot_longer(cache[["covar_cube"]], cache[["config"]][["general"]][["covariates"]]),
    aggregation_function = function(cases, time_left, time_right) {
      mean(cases)
    },
    grouping_columns = c("name", "id"),
    case_column = "value",
    time_columns = c("t", "t")
  ))
}

#' @export
#' @name aggregate_covar_cube_covariates
aggregate_covar_cube_covariates <- cache_fun_results(
  "covar_cube_covariates_aggregated",
  aggregate_covar_cube_covariates_no_cache
)
