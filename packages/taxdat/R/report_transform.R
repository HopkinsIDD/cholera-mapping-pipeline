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
  overlaps <- sf::st_intersects(unique_geometries, st_buffer(unique_geometries, -0.5 / 120))

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
      mean(normalize_cases_by_time(...), na.rm = T)
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

#' @name aggregate_covar_cube_covariates_no_cache
#' @title aggregate_covar_cube_covariates_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases mean by location periods
aggregate_covar_cube_covariates_no_cache <- function(config, cache, cholera_directory) {
  get_covar_cube(config = config, cache = cache, cholera_directory = cholera_directory)
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)

  return(aggregate_to_location_period(
    sf_object = tidyr::pivot_longer(cache[["covar_cube"]], cache[["config"]][["general"]][["covariates"]]),
    aggregation_function = function(cases, time_left, time_right) {
      mean(cases)
    },
    grouping_columns = c("name", "id", "t"),
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

#' @name disaggregate_grid_cases_mean_no_cache
#' @title disaggregate_grid_cases_mean_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases disaggregated to minimal grid scale
disaggregate_grid_cases_mean_no_cache <- function(config, cache, cholera_directory) {
  get_minimal_grid_population(config = config, cache = cache, cholera_directory = cholera_directory)
  get_mean_rates_sf(config = config, cache = cache, cholera_directory = cholera_directory)

  rc <- list()
  counter <- 1
  for (this_t in unique(cache[["mean_rates_sf"]]$t)) {
    local_mean_rates_sf <- cache[["mean_rates_sf"]] %>% dplyr::filter(t == this_t)
    for (pop_tile_idx in which(cache[["minimal_grid_population"]]$t == this_t)) {
      rc[[counter]] <- (
        stars::st_rasterize(local_mean_rates_sf, template = cache[["minimal_grid_population"]][["rast"]][[pop_tile_idx]] * NA) *
          cache[["minimal_grid_population"]][["rast"]][[pop_tile_idx]]
      ) %>%
        sf::st_as_sf() %>%
        dplyr::rename(cases = rates) %>%
        dplyr::mutate(t = this_t) %>%
        sf::st_as_sf()
      rc[[counter]] <- rc[[counter]][
        sf::st_intersects(sf::st_union(cache[["boundary_polygon"]]), sf::st_centroid(rc[[counter]]))[[1]],
      ]
      counter <- counter + 1
    }
  }
  return(do.call(what = dplyr::bind_rows, rc))
}


#' @export
#' @name disaggregate_grid_cases_mean
disaggregate_grid_cases_mean <- cache_fun_results(
  "grid_cases_mean_disaggregated",
  disaggregate_grid_cases_mean_no_cache
)

#' @export
#' @name aggregate_stan_array
#' @title aggregate_stan_array
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases disaggregated to minimal grid scale
aggregate_stan_array <- function(stan_array, aggregation_function, facet_by_chain = FALSE, ...) {
  if (!"draws_array" %in% class(stan_array)) {
    stop("This function operates on stan draws arrays")
  }

  dims_to_facet <- seq_len(length(dim(stan_array)))[-1]
  if (!facet_by_chain) {
    dims_to_facet <- dims_to_facet[-1]
  }

  return(apply(X = stan_array, MARGIN = dims_to_facet, FUN = aggregation_function, ...))
}

#' @name aggregate_grid_cases_mean_no_cache
#' @title aggregate_grid_cases_mean_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases disaggregated to minimal grid scale
aggregate_grid_cases_mean_no_cache <- function(config, cache, cholera_directory) {
  get_grid_cases(config = config, cache = cache, cholera_directory = cholera_directory)
  aggregate_stan_array(
    cache[["grid_cases"]],
    aggregation_function = mean,
    facet_by_chain = FALSE,
    na.rm = FALSE
  )
}

#' @export
#' @name aggregate_grid_cases_mean
aggregate_grid_cases_mean <- cache_fun_results(
  "grid_cases_mean",
  aggregate_grid_cases_mean_no_cache
)

#' @name aggregate_grid_cases_mean_by_chain_no_cache
#' @title aggregate_grid_cases_mean_by_chain_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases disaggregated to minimal grid scale
aggregate_grid_cases_mean_by_chain_no_cache <- function(config, cache, cholera_directory) {
  get_grid_cases(config = config, cache = cache, cholera_directory = cholera_directory)
  t(aggregate_stan_array(
    cache[["grid_cases"]],
    aggregation_function = mean,
    facet_by_chain = TRUE,
    na.rm = FALSE
  ))
}

#' @export
#' @name aggregate_grid_cases_mean_by_chain
aggregate_grid_cases_mean_by_chain <- cache_fun_results(
  "grid_cases_mean_by_chain",
  aggregate_grid_cases_mean_by_chain_no_cache
)


#' @name aggregate_modeled_cases_mean_by_chain_no_cache
#' @title aggregate_modeled_cases_mean_by_chain_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases disaggregated to minimal grid scale
aggregate_modeled_cases_mean_by_chain_no_cache <- function(config, cache, cholera_directory) {
  get_modeled_cases(config = config, cache = cache, cholera_directory = cholera_directory)
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  aggregate_stan_array(
    cache[["modeled_cases"]],
    aggregation_function = mean,
    facet_by_chain = TRUE,
    na.rm = FALSE
  ) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(updated_observation_id = as.numeric(gsub("modeled_cases\\[", "", gsub("]", "", rowname)))) %>%
    tidyr::pivot_longer(
      names_to = "chain",
      values_to = "modeled_cases",
      cols = as.character(seq_len(cache[["config"]][["stan"]][["nchain"]]))
    ) %>%
    dplyr::select(updated_observation_id, chain, modeled_cases)
}

#' @export
#' @name aggregate_modeled_cases_mean_by_chain
aggregate_modeled_cases_mean_by_chain <- cache_fun_results(
  "modeled_cases_mean_by_chain",
  aggregate_modeled_cases_mean_by_chain_no_cache
)
