#' @include plot_cache_function.R

#' @name plot_sf_with_fill
#' @title plot_sf_with_fill
#' @description plot the polygon with cases or rates information
#' @param name the name of the sf object
#' @param color_scale the color scale of the plot
#' @param fill_column the column name to fill the raster
#' @return ggplot object
plot_sf_with_fill <- function(name, color_scale_type, fill_column) {
  sf_object <- cache[[name]]
  fill_column <- grep(stringr::str_remove(fill_column, "suspected_cases"), names(sf_object),
    value = TRUE
  )
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf_object, ggplot2::aes_string(fill = fill_column)) +
    taxdat::color_scale(type = color_scale_type, use_case = "ggplot map") +
    taxdat::map_theme() +
    ggplot2::facet_wrap(~set)
  return(plot)
}

#' @name plot_observed_cases_polygon_raw
#' @title plot_observed_cases_polygon_raw
#' @description plot the polygon with observed cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' #' @return ggplot object
plot_observed_cases_polygon_raw <- function(config, cache, cholera_directory) {
  get_observed_polygon_cases_disjoint_aggregated(config, cache, cholera_directory)
  plot <- plot_sf_with_fill(
    name = "observed_polygon_cases_disjoint_aggregated",
    color_scale_type = "cases", fill_column = "attributes.fields.suspected_cases"
  )
  return(plot)
}

#' @name plot_modeled_cases_polygon_raw
#' @title plot_modeled_cases_polygon_raw
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return ggplot object
plot_modeled_cases_polygon_raw <- function(config, cache, cholera_directory) {
  get_modeled_polygon_cases_mean_disjoint_aggregated(config, cache, cholera_directory)
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = cache[["modeled_polygon_cases_mean_disjoint_aggregated"]],
      ggplot2::aes(fill = modeled_cases_mean)
    ) +
    taxdat::color_scale(
      type = "cases",
      use_case = "ggplot map"
    ) +
    taxdat::map_theme() +
    ggplot2::facet_wrap(~set)
  return(plot)
}


get_stan_input_no_cache <- function(config, cache, cholera_directory) {
  load(config[["file_names"]][["stan_input"]])
  require(bit64)
  require(sf)
  return(stan_input)
}
get_stan_input <- cache_fun_results("stan_input", get_stan_input_no_cache, overwrite = T, config = config)

#' @name get_sf_cases_resized_no_cache
#' @title get_sf_cases_resized_no_cache
#' @description load sf object (i.e.,sf_cases_resized) from stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return sf_cases_resized object
get_sf_cases_resized_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config, cache, cholera_direcotry)
  return(cache[["stan_input"]][["sf_cases_resized"]])
}

get_sf_cases_resized <- cache_fun_results("sf_cases_resized", get_sf_cases_resized_no_cache, overwrite = T, config = config)

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


# pull stan non-cached stan input
#' @name get_stan_input_no_cache
#' @title get_stan_input_no_cache
#' @description load stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return stan_input
get_stan_input_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config_filename)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  stan_input <- taxdat::read_file_of_type(file_names[["stan_input"]], "stan_input")
  require(bit64)
  require(sf)
  return(stan_input)
}

get_stan_input <- cache_fun_results(
  name = "stan_input", fun = get_stan_input_no_cache,
  overwrite = T, config = config
)

#' @name get_observed_polygon_cases_disjoint_no_cache
#' @title get_observed_polygon_cases_disjoint_no_cache
#' @description get observed cases by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return observed cases location periods
get_observed_polygon_cases_disjoint_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(config, cache, cholera_directory)
  get_sf_cases_resized(config, cache, cholera_directory)
  observed_polygon_cases_disjoint <- separate_by_overlap(cache[["sf_cases_resized"]],
    name_column = "locationPeriod_id"
  )
  return(observed_polygon_cases_disjoint)
}

get_observed_polygon_cases_disjoint <- cache_fun_results("observed_polygon_cases_disjoint",
  get_observed_polygon_cases_disjoint_no_cache,
  overwrite = T
)

# get_observed_polygon_cases_disjoint(config,cache)

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
      rc <- sf::st_sf(x = aggregation_function(
        cases = .x[[case_column]], time_left = .x[["TL"]],
        time_right = .x[["TR"]]
      ), geom = sf::st_geometry(.x)[1])
      names(rc)[[1]] <- case_column
      return(rc)
    }) %>%
    sf::st_as_sf() %>%
    return()
}

#' @name get_observed_polygon_cases_disjoint_aggregated_no_cache
#' @title get_observed_polygon_cases_disjoint_aggregated_no_cache
#' @description get modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return modeled cases mean by location periods
get_observed_polygon_cases_disjoint_aggregated_no_cache <- function(config, cache,
                                                                    cholera_directory) {
  get_observed_polygon_cases_disjoint(config, cache, cholera_directory)
  observed_polygon_cases_disjoint_aggregated <- aggregate_to_location_period(cache[["observed_polygon_cases_disjoint"]],
    aggregation_function = function(...) {
      mean(normalize_cases_by_time(...))
    }, grouping_columns = c("locationPeriod_id", "set"), case_column = "attributes.fields.suspected_cases"
  )
  return(observed_polygon_cases_disjoint_aggregated)
}

get_observed_polygon_cases_disjoint_aggregated <- cache_fun_results("observed_polygon_cases_disjoint_aggregated",
  get_observed_polygon_cases_disjoint_aggregated_no_cache,
  overwrite = T, config = config
)

# pull non-cached modeled cases (for modeled cases)
#' @name get_grid_cases_no_cache
#' @title get_grid_cases_no_cache
#' @description extrac modeled cases from model.rand
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  grid_cases
get_modeled_cases_no_cache <- function(config, cache, cholera_directory) {
  get_model_rand(config, cache, cholera_directory)
  modeled_cases <- as.array(cache[["model.rand"]])[, , grepl("modeled_cases", names(cache[["model.rand"]])),
    drop = FALSE
  ]
  return(modeled_cases)
}

get_modeled_cases <- cache_fun_results("modeled_cases", get_modeled_cases_no_cache,
  overwrite = T, config = config
)

# get new aggregated data: get_grid_cases_mean
get_modeled_cases_mean_no_cache <- function(config, cache, cholera_directory) {
  get_modeled_cases(config, cache, cholera_directory)
  modeled_cases_mean <- aggregate_to_modeled_cases_mean(cache[["modeled_cases"]],
    funs = "mean"
  )
  return(modeled_cases_mean)
}

get_modeled_cases_mean <- cache_fun_results("modeled_cases_mean", get_modeled_cases_mean_no_cache,
  overwrite = T, config = config
)

#' @name aggregate_to_modeled_cases_mean
#' @title aggregate_to_modeled_cases_mean
#' @description get the mean of the modeled cases for each observation
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled cases mean for each grid cell by times
aggregate_to_modeled_cases_mean <- function(modeled_cases, funs = "mean") {
  modeled_cases_mean_by_chain <- apply(modeled_cases, c(2, 3), mean)
  modeled_cases_mean <- apply(modeled_cases_mean_by_chain, c(2), mean)[grepl(
    "modeled_cases",
    names(apply(modeled_cases, c(3), mean))
  ) & !grepl(c("tfrac"), names(apply(
    modeled_cases,
    c(3), mean
  )))]
  return(modeled_cases_mean)
}

# integrate modeled ases into the polygon
#' @name merge_modeled_cases_mean_into_polygon_no_cache
#' @title merge_modeled_cases_mean_into_polygon_no_cache
#' @description merge the modeled cases mean into polygon
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return polygon filled with modeled cases
merge_modeled_cases_mean_into_polygon_no_cache <- function(config, cache, cholera_directory) {
  get_modeled_cases_mean(config, cache, cholera_directory)
  modeled_cases_mean <- cache[["modeled_cases_mean"]]
  get_sf_cases_resized(config, cache, cholera_directory)
  sf_object <- cache[["sf_cases_resized"]]
  sf_object$modeled_cases_mean <- c(modeled_cases_mean)
  return(sf_object)
}

merge_modeled_cases_mean_into_polygon <- cache_fun_results("sf_object", merge_modeled_cases_mean_into_polygon_no_cache,
  overwrite = T, config
)


#' @name get_modeled_polygon_cases_mean_disjoint_no_cache
#' @title get_modeled_polygon_cases_mean_disjoint_no_cache
#' @description get modeld cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return modeled cases mean by location periods
get_modeled_polygon_cases_mean_disjoint_no_cache <- function(config, cache, cholera_directory) {
  merge_modeled_cases_mean_into_polygon_no_cache(config, cache, cholera_directory)
  modeled_polygon_cases_mean_disjoint <- separate_by_overlap(cache[["sf_object"]],
    name_column = "locationPeriod_id"
  )
  return(modeled_polygon_cases_mean_disjoint)
}

get_modeled_polygon_cases_mean_disjoint <- cache_fun_results("modeled_polygon_cases_mean_disjoint",
  get_modeled_polygon_cases_mean_disjoint_no_cache,
  overwrite = T, config = config
)

#' @name get_modeled_polygon_cases_mean_disjoint_aggregated_no_cache
#' @title get_modeled_polygon_cases_mean_disjoint_aggregated_no_cache
#' @description get normalized (aggregated) modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return normalized (aggregated) modeled cases mean by location periods
get_modeled_polygon_cases_mean_disjoint_aggregated_no_cache <- function(config, cache,
                                                                        cholera_directory) {
  get_modeled_polygon_cases_mean_disjoint(config, cache, cholera_directory)
  modeled_polygon_cases_mean_disjoint_aggregated <- aggregate_to_location_period(cache[["modeled_polygon_cases_mean_disjoint"]],
    aggregation_function = function(...) {
      mean(normalize_cases_by_time(...))
    }, grouping_columns = c("locationPeriod_id", "set"), case_column = "modeled_cases_mean"
  )
  return(modeled_polygon_cases_mean_disjoint_aggregated)
}

get_modeled_polygon_cases_mean_disjoint_aggregated <- cache_fun_results("modeled_polygon_cases_mean_disjoint_aggregated",
  get_modeled_polygon_cases_mean_disjoint_aggregated_no_cache,
  overwrite = T, config = config
)
