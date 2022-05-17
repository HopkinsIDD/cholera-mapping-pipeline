#' @include plot_cache_function.R

#' @export
#' @name plot_sf_with_fill
#' @title plot_sf_with_fill
#' @description plot the polygon with cases or rates information
#' @param name the name of the sf object
#' @param color_scale the color scale of the plot
#' @param fill_column the column name to fill the raster
#' @return ggplot object
plot_sf_with_fill <- function(cache,name, color_scale_type, fill_column) {
    sf_object <- cache[[name]]
    # updated_fill_column <- fill_column
    # updated_fill_column <- grep(stringr::str_remove(updated_fill_column, fill_column), names(sf_object),
    #     value = TRUE)
    plot <- ggplot2::ggplot() + 
        ggplot2::geom_sf(data = sf_object, ggplot2::aes_string(fill = fill_column)) +
        taxdat::color_scale(type = color_scale_type, use_case = "ggplot map") + 
        taxdat::map_theme() +
        ggplot2::facet_wrap(~set)
    return(plot)
}

#' @export
#' @name plot_observed_cases_polygon_raw
#' @title plot_observed_cases_polygon_raw
#' @description plot the polygon with observed cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' #' @return ggplot object
plot_observed_cases_polygon_raw <- function(config, cache, cholera_directory) {
    aggregate_observed_polygon_cases_disjoint_aggregated(name="observed_polygon_cases_disjoint_aggregated",
                                                         config=config,
                                                         cholera_directory=cholera_directory,
                                                         cache=cache)
    plot <- plot_sf_with_fill(cache=cache,name = "observed_polygon_cases_disjoint_aggregated",
        color_scale_type = "cases", fill_column = "attributes.fields.suspected_cases")
    return(plot)
}

#' @export
#' @name plot_area_adjusted_observed_cases
#' @title plot_area_adjusted_observed_cases
#' @description plot the polygon with observed cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' #' @return ggplot object
plot_area_adjusted_observed_cases<- function(config, cache, cholera_directory) {
    aggregate_observed_polygon_cases_disjoint_aggregated(name="observed_polygon_cases_disjoint_aggregated",
                                                         config=config,
                                                         cholera_directory=cholera_directory,
                                                         cache=cache)
    cache[["area_adjusted_observed_polygon_cases_disjoint_aggregated"]]<-cache[["observed_polygon_cases_disjoint_aggregated"]]%>%
        rowwise()%>%
        mutate(area_adjusted_suspected_cases=as.numeric(attributes.fields.suspected_cases/sf::st_area(geom)))
    
    plot <- plot_sf_with_fill(cache=cache,name = "area_adjusted_observed_polygon_cases_disjoint_aggregated",
                              color_scale_type = "cases", fill_column = "area_adjusted_suspected_cases")
    return(plot)
}

#' @export
#' @name plot_raw_observations
#' @title plot_raw_observations
#' @description plot the polygon with number of observations
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' #' @return ggplot object
plot_raw_observations<- function(config, cache, cholera_directory) {
    aggregate_observed_polygon_cases_disjoint(name="observed_polygon_cases_disjoint",
                                                         config=config,
                                                         cholera_directory=cholera_directory,
                                                         cache=cache)
    cache[["observed_polygon_observations_disjoint_aggregated"]]<-cache[["observed_polygon_cases_disjoint"]]%>%
        group_by(locationPeriod_id)%>%
        mutate(observations=n())
    
    plot<-ggplot2::ggplot()+
        ggplot2::geom_sf(
            data = cache[["observed_polygon_observations_disjoint_aggregated"]],
            ggplot2::aes(fill = observations)
        ) +
        ggplot2::scale_fill_viridis_c("Observation") +
        ggplot2::facet_wrap(~set, ncol = 5) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")

    return(plot)
}

#' @export
#' @name plot_modeled_cases_polygon_raw
#' @title plot_modeled_cases_polygon_raw
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return ggplot object
plot_modeled_cases_polygon_raw <- function(config, cache, cholera_directory) {
    get_modeled_polygon_cases_mean_disjoint_aggregated(config=config, cache=cache, cholera_directory=cholera_directory)
    plot <- ggplot2::ggplot() + ggplot2::geom_sf(data = cache[["modeled_polygon_cases_mean_disjoint_aggregated"]],
        ggplot2::aes(fill = modeled_cases_mean)) + taxdat::color_scale(type = "attributes.fields.suspected_cases",
        use_case = "ggplot map") + taxdat::map_theme() + ggplot2::facet_wrap(~set)
    return(plot)
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

    sf_object[["set"]] <- unique_geometries[["set"]][match(sf_object[[name_column]],
        unique_geometries[[name_column]])]

    return(sf_object)
}

#' @export
#' @name normalize_cases_by_time
#' @title normalize_cases_by_time
#' @description normalize the cases reported in location periods to yearly data
#' @param cases cholera cases
#' @param time_left the lower bound of the location period
#' @param time_right the upper bound of the location periods
#' @return annual cholera cases
normalize_cases_by_time <- function(cases, time_left, time_right) {
    return(cases/as.numeric(time_right - time_left + 1) * 365)
}

# get new aggregated data: get_grid_cases_mean
#' @export
#' @name get_modeled_cases_mean_no_cache
get_modeled_cases_mean_no_cache <- function(config, cache, cholera_directory) {
    get_modeled_cases(config, cache, cholera_directory)
    modeled_cases_mean <- aggregate_to_modeled_cases_mean(cache[["modeled_cases"]],
        funs = "mean")
    return(modeled_cases_mean)
}
#' @export
#' @name get_modeled_cases_mean
get_modeled_cases_mean <- cache_fun_results("modeled_cases_mean", get_modeled_cases_mean_no_cache,
    overwrite = T, config = config)

#' @export
#' @name aggregate_to_modeled_cases_mean
#' @title aggregate_to_modeled_cases_mean
#' @description get the mean of the modeled cases for each observation
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return  modeled cases mean for each grid cell by times
aggregate_to_modeled_cases_mean <- function(modeled_cases, funs = "mean") {
    modeled_cases_mean_by_chain <- apply(modeled_cases, c(2, 3), mean)
    modeled_cases_mean <- apply(modeled_cases_mean_by_chain, c(2), mean)[grepl("modeled_cases",
        names(apply(modeled_cases, c(3), mean))) & !grepl(c("tfrac"), names(apply(modeled_cases,
        c(3), mean)))]
    return(modeled_cases_mean)
}

# integrate modeled ases into the polygon
#' @export
#' @name merge_modeled_cases_mean_into_polygon_no_cache
#' @title merge_modeled_cases_mean_into_polygon_no_cache
#' @description merge the modeled cases mean into polygon
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return polygon filled with modeled cases
merge_modeled_cases_mean_into_polygon_no_cache <- function(config, cache, cholera_directory) {
    get_modeled_cases_mean(config, cache, cholera_directory)
    modeled_cases_mean <- cache[["modeled_cases_mean"]]
    get_sf_cases_resized(config, cache, cholera_directory)
    sf_object <- cache[["sf_cases_resized"]]
    sf_object$modeled_cases_mean <- c(modeled_cases_mean)
    return(sf_object)
}
#' @export
#' @name merge_modeled_cases_mean_into_polygon
merge_modeled_cases_mean_into_polygon <- cache_fun_results("sf_object", merge_modeled_cases_mean_into_polygon_no_cache,
    overwrite = T, config)

#' @export
#' @name aggregate_modeled_polygon_cases_mean_disjoint_no_cache
#' @title aggregate_modeled_polygon_cases_mean_disjoint_no_cache
#' @description get modeld cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return modeled cases mean by location periods
aggregate_modeled_polygon_cases_mean_disjoint_no_cache <- function(config, cache, cholera_directory) {
    merge_modeled_cases_mean_into_polygon_no_cache(config, cache, cholera_directory)
    modeled_polygon_cases_mean_disjoint <- separate_by_overlap(cache[["sf_object"]],
        name_column = "locationPeriod_id")
    return(modeled_polygon_cases_mean_disjoint)
}
#' @export
#' @name aggregate_modeled_polygon_cases_mean_disjoint

aggregate_modeled_polygon_cases_mean_disjoint <- cache_fun_results("modeled_polygon_cases_mean_disjoint",
                                                                   aggregate_modeled_polygon_cases_mean_disjoint_no_cache, overwrite = T)

#' @export
#' @name aggregate_modeled_polygon_cases_mean_disjoint_aggregated_no_cache
#' @title aggregate_modeled_polygon_cases_mean_disjoint_aggregated_no_cache
#' @description get normalized (aggregated) modeled cases mean by polygon (location periods)
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return normalized (aggregated) modeled cases mean by location periods
aggregate_modeled_polygon_cases_mean_disjoint_aggregated <- function(config, cache,
    cholera_directory) {
    get_modeled_polygon_cases_mean_disjoint(config, cache, cholera_directory)
    modeled_polygon_cases_mean_disjoint_aggregated = aggregate_to_location_period(cache[["modeled_polygon_cases_mean_disjoint"]],
        aggregation_function = function(...) {
            mean(normalize_cases_by_time(...))
        }, grouping_columns = c("locationPeriod_id", "set"), case_column = "modeled_cases_mean")
    return(modeled_polygon_cases_mean_disjoint_aggregated)
}
#' @export
#' @name aggregate_modeled_polygon_cases_mean_disjoint_aggregated
aggregate_modeled_polygon_cases_mean_disjoint_aggregated <- cache_fun_results("modeled_polygon_cases_mean_disjoint_aggregated",
                                                                        aggregate_modeled_polygon_cases_mean_disjoint_aggregated_no_cache, overwrite = T)
