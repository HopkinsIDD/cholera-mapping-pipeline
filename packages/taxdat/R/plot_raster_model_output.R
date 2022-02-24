#' @include plot_cache_function.R

#' @name plot_raster_with_fill
#' @title plot_raster_with_fill
#' @description plot the rasters with cases or rates information
#' @param raster_object raster_object with cases or rates info
#' @param color_scale the color scale of the plot
#' @param fill_column the column name to fill the raster
#' @return ggplot object
plot_raster_with_fill <- function(name, color_scale_type, fill_column, cache) {
    raster_object <- cache[[name]]
    fill_column <- grep(stringr::str_remove(fill_column, "s$"), names(raster_object),
        value = TRUE)
    plot <- ggplot2::ggplot() + ggplot2::geom_sf(data = raster_object, ggplot2::aes_string(fill = fill_column),
        lwd = 0) + taxdat::color_scale(type = color_scale_type, use_case = "ggplot map") +
        taxdat::map_theme() + ggplot2::facet_wrap(~t)
    return(plot)
}

# plot modeled cases raster
#' @name plot_modeled_cases_raster
#' @title plot_modeled_cases_raster
#' @description plot the rasters with modeled cases
#' @param raster_object raster object with modeled cases mean (the mean of modeled cases from model.rand)
#' @param color_scale the color scale of the plot
#' @param fill_column modeled_cases mean
#' @return ggplot object
plot_modeled_cases_mean_raster <- function(config, cache, cholera_directory) {
    merge_modeled_cases_mean_into_raster(config, cache, cholera_directory)
    plot <- plot_raster_with_fill(name = "modeled_cases_mean_raster", color_scale_type = "cases",
        fill_column = "modeled_cases_mean", cache = cache)
    return(plot)
}


# plot modeled rates raster
#' @name plot_modeled_rates_raster
#' @title plot_modeled_rates_raster
#' @description plot the rasters with modeled rates
#' @param raster_object raster object with modeled rates mean (the mean of exponentiated log_lambda from model.rand)
#' @param color_scale the color scale of the plot
#' @param fill_column modeled rates mean
#' @return ggplot object
plot_modeled_rates_raster <- function(config, cache, cholera_directory) {
    merge_modeled_rates_mean_into_raster(config, cache, cholera_directory)
    plot <- plot_raster_with_fill(name = "modeled_rates_mean_raster", color_scale_type = "rates",
        fill_column = "modeled_rates_mean", cache = cache)
    return(plot)

}

########################################################### pull raster object
########################################################### from covar file
#' @name get_covar_cube_no_cache
#' @title get_covar_cube_no_cache
#' @description load stan input
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return covar cube
get_covar_cube_no_cache <- function(config, cache, cholera_directory) {
    config <- yaml::read_yaml(config_filename)
    file_names <- taxdat::get_filenames(config, cholera_directory)
    covar_cube <- taxdat::read_file_of_type(file_names[["covar"]], "covar_cube_output")
    require(bit64)
    require(sf)
    return(covar_cube)
}
# cache the results
get_covar_cube <- cache_fun_results(name = "covar_cube", fun = get_covar_cube_no_cache,
    overwrite = T, config = config)
# get_covar_cube(config=config,cache)

#' @name get_raster_object_no_cache
#' @title get_raster_object_no_cache
#' @description load covar_cube from stan input
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return covar_cube (raster object)
get_raster_object_no_cache <- function(config, cache, cholera_directory) {
    get_covar_cube(config, cache, cholera_directory)
    sf_grid <- cache[["covar_cube"]]$sf_grid
    return(sf_grid)
}
# cache the results
get_raster_object <- cache_fun_results(name = "sf_grid", fun = get_raster_object_no_cache,
    overwrite = T, config = config)
# get_raster_object(config=config,cache)

##### pull stan non-cached model.rand
#' @name get_model_rand_no_cache
#' @title get_model_rand_no_cache
#' @description load model.rand from stan output
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return model.rand
get_model_rand_no_cache <- function(config, cache, cholera_directory) {
    config <- yaml::read_yaml(config_filename)
    file_names <- taxdat::get_filenames(config, cholera_directory)
    model.rand <- taxdat::read_file_of_type(file_names[["stan_output"]], "model.rand")
    require(bit64)
    require(sf)
    return(model.rand)
}
get_model_rand <- cache_fun_results(name = "model.rand", fun = get_model_rand_no_cache,
    overwrite = T, config = config)
# get_model_rand(config=config,cache)


# pull non-cached modeled cases (for modeled cases)
#' @name get_modeled_cases_no_cache
#' @title get_modeled_cases_no_cache
#' @description extrac modeled cases from model.rand
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled_cases
get_modeled_cases_no_cache <- function(config, cache, cholera_directory) {
    get_model_rand(config, cache, cholera_directory)
    modeled_cases <- MCMCvis::MCMCchains(cache[["model.rand"]], params = "grid_cases")
    return(modeled_cases)
}
# cache the results
get_modeled_cases <- cache_fun_results("modeled_cases", get_modeled_cases_no_cache,
    overwrite = T, config = config)
# get_modeled_cases(config=config,cache)

# pull non-cached grid rates (for modeled cases)
#' @name get_modeled_rates_no_cache
#' @title get_modeled_rates_no_cache
#' @description extrac modeled rates (from log lambda) from model.rand
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled_rates
get_modeled_rates_no_cache <- function(config, cache, cholera_directory) {
    get_model_rand(config, cache, cholera_directory)
    modeled_rates <- exp(MCMCvis::MCMCchains(cache[["model.rand"]], params = "log_lambda"))
    return(modeled_rates)
}
# cache the results
get_modeled_rates <- cache_fun_results("modeled_rates", get_modeled_rates_no_cache,
    overwrite = T, config = config)
# get_modeled_rates(config=config,cache)


# get new aggregated data: get_modeled_cases_mean
get_modeled_cases_mean_no_cache <- function(config, cache, cholera_directory) {
    get_modeled_cases(config, cache, cholera_directory)
    modeled_cases_mean <- aggregate_to_modeled_cases_mean(cache[["modeled_cases"]],
        funs = "mean")
    return(modeled_cases_mean)
}
get_modeled_cases_mean <- cache_fun_results("modeled_cases_mean", get_modeled_cases_mean_no_cache,
    overwrite = T, config = config)
# get_modeled_cases_mean(config,cache)

#' @name aggregate_to_modeled_cases_mean
#' @title aggregate_to_modeled_cases_mean
#' @description get the mean of the modeled cases for each grid cell
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled cases mean for each grid cell by times
aggregate_to_modeled_cases_mean <- function(modeled_cases, funs = "mean") {
    modeled_cases_mean <- apply(modeled_cases, 2, mean)
    return(modeled_cases_mean)
}

# get new aggregated data: get_modeled_rates_mean
get_modeled_rates_mean_no_cache <- function(config, cache, cholera_directory) {
    get_modeled_rates(config, cache, cholera_directory)
    modeled_rates_mean <- aggregate_to_modeled_rates_mean(cache[["modeled_rates"]],
        funs = "mean")
    return(modeled_rates_mean)
}
get_modeled_rates_mean <- cache_fun_results("modeled_rates_mean", get_modeled_rates_mean_no_cache,
    overwrite = T, config = config)
# get_modeled_rates_mean(config=config,cache)

#' @name aggregate_to_modeled_rates_mean
#' @title aggregate_to_modeled_rates_mean
#' @description get the mean of the modeled rates each grid cell
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled rates mean for each grid cell by times
aggregate_to_modeled_rates_mean <- function(modeled_rates, funs = "mean") {
    modeled_rates_mean <- apply(modeled_rates, 2, mean)
    return(modeled_rates_mean)
}

###############'merge/attach' functions############################
# integrate grid cases mean into the raster
#' @name merge_modeled_cases_mean_into_raster
#' @title merge_modeled_cases_mean_into_raster
#' @description merge the modeled cases mean into raster
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  raster filled with modeled cases mean for each grid cell by times
merge_modeled_cases_mean_into_raster_no_cache <- function(config, cache, cholera_directory) {
    get_raster_object(config, cache, cholera_directory)
    modeled_cases_mean_raster <- cache[["sf_grid"]]
    get_modeled_cases_mean(config, cache, cholera_directory)
    modeled_cases_mean <- cache[["modeled_cases_mean"]]
    modeled_cases_mean_raster$modeled_cases_mean <- modeled_cases_mean
    return(modeled_cases_mean_raster)
}
merge_modeled_cases_mean_into_raster <- cache_fun_results("modeled_cases_mean_raster",
    merge_modeled_cases_mean_into_raster_no_cache, overwrite = T, config)
# merge_modeled_cases_mean_into_raster(config,cache)

# integrate modeled rates mean into the raster
#' @name merge_modeled_rates_mean_into_raster
#' @title merge_modeled_rates_mean_into_raster
#' @description merge the modeled rates into raster
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  raster filled with modeled rates mean for each grid cell by times
merge_modeled_rates_mean_into_raster_no_cache <- function(config, cache, cholera_directory) {
    get_raster_object(config, cache, cholera_directory)
    grid_rates_mean_raster <- cache[["sf_grid"]]
    get_modeled_rates_mean(config, cache, cholera_directory)
    modeled_rates_mean <- cache[["modeled_rates_mean"]]
    grid_rates_mean_raster$modeled_rates_mean <- modeled_rates_mean
    return(grid_rates_mean_raster)
}
merge_modeled_rates_mean_into_raster <- cache_fun_results("modeled_rates_mean_raster",
    merge_modeled_rates_mean_into_raster_no_cache, overwrite = T, config)
