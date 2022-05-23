#' @include plot_cache_function.R

#' @export
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
#' @export
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
#' @export
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


# get new aggregated data: get_modeled_rates_mean
#' @export
#' @name get_modeled_rates_mean_no_cache
get_modeled_rates_mean_no_cache <- function(config, cache, cholera_directory, ...) {
    get_modeled_rates(config, cache, cholera_directory, ...)
    modeled_rates_mean <- aggregate_to_modeled_rates_mean(cache[["modeled_rates"]],
        funs = "mean")
    return(modeled_rates_mean)
}
#' @export
#' @name get_modeled_rates_mean
get_modeled_rates_mean <- cache_fun_results("modeled_rates_mean", get_modeled_rates_mean_no_cache,
    overwrite = T, config = config)
# get_modeled_rates_mean(config=config,cache)

###############'merge/attach' functions############################
# integrate grid cases mean into the raster
#' @export
#' @name merge_modeled_cases_mean_into_raster
#' @title merge_modeled_cases_mean_into_raster
#' @description merge the modeled cases mean into raster
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  raster filled with modeled cases mean for each grid cell by times
merge_modeled_cases_mean_into_raster_no_cache <- function(config, cache, cholera_directory, ...) {
    get_raster_object(config, cache, cholera_directory, ...)
    modeled_cases_mean_raster <- cache[["sf_grid"]]
    get_modeled_cases_mean(config, cache, cholera_directory, ...)
    modeled_cases_mean <- cache[["modeled_cases_mean"]]
    modeled_cases_mean_raster$modeled_cases_mean <- modeled_cases_mean
    return(modeled_cases_mean_raster)
}
#' @export
#' @name merge_modeled_cases_mean_into_raster
merge_modeled_cases_mean_into_raster <- cache_fun_results("modeled_cases_mean_raster",
    merge_modeled_cases_mean_into_raster_no_cache, overwrite = T, config)

# integrate modeled rates mean into the raster
#' @export
#' @name merge_modeled_rates_mean_into_raster
#' @title merge_modeled_rates_mean_into_raster
#' @description merge the modeled rates into raster
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  raster filled with modeled rates mean for each grid cell by times
merge_modeled_rates_mean_into_raster_no_cache <- function(config, cache, cholera_directory, ...) {
    get_raster_object(config, cache, cholera_directory, ...)
    grid_rates_mean_raster <- cache[["sf_grid"]]
    get_modeled_rates_mean(config, cache, cholera_directory, ...)
    modeled_rates_mean <- cache[["modeled_rates_mean"]]
    grid_rates_mean_raster$modeled_rates_mean <- modeled_rates_mean
    return(grid_rates_mean_raster)
}
#' @export
#' @name merge_modeled_rates_mean_into_raster
merge_modeled_rates_mean_into_raster <- cache_fun_results("modeled_rates_mean_raster",
    merge_modeled_rates_mean_into_raster_no_cache, overwrite = T, config)

# stitch rate rasters from different single years together 
#' @export
#' @name stitch_rate_raster_stack
#' @title stitch_rate_raster_stack
#' @description stitch rate rasters from different single years together 
#' @param output_cache cached output
#' @param input_caches cached configs
#' @param cholera_directory cholera directory
#' @param config_filename full name of config
#' @param output_dir model output dir 
#' @return  cached output 
stitch_rate_raster_stack_no_cache <- function(output_cache, input_caches, cholera_directory, config_filename, output_dir){
  stitch_caches(output_cache, 
                name = "modeled_rates_raster", 
                input_caches, 
                initial_value = list(type = "first"), 
                combination_function = stack_case_rate_raster_in_sf, 
                cholera_directory, 
                config_filename, 
                output_dir)
}

#' @export
#' @name stitch_rate_raster_stack
stitch_rate_raster_stack <- cache_fun_results("modeled_rates_raster",
    stitch_rate_raster_stack_no_cache, overwrite = T, config_filename) 

# get the mean of the rates
#' @export
#' @name get_mean_rate_raster_in_sf
#' @title get_mean_rate_raster_in_sf
#' @description get the mean of the rates 
#' @param output_cache cached output
#' @return  cached output 
get_mean_rate_raster_in_sf_no_cache <- function(output_cache){
  if("modeled_rates_mean_raster" %in% names(output_cache)){
    output_cache[["modeled_rates_mean_raster"]]$grp = sapply(st_equals(output_cache[["modeled_rates_mean_raster"]]), max)
    output_cache[["modeled_rates_mean_raster"]] <- output_cache[["modeled_rates_mean_raster"]] %>% 
      group_by(grp) %>% 
      summarise(modeled_rates_mean = mean(modeled_rates_mean, na.rm = TRUE), t = "Annualized") %>% 
      select(t, modeled_rates_mean) 

  }else{
    output_cache[["modeled_rates_raster"]]$grp = sapply(st_equals(output_cache[["modeled_rates_raster"]]), max)
    output_cache[["modeled_rates_mean_raster"]] <- output_cache[["modeled_rates_raster"]] %>% 
      group_by(grp) %>% 
      summarise(modeled_rates_mean = mean(modeled_rates, na.rm = TRUE), t = "Annualized") %>% 
      select(t, modeled_rates_mean) 
  }
  
}

#' @export
#' @name get_mean_rate_raster_in_sf
get_mean_rate_raster_in_sf <- cache_fun_results("modeled_cases_mean_raster", get_mean_rate_raster_in_sf_no_cache, overwrite = T) 

# stitch case rasters from different single years together 
#' @export
#' @name stitch_case_raster_stack
#' @title stitch_case_raster_stack
#' @description stitch case rasters from different single years together 
#' @param output_cache cached output
#' @param input_caches cached configs
#' @param cholera_directory cholera directory
#' @param config_filename full name of config
#' @param output_dir model output dir 
#' @return  cached output 
stitch_case_raster_stack_no_cache <- function(output_cache, input_caches, cholera_directory, config_filename, output_dir){
  stitch_caches(output_cache, 
                name = "modeled_cases_raster", 
                input_caches, 
                initial_value = list(type = "first"), 
                combination_function = stack_case_rate_raster_in_sf, 
                cholera_directory, 
                config_filename, 
                output_dir)
}

#' @export
#' @name stitch_case_raster_stack
stitch_case_raster_stack <- cache_fun_results("modeled_cases_raster",
    stitch_case_raster_stack_no_cache, overwrite = T, config_filename)

# get the mean of the cases 
#' @export
#' @name get_mean_case_raster_in_sf
#' @title get_mean_case_raster_in_sf
#' @description get the mean of the cases 
#' @param output_cache cached output
#' @return  cached output 
get_mean_case_raster_in_sf_no_cache <- function(output_cache){
  if("modeled_cases_mean_raster" %in% names(output_cache)){
    output_cache[["modeled_cases_mean_raster"]]$grp = sapply(st_equals(output_cache[["modeled_cases_mean_raster"]]), max)
    output_cache[["modeled_cases_mean_raster"]] <- output_cache[["modeled_cases_mean_raster"]] %>% 
      group_by(grp) %>% 
      summarise(modeled_cases_mean = mean(modeled_cases_mean, na.rm = TRUE), t = "Annualized") %>% 
      select(t, modeled_cases_mean)

  }else{
    output_cache[["modeled_cases_raster"]]$grp = sapply(st_equals(output_cache[["modeled_cases_raster"]]), max)
    output_cache[["modeled_cases_mean_raster"]] <- output_cache[["modeled_cases_raster"]] %>% 
      group_by(grp) %>% 
      summarise(modeled_cases_mean = mean(modeled_cases, na.rm = TRUE), t = "Annualized") %>% 
      select(t, modeled_cases_mean) 
  }
  
}

#' @export
#' @name get_mean_case_raster_in_sf
get_mean_case_raster_in_sf <- cache_fun_results("modeled_cases_mean_raster", get_mean_case_raster_in_sf_no_cache, overwrite = T) 

# general stitch function
#' @export
#' @name stitch_caches
#' @title stitch_caches
#' @description general stitch function that can take different combination function calls 
#' @param output_cache cached output
#' @param name name of the field in the cached output that will be updated 
#' @param input_caches cached configs
#' @param initial_value initial values 
#' @param combination_function specific function than can stitch cached output 
#' @return  cached output 
stitch_caches_no_cache <- function(
  output_cache,
  name,
  input_caches,
  initial_value = list(type = "first", value = NULL),
  combination_function = function(x,y,...) {return(x + y)},
  ...
  ){
  if (initial_value$type == "first") {
    output_cache[[paste0(name, "_config")]] <- input_caches[[1]]
    input_caches <- input_caches[-1]
  } else if (initial_value$type == "fixed") {
    output_cache[[paste0(name, "_config")]] <- initial_value$value
  }
  
  combination_function(output_cache, name, ...) 
  
  return(invisible())
}

#' @export
#' @name stitch_caches
stitch_caches <- cache_fun_results("modeled_cases",stitch_caches_no_cache, overwrite = T) #this may be problematic

#  a specific stitch function that can stack model output from different year together 
#' @export
#' @name stack_case_rate_raster_in_sf
#' @title stack_case_rate_raster_in_sf
#' @description a specific stitch function that can stack model output from different year together 
#' @param output_cache cached output
#' @param name name of the field in the cached output that will be updated 
#' @param cholera_directory 
#' @param config_filename 
#' @param output_dir 
#' @return  cached output 
stack_case_rate_raster_in_sf_no_cache <- function( output_cache, 
                                          name, 
                                          cholera_directory, 
                                          config_filename, 
                                          output_dir) {
  ## Get the file names 
  config_fn <- config_filename
  local_output_dir <- paste0(cholera_directory, "/", output_dir)
  config <- yaml::read_yaml(config_fn)

  file_names <- taxdat::get_filenames(config, cholera_directory)
  file_names <- unlist(lapply(names(file_names), function(file_name){
                              gsub("^.*?/Analysis/data", local_output_dir, file_names[file_name])
                              }))

  ## Get the new sf dataset 
  modeled_cases_rates_raster <- taxdat::get_case_raster(preprocessed_data_filename = file_names["data"],
                                                        covar_data_filename = file_names["covar"],
                                                        model_output_filenames = file_names["stan_output"])
  names(modeled_cases_rates_raster)[ncol(modeled_cases_rates_raster) - 1] <- "modeled_cases"
  names(modeled_cases_rates_raster)[ncol(modeled_cases_rates_raster)] <- "modeled_rates"
  
  ## Stitch together 
  if(is.null(output_cache[[name]])) {
    output_cache[[name]] <- modeled_cases_rates_raster 
  } else {
    modeled_cases_rates_raster$t <- max(output_cache[[name]]$t + 1)
    output_cache[[name]] <- rbind(output_cache[[name]], modeled_cases_rates_raster)
  } 
}

#' @export
#' @name stack_case_rate_raster_in_sf
stack_case_rate_raster_in_sf <- cache_fun_results("modeled_cases",
    stack_case_rate_raster_in_sf_no_cache, overwrite = T, config)

# drop the years with invalid model output 
#' @export
#' @name remove_dropped_years
#' @title remove_dropped_years
#' @description drop the years with invalid model output
#' @param output_cache cached output that is an environment variable
#' @param country country code 
#' @param dropped_years a vector of country codes and corresponding dropped years 
#' @param full_year_vector a vector of years simulated in the model (the default is 2015:2019)
#' @return  cached output, which is a sf dataframe with rate values or case values
remove_dropped_years <- function(output_cache, country, dropped_years = NULL, full_year_vector = 2015:2019){
  if(country %in% names(dropped_years)){

    if("modeled_cases_mean_raster" %in% names(output_cache)){
      if(length(unique(output_cache$modeled_cases_mean_raster$t)) == length(full_year_vector)){
        index_to_remove <- match(dropped_years[[country]], full_year_vector)
        output_cache$modeled_cases_mean_raster <- output_cache$modeled_cases_mean_raster %>%
          filter(!t %in% index_to_remove)
      }else{
        warning(paste0("The dropped years check in country ", country, " did not get passed, be cautions with the output. "))
        return(invisible())
      }

    }else if("modeled_rates_mean_raster" %in% names(output_cache)){
      if(length(unique(output_cache$modeled_rates_mean_raster$t)) == length(full_year_vector)){
        index_to_remove <- match(dropped_years[[country]], full_year_vector)
        output_cache$modeled_rates_mean_raster <- output_cache$modeled_rates_mean_raster %>%
          filter(!t %in% index_to_remove)
      }else{
        warning(paste0("The dropped years check in country ", country, " did not get passed, be cautions with the output. "))
        return(invisible())
      }
    }

  }

}

# check whether a certain config doesn't have any corresponding model output
#' @export
#' @name check_unused_config_by_output
#' @title check_unused_config_by_output
#' @description check whether a certain config doesn't have any corresponding model output, deleting the config is optional 
#' @param config_filename full name of the config to be checked 
#' @param cholera_directory cholera directory 
#' @param output_dir dir where the model output is saved 
#' @param delete_unused_config whether to delete the unused config (default is FALSE)
#' @return  if the current config is wrong, this function will return a correct config name
check_unused_config_by_output <- function(config_filename, 
                                          cholera_directory, 
                                          output_dir, 
                                          delete_unused_config = FALSE){
  ### The first check (for single year run)
  config <- yaml::read_yaml(config_filename)
  supposed_output_name <- taxdat::make_map_name(config)
  if(!any(grepl(supposed_output_name, list.files(paste0(cholera_directory, "/", output_dir)))) 
      & delete_unused_config 
      & file.exists(config_filename)){
    file.remove(config_filename)
  }else if(!any(grepl(supposed_output_name, list.files(paste0(cholera_directory, "/", output_dir))))){
    return(TRUE)
  }

  ### The ultimate check (for multi-year run)
  local_output_dir <- paste0(cholera_directory, "/", output_dir)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  file_names <- unlist(lapply(names(file_names), function(file_name){
                              gsub("^.*?/Analysis/data", local_output_dir, file_names[file_name])
                              }))
  if(file.exists(file_names["stan_output"])){
    return(FALSE)
  }else{
    ## Deal with the old one 
    if(delete_unused_config & file.exists(config_filename)){
      file.remove(config_filename)
    }
    ## Try other configs
    config_dir <- stringr::str_extract(config_filename, ".*/")
    all_configs <- list.files(config_dir, ".yml$")
    for (configs in all_configs){
      full_config_fn <- paste0(config_dir, configs)
      config <- yaml::read_yaml(full_config_fn)
      
      file_names <- taxdat::get_filenames(config, cholera_directory)
      file_names <- unlist(lapply(names(file_names), function(file_name){
                                  gsub("^.*?/Analysis/data", local_output_dir, file_names[file_name])
                                  }))
      if(file.exists(file_names["stan_output"])){
        return(full_config_fn) 
      }else{
        next 
      }
    }
    stop("There is no correct config file for the existing model stan output, please check. ") 
  }
  
}

#  plot the continent 
#' @export
#' @name plot_raster_with_fill_and_continent_shp
#' @title plot_raster_with_fill_and_continent_shp
#' @description plot the continent 
#' @param name name of the field to be plotted in the cached output 
#' @param color_scale_type 
#' @param fill_column name of the variable to be plotted 
#' @param cache cached output to be plotted 
#' @param shapefilename local dir to teh shapefile
#' @param customize_color_scale 
#' @param colors 
#' @return  ggplot object 
plot_raster_with_fill_and_continent_shp <- function(name, color_scale_type, fill_column, cache, shapefilename, 
                                            customize_color_scale = FALSE, colors = NULL) {
    raster_object <- cache[[name]]
    fill_column <- grep(stringr::str_remove(fill_column, "s$"), names(raster_object), value = TRUE)
    shp <- sf::st_boundary(sf::st_read(shapefilename)) 
    
    if(customize_color_scale){
      if(is.null(colors)){colors <- c("#FED98E", "#FE9929", "#D95F0E", "#993404")}
      self_color_scale <- ggplot2::scale_fill_gradientn(colours=colors)
      plot <- ggplot2::ggplot() + ggplot2::geom_sf(data = raster_object, ggplot2::aes_string(fill = fill_column), lwd = 0, color = NA) + 
              self_color_scale + 
              taxdat::map_theme() + ggplot2::facet_wrap(~t) + geom_sf(data = shp, lwd = 0.1) + coord_sf()
    }else{
      plot <- ggplot2::ggplot() + ggplot2::geom_sf(data = raster_object, ggplot2::aes_string(fill = fill_column), lwd = 0, color = NA) + 
              taxdat::color_scale(type = color_scale_type, use_case = "ggplot map") + 
              taxdat::map_theme() + ggplot2::facet_wrap(~t) + geom_sf(data = shp, lwd = 0.1) + coord_sf()
    }
    
    return(plot)
}