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
get_covar_cube_no_cache <- function(config, cache, cholera_directory, self_defined_output_dir = NULL) {
    config <- yaml::read_yaml(config_filename)
    file_names <- taxdat::get_filenames(config, cholera_directory)
    print(paste0("The self_defined_output_dir is ", self_defined_output_dir))
    if(!is.null(self_defined_output_dir)){
        local_output_dir <- paste0(cholera_directory, "/", self_defined_output_dir)
        file_names <- unlist(lapply(names(file_names), function(file_name){
                                    gsub("^.*?/Analysis/data", local_output_dir, file_names[file_name])
                                    }))
    }
    
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
get_raster_object_no_cache <- function(config, cache, cholera_directory, ...) {
    get_covar_cube(config, cache, cholera_directory, ...) 
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
get_model_rand_no_cache <- function(config, cache, cholera_directory, self_defined_output_dir = NULL) {
    config <- yaml::read_yaml(config_filename)
    file_names <- taxdat::get_filenames(config, cholera_directory)
    print(paste0("The self_defined_output_dir is ", self_defined_output_dir))
    if(!is.null(self_defined_output_dir)){
        local_output_dir <- paste0(cholera_directory, "/", self_defined_output_dir)
        file_names <- unlist(lapply(names(file_names), function(file_name){
                                    gsub("^.*?/Analysis/data", local_output_dir, file_names[file_name])
                                    }))
    }

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
get_modeled_cases_no_cache <- function(config, cache, cholera_directory, ...) {
    get_model_rand(config, cache, cholera_directory, ...)
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
get_modeled_rates_no_cache <- function(config, cache, cholera_directory, ...) {
    get_model_rand(config, cache, cholera_directory, ...) 
    modeled_rates <- exp(MCMCvis::MCMCchains(cache[["model.rand"]], params = "log_lambda"))
    return(modeled_rates)
}
# cache the results
get_modeled_rates <- cache_fun_results("modeled_rates", get_modeled_rates_no_cache,
    overwrite = T, config = config)
# get_modeled_rates(config=config,cache)


# get new aggregated data: get_modeled_cases_mean
get_modeled_cases_mean_no_cache <- function(config, cache, cholera_directory, ...) {
    get_modeled_cases(config, cache, cholera_directory, ...)
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
get_modeled_rates_mean_no_cache <- function(config, cache, cholera_directory, ...) {
    get_modeled_rates(config, cache, cholera_directory, ...)
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
merge_modeled_cases_mean_into_raster_no_cache <- function(config, cache, cholera_directory, ...) {
    get_raster_object(config, cache, cholera_directory, ...)
    modeled_cases_mean_raster <- cache[["sf_grid"]]
    get_modeled_cases_mean(config, cache, cholera_directory, ...)
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
merge_modeled_rates_mean_into_raster_no_cache <- function(config, cache, cholera_directory, ...) {
    get_raster_object(config, cache, cholera_directory, ...)
    grid_rates_mean_raster <- cache[["sf_grid"]]
    get_modeled_rates_mean(config, cache, cholera_directory, ...)
    modeled_rates_mean <- cache[["modeled_rates_mean"]]
    grid_rates_mean_raster$modeled_rates_mean <- modeled_rates_mean
    return(grid_rates_mean_raster)
}
merge_modeled_rates_mean_into_raster <- cache_fun_results("modeled_rates_mean_raster",
    merge_modeled_rates_mean_into_raster_no_cache, overwrite = T, config)

# stitch rate rasters from different single years together 
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
stitch_rate_raster_stack <- cache_fun_results("modeled_rates_raster",
    stitch_rate_raster_stack_no_cache, overwrite = T, config_filename) 

# get the mean of the rates
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
get_mean_rate_raster_in_sf <- cache_fun_results("modeled_cases_mean_raster", get_mean_rate_raster_in_sf_no_cache, overwrite = T) 

# stitch case rasters from different single years together 
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
stitch_case_raster_stack <- cache_fun_results("modeled_cases_raster",
    stitch_case_raster_stack_no_cache, overwrite = T, config_filename)

# get the mean of the cases 
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
get_mean_case_raster_in_sf <- cache_fun_results("modeled_cases_mean_raster", get_mean_case_raster_in_sf_no_cache, overwrite = T) 

# general stitch function
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
stitch_caches <- cache_fun_results(name, stitch_caches_no_cache, overwrite = T) #this may be problematic

#  a specific stitch function that can stack model output from different year together 
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
stack_case_rate_raster_in_sf <- cache_fun_results(name,
    stack_case_rate_raster_in_sf_no_cache, overwrite = T, config_filename)

# drop the years with invalid model output 
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
