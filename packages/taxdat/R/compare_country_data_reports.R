get_config_no_cache <- function(name = "config", cache, config, cholera_directory){
  config <- yaml::read_yaml(paste0(cholera_directory, config))
  return(config)
}
#' @export
#' @name get_config
get_config <- cache_fun_results_new(name = "config", fun = get_config_no_cache, cache = cache, overwrite = T)

stitch_configs <- function(output_cache, input_caches){
  stitch_caches(
                name = "config", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_configs_as_in_lists
  )

}

stack_configs_as_in_lists_no_cache <- function(name = "config", cache, input_cache, ...){
  
  config1 <- cache[[paste0(name, "_initialized")]]
  config2 <- input_cache[[1]][[name]]
  names(config2) <- paste0(names(config2), "_2")
  
  rm("config_initialized", envir = cache)
  return(append(config1, config2))
}
#' @export
#' @name stack_configs_as_in_lists
stack_configs_as_in_lists <- cache_fun_results_new(name = "config", fun = stack_configs_as_in_lists_no_cache, cache = output_cache, overwrite = T)


stitch_gam_input <- function(output_cache, input_caches){
  stitch_caches(
                name = "initial_values_data", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_gam_input_as_in_df
  )

}

stack_gam_input_as_in_df_no_cache <- function(name = "initial_values_data", cache, input_cache, ...){
  df1 <- cache[[paste0(name, "_initialized")]]$gam_fit_input
  df2 <- input_cache[[1]][[name]]$gam_fit_input
  gam_fit_input <- plyr::rbind.fill(df1 %>% mutate(stitch_source = 1), df2 %>% mutate(stitch_source = 2))
  replace_list <- list("gam_fit_input" = gam_fit_input)

  rm("initial_values_data_initialized", envir = cache)
  return(replace_list)
}
#' @export
#' @name stack_gam_input_as_in_df
stack_gam_input_as_in_df <- cache_fun_results_new(name = "initial_values_data", fun = stack_gam_input_as_in_df_no_cache, cache = output_cache, overwrite = T)


stitch_gam_output <- function(output_cache, input_caches){
  stitch_caches(
                name = "gam_output_df", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_gam_output_as_in_df
  )

}

stack_gam_output_as_in_df_no_cache <- function(name = "gam_output_df", cache, input_cache, ...){
  df1 <- cache[[paste0(name, "_initialized")]]
  df2 <- input_cache[[1]][[name]]
  gam_output_df <- plyr::rbind.fill(df1 %>% mutate(stitch_source = 1), df2 %>% mutate(stitch_source = 2))
  
  rm("gam_output_df_initialized", envir = cache)
  return(gam_output_df)
}
#' @export
#' @name stack_gam_output_as_in_df
stack_gam_output_as_in_df <- cache_fun_results_new(name = "gam_output_df", fun = stack_gam_output_as_in_df_no_cache, cache = output_cache, overwrite = T)




a <- plyr::rbind.fill(cache1$initial_values_data$gam_fit_input %>% mutate(source = 1), cache2$initial_values_data$gam_fit_input %>% mutate(source = 2))



a <- rbind(cache1$initial_values_data$gam_fit_input %>% mutate(source = 1), cache2$initial_values_data$gam_fit_input %>% mutate(source = 2))

a <- dplyr::left_join(cache1$initial_values_data$gam_fit_input, 
  (cache2$initial_values_data$gam_fit_input %>% select(ind, sx, sy, y) %>% rename(y2)), by = c("ind", ))








stitch_configs <- function(output_cache, input_caches){
  stitch_caches(output_cache, 
                name = "configs", 
                input_caches, 
                combination_function = stack_configs_as_in_lists, 
                combination_function_name = "stack_configs_as_in_lists")
}

stack_configs_as_in_lists_no_cache <- function(name = "configs",
                                              output_cache,
                                              ...){
  config1 <- yaml::read_yaml(output_cache[[paste0(name, "_input")]][[1]])
  config2 <- yaml::read_yaml(output_cache[[paste0(name, "_input")]][[2]])
  output_cache[[name]]$config1 <- config1
  output_cache[[name]]$config2 <- config2
  rm("configs_input", envir = output_cache)
  
}
#' @export
#' @name stack_configs_as_in_lists
stack_configs_as_in_lists <- cache_fun_results_new(name = "configs", fun = stack_configs_as_in_lists_no_cache, cache = output_cache, overwrite = T)



stitch_GAM_input <- function(output_cache, input_caches, ...){
  stitch_caches(output_cache, 
                name = "initial_values_data", 
                input_caches, 
                combination_function = stack_GAM_input, 
                combination_function_name = "stack_GAM_input", 
                ...)
}

stack_GAM_input_no_cache <- function(name = "initial_values_data", 
                                    output_cache,
                                    cholera_directory){
  config1 <- output_cache[[paste0(name, "_input")]][[1]]
  config2 <- output_cache[[paste0(name, "_input")]][[2]]

  taxdat::get_initial_values(name="initial_values_data1",cache=output_cache,config = config1,cholera_directory = cholera_directory)
  taxdat::get_initial_values(name="initial_values_data2",cache=output_cache,config = config2,cholera_directory = cholera_directory)
  output_cache[[name]]$initial_values_data1 <- output_cache$initial_values_data1
  output_cache[[name]]$initial_values_data2 <- output_cache$initial_values_data2

  # output_cache[[paste0(name, "_input")]] <- NULL
  rm("initial_values_data_input", envir = output_cache)
  rm("initial_values_data1", envir = output_cache)
  rm("initial_values_data2", envir = output_cache)
  
}
#' @export
#' @name stack_GAM_input
stack_GAM_input <- cache_fun_results_new(name = "initial_values_data", fun = stack_GAM_input_no_cache, cache = output_cache, overwrite = T)



stitch_GAM_input_figure <- function(output_cache, input_caches, ...){
  stitch_caches(output_cache, 
                name = "GAM_input_figure", 
                input_caches = NULL, 
                combination_function = stack_GAM_input_figure, 
                combination_function_name = "stack_GAM_input_figure", 
                ...)
}

stack_GAM_input_figure_no_cache <- function(name = "GAM_input_figure", 
                                    output_cache,
                                    ...){
  fig_cases1 <- plot_gam_fit_input_cases(name="initial_values_data1", cache = output_cache$initial_values_data)
  fig_cases2 <- plot_gam_fit_input_cases(name="initial_values_data2", cache = output_cache$initial_values_data)
  fig_rates1 <- plot_gam_fit_input_rates(name="initial_values_data1", cache = output_cache$initial_values_data)
  fig_rates2 <- plot_gam_fit_input_rates(name="initial_values_data2", cache = output_cache$initial_values_data)

  output_cache[[name]]$fig_cases1 <- fig_cases1
  output_cache[[name]]$fig_cases2 <- fig_cases2
  output_cache[[name]]$fig_rates1 <- fig_rates1
  output_cache[[name]]$fig_rates2 <- fig_rates2

  rm(fig_cases1, fig_cases2, fig_rates1, fig_rates2)  
}
#' @export
#' @name stack_GAM_input_figure
stack_GAM_input_figure <- cache_fun_results_new(name = "GAM_input_figure", fun = stack_GAM_input_figure_no_cache, cache = output_cache, overwrite = T)



stitch_GAM_output <- function(output_cache, input_caches, ...){
  stitch_caches(output_cache, 
                name = "gam_output_df", 
                input_caches, 
                combination_function = stack_GAM_output, 
                combination_function_name = "stack_GAM_output", 
                ...)
}

stack_GAM_output_no_cache <- function(name = "gam_output_df", 
                                    output_cache,
                                    cholera_directory){
  config1 <- output_cache[[paste0(name, "_input")]][[1]]
  config2 <- output_cache[[paste0(name, "_input")]][[2]]

  output_cache[[name]]$gam_output_df1 <- get_gam_values_no_cache(config = config1, cholera_directory = cholera_directory, cache = output_cache, name = "initial_values_data1")
  output_cache[[name]]$gam_output_df2 <- get_gam_values_no_cache(config = config2, cholera_directory = cholera_directory, cache = output_cache, name = "initial_values_data2")

  rm("gam_output_df_input", envir = output_cache)
  if(!"stan_input1" %in% names(output_cache$stan_input) & !"stan_input2" %in% names(output_cache$stan_input)){
    rm("stan_input", envir = output_cache)
  }
}
#' @export
#' @name stack_GAM_output
stack_GAM_output <- cache_fun_results_new(name = "gam_output_df", fun = stack_GAM_output_no_cache, cache = output_cache, overwrite = T)


stitch_GAM_output_figure <- function(output_cache, input_caches, ...){
  stitch_caches(output_cache, 
                name = "GAM_output_figure", 
                input_caches = NULL, 
                combination_function = stack_GAM_output_figure, 
                combination_function_name = "stack_GAM_output_figure", 
                ...)
}

stack_GAM_output_figure_no_cache <- function(name = "GAM_output_figure", 
                                    output_cache,
                                    ...){
  fig_cases1 <- plot_gam_fit_output_cases(name="gam_output_df1", cache = output_cache$gam_output_df)
  fig_cases2 <- plot_gam_fit_output_cases(name="gam_output_df2", cache = output_cache$gam_output_df)
  fig_rates1 <- plot_gam_fit_output_rates(name="gam_output_df1", cache = output_cache$gam_output_df)
  fig_rates2 <- plot_gam_fit_output_rates(name="gam_output_df2", cache = output_cache$gam_output_df)

  output_cache[[name]]$fig_cases1 <- fig_cases1
  output_cache[[name]]$fig_cases2 <- fig_cases2
  output_cache[[name]]$fig_rates1 <- fig_rates1
  output_cache[[name]]$fig_rates2 <- fig_rates2

  rm(fig_cases1, fig_cases2, fig_rates1, fig_rates2)  
}
#' @export
#' @name stack_GAM_output_figure
stack_GAM_output_figure <- cache_fun_results_new(name = "GAM_output_figure", fun = stack_GAM_output_figure_no_cache, cache = output_cache, overwrite = T)


stitch_used_data_table <- function(output_cache, input_caches, ...){
  stitch_caches(output_cache, 
                name = "used_data_table", 
                input_caches, 
                combination_function = stack_used_data_table, 
                combination_function_name = "stack_used_data_table", 
                ...)
}

stack_used_data_table_no_cache <- function(name = "used_data_table", 
                                    output_cache,
                                    cholera_directory){
  config1 <- output_cache[[paste0(name, "_input")]][[1]]
  config2 <- output_cache[[paste0(name, "_input")]][[2]]

  used_table1 <- plot_ObservationSummary_table(config = config1, cache = output_cache, cholera_directory = cholera_directory)
  if(!"stan_input1" %in% names(output_cache$stan_input) & !"stan_input2" %in% names(output_cache$stan_input)){
    rm("stan_input", envir = output_cache)
  }
  if(!"sf_cases_resized1" %in% names(output_cache$sf_cases_resized) & !"sf_cases_resized2" %in% names(output_cache$sf_cases_resized)){
    rm("sf_cases_resized", envir = output_cache)
  }

  used_table2 <- plot_ObservationSummary_table(config = config2, cache = output_cache, cholera_directory = cholera_directory)
  if(!"stan_input1" %in% names(output_cache$stan_input) & !"stan_input2" %in% names(output_cache$stan_input)){
    rm("stan_input", envir = output_cache)
  }
  if(!"sf_cases_resized1" %in% names(output_cache$sf_cases_resized) & !"sf_cases_resized2" %in% names(output_cache$sf_cases_resized)){
    rm("sf_cases_resized", envir = output_cache)
  }

  output_cache[[name]]$used_data_table1 <- used_table1
  output_cache[[name]]$used_data_table2 <- used_table2

  rm("used_data_table_input", envir = output_cache)
  rm(config1, config2, used_table1, used_table2)  
}
#' @export
#' @name stack_GAM_output_figure
stack_used_data_table <- cache_fun_results_new(name = "used_data_table", fun = stack_used_data_table_no_cache, cache = output_cache, overwrite = T)



used_table <- plot_ObservationSummary_table(config = params$config1, cache = comparison_cache, cholera_directory = params$cholera_directory)
dropped_table <- plot_DroppedData_table(config = params$config1, cache = comparison_cache, cholera_directory = params$cholera_directory)

# get_gam_values_no_cache(config = params$config1, cholera_directory = params$cholera_directory, cache = comparison_cache, name = "initial_values_data1")
# stitch_GAMs(output_cache = comparison_cache, input_caches = list(list(params$config1, params$config2)))
# taxdat::get_initial_values(name="initial_values_data",cache=comparison_cache,config = params$config1,cholera_directory = params$cholera_directory)
# taxdat::get_initial_values(name="initial_values_data1",cache=comparison_cache,config = params$config1,cholera_directory = params$cholera_directory)
# taxdat::get_initial_values(name="initial_values_data2",cache=comparison_cache,config = params$config1,cholera_directory = params$cholera_directory)



# plot_stitched_figures_side_by_side <- function(name, cache, name2, name3 = NULL){
#   # name3 check
#   if(!is.null(name3)){stop("Not developed yet. ")}

#   # testing
#   fig_vector <- gridExtra::grid.arrange(cache[[name]][[paste0(name2[1], "1")]], cache[[name]][[paste0(name2[2], "2")]], ncol = 2)
#   return(fig_vector)

#   # plot two figs side by side 
#   fig_vector <- c()
#   for(names in name2){
#     fig_vector <- c(fig_vector, gridExtra::grid.arrange(cache[[name]][[paste0(names, "1")]], cache[[name]][[paste0(names, "2")]], ncol = 2))
#   }
  
#   return(fig_vector)
# }
