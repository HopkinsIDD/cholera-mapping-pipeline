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


stitch_used_data_table <- function(output_cache, input_caches){
  stitch_caches(
                name = "used_data_table", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = intersperse_used_data_table_as_in_df
  )

}

intersperse_used_data_table_as_in_df_no_cache <- function(name = "used_data_table", cache, input_cache, ...){
  df1 <- cache[[paste0(name, "_initialized")]]
  df2 <- input_cache[[1]][[name]]
  intersperse_df <- rbind(df1 %>% mutate(stitch_source = 1), df2 %>% mutate(stitch_source = 2)) %>% arrange(year) 
  intersperse_df_aesthetic <- intersperse_df %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
    # dplyr::rename(year=year, `Observations`=n_obs, `Suspected cases`=n_cases, `Location periods`=n_lp, `Observation collections`=n_OCs)%>%
    kableExtra::kable(col.names = c("year", "# observations", "# suspected cases", "# location periods", "# observation collections", "table source")) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = T) %>%
    kableExtra::row_spec(nrow(intersperse_df), bold = T)
  
  rm("used_data_table_initialized", envir = cache)
  return(intersperse_df_aesthetic)
}
#' @export
#' @name intersperse_used_data_table_as_in_df
intersperse_used_data_table_as_in_df <- cache_fun_results_new(name = "used_data_table", fun = intersperse_used_data_table_as_in_df_no_cache, cache = output_cache, overwrite = T)


stitch_dropped_data_table <- function(output_cache, input_caches){
  stitch_caches(
                name = "dropped_data_table", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = intersperse_dropped_data_table_as_in_df
  )

}

intersperse_dropped_data_table_as_in_df_no_cache <- function(name = "dropped_data_table", cache, input_cache, ...){
  df1 <- cache[[paste0(name, "_initialized")]]
  df2 <- input_cache[[1]][[name]]
  intersperse_df <- rbind(df1 %>% mutate(stitch_source = 1), df2 %>% mutate(stitch_source = 2)) %>% arrange(year) 
  intersperse_df_aesthetic <- intersperse_df %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
    # dplyr::rename(year=year, `Observations`=n_obs, `Suspected cases`=n_cases, `Location periods`=n_lp, `Observation collections`=n_OCs)%>%
    kableExtra::kable(col.names = c("year", "# dropped observations", "# dropped suspected cases", "# dropped location periods",  
      "dropped location periods", "# dropped  observation collections",  "dropped observation collections", "table source")) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::row_spec(nrow(intersperse_df), bold = T)
  
  rm("dropped_data_table_initialized", envir = cache)
  return(intersperse_df_aesthetic)
}
#' @export
#' @name intersperse_dropped_data_table_as_in_df
intersperse_dropped_data_table_as_in_df <- cache_fun_results_new(name = "dropped_data_table", fun = intersperse_dropped_data_table_as_in_df_no_cache, cache = output_cache, overwrite = T)


stitch_covar_cube <- function(output_cache, input_caches){
  stitch_caches(
                name = "covar_cube", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = overlay_covar_cube_as_in_array
  )

}

overlay_covar_cube_as_in_array_no_cache <- function(name = "covar_cube", cache, input_cache, ...){
  array1 <- cache[[paste0(name, "_initialized")]]
  array2 <- input_cache[[1]][[name]]
  if(any(dim(array1) != dim(array2))){
    if(dim(array1)[1] != dim(array2)[1] | dim(array1)[2] != dim(array2)[2]){
      stop("The covar cube from two runs differ in terms of more than just the number of covariates. ")
    }

    if(dim(array1)[3] < dim(array2)[3]){
      NA_array <- array(NA, dim = c( dim(array2)[1], dim(array2)[2], dim(array2)[3] - dim(array1)[3] ))
      array1 <- abind(array1, NA_array, along = 3)
    }else{
      NA_array <- array(NA, dim = c( dim(array1)[1], dim(array1)[2], dim(array1)[3] - dim(array2)[3] ))
      array2 <- abind(array2, NA_array, along = 3)
    }

  }
  array <- abind(array1, array2, along = 1)
  
  rm("covar_cube_initialized", envir = cache)
  return(array)
}
#' @export
#' @name overlay_covar_cube_as_in_array
overlay_covar_cube_as_in_array <- cache_fun_results_new(name = "covar_cube", fun = overlay_covar_cube_as_in_array_no_cache, cache = output_cache, overwrite = T)


stitch_sf_grid <- function(output_cache, input_caches){
  stitch_caches(
                name = "sf_grid", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_sf_grid_as_in_sf
  )

}

stack_sf_grid_as_in_sf_no_cache <- function(name = "sf_grid", cache, input_cache, ...){
  sf1 <- cache[[paste0(name, "_initialized")]]
  sf2 <- input_cache[[1]][[name]]
  stacked_sf <- rbind(sf1 %>% mutate(stitch_source = 1), sf2 %>% mutate(stitch_source = 2)) 
  
  rm("sf_grid_initialized", envir = cache)
  return(stacked_sf)
}
#' @export
#' @name stack_sf_grid_as_in_sf
stack_sf_grid_as_in_sf <- cache_fun_results_new(name = "sf_grid", fun = stack_sf_grid_as_in_sf_no_cache, cache = output_cache, overwrite = T)


stitch_disaggregated_case_sf <- function(output_cache, input_caches){
  stitch_caches(
                name = "disaggregated_case_sf", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_disaggregated_case_as_in_sf
  )

}

stack_disaggregated_case_as_in_sf_no_cache <- function(name = "disaggregated_case_sf", cache, input_cache, ...){
  sf1 <- cache[[paste0(name, "_initialized")]]
  sf2 <- input_cache[[1]][[name]]

  stacked_sf <- rbind(sf1 %>% mutate(stitch_source = "1"), sf2 %>% mutate(stitch_source = "2"))
  sf2_nogeo <- sf2 %>% st_drop_geometry() %>% rename(value2 = value)
  diff_sf <- left_join(sf1, sf2_nogeo, by = c("t", "id")) %>% mutate(value = value - value2, stitch_source = "diff") %>% dplyr::select(- value2)
  stacked_sf <- rbind(stacked_sf, diff_sf)
  rm(diff_sf)

  rm("disaggregated_case_sf_initialized", envir = cache)
  return(stacked_sf)
}
#' @export
#' @name stack_disaggregated_case_as_in_sf
stack_disaggregated_case_as_in_sf <- cache_fun_results_new(name = "disaggregated_case_sf", fun = stack_disaggregated_case_as_in_sf_no_cache, cache = output_cache, overwrite = T)


#' @export
#' @name plot_disaggregated_modeled_cases_time_varying_stitched
#' @title plot_disaggregated_modeled_cases_time_varying_stitched
#' @description add
#' @param disaggregated_case_sf disaggregated case raster object
#' @param country_iso the iso code of the country
#' @param render default is TRUE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @param diff_only whether or not plot the diff col only 
#' @return ggplot object with modeled cases map
plot_disaggregated_modeled_cases_time_varying_stitched <- function( disaggregated_case_sf,
                                                                    country_iso, 
                                                                    render = T,
                                                                    plot_file = NULL,
                                                                    width = NULL,
                                                                    height = NULL, 
                                                                    diff_only = FALSE){

  if(country_iso == "ZNZ"){
    boundary_sf <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% 
                                                    c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
  }else{
    boundary_sf <- rgeoboundaries::gb_adm0(country_iso)
  }

  plt <- ggplot2::ggplot()
  if(!diff_only){
    plt <-   ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = disaggregated_case_sf %>% filter(stitch_source != "diff"),
        ggplot2::aes(fill = value),color=NA,size=0.00001)+
      taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = TRUE)+
      ggplot2::geom_sf(data=boundary_sf,fill=NA,color="black",size=0.05)+
      ggplot2::labs(fill="Incidence\n [cases/year]")+
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(legend.text =  ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))+
      ggplot2::facet_wrap(t ~ stitch_source, ncol = 2) 
  }else{
    plt <-   ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = disaggregated_case_sf %>% filter(stitch_source == "diff"),
        ggplot2::aes(fill = value),color=NA,size=0.00001)+
      taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
      ggplot2::geom_sf(data=boundary_sf,fill=NA,color="black",size=0.05)+
      ggplot2::labs(fill="Incidence\n [cases/year]")+
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(legend.text =  ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))+
      ggplot2::facet_wrap(~ t) 
  }

  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  if(render) {
    plt
  }
}












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
