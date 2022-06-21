#' @name get_config_no_cache
#' @title get_config_no_cache
#' @description get_config_no_cache
#' @param name name of the cache
#' @param cache cache to save the object
#' @param config relative path to config
#' @param cholera_directory absolute path to the cholera project directory
#' @return the config file
get_config_no_cache <- function(name = "config", cache, config, cholera_directory){
  config <- yaml::read_yaml(paste0(cholera_directory, config))
  return(config)
}
#' @export
#' @name get_config
get_config <- cache_fun_results_new(name = "config", fun = get_config_no_cache, cache = cache, overwrite = T)

#' @export
#' @name stitch_configs
#' @title stitch_configs
#' @description stitch configs together
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_configs <- function(output_cache, input_caches){
  stitch_caches(
                name = "config", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_configs_as_in_lists
  )

}

#' @name stack_configs_as_in_lists_no_cache
#' @title stack_configs_as_in_lists_no_cache
#' @description stack_configs_as_in_lists_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
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

#' @export
#' @name stitch_gam_input
#' @title stitch_gam_input
#' @description stitch_gam_input
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_gam_input <- function(output_cache, input_caches){
  stitch_caches(
                name = "initial_values_data", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_gam_input_as_in_df
  )

}

#' @name stack_gam_input_as_in_df_no_cache
#' @title stack_gam_input_as_in_df_no_cache
#' @description stack_gam_input_as_in_df_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
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

#' @export
#' @name stitch_gam_output
#' @title stitch_gam_output
#' @description stitch_gam_output
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_gam_output <- function(output_cache, input_caches){
  stitch_caches(
                name = "gam_output_df", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_gam_output_as_in_df
  )

}

#' @name stack_gam_output_as_in_df_no_cache
#' @title stack_gam_output_as_in_df_no_cache
#' @description stack_gam_output_as_in_df_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
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

#' @export
#' @name stitch_used_data_table
#' @title stitch_used_data_table
#' @description stitch_used_data_table
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_used_data_table <- function(output_cache, input_caches){
  stitch_caches(
                name = "used_data_table", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = intersperse_used_data_table_as_in_df
  )

}

#' @name intersperse_used_data_table_as_in_df_no_cache
#' @title intersperse_used_data_table_as_in_df_no_cache
#' @description intersperse_used_data_table_as_in_df_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
intersperse_used_data_table_as_in_df_no_cache <- function(name = "used_data_table", cache, input_cache, ...){
  df1 <- cache[[paste0(name, "_initialized")]]
  df2 <- input_cache[[1]][[name]]
  intersperse_df <- rbind(df1 %>% mutate(stitch_source = 1), df2 %>% mutate(stitch_source = 2)) %>% arrange(year) 
  intersperse_df_aesthetic <- intersperse_df %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
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

#' @export
#' @name stitch_dropped_data_table
#' @title stitch_dropped_data_table
#' @description stitch_dropped_data_table
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_dropped_data_table <- function(output_cache, input_caches){
  stitch_caches(
                name = "dropped_data_table", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = intersperse_dropped_data_table_as_in_df
  )

}

#' @name intersperse_dropped_data_table_as_in_df_no_cache
#' @title intersperse_dropped_data_table_as_in_df_no_cache
#' @description intersperse_dropped_data_table_as_in_df_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
intersperse_dropped_data_table_as_in_df_no_cache <- function(name = "dropped_data_table", cache, input_cache, ...){
  df1 <- cache[[paste0(name, "_initialized")]]
  df2 <- input_cache[[1]][[name]]
  intersperse_df <- rbind(df1 %>% mutate(stitch_source = 1), df2 %>% mutate(stitch_source = 2)) %>% arrange(year) 
  intersperse_df_aesthetic <- intersperse_df %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
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

#' @export
#' @name stitch_covar_cube
#' @title stitch_covar_cube
#' @description stitch_covar_cube
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_covar_cube <- function(output_cache, input_caches){
  stitch_caches(
                name = "covar_cube", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = overlay_covar_cube_as_in_array
  )

}

#' @name overlay_covar_cube_as_in_array_no_cache
#' @title overlay_covar_cube_as_in_array_no_cache
#' @description overlay_covar_cube_as_in_array_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
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

#' @export
#' @name stitch_sf_grid
#' @title stitch_sf_grid
#' @description stitch_sf_grid
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_sf_grid <- function(output_cache, input_caches){
  stitch_caches(
                name = "sf_grid", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_sf_grid_as_in_sf
  )

}

#' @name stack_sf_grid_as_in_sf_no_cache
#' @title stack_sf_grid_as_in_sf_no_cache
#' @description stack_sf_grid_as_in_sf_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
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

#' @export
#' @name stitch_disaggregated_case_sf
#' @title stitch_disaggregated_case_sf
#' @description stitch_disaggregated_case_sf
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_disaggregated_case_sf <- function(output_cache, input_caches){
  stitch_caches(
                name = "disaggregated_case_sf", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = stack_disaggregated_case_as_in_sf
  )

}

#' @name stack_disaggregated_case_as_in_sf_no_cache
#' @title stack_disaggregated_case_as_in_sf_no_cache
#' @description stack_disaggregated_case_as_in_sf_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
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
#' @description plot modeled case rasters the stitched version 
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
    plt <- ggplot2::ggplot() +
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
    plt <- ggplot2::ggplot() +
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

#' @export
#' @name plot_modeled_cases_scatter_plot_by_grids_stitched
#' @title plot_modeled_cases_scatter_plot_by_grids_stitched
#' @description plot_modeled_cases_scatter_plot_by_grids_stitched
#' @param cache output cache that has the needed data
#' @return the ggplot object
plot_modeled_cases_scatter_plot_by_grids_stitched <- function(cache){
  df1 <- cache$disaggregated_case_sf %>% 
    sf::st_drop_geometry() %>% 
    filter(stitch_source == "1") %>% 
    rename(value1 = value) %>% 
    dplyr::select(- stitch_source)

  df2 <- cache$disaggregated_case_sf %>% 
    sf::st_drop_geometry() %>% 
    filter(stitch_source == "2") %>% 
    rename(value2 = value) %>% 
    dplyr::select(- stitch_source)

  clean_df <- left_join(df1, df2, by = c("t", "id"))
  rm(df1, df2)

  plt <- clean_df %>%
    ggplot(aes(x = value1, y = value2)) + 
    geom_point(
        color="#69b3a2",
        alpha=0.5,
        size=1) +
    theme_minimal() + 
    coord_fixed(ratio = 1) + 
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap( ~ t)

  return(plt)
}

#' @export
#' @name plot_modeled_cases_scatter_plot_by_admin_stitched
#' @title plot_modeled_cases_scatter_plot_by_admin_stitched
#' @description plot_modeled_cases_scatter_plot_by_admin_stitched
#' @param cache output cache that has the needed data
#' @param country_iso country code
#' @param admin_level at which level the scatter plot will be based on
#' @param district_name whether to put district names as labels on the scatter plot
#' @param label_threshold label the point with its district name if the relative discrepancy goes beyound a certain threshold
#' @return the ggplot object
plot_modeled_cases_scatter_plot_by_admin_stitched <- function(cache, country_iso, admin_level = 1, district_name = FALSE, label_threshold = 0.1){
  df1 <- cache$disaggregated_case_sf %>% 
    filter(stitch_source == "1") %>% 
    rename(value1 = value) %>% 
    dplyr::select(- stitch_source)

  df2 <- cache$disaggregated_case_sf %>% 
    filter(stitch_source == "2") %>% 
    rename(value2 = value) %>% 
    dplyr::select(- stitch_source)
  
  shapefiles <- eval(parse(text = paste0("gb_adm", admin_level, "(country_iso)")))
  for(i in 1:nrow(shapefiles)){
    district_shapefile <- shapefiles[i, ]
    district_idx_vector <- sf::st_intersects(sf::st_centroid(df1), district_shapefile, sparse = FALSE)
    df1_dist <- df1[district_idx_vector, ] %>% sf::st_drop_geometry() 
    df2_dist <- df2[district_idx_vector, ] %>% sf::st_drop_geometry() 

    if(!exists("dist_df")){
      dist_df <- left_join(df1_dist, df2_dist, by = c("t", "id")) %>% mutate(district = district_shapefile$shapeName)
    }else{
      dist_df_tmp <- left_join(df1_dist, df2_dist, by = c("t", "id")) %>% mutate(district = district_shapefile$shapeName)
      dist_df <- rbind(dist_df, dist_df_tmp)
      rm(dist_df_tmp)
    }
  }
  
  dist_df <- dist_df %>% 
    group_by(t, district) %>%
    summarise(value1 = sum(value1, na.rm = TRUE), value2 = sum(value2, na.rm = TRUE))
  rm(df1, df1_dist, df2, df2_dist)

  plt <- dist_df %>%
    ggplot(aes(x = value1, y = value2)) + 
    geom_point(
        color="#69b3a2",
        alpha=0.5,
        size=1) +
    {if(district_name) ggrepel::geom_text_repel(aes(label = ifelse(abs(value1-value2) >= label_threshold * value1, as.character(district), '')), 
                                                max.overlaps = Inf, size = 2)} + 
    theme_minimal() + 
    coord_fixed(ratio = 1) + 
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap( ~ t)

  return(plt)
}

#' @export
#' @name stitch_data_fidelity
#' @title stitch_data_fidelity
#' @description stitch_data_fidelity
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_data_fidelity <- function(output_cache, input_caches){
  stitch_caches(
                name = "data_fidelity", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = merge_data_fidelity_as_in_df
  )

}

#' @name merge_data_fidelity_as_in_df_no_cache
#' @title merge_data_fidelity_as_in_df_no_cache
#' @description merge_data_fidelity_as_in_df_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
merge_data_fidelity_as_in_df_no_cache <- function(name = "data_fidelity", cache, input_cache, ...){
  df1 <- cache[[paste0(name, "_initialized")]] %>% rename(modeled_cases1 = `modeled cases`)
  df2 <- input_cache[[1]][[name]] %>% rename(modeled_cases2 = `modeled cases`) %>% dplyr::select(chain, variable, modeled_cases2)

  merged_df <- left_join(df1, df2, by = c("chain", "variable"))

  rm("data_fidelity_initialized", envir = cache)
  return(merged_df)
}
#' @export
#' @name merge_data_fidelity_as_in_df
merge_data_fidelity_as_in_df <- cache_fun_results_new(name = "data_fidelity", fun = merge_data_fidelity_as_in_df_no_cache, cache = output_cache, overwrite = T)

#' @export
#' @name plot_modeled_cases_scatter_plot_by_oc_uid_stitched
#' @title plot_modeled_cases_scatter_plot_by_oc_uid_stitched
#' @description plot_modeled_cases_scatter_plot_by_oc_uid_stitched
#' @param cache output cache that has the needed data
#' @return the ggplot object
plot_modeled_cases_scatter_plot_by_oc_uid_stitched <- function(cache){
  plt <- cache$data_fidelity %>%
    ggplot(aes(x = modeled_cases1, y = modeled_cases2, color = oc_uid)) + 
    geom_point(size=1) +
    theme_minimal() + 
    coord_fixed(ratio = 1) + 
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap( ~ chain)

  return(plt)
}

#' @export
#' @name stitch_admin_case_summary_table
#' @title stitch_admin_case_summary_table
#' @description stitch_admin_case_summary_table
#' @param output_cache output cache that will save the stitched object
#' @param input_caches two separate cache that have the input objects
#' @return the stitched object saved in the output cache
stitch_admin_case_summary_table <- function(output_cache, input_caches){
  stitch_caches(
                name = "admin_case_summary_table", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = merge_admin_case_summary_table_as_in_df
  )

}

#' @name merge_admin_case_summary_table_as_in_df_no_cache
#' @title merge_admin_case_summary_table_as_in_df_no_cache
#' @description merge_admin_case_summary_table_as_in_df_no_cache
#' @param name name of the object saved in both output cache and input cache
#' @param cache output cache that will save the stitched object
#' @param input_cache cache that has the input object
#' @return the stitched object
merge_admin_case_summary_table_as_in_df_no_cache <- function(name = "admin_case_summary_table", cache, input_cache, ...){
  df1 <- cache[[paste0(name, "_initialized")]] 
  df2 <- input_cache[[1]][[name]] 
  merged_df <- left_join(df1, df2, by = c("adminlevel"))

  for(name in names(df1)[names(df1) != "adminlevel"]){
    merged_df[[name]] <- merged_df[[paste0(name, ".x")]] - merged_df[[paste0(name, ".y")]]
  }
  merged_df <- merged_df[, (names(merged_df) %in% names(df1))]

  stacked_df <- rbind(df1 %>% mutate(stitch_source = "1"), df2 %>% mutate(stitch_source = "2"))
  df <- rbind(stacked_df, merged_df %>% mutate(stitch_source = "diff"))

  rm(df1, df2, merged_df, stacked_df)
  return(df)
}
#' @export
#' @name merge_admin_case_summary_table_as_in_df
merge_admin_case_summary_table_as_in_df <- cache_fun_results_new(name = "admin_case_summary_table", fun = merge_admin_case_summary_table_as_in_df_no_cache, cache = output_cache, overwrite = T)

#' @export
#' @name plot_cases_by_admin_table_stitched
#' @title plot_cases_by_admin_table_stitched
#' @description plot_cases_by_admin_table_stitched
#' @param cache output cache that has the needed data
#' @param type if it's intersperse, then tables from different sources will be interweaved for comparison; if it's difference, then only the difference between two runs will be shown 
#' @return the kable object
plot_cases_by_admin_table_stitched <- function(cache, type){
  if(type == "intersperse"){
    admin_case_table <- cache$admin_case_summary_table %>%
      filter(stitch_source != "diff") %>% 
      dplyr::arrange(adminlevel, stitch_source) 
    admin_case_table %>% 
      dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
      kableExtra::kable(col.names = c('Admin Level', names(cache$admin_case_summary_table)[!names(cache$admin_case_summary_table) %in% c("adminlevel", "mean_across_years", "stitch_source")], 
        'Mean across Years', "Stitch Source")) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
      kableExtra::kable_paper(full_width = F) %>%
      kableExtra::row_spec(nrow(admin_case_table), bold = T)
  }else if(type == "difference"){
    admin_case_table <- cache$admin_case_summary_table %>%
      filter(stitch_source == "diff") %>% 
      dplyr::select(- stitch_source) 
    admin_case_table %>% 
      dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
      kableExtra::kable(col.names = c('Admin Level', names(cache$admin_case_summary_table)[!names(cache$admin_case_summary_table) %in% c("adminlevel", "mean_across_years", "stitch_source")], 
        'Mean across Years')) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
      kableExtra::kable_paper(full_width = F) %>%
      kableExtra::row_spec(nrow(admin_case_table), bold = T)
  }else{
    stop("Wrong type input for the plot_cases_by_admin_table_stitched function. ")
  }
}

#' @export
#' @name plot_AIC_BIC_comparison_table
#' @title plot_AIC_BIC_comparison_table
#' @description plot_AIC_BIC_comparison_table
#' @param cache1 cache from run 1
#' @param cache2 cache from run 2
#' @return the kable object
plot_AIC_BIC_comparison_table <- function(cache1, cache2){
  AIC1 <- AIC(cache1$initial_values_data$gam_fit_output)
  BIC1 <- BIC(cache1$initial_values_data$gam_fit_output)
  AIC2 <- AIC(cache2$initial_values_data$gam_fit_output)
  BIC2 <- BIC(cache2$initial_values_data$gam_fit_output)
  table <- tibble::tibble(source = c(1, 2), AIC = c(AIC1, AIC2), BIC = c(BIC1, BIC2))
  table %>% 
    kableExtra::kable(col.names = c("Source", "AIC", "BIC")) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::row_spec(0, bold = T)
}

#' @export
#' @name plot_config_comparison_table
#' @title plot_config_comparison_table
#' @description plot_config_comparison_table
#' @param configs the list that has the config pathways from two runs 
#' @param cache cache that has the config content
#' @return the kable object
plot_config_comparison_table <- function(configs, cache){
  for(name in names(cache$config)){
    if(is.null(cache$config[[name]])){
      cache$config[[name]] <- "NULL"
    }
    if(all(is.na(cache$config[[name]]))){
      cache$config[[name]] <- "NA"
    }
  }

  names <- names(cache$config)[!grepl("_2", names(cache$config))]
  config_table <- tibble(item = "Config File Name", source1 = configs[[1]], source2 = configs[[2]], diff = " ")

  for(name in names){
    config_table <- config_table %>% 
      add_row(item = name, source1 = as.character(cache$config[[name]]), source2 = as.character(cache$config[[paste0(name, "_2")]]))
  }
  config_table <- config_table %>% 
    mutate(diff = ifelse(source1 == source2, "", "√"))

  config_table %>% 
    kableExtra::kable(col.names = c("Config Item", "Source 1", "Source 2", "Whether Different")) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::row_spec(0, bold = T)
}
