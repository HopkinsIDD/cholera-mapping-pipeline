#' @include plot_cache_function.R

#' @export
#' @name plot_gam_fit_input_cases
#' @title plot_gam_fit_input_cases
#' @description plot the rasters with gam fitted input cases
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
#' @return ggplot object
plot_gam_fit_input_cases <- function(name="initial_values_data", cache) {
   cache[[name]]$gam_fit_input %>%
    dplyr::group_by(sx, sy) %>%
    dplyr::summarize(y = mean(y)) %>%
    ggplot() +
    geom_tile(aes(x = sx, y = sy, fill = y)) +
    taxdat::map_theme() +
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
    ggplot2::labs(fill="Cases")
}

#' @export
#' @name plot_gam_fit_input_cases_stitched
#' @title plot_gam_fit_input_cases_stitched
#' @description plot the rasters with gam fitted input cases
#' @param name the name of the dataset (initial values)
#' @param cache the cache environment
#' @return ggplot object
plot_gam_fit_input_cases_stitched <- function(name = "initial_values_data", cache) {
   cache[[name]]$gam_fit_input %>%
    dplyr::group_by(stitch_source, sx, sy) %>%
    dplyr::summarize(y = mean(y)) %>%
    ggplot() +
    geom_tile(aes(x = sx, y = sy, fill = y)) +
    facet_wrap(~stitch_source, ncol = 2, scales = "free") +
    taxdat::map_theme() +
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
    ggplot2::labs(fill = "Cases")
}

#' @export
#' @name plot_gam_fit_input_rates
#' @title plot_gam_fit_input_rates
#' @description plot the rasters with gam fitted input rates
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
#' @return ggplot object
plot_gam_fit_input_rates <- function(name="initial_values_data",cache) {
  cache[[name]]$gam_fit_input %>%
    dplyr::group_by(sx, sy) %>%
    dplyr::summarize(y = mean(y), pop = mean(pop)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(y = ifelse(y == 0, 1e-99, y)) %>%
    ggplot() +
    geom_tile(aes(x = sx, y = sy, fill = y / pop)) +
    taxdat::map_theme() +
    taxdat::color_scale(type = "rates", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::labs(fill="Incidence rate")
}

#' @export
#' @name plot_gam_fit_input_rates_stitched
#' @title plot_gam_fit_input_rates_stitched
#' @description plot the rasters with gam fitted input rates
#' @param name the name of the dataset (initial values)
#' @param cache the cache environment
#' @return ggplot object
plot_gam_fit_input_rates_stitched <- function(name = "initial_values_data", cache) {
  cache[[name]]$gam_fit_input %>%
    dplyr::group_by(stitch_source, sx, sy) %>%
    dplyr::summarize(y = mean(y), pop = mean(pop)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(y = ifelse(y == 0, 1e-99, y)) %>%
    ggplot() +
    geom_tile(aes(x = sx, y = sy, fill = y / pop)) +
    facet_wrap(~stitch_source, ncol = 2, scales = "free") +
    taxdat::map_theme() +
    taxdat::color_scale(type = "rates", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::labs(fill="Incidence rate")
}

#' @export
#' @name plot_gam_fit_output_cases
#' @title plot_gam_fit_output_cases
#' @description plot the rasters with gam fitted output cases
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
#' @return ggplot object
plot_gam_fit_output_cases <- function(name="gam_output_df",cache) {
  cache[[name]] %>%
    ggplot() +
    geom_tile(aes(x = sx, y = sy, fill =y)) +
    taxdat::map_theme() +
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
    ggplot2::labs(fill="Cases")+
    ggplot2::facet_wrap(~t)
}

#' @export
#' @name plot_gam_fit_output_cases_stitched
#' @title plot_gam_fit_output_cases_stitched
#' @description plot the rasters with gam fitted output cases
#' @param name the name of the dataset (initial values)
#' @param cache the cache environment
#' @param year_vector vector of years to plot 
#' @return ggplot object
plot_gam_fit_output_cases_stitched <- function(name = "gam_output_df", cache, year_vector){
  cache[[name]] %>%
    mutate( t = t + (min(year_vector) - min(cache[[name]]$t)) ) %>% 
    ggplot() +
    geom_tile(aes(x = sx, y = sy, fill =y)) +
    taxdat::map_theme() +
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
    ggplot2::labs(fill="Cases")+
    ggplot2::facet_wrap(t~stitch_source, ncol = 2)
}

#' @export
#' @name plot_gam_fit_output_rates
#' @title plot_gam_fit_input_rates
#' @description plot the rasters with gam fitted output rates
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
#' @return ggplot object
plot_gam_fit_output_rates <- function(name="gam_output_df",cache) {
  cache[[name]] %>%
    ggplot() +
    geom_tile(aes(x = sx, y = sy, fill =lambda)) +
    taxdat::map_theme() +
    taxdat::color_scale(type = "rates", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::labs(fill="Incidence rate")+
    ggplot2::facet_wrap(~t)
}

#' @export
#' @name plot_gam_fit_output_rates_stitched
#' @title plot_gam_fit_input_rates_stitched
#' @description plot the rasters with gam fitted output rates
#' @param name the name of the dataset (initial values)
#' @param cache the cache environment
#' @param year_vector vector of years to plot 
#' @return ggplot object
plot_gam_fit_output_rates_stitched <- function(name = "gam_output_df", cache, year_vector){
  cache[[name]] %>%
    mutate( t = t + (min(year_vector) - min(cache[[name]]$t)) ) %>% 
    ggplot() +
    geom_tile(aes(x = sx, y = sy, fill =lambda)) +
    taxdat::map_theme() +
    taxdat::color_scale(type = "rates", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::labs(fill="Incidence rate")+
    ggplot2::facet_wrap(t~stitch_source, ncol = 2)
}

#' @export
#' @name plot_gam_cases_scatter_plot_stitched
#' @title plot_gam_cases_scatter_plot_stitched
#' @description plot_gam_cases_scatter_plot_stitched
#' @param cache output cache that has the needed data
#' @return the ggplot object
plot_gam_cases_scatter_plot_stitched <- function(cache){
  plt <- cache$gam_output_df %>%
    sf::st_drop_geometry() %>% 
    dplyr::select(id, t, sx, sy, stitch_source, y) %>% 
    mutate(stitch_source = paste0("source", stitch_source)) %>% 
    tidyr::spread(stitch_source, y) %>% 
    ggplot(aes(x = source1, y = source2)) + 
    geom_point(size=1, color="#69b3a2", alpha=0.3) +
    theme_minimal() + 
    coord_fixed(ratio = 1) + 
    geom_abline(intercept = 0, slope = 1, size = 0.2) +
    facet_wrap( ~ t, ncol = 3) + 
    labs(x = "GAM output cases from model 1", y = "GAM output cases from model 2")

  return(plt)
}

#' @export
#' @name plot_gam_rates_scatter_plot_stitched
#' @title plot_gam_rates_scatter_plot_stitched
#' @description plot_gam_rates_scatter_plot_stitched
#' @param cache output cache that has the needed data
#' @return the ggplot object
plot_gam_rates_scatter_plot_stitched <- function(cache){
  plt <- cache$gam_output_df %>%
    sf::st_drop_geometry() %>% 
    dplyr::select(id, t, sx, sy, stitch_source, lambda) %>% 
    mutate(stitch_source = paste0("source", stitch_source)) %>% 
    tidyr::spread(stitch_source, lambda) %>% 
    ggplot(aes(x = source1, y = source2)) + 
    geom_point(size=1, color="#69b3a2", alpha=0.3) +
    theme_minimal() + 
    coord_fixed(ratio = 1) + 
    geom_abline(intercept = 0, slope = 1, size = 0.2) +
    facet_wrap( ~ t, ncol = 3) + 
    labs(x = "GAM output rates from model 1", y = "GAM output rates from model 2")

  return(plt)
}

#' @export
#' @name stitch_gam_input_with_gam_output_rate
#' @title stitch_gam_input_with_gam_output_rate
#' @description stitch gam input and gam output together (from the same pipeline model)
#' @param output_cache the output cache
#' @param input_caches the list of input caches 
stitch_gam_input_with_gam_output_rate <- function(output_cache, input_caches){
  stitch_caches(
                name = "gam_fit_input", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = merge_gamoutput_to_gaminput_as_in_df
  )

}

#' @export
#' @name merge_gamoutput_to_gaminput_as_in_df_no_cache
#' @title merge_gamoutput_to_gaminput_as_in_df_no_cache
#' @description the actual merge function 
#' @param name name of the object in the cache to use 
#' @param cache cache environment 
#' @param input_cache the input cache 
#' @return merged object 
merge_gamoutput_to_gaminput_as_in_df_no_cache <- function(name = "gam_fit_input", cache, input_cache, ...){
  # the name argument will not be used here because the output name will be different from the input name 
  df1 <- cache[[paste0("gam_fit_input", "_initialized")]] %>% dplyr::mutate(input_rate = y / pop, t = as.integer(obs_year))
  df2 <- input_cache[[1]] %>% dplyr::select(sx, sy, t, lambda) %>% rename(output_rate = lambda)
  #QZ: df1 and df2 have different lengths (ind in df1 and id in df2 have the same length, some regions and years have more than one input data with different tfrac; y in df1 seem to have take tfrac into account)

  merged_df <- dplyr::left_join(df1, df2, by = c("sx", "sy", "t"))
  rm(df1, df2)

  rm("gam_fit_input_initialized", envir = cache)
  return(merged_df)
}
#' @export
#' @name merge_gamoutput_to_gaminput_as_in_df
merge_gamoutput_to_gaminput_as_in_df <- cache_fun_results_new(name = "gam_input_with_gam_output", fun = merge_gamoutput_to_gaminput_as_in_df_no_cache, cache = output_cache, overwrite = F)

#' @export
#' @name plot_gam_input_vs_output_rate_scatter_plot
#' @title plot_gam_input_vs_output_rate_scatter_plot
#' @description plot_gam_input_vs_output_rate_scatter_plot
#' @param cache cache environment 
#' @return ggplot object 
plot_gam_input_vs_output_rate_scatter_plot <- function(cache){
  stitch_gam_input_with_gam_output_rate(output_cache = cache, input_caches = list(cache$initial_values_data, cache$gam_output_df))
  plot_model_comparison_scatter_plot(cache=cache,name="gam_input_with_gam_output",column_x="input_rate",column_y="output_rate",use_log=F)
}

#' @export
#' @name stitch_gam_output_with_stan_output_rate
#' @title stitch_gam_output_with_stan_output_rate
#' @description stitch stan output and gam output together (from the same pipeline model)
#' @param output_cache the output cache
#' @param input_caches the list of input caches 
stitch_gam_output_with_stan_output_rate <- function(output_cache, input_caches){
  stitch_caches(
                name = "gam_output_df", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = merge_stanoutput_to_gamoutput_rate_as_in_sf
  )

}

#' @export
#' @name merge_gamoutput_to_gaminput_as_in_df_no_cache
#' @title merge_gamoutput_to_gaminput_as_in_df_no_cache
#' @description the actual merge function 
#' @param name name of the object in the cache to use 
#' @param cache cache environment 
#' @param input_cache the input cache 
#' @return merged object 
merge_stanoutput_to_gamoutput_rate_as_in_sf_no_cache <- function(name, cache, input_cache, ...){
  # the name argument will not be used here because the output name will be different from the input name 
  sf <- cache$gam_output_df %>% dplyr::select(t, lambda) %>% rename(rate_gam = lambda)
  array <- apply(cache$modeled_rates, c(3), mean)
  sf$rate_stan <- array
  rm(array)
  
  rm("gam_output_df_initialized", envir = cache)
  return(sf)
}
#' @export
#' @name merge_stanoutput_to_gamoutput_rate_as_in_sf
merge_stanoutput_to_gamoutput_rate_as_in_sf <- cache_fun_results_new(name = "gam_output_with_stan_output_rate", fun = merge_stanoutput_to_gamoutput_rate_as_in_sf_no_cache, cache = output_cache, overwrite = F)

#' @export
#' @name plot_stan_output_vs_gam_output_rate_scatter_plot
#' @title plot_stan_output_vs_gam_output_rate_scatter_plot
#' @description plot_stan_output_vs_gam_output_rate_scatter_plot
#' @param cache cache environment 
#' @return ggplot object 
plot_stan_output_vs_gam_output_rate_scatter_plot <- function(cache){
  stitch_gam_output_with_stan_output_rate(output_cache = cache, input_caches = list(cache$initial_values_data, cache$gam_output_df))
  plot_model_comparison_scatter_plot(cache=cache,name="gam_output_with_stan_output_rate",column_x="rate_gam",column_y="rate_stan",use_log=F)
}


#' @export
#' @name stitch_gam_output_with_stan_output_case
#' @title stitch_gam_output_with_stan_output_case
#' @description stitch stan output and gam output together (from the same pipeline model)
#' @param output_cache the output cache
#' @param input_caches the list of input caches 
stitch_gam_output_with_stan_output_case <- function(output_cache, input_caches){
  stitch_caches(
    name = "gam_output_df", 
    output_cache = output_cache, 
    input_caches = input_caches,
    initial_value = list(type = 'first'), 
    combination_function = merge_stanoutput_to_gamoutput_case_as_in_sf
  )
  
}

#' @export
#' @name merge_stanoutput_to_gamoutput_case_as_in_sf_no_cache
#' @title merge_stanoutput_to_gamoutput_case_as_in_sf_no_cache
#' @description the actual merge function 
#' @param name name of the object in the cache to use 
#' @param cache cache environment 
#' @param input_cache the input cache 
#' @return merged object 
merge_stanoutput_to_gamoutput_case_as_in_sf_no_cache <- function(name, cache, input_cache, ...){
  # the name argument will not be used here because the output name will be different from the input name 
  sf <- cache$gam_output_df %>% dplyr::select(t, y) %>% rename(case_gam = y)
  array <- apply(cache$modeled_cases, c(3), mean)
  sf$case_stan <- array
  rm(array)
  
  rm("gam_output_df_initialized", envir = cache)
  return(sf)
}
#' @export
#' @name merge_stanoutput_to_gamoutput_case_as_in_sf
merge_stanoutput_to_gamoutput_case_as_in_sf <- cache_fun_results_new(name = "gam_output_with_stan_output_case", fun = merge_stanoutput_to_gamoutput_case_as_in_sf_no_cache, cache = output_cache, overwrite = F)

#' @export
#' @name plot_stan_output_vs_gam_output_case_scatter_plot
#' @title plot_stan_output_vs_gam_output_case_scatter_plot
#' @description plot_stan_output_vs_gam_output_case_scatter_plot
#' @param cache cache environment 
#' @return ggplot object 
plot_stan_output_vs_gam_output_case_scatter_plot <- function(cache){
  stitch_gam_output_with_stan_output_case(output_cache = cache, input_caches = list(cache$initial_values_data, cache$gam_output_df))
  plot_model_comparison_scatter_plot(cache=cache,name="gam_output_with_stan_output_case",column_x="case_gam",column_y="case_stan",use_log=F)
}

#' @export
#' @name plot_model_comparison_scatter_plot
#' @title plot_model_comparison_scatter_plot
#' @description plot_model_comparison_scatter_plot
#' @param cache cache environment 
#' @return ggplot object 
plot_model_comparison_scatter_plot <- function(cache,name,column_x="rate_gam",column_y="rate_stan",use_log=FALSE){
  if(!use_log){
    plt<-cache[[name]]%>%
      ggplot2::ggplot(ggplot2::aes_string(x=column_x,y=column_y))+
      geom_abline(intercept=0,slope=1,size=0.2)+
      geom_point(
        color="#69b3a2",
        alpha=0.5,
        size=1)+
      labs(x=column_x,y=column_y)+
      theme_minimal()+
      coord_fixed(ratio = 1) + 
      facet_wrap(~t)    
  }else{
    plt<-ggplot2::ggplot(ggplot2::aes_string(x=column_x,y=column_y))+
        coord_trans(x="log10", y="log10") + 
        geom_abline(intercept=0,slope=1,size=0.2)+
        geom_point(
          color="#69b3a2",
          alpha=0.5,
          size=1)+
        labs(x=column_x,y=column_y)+
        theme_minimal()+
        coord_fixed(ratio = 1) + 
        facet_wrap(~t) 
  }
return(plt)

}
