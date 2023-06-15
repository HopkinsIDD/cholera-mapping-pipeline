#' @include plot_cache_function.R

#' @export
#' @name plot_gam_fit_input
#' @title plot_gam_fit_input
#' @description plot the rasters with gam fitted input cases
#' @param name the name of the dataset (initial values)
#' @param cache the cache environment
#' @param mean whether it's the average across all years 
#' @param type rates or cases 
#' @return ggplot object
plot_gam_fit_input <- function(name="initial_values_data", cache, mean = FALSE, type) {
  # Rates/cases 
  type_nrml <- ifelse(grepl('ase var', tolower(type)), "case variance", ifelse(grepl('ate var', tolower(type)),"rate variance",ifelse(grepl('ase', tolower(type)),"cases",ifelse(grepl('ate', tolower(type)),"rates","observations"))))
  
  # If to get the mean figure 
   if(mean){
     cache[[name]]$gam_fit_input %>%
       dplyr::group_by(sx, sy) %>%
       { if(type_nrml == "cases") dplyr::summarize(., y = mean(y)) else dplyr::summarize(., y = mean(y), pop = mean(pop)) } %>%
       dplyr::ungroup() %>%
       { if(type_nrml == "rates") dplyr::mutate(., y = ifelse(y == 0, 1e-99, y)) else dplyr::mutate(., pop = 1) } %>%
       ggplot2::ggplot() +
       ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill = y / pop), color = 'black') +
       taxdat::color_scale(type = type_nrml, use_case = "ggplot map", use_log = TRUE)+
       ggplot2::labs(fill = ifelse(type_nrml == "cases", "Incidence [cases/year]", "Incidence rate (per 1e5)")) + 
       ggplot2::coord_fixed(ratio = 1) + 
       taxdat::map_theme()
   # If to get the by-year plots 
  }else if (!mean & type_nrml %in%c("cases","rates")){
    # year_name_vector <- names(cache[[name]]$gam_fit_input)[grepl("^year", names(cache[[name]]$gam_fit_input))]
    # cache[[name]]$gam_fit_input_tmp <- cache[[name]]$gam_fit_input
    # invisible(lapply(year_name_vector, function(x){
    #   cache[[name]]$gam_fit_input_tmp[[x]]  [cache[[name]]$gam_fit_input_tmp[[x]] == 1] <- as.numeric(gsub('year_', '', x))
    # }))

    cache[[name]]$gam_fit_input %>%
      # rowwise() %>% 
      # mutate(t = sum(across(starts_with("year")), na.rm = T)) %>% 
      dplyr::mutate(t = as.integer(obs_year)) %>% 
      { if(type_nrml == "rates") dplyr::mutate(., y = ifelse(y == 0, 1e-99, y), y = y / pop) else dplyr::mutate(., y = y) } %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill = y), color = 'black') +
      taxdat::color_scale(type = type_nrml, use_case = "ggplot map", use_log = TRUE)+
      ggplot2::labs(fill = ifelse(type_nrml == "cases", "Incidence [cases/year]", "Incidence rate (per 1e5)")) + 
      ggplot2::facet_wrap( ~ t) + 
      ggplot2::coord_fixed(ratio = 1) + 
      taxdat::map_theme()
    
  } else if(type_nrml=="observations"){
    
    cache[[name]]$gam_fit_input %>%
      dplyr::mutate(t = as.integer(obs_year)) %>% 
      { if(type_nrml == "observations") dplyr::group_by(.,t,sx, sy)%>%dplyr::summarize(number_of_obs =  n())} %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill = number_of_obs), color = 'black') +
      taxdat::color_scale(type = type_nrml, use_case = "ggplot map", use_log = FALSE)+
      ggplot2::labs(fill = "Number of observations \nper grid cell\n ") +
      ggplot2::facet_wrap( ~ t) +
      ggplot2::coord_fixed(ratio = 1) +
      taxdat::map_theme()
  } else {
    cache[["initial_values_data"]]$gam_fit_input %>%
      dplyr::mutate(t = as.integer(obs_year)) %>% 
      { if(type_nrml == "case variance") dplyr::group_by(.,t,sx, sy)%>%dplyr::summarize(variance = var(y)) else dplyr::group_by(.,t,sx, sy)%>%dplyr::summarize(variance = var(y/pop*1e5))} %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill = variance), color = 'black') +
      taxdat::color_scale(type = type_nrml, use_case = "ggplot map", use_log = TRUE)+
      ggplot2::labs(fill = ifelse(type_nrml == "case variance", "Variance of cases \nper grid cell\n ", "Variance of rates per 1e5\n grid cell\n ")) + 
      ggplot2::facet_wrap( ~ t) + 
      ggplot2::coord_fixed(ratio = 1) + 
      taxdat::map_theme()
  }

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
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill = y)) +
    ggplot2::facet_wrap(~stitch_source, ncol = 2, scales = "free") +
    taxdat::map_theme() +
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
    ggplot2::labs(fill = "Cases")
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
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill = y / pop)) +
    ggplot2::facet_wrap(~stitch_source, ncol = 2, scales = "free") +
    taxdat::map_theme() +
    taxdat::color_scale(type = "rates", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::labs(fill="Incidence rate")
}

#' @export
#' @name plot_gam_fit_output_cases
#' @title plot_gam_fit_output_cases
#' @description plot the rasters with gam fitted output cases
#' @param name the name of the dataset (initial values)
#' @param cache the cache environment
#' @param type what to plot 
#' @return ggplot object
plot_gam_fit_output <- function(name="gam_output_df",cache,type) {
  type_nrml <- ifelse(grepl('ase', tolower(type)), "cases", "rates")

  cache[[name]] %>%
    { if(type_nrml == "rates") dplyr::mutate(., y = lambda) else dplyr::mutate(., y = y) } %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill = y), color = 'black') +
    taxdat::map_theme() +
    taxdat::color_scale(type = type_nrml, use_case = "ggplot map", use_log = (type_nrml == 'rates'))+
    ggplot2::labs(fill = ifelse(type_nrml == "cases", "Incidence [cases/year]", "Incidence rate (per 1e5)")) + 
    ggplot2::coord_fixed(ratio = 1) + 
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
    dplyr::mutate( t = t + (min(year_vector) - min(cache[[name]]$t)) ) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill =y)) +
    taxdat::map_theme() +
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
    ggplot2::labs(fill="Cases")+
    ggplot2::facet_wrap(t~stitch_source, ncol = 2)
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
    dplyr::mutate( t = t + (min(year_vector) - min(cache[[name]]$t)) ) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = sx, y = sy, fill =lambda)) +
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
    ggplot2::ggplot(ggplot2::aes(x = source1, y = source2)) + 
    ggplot2::geom_point(size=1, color="#69b3a2", alpha=0.3) + 
    ggplot2::theme_minimal() + 
    ggplot2::coord_fixed(ratio = 1) + 
    ggplot2::geom_abline(intercept = 0, slope = 1, size = 0.2) +
    ggplot2::facet_wrap( ~ t, ncol = 3) + 
    ggplot2::labs(x = "GAM output cases from model 1", y = "GAM output cases from model 2")

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
    dplyr::mutate(stitch_source = paste0("source", stitch_source)) %>% 
    tidyr::spread(stitch_source, lambda) %>% 
    ggplot2::ggplot(ggplot2::aes(x = source1, y = source2)) + 
    ggplot2::geom_point(size=1, color="#69b3a2", alpha=0.3) +
    ggplot2::theme_minimal() + 
    ggplot2::coord_fixed(ratio = 1) + 
    ggplot2::geom_abline(intercept = 0, slope = 1, size = 0.2) +
    ggplot2::facet_wrap( ~ t, ncol = 3) + 
    ggplot2::labs(x = "GAM output rates from model 1", y = "GAM output rates from model 2")

  return(plt)
}

#' @export
#' @name stitch_gam_input_with_gam_output_rate
#' @title stitch_gam_input_with_gam_output_rate
#' @description stitch gam input and gam output together (from the same pipeline model)
#' @param output_cache the output cache
#' @param input_caches the list of input caches 
#' @param type raw or averaged 
stitch_gam_input_with_gam_output_rate <- function(output_cache, input_caches, type){
  stitch_caches(
                name = "gam_fit_input", 
                output_cache = output_cache, 
                input_caches = input_caches,
                initial_value = list(type = 'first'), 
                combination_function = ifelse(grepl("raw", tolower(type)), merge_gamoutput_to_gaminput_as_in_df, merge_gamoutput_to_averaged_gaminput_as_in_df)
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
#' @name merge_gamoutput_to_averaged_gaminput_as_in_df_no_cache
#' @title merge_gamoutput_to_averaged_gaminput_as_in_df_no_cache
#' @description the actual merge function 
#' @param name name of the object in the cache to use 
#' @param cache cache environment 
#' @param input_cache the input cache 
#' @return merged object 
merge_gamoutput_to_averaged_gaminput_as_in_df_no_cache <- function(name = "gam_fit_input", cache, input_cache, ...){
  # the name argument will not be used here because the output name will be different from the input name 
  df1 <- cache[[paste0("gam_fit_input", "_initialized")]] %>% 
    dplyr::mutate(input_rate = y / pop, t = as.integer(obs_year)) %>% 
    dplyr::group_by(t, sx, sy) %>%
    dplyr::summarize(input_rate = mean(input_rate))

  df2 <- input_cache[[1]] %>% dplyr::select(sx, sy, t, lambda) %>% rename(output_rate = lambda)
  #QZ: df1 and df2 have different lengths (ind in df1 and id in df2 have the same length, some regions and years have more than one input data with different tfrac; y in df1 seem to have take tfrac into account)

  merged_df <- dplyr::left_join(df1, df2, by = c("sx", "sy", "t"))
  rm(df1, df2)

  rm("gam_fit_input_initialized", envir = cache)
  return(merged_df)
}
#' @export
#' @name merge_gamoutput_to_averaged_gaminput_as_in_df
merge_gamoutput_to_averaged_gaminput_as_in_df <- cache_fun_results_new(name = "averaged_gam_input_with_gam_output", fun = merge_gamoutput_to_averaged_gaminput_as_in_df_no_cache, cache = output_cache, overwrite = F)


#' @export
#' @name plot_gam_input_vs_output_rate_scatter_plot
#' @title plot_gam_input_vs_output_rate_scatter_plot
#' @description plot_gam_input_vs_output_rate_scatter_plot
#' @param cache cache environment 
#' @param type raw or averaged 
#' @return ggplot object 
plot_gam_input_vs_output_rate_scatter_plot <- function(cache, type){
  stitch_gam_input_with_gam_output_rate(output_cache = cache, input_caches = list(cache$initial_values_data, cache$gam_output_df), type)
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
      ggplot2::geom_abline(intercept=0,slope=1,size=0.2)+
      ggplot2::geom_point(
        color="#69b3a2",
        alpha=0.5,
        size=1)+
      ggplot2::labs(x=column_x,y=column_y)+
      ggplot2::theme_minimal()+ 
      ggplot2::coord_equal(xlim=c(0,max(cache[[name]][[column_x]], cache[[name]][[column_y]])),
                  ylim=c(0,max(cache[[name]][[column_x]], cache[[name]][[column_y]]))) + 
      # coord_fixed(ratio = 1) + 
      ggplot2::facet_wrap(~t)    
  }else{
    plt<-cache[[name]]%>%
        ggplot2::ggplot(ggplot2::aes_string(x=column_x,y=column_y))+
        ggplot2::coord_trans(x="log10", y="log10") + 
        ggplot2::geom_abline(intercept=0,slope=1,size=0.2)+
        ggplot2::geom_point(
          color="#69b3a2",
          alpha=0.5,
          size=1)+
        ggplot2::labs(x=column_x,y=column_y)+
        ggplot2::theme_minimal()+
        ggplot2::coord_equal(xlim=c(0,log10(max(cache[[name]][[column_x]]), log10(cache[[name]][[column_y]]))),
                    xlim=c(0,log10(max(cache[[name]][[column_x]]), log10(cache[[name]][[column_y]]))) ) + 
        # coord_fixed(ratio = 1) + 
        ggplot2::facet_wrap(~t) 
  }
return(plt)

}
