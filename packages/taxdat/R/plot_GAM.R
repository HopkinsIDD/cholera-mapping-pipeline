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
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
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
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
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
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
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
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
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
