#' @include plot_cache_function.R

#' @export
#' @name plot_gam_fit_input_cases
#' @title plot_gam_fit_input_cases
#' @description plot the rasters with gam fitted input cases
#' @name name the name of the dataset (initial values)
#' @name cache the cache environment
#' @return ggplot object
plot_gam_fit_input_cases <- function(name="initial_values_data",cache) {
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