#' @include report_cache.R
#' @include report_data.R

#' @export
#' @name plot_sf_with_fill
#' @title plot_sf_with_fill
#' @description plot the polygon with cases or rates information
#' @param name the name of the sf object
#' @param color_scale the color scale of the plot
#' @param fill_column the column name to fill the raster
#' @return ggplot object
plot_sf_with_fill <- function(cache, name, color_scale_type, fill_column, facet_column = "set", geometry_column = "geometry") {
  sf_object <- cache[[name]]
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf_object, ggplot2::aes_string(geometry = geometry_column, fill = fill_column)) +
    taxdat::color_scale(type = color_scale_type, use_case = "ggplot map") +
    taxdat::map_theme()
  if (!is.null(facet_column)) {
    plot <- plot +
      ggplot2::facet_wrap(formula(paste("~", paste(facet_column, collapse = " + "))))
  }
  return(plot)
}

#' @export
#' @name plot_gam_fit_input_cases
#' @title plot_gam_fit_input_cases
#' @description plot the rasters with gam fitted input cases
#' @param config File path to the config
#' @param cache the cache environment
#' @param cholera_directory Directory of cholera-mapping-pipeline
#' @return ggplot object
plot_gam_fit_input_cases <- function(config, cache, cholera_directory) {
  get_initial_values_df(config = config, cache = cache, cholera_directory = cholera_directory)
  return(plot_sf_with_fill(cache, "initial_values_df", color_scale_type = "cases", fill_column = "suspected_cases", geometry_column = "shape", facet_column = "t"))
}

#' @export
#' @name plot_gam_fit_input_rates
#' @title plot_gam_fit_input_rates
#' @description plot the rasters with gam fitted input rates
#' @param config File path to the config
#' @param cache the cache environment
#' @param cholera_directory Directory of cholera-mapping-pipeline
#' @return ggplot object
plot_gam_fit_input_rates <- function(config, cache, cholera_directory) {
  get_initial_values_df(config = config, cache = cache, cholera_directory = cholera_directory)
  return(plot_sf_with_fill(cache, "initial_values_df", color_scale_type = "rates", fill_column = "suspected_cases/population", geometry_column = "shape", facet_column = "t"))
}

#' @export
#' @name plot_gam_fit_output_cases
#' @title plot_gam_fit_output_cases
#' @description plot the rasters with gam fitted output cases
#' @param config File path to the config
#' @param cache the cache environment
#' @param cholera_directory Directory of cholera-mapping-pipeline
#' @return ggplot object
plot_gam_fit_output_cases <- function(config, cache, cholera_directory) {
  get_covar_cube(config = config, cache = cache, cholera_directory = cholera_directory)
  return(plot_sf_with_fill(cache, "covar_cube", color_scale_type = "cases", fill_column = "gam_output", facet_column = "t"))
}

#' @export
#' @name plot_gam_fit_output_rates
#' @title plot_gam_fit_output_rates
#' @description plot the rasters with gam fitted output rates
#' @param config File path to the config
#' @param cache the cache environment
#' @param cholera_directory Directory of cholera-mapping-pipeline
#' @return ggplot object
plot_gam_fit_output_rates <- function(config, cache, cholera_directory) {
  get_covar_cube(config = config, cache = cache, cholera_directory = cholera_directory)
  return(plot_sf_with_fill(cache, "covar_cube", color_scale_type = "rates", fill_column = "gam_output/population", facet_column = "t"))
}

#' @export
#' @name plot_observed_cases_polygon_raw
#' @title plot_observed_cases_polygon_raw
#' @description plot the polygon with observed cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' #' @return ggplot object
plot_observed_cases_polygon_raw <- function(config, cache, cholera_directory) {
  aggregate_observed_polygon_cases_disjoint_aggregated(
    config = config,
    cholera_directory = cholera_directory,
    cache = cache
  )
  plot <- plot_sf_with_fill(
    cache = cache, name = "observed_polygon_cases_disjoint_aggregated",
    color_scale_type = "cases",
    fill_column = "suspected_cases",
    geometry_column = "geom",
    facet_column = "set"
  )
  return(plot)
}

#' @export
#' @name plot_area_adjusted_observed_cases
#' @title plot_area_adjusted_observed_cases
#' @description plot the polygon with observed cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' #' @return ggplot object
plot_area_adjusted_observed_cases <- function(config, cache, cholera_directory) {
  aggregate_observed_polygon_cases_disjoint_aggregated(
    config = config,
    cholera_directory = cholera_directory,
    cache = cache
  )

  plot <- plot_sf_with_fill(
    cache = cache, name = "observed_polygon_cases_disjoint_aggregated",
    color_scale_type = "cases", fill_column = "suspected_cases / sf::st_area(geom)",
    facet_column = "set", geometry_column = "geom"
  )
  return(plot)
}

#' @export
#' @name plot_raw_observations
#' @title plot_raw_observations
#' @description plot the polygon with number of observations
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' #' @return ggplot object
plot_raw_observations <- function(config, cache, cholera_directory) {
  aggregate_observed_polygon_cases_disjoint_counted(
    config = config,
    cholera_directory = cholera_directory,
    cache = cache
  )

  plot <- plot_sf_with_fill(
    cache = cache, name = "observed_polygon_cases_disjoint_counted",
    color_scale_type = "observation_counts", fill_column = "suspected_cases", facet_column = "set", geometry_column = "geom"
  )

  return(plot)
}

#' @export
#' @name plot_time_varying_pop_raster
#' @description plot the time varying population raster
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
plot_time_varying_pop_raster <- function(config, cache, cholera_directory) {
  get_covar_cube(config = config, cache = cache, cholera_directory = cholera_directory)

  plot <- plot_sf_with_fill(
    cache = cache, name = "covar_cube",
    color_scale_type = "population", fill_column = "population", facet_column = "t", geometry_column = "geometry"
  )

  return(plot)
}


#' @export
#' @name plot_raster_covariates
#' @title plot_raster_covariates
#' @description add
#' @param covar_data_filename covariates rdata filename
#' @param render default is FALSE
#' @return ggplot object with covariate raster
plot_raster_covariates <- function(config, cache, cholera_directory) {
  warning("This function needs to be changed to get the standardization in")
  aggregate_covar_cube_covariates(config = config, cache = cache, cholera_directory = cholera_directory)

  return(plot_sf_with_fill(cache, "covar_cube_covariates_aggregated", color_scale_type = "covariate", fill_column = "value", facet_column = "name", geometry_column = "geom"))

  ## only displaying the first row for now
  cache[["covar_cube"]] %>%
    dplyr::filter(t == min(t)) %>%
    tidyr::pivot_longer(cache[["config"]][["general"]][["covariates"]]) %>%
    covar_layers() <- covar_cube[, , -1, drop = F]

  ## plot first time point of all covariates for now
  pltdata_dummy <-
    tidyr::gather(
      dplyr::filter(
        pltdata,
        t == 1
      ),
      one_of(dimnames(covar_cube_output$covar_cube)[[3]]),
      key = "covars", value = "value"
    )

  plt <- pltdata_dummy %>%
    group_by(covars) %>%
    do(gg = {
      ggplot(., ggplot2::aes(fill = value, color = value)) +
        ggplot2::geom_sf() +
        ggplot2::facet_wrap(~covars) +
        ggplot2::scale_fill_viridis_c(
          aesthetics = c("colour", "fill"),
          guide = ggplot2::guide_colorbar(title = "Covariate at time 1"),
          option = "B", na.value = "white"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.title = element_text(size = 10)
        )
    }) %>%
    .$gg %>%
    gridExtra::grid.arrange(grobs = ., nrow = 2)
  return(plt)
}
