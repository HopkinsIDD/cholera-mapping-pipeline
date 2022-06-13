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
plot_sf_with_fill <- function(cache, name, color_scale_type, fill_column, facet_column = "set", geometry_column = "geometry", color_scale_use_log = NA) {
  if (is.na(color_scale_use_log)) {
    color_scale_use_log <- ifelse(color_scale_type %in% c("population"), TRUE, FALSE)
  }
  sf_object <- cache[[name]]
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf_object, ggplot2::aes_string(geometry = geometry_column, fill = fill_column)) +
    taxdat::color_scale(type = color_scale_type, use_case = "ggplot map", use_log = color_scale_use_log) +
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
    color_scale_type = "population", fill_column = "population", facet_column = "t", geometry_column = "geometry", color_scale_use_log = TRUE
  )

  return(plot)
}


#' @export
#' @name plot_raster_covariates
#' @title plot_raster_covariates
#' @description add
#' @param config File path to the config
#' @param cache the cache environment
#' @param cholera_directory Directory of cholera-mapping-pipeline
#' @return ggplot object with covariate raster
plot_raster_covariates <- function(config, cache, cholera_directory) {
  warning("This function needs to be changed to get the standardization in")
  aggregate_covar_cube_covariates(config = config, cache = cache, cholera_directory = cholera_directory)

  return(plot_sf_with_fill(cache, "covar_cube_covariates_aggregated", color_scale_type = "covariate", fill_column = "value", facet_column = "name", geometry_column = "geom"))
}

#' @export
#' @name plot_disaggregated_modeled_cases_time_varying
#' @title plot_disaggregated_modeled_cases_time_varying
#' @description add
#' @param config File path to the config
#' @param cache the cache environment
#' @param cholera_directory Directory of cholera-mapping-pipeline
#' @return ggplot object with modeled cases map
plot_disaggregated_modeled_cases_time_varying <- function(config, cache, cholera_directory) {
  get_boundary_polygon(config = config, cache = cache, cholera_directory = cholera_directory)

  boundary <- get[["config"]][["general"]][["location_name"]]

  if (as.character(stringr::str_extract(params$config, "[A-Z]{3}")) == "ZNZ") {
    boundary_sf <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in%
      c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
  } else {
    boundary_sf <- rgeoboundaries::gb_adm0(as.character(stringr::str_extract(params$config, "[A-Z]{3}")))
  }

  plt <- ggplot2::ggplot()
  plt <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = disaggregated_case_sf,
      ggplot2::aes(fill = value), color = NA, size = 0.00001
    ) +
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = TRUE) +
    ggplot2::geom_sf(data = boundary_sf, fill = NA, color = "black", size = 0.05) +
    ggplot2::labs(fill = "Incidence\n [cases/year]") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(legend.text = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggplot2::facet_wrap(~t, ncol = length(unique(disaggregated_case_sf$t)))

  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width, heigth = height)
  }
  if (render) {
    plt
  }
}
