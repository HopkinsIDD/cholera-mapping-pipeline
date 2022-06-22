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
#' @name plot_model_fidelity
#' @title plot_model_fidelity
#' @description add
#' @param data_fidelity data_fidelity object
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity <- function(cache,
                                name,
                                tfrac_function = function(x, tfrac) {
                                  return(x)
                                },
                                inv_tfrac_function = function(x, tfrac) {
                                  return(x)
                                },
                                scale = "sqrt") {
  max_value <-
    cache[[name]] %>%
    dplyr::mutate(
      modeled_cases = inv_tfrac_function(modeled_cases, tfrac),
      observed_cases = tfrac_function(observed_cases, tfrac)
    ) %>%
    summarize(max_value = max(c(observed_cases, modeled_cases))) %>%
    .$max_value

  plt <- cache[[name]] %>%
    dplyr::mutate(
      modeled_cases = inv_tfrac_function(modeled_cases, tfrac),
      observed_cases = tfrac_function(observed_cases, tfrac)
    ) %>%
    ggplot() +
    ggplot2::geom_point(ggplot2::aes(y = modeled_cases, x = observed_cases, col = chain)) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(1, max_value), ylim = c(1, max_value)) +
    taxdat::plot_theme() +
    ggplot2::facet_wrap(~censored)
  if (scale == "sqrt") {
    plt <- plt + scale_x_sqrt() + scale_y_sqrt()
  } else if (scale == "log") {
    plt <- plt + scale_x_log10() + scale_y_log10()
  }
  return(plt)
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

  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  if (length(cache[["config"]][["general"]][["covariates"]]) == 0) {
    return(invisible(NULL))
  }

  aggregate_covar_cube_covariates(config = config, cache = cache, cholera_directory = cholera_directory)

  return(plot_sf_with_fill(cache, "covar_cube_covariates_aggregated", color_scale_type = "covariate", fill_column = "value", facet_column = c("name","t"), geometry_column = "geom"))
}

#' @export
#' @name plot_raster_covariates_datagen
#' @title plot_raster_covariates_datagen
#' @description add
#' @param config File path to the config
#' @param cache the cache environment
#' @param cholera_directory Directory of cholera-mapping-pipeline
#' @return ggplot object with covariate raster
plot_raster_covariates_datagen <- function(config, cache, cholera_directory) {
  warning("This function needs to be changed to get the standardization in")
  
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  if (length(cache[["config"]][["test_metadata"]]) == 0) {
    return(invisible(NULL))
  }
  
  data_simulation_covs<-readRDS(cache[["config"]][["test_metadata"]][["Cov_data_simulation_filename"]])
  
  cache[["data_simulation_covs"]]<-as.data.frame(do.call(rbind, data_simulation_covs[2:(length(data_simulation_covs))])) %>%
    mutate(value=covariate,covariate=paste("Covariate",rep(2:length(data_simulation_covs),each=nrow(data_simulation_covs[[1]]))))# Convert list to data frame columns
  cache[["data_simulation_covs"]]<-sf::st_as_sf(cache[["data_simulation_covs"]])
  
  return(plot_sf_with_fill(cache, "data_simulation_covs", color_scale_type = "covariate", fill_column = "value", facet_column = c("covariate","t"), geometry_column = "geometry"))
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
  disaggregate_grid_cases_mean(config = config, cache = cache, cholera_directory = cholera_directory)

  plot <- plot_sf_with_fill(
    cache = cache, name = "grid_cases_mean_disaggregated",
    color_scale_type = "cases", fill_column = "cases", facet_column = "t", geometry_column = "geometry"
  ) +
    ggplot2::geom_sf(data = cache[["boundary_polygon"]], fill = NA, color = "black", size = 0.05)

  return(plot)
}

#' @export
#' @name plot_disaggregated_modeled_cases_time_varying
#' @title plot_disaggregated_modeled_cases_time_varying
#' @description add
#' @param config File path to the config
#' @param cache the cache environment
#' @param cholera_directory Directory of cholera-mapping-pipeline
#' @return ggplot object with modeled cases map
plot_modeled_rates_time_varying <- function(config, cache, cholera_directory) {
  get_boundary_polygon(config = config, cache = cache, cholera_directory = cholera_directory)
  aggregate_grid_cases_mean(config = config, cache = cache, cholera_directory = cholera_directory)

  plot <- plot_sf_with_fill(
    cache = cache, name = "mean_rates_sf",
    color_scale_type = "rates", fill_column = "rates", facet_column = "t", geometry_column = "geometry"
  ) +
    ggplot2::geom_sf(data = cache[["boundary_polygon"]], fill = NA, color = "black", size = 0.05)

  return(plot)
}

#' @export
plot_model_fidelity_tfrac_adjusted <- function(config, cache, cholera_directory) {
  get_data_fidelity_df(config = config, cache = cache, cholera_directory = cholera_directory)
  return(plot_model_fidelity(cache, "data_fidelity_df", scale = "fixed"))
}

#' @export
plot_model_fidelity_tfrac_unadjusted <- function(config, cache, cholera_directory) {
  get_data_fidelity_df(config = config, cache = cache, cholera_directory = cholera_directory)
  return(plot_model_fidelity(cache, "data_fidelity_df", scale = "fixed"))
}

#' @export
plot_stan_parameter_traceplot <- function(config, cache, cholera_directory) {
  get_stan_parameters_of_interest(config = config, cache = cache, cholera_directory = cholera_directory)
  get_stan_parameter_draws(config = config, cache = cache, cholera_directory = cholera_directory)

  plt <- cache$stan_parameter_draws %>%
    reshape2::melt() %>%
    dplyr::mutate(chain = as.factor(chain)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = iteration, y = value, col = chain, group = chain)) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    taxdat::plot_theme()
  return(plt)
}

#' @export
plot_stan_parameter_violin <- function(config, cache, cholera_directory) {
  get_stan_parameters_of_interest(config = config, cache = cache, cholera_directory = cholera_directory)
  get_stan_parameter_draws(config = config, cache = cache, cholera_directory = cholera_directory)

  plt <- cache$stan_parameter_draws %>%
    reshape2::melt() %>%
    dplyr::mutate(chain = as.factor(chain)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_violin(ggplot2::aes(x = variable, y = value)) +
    # ggplot2::facet_wrap(~variable, scales = "free") +
    taxdat::plot_theme()
  return(plt)
}

#' @export
plot_rhat <- function(config, cache, cholera_directory) {
  get_cmdstan_fit(config = config, cache = cache, cholera_directory = cholera_directory)
  get_rhat_threshold(config = config, cache = cache, cholera_directory = cholera_directory)
  percent_above <- cache$cmdstan_fit$summary(variables = "modeled_cases", "rhat") %>%
    dplyr::summarize(percent_above = mean(rhat > cache[["rhat_threshold"]]))

  plt <- cache$cmdstan_fit$summary(variables = "modeled_cases", "rhat") %>%
    dplyr::mutate(variable = as.numeric(gsub("^modeled_cases.", "", gsub("]$", "", variable)))) %>%
    ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = variable, y = rhat)) +
    scale_y_continuous(trans = "log10") +
    ggplot2::xlab("Observation") +
    ggplot2::ggtitle(glue::glue("Fraction above threshold: {format(round(percent_above*100, 2))}%")) +
    ggplot2::geom_hline(yintercept = cache[["rhat_threshold"]], col = "red") +
    taxdat::plot_theme()

  return(plt)
}
