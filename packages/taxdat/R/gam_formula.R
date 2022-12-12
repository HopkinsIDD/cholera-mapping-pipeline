#' @export
#' @name compute_gam_offset
#' @description Give the mean rate equivalent to stan_data$meanrate in old pipeline
#' to calculate the offset in gam model
#' @param initial_values_df
compute_gam_offset <- function(initial_values_df) {
  tmp_stan_data <- list(
    pop = initial_values_df[["population"]],
    map_obs_loctime_loc = as.array(taxdat::cast_to_int32(initial_values_df[["updated_temporal_location_id.x"]])),
    map_obs_loctime_obs = as.array(taxdat::cast_to_int32(initial_values_df[["updated_observation_id"]])),
    tfrac = as.array(initial_values_df[["tfrac"]]),
    y = as.array(pmax(pmin(initial_values_df[[cases_column]], initial_values_df[[paste0(cases_column, "_R")]], na.rm = TRUE),
      initial_values_df[[paste0(cases_column, "_L")]],
      na.rm = TRUE
    )),
    map_loc_grid_loc = as.array(taxdat::cast_to_int32(initial_values_df[["updated_temporal_location_id.y"]])),
    map_loc_grid_grid = as.array(taxdat::cast_to_int32(initial_values_df[["spacetime_grid_id"]]))
  )

  y_tfrac <- tibble::tibble(
    tfrac = tmp_stan_data$tfrac,
    map_obs_loctime_obs = tmp_stan_data$map_obs_loctime_obs
  ) %>%
    dplyr::group_by(map_obs_loctime_obs) %>%
    dplyr::summarize(tfrac = mean(tfrac)) %>% # We take the average over time so we can sum population over time
    .[["tfrac"]]

  nobs <- length(unique(tmp_stan_data$map_obs_loctime_obs))
  # Compute population corresponding to each observation
  aggpop <- rep(0, nobs)
  for (i in 1:nobs) {
    lps <- tmp_stan_data$map_obs_loctime_loc[which(tmp_stan_data$map_obs_loctime_obs == i)]
    for (lp in lps) { # This is summed over years so tfrac is averaged over years
      aggpop[i] <- aggpop[i] + sum(tmp_stan_data$pop[tmp_stan_data$map_loc_grid_grid[tmp_stan_data$map_loc_grid_loc == lp]])
    }
  }

  # Compute the mean incidence
  # Note that this is in the model's temporal resolution
  meanrate <- sum(tmp_stan_data$y * y_tfrac) / sum(aggpop)
  if (meanrate < 1e-10) {
    meanrate <- 1e-10
    print("The mean rate was less than 1e-10, so increased it to 1e-10")
    warning("The mean rate was less than 1e-10, so increased it to 1e-10")
  }

  ey <- tmp_stan_data$pop * meanrate
  gam_offset <- log(ey) + log(tmp_stan_data$tfrac)

  gam_offset
}

#' @export
#' @name get_gam_formula
#' @description Given a config and covariate column names, write a gam formula to use for initial values
#' @param initial_values_config The initial values section of the mapping pipeline config
#' @param covariate_names Names of to use in the formula for the covariates
get_gam_formula <- function(cases_column_name,
                            include_spatial_smoothing,
                            include_covariates,
                            covariate_names,
                            max_knots,
                            include_time_slice_effect) {
  knots <- min(max_knots, 30)
  gam_formula <- paste(cases_column_name, "~ 1")
  if (include_spatial_smoothing) {
    gam_formula <- paste(gam_formula, paste("s(x, y, k =", knots, ")"), sep = " + ")
  }
  if (include_covariates) {
    gam_formula <- paste(gam_formula, paste(covariate_names, collapse = " + "), sep = " + ")
  }
  if (include_time_slice_effect) {
    gam_formula <- paste(gam_formula, "as.factor(t)", sep = " + ")
  }

  return(as.formula(gam_formula))
}
