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
