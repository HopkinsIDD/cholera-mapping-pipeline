#' @export
#' @name get_gam_formula
#' @description Given a config and covariate column names, write a gam formula to use for initial values
#' @param initial_values_config The initial values section of the mapping pipeline config
#' @param covariate_names Names of to use in the formula for the covariates
get_gam_formula <- function(cases_column_name, covariate_names) {
    gam_formula <- paste(paste(cases_column_name, "~ s(x,y)"), paste(covariate_names, 
        collapse = " + "), sep = " + ")

    return(as.formula(gam_formula))
}
