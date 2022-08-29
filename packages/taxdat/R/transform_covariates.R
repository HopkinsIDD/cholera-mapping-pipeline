#' @description Apply a unary transformation to each covariate
#' @param covariates A data fram with a column for each covariate
#' @param transformations A list of lists. The inner lists should be name, transform_name, transform_function, with transform_function being a unary function to apply to covariates[[name]]. Transform name is used for debug output
#' @param verbose boolean whether to print debug output
#' @export
transform_covariates <- function(covariates, transformations, verbose = FALSE) {
  for (transformation in transformations) {
    if (!(transformation[["name"]] %in% names(covariates)[[3]])) {
      stop(paste("Trying to perform a transform on a covariate (", transformation[["name"]], ") that does not exist. Allowed covariates are", paste(names(covariates), collapse = ", ")))
    }
    if (verbose) {
      print(paste("Transforming", transformation[["name"]], "by", transformation[["transform_name"]]))
    }
    covariates[, , transformation[["name"]]] <- transformation[["transform_function"]](covariates[ , ,transformation[["name"]]])
  }
  return(covariates)
}
