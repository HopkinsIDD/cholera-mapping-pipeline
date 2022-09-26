#' @description Apply a unary transformation to each covariate
#' @param covariates A data fram with a column for each covariate
#' @param transformations A list of lists. The inner lists should be name, transform_name, transform_function, with transform_function being a unary function to apply to covariates[[name]]. Transform name is used for debug output
#' @param verbose boolean whether to print debug output
#' @export
transform_covariates <- function(covariates, transformations, verbose = FALSE) {
  new_cov_array<-array(dim=c(dim(covariates)[1],dim(covariates)[2],dim(covariates)[3]+length(transformations)))
  new_cov_array[,,1:dim(covariates)[3]]<-covariates
  dimnames(new_cov_array)[[3]]<-c(dimnames(covariates)[[3]],rep(NA,length(transformations)))
  
  new_id<-1
  for (transformation in transformations) {
    if (!(transformation[["name"]] %in% names(covariates[1,1,]))) {#QZ: update to obtain the name of the covariate
      stop(paste("Trying to perform a transform on a covariate (", transformation[["name"]], ") that does not exist. Allowed covariates are", paste(names(covariates), collapse = ", ")))
    }
    if (verbose) {
      print(paste("Transforming", transformation[["name"]], "by", transformation[["transform_name"]]))
    }
    
    new_cov_array[, , dim(covariates)[3]+new_id] <- transformation[["transform_function"]](covariates[ , ,transformation[["name"]]])##QZ: add a new covariate
    dimnames(new_cov_array)[[3]][dim(covariates)[3]+new_id]<-c(transformation[["transform_name"]])
    new_id<-new_id+1
  }
  return(new_cov_array)
}
