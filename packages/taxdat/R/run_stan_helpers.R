#' @title Get stan parameters
#' @description Gets or sets parameters of the stan model
#'
#' @param config the configuration yaml
#' @param sigma_eta_scale the default value for the scale of the yearly random effect
#' @param beta_sigma_scale the default value for the scale of the standard deviation of the prior of the regression coefficients
#' @return a list with parameter values
#' @export 
get_stan_parameters <- function(config,
                                sigma_eta_scale = 5,
                                beta_sigma_scale = 10,
                                covar_warmup = T
) {
  
  default_params <- list(sigma_eta_scale = sigma_eta_scale,
                         beta_sigma_scale = beta_sigma_scale)
  
  # For each parameter check if specified in config, if not use default value
  params <- purrr::map(names(default_params), 
                       ~ifelse(is.null(config[[.]]), default_params[[.]], config[[.]]))
  
  names(params) <- default_params
  
  return(params)
}