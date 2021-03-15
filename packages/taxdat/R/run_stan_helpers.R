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
                                warmup = T,
                                covar_warmup = T,
                                time_effect = F,
                                time_effect_autocorr = F,
                                censoring = F,
                                use_weights = T,
                                overdispersion = NA
) {
  
  default_params <- list(sigma_eta_scale = sigma_eta_scale,
                         beta_sigma_scale = beta_sigma_scale,
                         warmup = warmup,
                         covar_warmup = covar_warmup,
                         time_effect = time_effect,
                         time_effect_autocorr = time_effect_autocorr,
                         censoring = censoring,
                         use_weights = use_weights,
                         overdispersion = overdispersion)
  
  # For each parameter check if specified in config, if not use default value
  params <- purrr::map(names(default_params), 
                       ~ifelse(is.null(config[[.]]), default_params[[.]], config[[.]]))
  
  names(params) <- names(default_params)
  
  return(params)
}