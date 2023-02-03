#' @title Get stan parameters
#' @description Gets or sets parameters of the stan model
#'
#' @param config the configuration yaml
#' @param sigma_eta_scale the default value for the scale of the yearly random effect
#' @param beta_sigma_scale the default value for the scale of the standard deviation of the prior of the regression coefficients
#' @return a list with parameter values
#' @export 
get_stan_parameters <- function(config,
                                sigma_eta_scale = 1,
                                beta_sigma_scale = 1,
                                warmup = T,
                                covar_warmup = T,
                                time_effect = F,
                                time_effect_autocorr = F,
                                censoring = F,
                                use_weights = F,
                                use_rho_prior = F,
                                use_pop_weight = T,
                                censoring_thresh = .95,
                                od_param = 1,
                                obs_model = 1,
                                exp_prior = 0,
                                use_intercept = 0,
                                do_zerosum_cnst = 0,
                                do_infer_sd_eta = 0, 
                                ncores = 4,
                                model = 'mapping_model_inference.stan',
                                genquant = 'mapping_model_generate.stan',
                                niter = 2000,
                                recompile = T
) {
  
  default_params <- list(sigma_eta_scale = sigma_eta_scale,
                         beta_sigma_scale = beta_sigma_scale,
                         warmup = warmup,
                         covar_warmup = covar_warmup,
                         time_effect = time_effect,
                         time_effect_autocorr = time_effect_autocorr,
                         censoring = censoring,
                         use_weights = use_weights,
                         use_rho_prior = use_rho_prior,
                         use_pop_weight = use_pop_weight,
                         censoring_thresh = censoring_thresh,
                         od_param = od_param,
                         obs_model = obs_model,
                         exp_prior = exp_prior,
                         use_intercept = use_intercept,
                         do_zerosum_cnst = do_zerosum_cnst,
                         do_infer_sd_eta = do_infer_sd_eta, 
                         ncores = ncores,
                         model = model,
                         genquant = genquant,
                         niter = niter,
                         recompile = recompile)
  
  # For each parameter check if specified in config, if not use default value
  params <- purrr::map(names(default_params), 
                       ~ifelse(is.null(config[[.]]), default_params[[.]], config[[.]]))
  
  names(params) <- names(default_params)
  
  return(params)
}