#' @title Get default stan parameters
#' @description Gets or sets default parameters of the stan model
#' @param config the configuration yaml
#' @param sigma_eta_scale the default value for the scale of the yearly random effect
#' @param beta_sigma_scale the default value for the scale of the standard deviation of the prior of the regression coefficients
#' @param warmup default warmup
#' @param covar_warmup default covar_warmup
#' @param time_effect default time_effect
#' @param time_effect_autocorr default time_effect_autocorr
#' @param use_weights default use_weights
#' @param use_rho_prior default use_rho_prior
#' @param exp_prior default exp_prior
#' @param use_intercept default use_intercept
#' @param do_zerosum_cnst default do_zerosum_cnst
#' @param do_infer_sd_eta default do_infer_sd_eta
#' @param ncores default ncores
#' @param model default Stan model code file
#' @param genquant default genquant model code file
#' @param recompile default recompile
#' @return a list with parameter values
#' @export 
get_stan_parameters <- function(config, 
                                sigma_eta_scale = 1, 
                                beta_sigma_scale = 1, 
                                warmup = T, 
                                covar_warmup = T, 
                                time_effect = F, 
                                time_effect_autocorr = F, 
                                use_weights = F, 
                                use_rho_prior = F, 
                                exp_prior = 0, 
                                use_intercept = 0, 
                                do_zerosum_cnst = 0, 
                                do_infer_sd_eta = 0,
                                ncores = 4, 
                                model = "mapping_model_inference.stan", 
                                genquant = "mapping_model_generate.stan",
                                # iter_warmup = 1000, 
                                # iter_sampling = 1000, ### these two parameters already have their own check functions 
                                recompile = T) {

    default_params <- list( sigma_eta_scale = sigma_eta_scale,
                            beta_sigma_scale = beta_sigma_scale,
                            warmup = warmup, 
                            covar_warmup = covar_warmup, 
                            time_effect = time_effect,
                            time_effect_autocorr = time_effect_autocorr, 
                            use_weights = use_weights, 
                            use_rho_prior = use_rho_prior,
                            exp_prior = exp_prior, 
                            use_intercept = use_intercept, 
                            do_zerosum_cnst = do_zerosum_cnst,
                            do_infer_sd_eta = do_infer_sd_eta, 
                            ncores = ncores, 
                            model = model, 
                            genquant = genquant,
                            # iter_warmup = iter_warmup, 
                            # iter_sampling = iter_sampling, 
                            recompile = recompile)

    # For each parameter check if specified in config, if not use default value
    params <- purrr::map(names(default_params), ~ifelse(is.null(config[[.]]), default_params[[.]],
        config[[.]]))

    names(params) <- names(default_params)

    return(params)
}
