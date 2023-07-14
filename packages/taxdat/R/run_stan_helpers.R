#' @title Get default stan parameters
#' @description Gets or sets default parameters of the stan model
#' @param config the configuration yaml
#' @param ncores default ncores
#' @param model default Stan model code file
#' @param genquant default genquant model code file
#' @param recompile default recompile
#' @return a list with parameter values
#' @export
get_stan_parameters <- function(config,
                                ncores = 4,
                                model = "mapping_model_inference.stan",
                                genquant = "mapping_model_generate.stan",
                                iter_warmup = 1000,
                                iter_sampling = 1000,
                                recompile = T) {

    default_params <- list(ncores = ncores,
                            model = model,
                            genquant = genquant,
                            iter_warmup = check_stan_iter_warmup(iter_warmup),
                            iter_sampling = check_stan_iter_sampling(iter_sampling),
                            recompile = recompile)

    # For each parameter check if specified in config, if not use default value
    params <- purrr::map(names(default_params), ~ifelse(is.null(config[[.]]), default_params[[.]],
        config[[.]]))

    names(params) <- names(default_params)

    return(params)
}
