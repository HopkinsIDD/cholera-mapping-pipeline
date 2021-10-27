
#' @title Automate generation of config files
#' @name automate_mapping_config
#' @description Automate generation of model configuration files from a dataframe of parameters. Generates one config per row in the p dataframe.
#'
#' @param p dataframe with config parameters, one config per row
#' @param covariate_names character vector with names of covariates to include in the model
#' @param countries_names character vector with country codes to include in the model
#' @param countries_ids character vector with country location ids to include in the model
#'
#' @return string with yaml config file 
#' @export
automate_mapping_config <- function(p, covariate_names, countries_names, countries_ids){
  
  map_name <- paste0(paste(countries_names, collapse="_"), "_", lubridate::year(as.Date(p$start_time)), "_", lubridate::year(as.Date(p$end_time)), "_", p$res_space, "km")

  cat(paste0(
    "name: '", map_name, "'\n",
    "countries: ['", paste(countries_ids, collapse="','"), "']\n",
    "countries_name: ['", paste(countries_names, collapse="','"), "']\n",
    "aoi: '", p$aoi, "'\n",
    "res_space: ", p$res_space, "\n",
    "res_time: '", p$res_time, "'\n",
    "smoothing_period: ", p$smoothing_period, "\n",
    "case_definition: '", p$case_definition, "'\n",
    "start_time: '", as.Date(p$start_time), "'\n",
    "end_time: '", as.Date(p$end_time), "'\n",
    "covariate_choices: ['", paste(covariate_names, collapse="','"), "']\n",
    "data_source: '", p$data_source, "'\n",
    "ovrt_metadata_table: no\n",
    "ingest_covariates: ", p$ingest_covariates, "\n",
    "ingest_new_covariates: no\n",
    "covar_warmup: ", p$covar_warmup, "\n",
    "censoring: ", p$censoring, "\n",
    "aggregate: ", p$aggregate, "\n",
    "time_effect: ", p$time_effect, "\n",
    "time_effect_autocorr: ", p$time_effect_autocorr, "\n",
    "beta_sigma_scale: ", p$beta_sigma_scale, "\n",
    "sigma_eta_scale: ", p$sigma_eta_scale, "\n",
    "tfrac_thresh: ", p$tfrac_thresh, "\n",
    "set_tfrac: ", p$set_tfrac, "\n",
    "stan:\n",
    "  ncores: ", p$ncores, "\n",
    "  model: '", p$model, ".stan'\n",
    "  niter: ", p$niter, "\n",
    "  recompile: ", p$recompile, "\n"
  ))
}



#' @title Automate generation of config files -- just for the Dec 2021 runs
#' @name auto_config_Dec2021
#' @description Automate generation of model configuration files from a dataframe of parameters. Generates one config per row in the p dataframe.
#'
#' @param p dataframe with config parameters, one config per row
#' @param covariate_names character vector with names of covariates to include in the model
#' @param countries_names character vector with country codes to include in the model
#' @param countries_ids character vector with country location ids to include in the model
#'
#' @return string with yaml config file 
#' @export
auto_config_Dec2021 <- function(p, covariate_names, countries_names, countries_ids){
  
  map_name <- paste0(paste(countries_names, collapse="_"), "_", lubridate::year(as.Date(p$start_time)), "_", lubridate::year(as.Date(p$end_time)), "_", p$res_space, "km")

  cat(paste0(
    "name: '", map_name, "'\n",
    "countries: ['", paste(countries_ids, collapse="','"), "']\n",
    "countries_name: ['", paste(countries_names, collapse="','"), "']\n",
    "OCs: ", "\n",
    "aoi: '", p$aoi, "'\n",
    "res_space: ", p$res_space, "\n",
    "res_time: '", p$res_time, "'\n",
    "smoothing_period: ", p$smoothing_period, "\n",
    "case_definition: '", p$case_definition, "'\n",
    "start_time: '", as.Date(p$start_time), "'\n",
    "end_time: '", as.Date(p$end_time), "'\n",
    "covariate_choices: ['", paste(covariate_names, collapse="','"), "']\n",
    "data_source: '", p$data_source, "'\n",
    "ovrt_metadata_table: no\n",
    "ingest_covariates: ", p$ingest_covariates, "\n",
    "ingest_new_covariates: no\n",
    "warmup: ", p$warmup, "\n",
    "covar_warmup: ", p$covar_warmup, "\n",
    "censoring: ", p$censoring, "\n",
    "aggregate: ", p$aggregate, "\n",
    "use_weights: ", p$use_weights, "\n",
    "time_effect: ", p$time_effect, "\n",
    "time_effect_autocorr: ", p$time_effect_autocorr, "\n",
    "beta_sigma_scale: ", p$beta_sigma_scale, "\n",
    "sigma_eta_scale: ", p$sigma_eta_scale, "\n",
    "stan:\n",
    "  ncores: ", p$ncores, "\n",
    "  model: '", p$model, ".stan'\n",
    "  niter: ", p$niter, "\n",
    "  recompile: ", p$recompile, "\n"
  ))
}