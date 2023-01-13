
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

  unspecified_parameter_check <- function(param = NULL, null_check = is.null(param)){
    if(null_check){return(TRUE)}

    if(is.null(param) | identical(param, character(0)) | (stringr::str_count(param, " ") == nchar(param))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }

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
    "data_source: '", p$data_source, "'\n",
    "ovrt_metadata_table: no\n",
    { if(unspecified_parameter_check(p$OCs)) "" 
      else paste0("OCs: '", p$OCs, "'\n") },
    { if(unspecified_parameter_check(p$taxonomy)) "" 
      else "taxonomy: 'taxonomy-working/working-entry1'\n" },
    { if(unspecified_parameter_check(covariate_names)) "covariate_choices:\n" 
      else paste0("covariate_choices: ['", paste(covariate_names, collapse="','"), "']\n") },
    "obs_model: ", ifelse(unspecified_parameter_check(p$obs_model), 1, p$obs_model), "\n",
    { if(unspecified_parameter_check(p$obs_model)) "od_param: NULL\n" 
      else if(as.numeric(p$obs_model) == 1) "od_param: NULL\n" 
      else paste0("od_param: ", p$od_param, "\n") },
    "time_effect: ", ifelse(unspecified_parameter_check(p$time_effect), "no", p$time_effect), "\n",
    "time_effect_autocorr: ", ifelse(unspecified_parameter_check(p$time_effect_autocorr), "no", p$time_effect_autocorr), "\n",
    "use_intercept: ", ifelse(unspecified_parameter_check(p$use_intercept), 0, p$use_intercept), "\n",
    { if(unspecified_parameter_check(p$covariate_transformations)) "" 
      else paste0("covariate_transformations: ", p$covariate_transformations, "\n") },
    "beta_sigma_scale: ", ifelse(unspecified_parameter_check(p$beta_sigma_scale), 1, p$beta_sigma_scale), "\n",
    "sigma_eta_scale: ", ifelse(unspecified_parameter_check(p$sigma_eta_scale), 1, p$sigma_eta_scale), "\n",
    "exp_prior: ", ifelse(unspecified_parameter_check(p$exp_prior), "no", p$exp_prior), "\n",
    "do_infer_sd_eta: ", ifelse(unspecified_parameter_check(p$do_infer_sd_eta), 0, p$do_infer_sd_eta), "\n",
    "do_zerosum_cnst: ", ifelse(unspecified_parameter_check(p$do_zerosum_cnst), 0, p$do_zerosum_cnst), "\n",
    { if(unspecified_parameter_check(p$use_weights)) "" 
      else paste0("use_weights: ", p$use_weights, "\n") },
    "covar_warmup: ", ifelse(unspecified_parameter_check(p$covar_warmup), "yes", p$covar_warmup), "\n",
    "warmup: ", ifelse(unspecified_parameter_check(p$warmup), "yes", p$warmup), "\n",
    "aggregate: ", ifelse(unspecified_parameter_check(p$aggregate), stop("Parameter aggregate should be specified"), p$aggregate), "\n",
    "tfrac_thresh: ", ifelse(unspecified_parameter_check(p$tfrac_thresh), 0, p$tfrac_thresh), "\n",
    "censoring: ", ifelse(unspecified_parameter_check(p$censoring), "no", p$censoring), "\n",
    "censoring_thresh: ", ifelse(unspecified_parameter_check(p$censoring_thresh), 0.95, p$censoring_thresh), "\n",
    "set_tfrac: ", ifelse(unspecified_parameter_check(p$set_tfrac), "", p$set_tfrac), "\n",
    "snap_tol: '", ifelse(unspecified_parameter_check(p$snap_tol), 7/365, p$snap_tol), "'\n",
    "use_pop_weight: ", ifelse(unspecified_parameter_check(p$use_pop_weight), "yes", p$use_pop_weight), "\n",
    "sfrac_thresh: ", ifelse(unspecified_parameter_check(p$sfrac_thresh), 1e-3, p$sfrac_thresh), "\n",
    "ingest_covariates: ", ifelse(unspecified_parameter_check(p$ingest_covariates), "no", p$ingest_covariates), "\n",
    "ingest_new_covariates: ", ifelse(unspecified_parameter_check(p$ingest_new_covariates), "no", p$ingest_new_covariates), "\n",
    "stan:\n",
    "  ncores: ", p$ncores, "\n",
    "  model: '", p$model, ".stan'\n",
    "  genquant: '", p$genquant, ".stan'\n",
    "  niter: ", p$niter, "\n",
    "  recompile: ", p$recompile, "\n"
  ))
}

