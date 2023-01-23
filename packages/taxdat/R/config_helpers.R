
#' @title Automate generation of config files
#' @name automate_mapping_config
#' @description Automate generation of model configuration files from a dataframe of parameters. Generates one config per row in the p dataframe.
#'
#' @param p dataframe with config parameters, one config per row
#' @param covariate_names character vector with names of covariates to include in the model
#' @param countries_names character vector with country codes to include in the model
#' @param countries_ids character vector with country location ids to include in the model
#' @param config_fname the name of the config file that will be generated or checked and updated 
#' @param check_existing_config whether just check an existing config file or make a new one 
#' @return string with yaml config file 
#' @export
automate_mapping_config <- function(p, covariate_names, countries_names, countries_ids, config_fname, check_existing_config){
  
  if(!check_existing_config){
    ### First make the initial config file no matter if it's correct or not 
    map_name <- paste0(paste(countries_names, collapse="_"), "_", lubridate::year(as.Date(p$start_time)), "_", lubridate::year(as.Date(p$end_time)), "_", p$res_space, "km")

    sink(file = config_fname)
    cat(paste0(
      "name: '", ifelse(unspecified_parameter_check(p$name), map_name, p$name), "'\n",
      "countries: ['", paste(countries_ids, collapse="','"), "']\n",
      "countries_name: ['", paste(countries_names, collapse="','"), "']\n",
      "country_data_report_filename: '", p$country_data_report_filename, "'\n",
      "aoi: '", p$aoi, "'\n",
      "res_space: ", p$res_space, "\n",
      "res_time: '", p$res_time, "'\n",
      { if(unspecified_parameter_check(p$grid_rand_effects_N)) "" 
        else paste0("grid_rand_effects_N: ", p$grid_rand_effects_N, "\n") },
      { if(unspecified_parameter_check(p$case_definition)) "" 
        else paste0("case_definition: '", p$case_definition, "'\n") },
      "start_time: '", as.Date(p$start_time), "'\n",
      "end_time: '", as.Date(p$end_time), "'\n",
      { if(unspecified_parameter_check(p$data_source)) "" 
        else paste0("data_source: '", p$data_source, "'\n") },
      { if(unspecified_parameter_check(p$ovrt_metadata_table)) "" 
        else paste0("ovrt_metadata_table: ", p$ovrt_metadata_table, "\n") },
      { if(unspecified_parameter_check(p$OCs)) "" 
        else paste0("OCs: ['", paste(p$OCs, collapse="','"), "']\n") },
      { if(unspecified_parameter_check(p$taxonomy)) "" 
        else paste0("taxonomy: '", p$taxonomy, "'\n") },
      { if(unspecified_parameter_check(covariate_names)) "" 
        else paste0("covariate_choices: ['", paste(covariate_names, collapse="','"), "']\n") },
      { if(unspecified_parameter_check(p$obs_model)) "" 
        else paste0("obs_model: ", p$obs_model, "\n") },
      { if(unspecified_parameter_check(p$od_param)) "" 
        else paste0("od_param: ", p$od_param, "\n") },
      { if(unspecified_parameter_check(p$time_effect)) "" 
        else paste0("time_effect: ", p$time_effect, "\n") },
      { if(unspecified_parameter_check(p$time_effect_autocorr)) "" 
        else paste0("time_effect_autocorr: ", p$time_effect_autocorr, "\n") },
      { if(unspecified_parameter_check(p$use_intercept)) "" 
        else paste0("use_intercept: ", p$use_intercept, "\n") },
      { if(unspecified_parameter_check(p$covariate_transformations)) "" 
        else paste0("covariate_transformations: ", p$covariate_transformations, "\n") },
      "beta_sigma_scale: ", ifelse(unspecified_parameter_check(p$beta_sigma_scale), "", p$beta_sigma_scale), "\n",
      "sigma_eta_scale: ", ifelse(unspecified_parameter_check(p$sigma_eta_scale), "", p$sigma_eta_scale), "\n",
      "exp_prior: ", ifelse(unspecified_parameter_check(p$exp_prior), "", p$exp_prior), "\n",
      "do_infer_sd_eta: ", ifelse(unspecified_parameter_check(p$do_infer_sd_eta), "", p$do_infer_sd_eta), "\n",
      "do_zerosum_cnst: ", ifelse(unspecified_parameter_check(p$do_zerosum_cnst), "", p$do_zerosum_cnst), "\n",
      { if(unspecified_parameter_check(p$use_weights)) "" 
        else paste0("use_weights: ", p$use_weights, "\n") },
      "covar_warmup: ", ifelse(unspecified_parameter_check(p$covar_warmup), "", p$covar_warmup), "\n",
      "warmup: ", ifelse(unspecified_parameter_check(p$warmup), "", p$warmup), "\n",
      "aggregate: ", ifelse(unspecified_parameter_check(p$aggregate), stop("Parameter aggregate should be specified"), p$aggregate), "\n",
      "tfrac_thresh: ", ifelse(unspecified_parameter_check(p$tfrac_thresh), "", p$tfrac_thresh), "\n",
      "censoring: ", ifelse(unspecified_parameter_check(p$censoring), "", p$censoring), "\n",
      "censoring_thresh: ", ifelse(unspecified_parameter_check(p$censoring_thresh), "", p$censoring_thresh), "\n",
      { if(unspecified_parameter_check(p$set_tfrac)) "" 
        else paste0("set_tfrac: ", p$set_tfrac, "\n") },
      "snap_tol: '", ifelse(unspecified_parameter_check(p$snap_tol), "", p$snap_tol), "'\n",
      "use_pop_weight: ", ifelse(unspecified_parameter_check(p$use_pop_weight), "", p$use_pop_weight), "\n",
      "sfrac_thresh: ", ifelse(unspecified_parameter_check(p$sfrac_thresh), "", p$sfrac_thresh), "\n",
      "ingest_covariates: ", ifelse(unspecified_parameter_check(p$ingest_covariates), "", p$ingest_covariates), "\n",
      "ingest_new_covariates: ", ifelse(unspecified_parameter_check(p$ingest_new_covariates), "", p$ingest_new_covariates), "\n",
      "stan:\n",
      "  ncores: ", ifelse(unspecified_parameter_check(p$ncores), "", p$ncores), "\n",
      "  model: '", ifelse(unspecified_parameter_check(p$model), "", p$model), ".stan'\n",
      "  genquant: '", ifelse(unspecified_parameter_check(p$genquant), "", p$genquant), ".stan'\n",
      "  niter: ", ifelse(unspecified_parameter_check(p$niter), "", p$niter), "\n",
      "  recompile: ", ifelse(unspecified_parameter_check(p$recompile), "", p$recompile), "\n"
    ))
    sink()
  }

  ### Check and update the config file 
  taxdat::check_update_config(config_fname)

}

#' @title unspecified_parameter_check
#' @name unspecified_parameter_check
#' @description check if there's one parameter really unspecified 
#' @param param parameter to check
#' @param null_check initial check to pass to enable the function if the parameter doesn't exist at all 
#' @return T/F
#' @export
unspecified_parameter_check <- function(param = NULL, null_check = is.null(param)){
    if(null_check){return(TRUE)}

    if(identical(param, character(0)) | (stringr::str_count(param, " ") == nchar(param))){
      return(TRUE)
    }else{
      return(FALSE)
    }
}
