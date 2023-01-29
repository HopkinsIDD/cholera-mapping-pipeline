
#' @title Automate generation of config files
#' @name automate_mapping_config
#' @description Automate generation of model configuration files from a dataframe of parameters. Generates one config per row in the p dataframe.
#' @include setup_helpers.R
#' @param cholera_directory the cholera mapping directory 
#' @param p dataframe with config parameters, one config per row
#' @param covariate_names character vector with names of covariates to include in the model
#' @param countries_names character vector with country codes to include in the model
#' @param countries_ids character vector with country location ids to include in the model
#' @param config_fname the name of the config file that will be generated or checked and updated 
#' @return string with yaml config file 
#' @export
automate_mapping_config <- function(cholera_directory, p, covariate_names, countries_names, countries_ids, config_fname){
  
  ### First make the initial config file no matter if it's correct or not 
  map_name <- paste0(paste(countries_names, collapse="_"), "_", lubridate::year(as.Date(p$start_time)), "_", lubridate::year(as.Date(p$end_time)), "_", p$res_space, "km")

  this_script_path <- paste0(cholera_directory, '/', 'packages/taxdat/R/config_helpers.R')
  start = match("  non_loop_part <- paste0(", readLines(this_script_path))
  end = match("  ) #the end of non_loop_part", readLines(this_script_path))
  file.lines <- scan(this_script_path, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  non_loop_list <- stringr::str_replace_all(unlist(stringr::str_extract_all(file.lines, "[[A-Za-z]+[_]+]*[A-Za-z]+:")), ":", "")
  loopable_list <- names(p)[!names(p) %in% non_loop_list]

  non_loop_part <- paste0(
    "name: '", ifelse(unspecified_parameter_check(p$name), map_name, p$name), "'\n",
    "countries: ['", paste(countries_ids, collapse="','"), "']\n",
    "countries_name: ['", paste(countries_names, collapse="','"), "']\n",
    "aoi: '", p$aoi, "'\n",
    "res_space: ", p$res_space, "\n",
    "res_time: '", p$res_time, "'\n",
    "start_time: '", as.Date(p$start_time), "'\n",
    "end_time: '", as.Date(p$end_time), "'\n",
    "OCs: ", ifelse(unspecified_parameter_check(p$OCs), "", paste0("['", paste(p$OCs, collapse="','"), "']")), "\n",
    "covariate_choices: ", ifelse(unspecified_parameter_check(covariate_names), "", paste0("['", paste(covariate_names, collapse="','"), "']")), "\n",
    "aggregate: ", ifelse(unspecified_parameter_check(p$aggregate), stop("Parameter aggregate should be specified"), p$aggregate), "\n",
    "snap_tol: '", p$snap_tol, "'\n",
    "stan:\n",
    "  ncores: ", ifelse(unspecified_parameter_check(p$ncores), "", p$ncores), "\n",
    "  model: '", ifelse(unspecified_parameter_check(p$model), "", p$model), ".stan'\n",
    "  genquant: '", ifelse(unspecified_parameter_check(p$genquant), "", p$genquant), ".stan'\n",
    "  niter: ", ifelse(unspecified_parameter_check(p$niter), "", p$niter), "\n",
    "  recompile: ", ifelse(unspecified_parameter_check(p$recompile), "", p$recompile), "\n", 
    "file_names:\n",
    { if(unspecified_parameter_check(p$output_directory)) "" 
      else paste0("  output_directory: ", p$output_directory, "\n") },
    { if(unspecified_parameter_check(p$observations_filename)) "" 
      else paste0("  observations_filename: ", p$observations_filename, "\n") },
    { if(unspecified_parameter_check(p$covariate_filename)) "" 
      else paste0("  covariate_filename: ", p$covariate_filename, "\n") },
    { if(unspecified_parameter_check(p$stan_input_filename)) "" 
      else paste0("  stan_input_filename: ", p$stan_input_filename, "\n") },
    { if(unspecified_parameter_check(p$initial_values_filename)) "" 
      else paste0("  initial_values_filename: ", p$initial_values_filename, "\n") },
    { if(unspecified_parameter_check(p$stan_output_filename)) "" 
      else paste0("  stan_output_filename: ", p$stan_output_filename, "\n") },
    { if(unspecified_parameter_check(p$stan_genquant_filename)) "" 
      else paste0("  stan_genquant_filename: ", p$stan_genquant_filename, "\n") }, 
    { if(unspecified_parameter_check(p$country_data_report_filename)) "" 
      else paste0("  country_data_report_filename: ", p$country_data_report_filename, "\n") }, 
    { if(unspecified_parameter_check(p$data_comparison_report_filename)) "" 
      else paste0("  data_comparison_report_filename: ", p$data_comparison_report_filename, "\n") }
  ) #the end of non_loop_part

  combined <- paste0(non_loop_part, paste0(sapply(loopable_list, function(x){
    ifelse(unspecified_parameter_check(p[[x]]), "", paste0(x, ": ", p[[x]], "\n"))
  }), collapse = ""), collapse = "")

  sink(file = config_fname)
  cat(combined)
  sink()
  
  ### Check and update the config file 
  check_update_config(cholera_directory, config_fname)

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
