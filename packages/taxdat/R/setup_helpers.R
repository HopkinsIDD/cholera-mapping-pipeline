#' @include config_helpers.R file_name_functions.R run_stan_helpers.R
#' @title check_update_config
#' @description Check whether parameter values exist or are valid and then update them 
#' @param cholera_directory the cholera directory 
#' @param config_fname pathway to the config 
#' @param covariate_list_dir where to find the file that lists all the covariate names 
#' @return the actual config file 
#' @export
check_update_config <- function(cholera_directory, config_fname, covariate_list_dir = "Layers/covariate_dictionary.yml"){
  
  ### Read in the config file first 
  cholera_testing <- as.logical(Sys.getenv("CHOLERA_TESTING", "FALSE"))
  config_file <- yaml::read_yaml(config_fname)

  ### Use the default .yaml file to get the list of covariates 
  all_covariates <- c("", names(yaml::read_yaml(paste0(cholera_directory, '/', covariate_list_dir))))

  ### Make the check list 
  check_list <- get_all_config_options()

  ### The stan check
  iteration_params <- check_list[['stan']]
  updated_stan_parameters <- get_stan_parameters(config_file)
  iteration_unrelated <- updated_stan_parameters[!names(updated_stan_parameters) %in% iteration_params]
  config_file <- append(config_file[!names(config_file) %in% names(iteration_unrelated)], iteration_unrelated)
  
  config_file$stan <- lapply(iteration_params, function(x){
    updated_stan_parameters[[x]]
  })
  names(config_file$stan) <- iteration_params

  ### The general check 
  for(nm in names(check_list)){
    if(nm == "od_param"){
      config_file[[nm]] <- check_list[[nm]](config_file[["obs_model"]], config_file[[nm]])
    }else if(nm == "covariate_choices"){
      config_file[[nm]] <- check_list[[nm]](config_file[[nm]], all_covariates)
    }else if(is.function(check_list[[nm]])){
      config_file[[nm]] <- check_list[[nm]](config_file[[nm]])
    }else{
      config_file[[nm]] <- config_file[[nm]]
    }   

    if(!nm %in% names(config_file)){
      eval(parse(text = paste0("tmp_list <- list(", nm, " = NULL)")))
      config_file <- append(config_file, tmp_list, after = (match(nm, names(check_list))-1)) 
    }  
  }

  ### Reorder all the parameters 
  config_file <- config_file[names(check_list)]

  ### Remove certain optional parameters if they're not specified (taxonomy, covariate_transformations, use_weights)
  for(optional_names in c("taxonomy", "covariate_transformations", "use_weights")){
    if(is.null(config_file[[optional_names]])){config_file <- config_file[names(config_file) != optional_names]}
  }

  ### Get the _filenames 
  config_file$file_names <- as.list(get_filenames(config_file, cholera_directory, for_config = TRUE, short_names = TRUE))
  names(config_file$file_names) <- sapply(names(config_file$file_names), function(x){check_list$file_names[[x]]})

  ### Save the config file 
  yaml::write_yaml(config_file, config_fname)

}

#' @include file_name_functions.R
#' @title check_aoi 
#' @description Checks whether the aoi is valid
#' @param aoi the aoi parameter in the config
#' @return aoi if valid
#' @export
check_aoi <- function(aoi) {
  cholera_testing <- as.logical(Sys.getenv("CHOLERA_TESTING", "FALSE"))
  if(cholera_testing){
    return(aoi)
  }else{
    return("raw")
  } 
}

#' @include file_name_functions.R
#' @title check_res_space
#' @description Checks whether the spatial resolution is valid
#' @param res_space the spatial resolution parameter in the config
#' @return res_space if valid
#' @export
check_res_space <- function(res_space) {
  if(is.null(res_space)){
    stop("The res_space parameter should not be blank because there is no default")
  }else{
    if(is.numeric(res_space)){
      return(res_space)
    }else{
      stop("The res_space parameter is not in the numeric type")
    }
  }
}

#' @include file_name_functions.R
#' @title check_time
#' @description Checks whether the start_time/end_time is valid
#' @param time the start_time/end_time parameter in the config
#' @return start_time/end_time if valid
#' @export
check_time <- function(time) {
  defaultW <- getOption("warn") 
  options(warn = -1) 
 
  if(is.null(time)){
    options(warn = defaultW)
    stop("The start_time/end_time parameter should not be blank because there is no default")
  }else if(is.na(time)){
    options(warn = defaultW)
    stop("The start_time/end_time parameter should not be blank because there is no default")
  }else{
    tryCatch(
      expr = {
        time_updated <- lubridate::ymd(time)
      },
      error = function(e){
        message('Caught an error, the start_time/end_time parameter cannot be easily transformed into "Date" variable. ')
        print(e)
      }
    )    

    if(inherits(time_updated, 'Date') & !is.na(time_updated)){
      options(warn = defaultW)
      return(as.character(time))
    }else{
      options(warn = defaultW)
      stop("The start_time/end_time parameter is not in the Date type")
    }
  }
}

#' @include file_name_functions.R
#' @title check_data_source
#' @description Checks whether the data_source is valid
#' @param data_source the data_source parameter in the config
#' @return data_source if valid
#' @export
check_data_source <- function(data_source) {
  if(is.null(data_source)){
    return("sql")
  }else if(tolower(data_source) == "sql"){
    return("sql")
  }else if(tolower(data_source) == "api"){
    warning("The API is not currently functional for mapping pipeline purposes, use with caution. ")
    return(data_source)
  }else{
    warning("The data_source parameter can only be either api or sql, now using the default. ")
    return("sql")
  }
}

#' @include file_name_functions.R
#' @title check_ovrt_metadata_table
#' @description Checks whether the ovrt_metadata_table is valid
#' @param ovrt_metadata_table the ovrt_metadata_table parameter in the config
#' @return ovrt_metadata_table if valid
#' @export
check_ovrt_metadata_table <- function(ovrt_metadata_table) {
  if(is.null(ovrt_metadata_table)){return("no")}
  if(!tolower(ovrt_metadata_table) %in% c("yes", "no")){
    if(!tolower(ovrt_metadata_table) %in% c("true", "false")){
      stop("The ovrt_metadata_table parameter has to be true/false or yer/no. ")
    }
    return(as.logical(ovrt_metadata_table))
  }
  return(ovrt_metadata_table)
}

#' @include file_name_functions.R
#' @title check_taxonomy
#' @description Checks whether the taxonomy is valid
#' @param taxonomy the taxonomy parameter in the config
#' @return taxonomy if valid
#' @export
check_taxonomy <- function(taxonomy) {
  if(is.null(taxonomy)){
    return(NULL)
  }else if(!is.null(taxonomy)){
    if(!identical(taxonomy, "taxonomy-working/working-entry1")){
      warning('The taxonomy parameter can only be "taxonomy-working/working-entry1", now using this default. ')
    }
  }
  
  return("taxonomy-working/working-entry1")
}

#' @include file_name_functions.R
#' @title check_obs_model
#' @description Checks whether the obs_model is valid
#' @param obs_model the obs_model parameter in the config
#' @return obs_model if valid
#' @export
check_obs_model <- function(obs_model) {
  if(is.null(obs_model)){
    warning("The obs_model parameter cannot be null, now returning the default value 1, meaning the poisson observation model will be used in Stan. ")
    return(1)
  }

  tryCatch(
    expr = {
      updated_obs_model <- as.numeric(obs_model)
    },
    error = function(e){
      message('Caught an error, the obs_model parameter cannot be easily transformed into "numeric" variable. ')
      print(e)
    }
  )    

  if(is.na(updated_obs_model)){
    warning("The obs_model parameter can only be 1, 2, or 3, now returning the default value 1, meaning the poisson observation model will be used in Stan. ")
    return(1)
  }else if(!updated_obs_model %in% 1:3){
    warning("The obs_model parameter can only be 1, 2, or 3, now returning the default value 1, meaning the poisson observation model will be used in Stan. ")
    return(1)
  }

  return(updated_obs_model)
}

#' @include file_name_functions.R
#' @title check_od_param
#' @description Checks whether the od_param is valid
#' @param obs_model the obs_model parameter in the config
#' @param od_param the od_param parameter in the config
#' @return od_param if valid
#' @export
check_od_param <- function(obs_model, od_param) {
  if(is.null(od_param)){return(NULL)}
  if(as.numeric(obs_model) == 2 | as.numeric(obs_model) == 3){
    tryCatch(
      expr = {
        updated_od_param <- as.numeric(od_param)
      },
      error = function(e){
        message('Caught an error, the od_param parameter cannot be easily transformed into "numeric" variable. ')
        print(e)
      }
    )    
    return(updated_od_param)
  }

  return(NULL)
}

#' @include file_name_functions.R
#' @title check_censoring
#' @description Checks whether the censoring is valid
#' @param censoring the obs_model parameter in the config
#' @return censoring if valid
#' @export
check_censoring <- function(censoring) {
  if(is.null(censoring)){
    return("no")
  }
  if(!tolower(censoring) %in% c("yes", "no")){
    if(!tolower(censoring) %in% c("true", "false")){
      stop("The censoring parameter has to be true/false or yer/no. ")
    }
    return(as.logical(censoring))
  }
  return(censoring)
}

#' @include file_name_functions.R
#' @title check_censoring_thresh
#' @description Checks whether the censoring_thresh is valid
#' @param censoring_thresh the obs_model parameter in the config
#' @return censoring_thresh if valid
#' @export
check_censoring_thresh <- function(censoring_thresh) {
  if(is.null(censoring_thresh)){return(0.95)}

  tryCatch(
    expr = {
      updated_censoring_thresh <- as.numeric(censoring_thresh)
    },
    error = function(e){
      message('Caught an error, the censoring_thresh parameter cannot be easily transformed into "numeric" variable. ')
      print(e)
    }
  )    

  if(!is.na(updated_censoring_thresh)){
    if(updated_censoring_thresh > 1){
      warning("The censoring_thresh parameter is bigger than 1, now returning 1. ")
      return(1)
    }else if(updated_censoring_thresh < 0){
      stop("The censoring_thresh parameter should not be under 0")
    }else{
      return(updated_censoring_thresh)
    }
    
  }else{
    return(0.95)
  }
  
}

#' @include file_name_functions.R
#' @title check_use_pop_weight
#' @description Checks whether the use_pop_weight is valid
#' @param use_pop_weight the use_pop_weight parameter in the config
#' @return use_pop_weight if valid
#' @export
check_use_pop_weight <- function(use_pop_weight) {
  if(is.null(use_pop_weight)){return("yes")}
  if(!tolower(use_pop_weight) %in% c("yes", "no")){
    if(!tolower(use_pop_weight) %in% c("true", "false")){
      stop("The use_pop_weight parameter has to be true/false or yer/no. ")
    }
    return(as.logical(use_pop_weight))
  }
  return(use_pop_weight)
}

#' @include file_name_functions.R
#' @title check_ingest_covariates
#' @description Checks whether the ingest_covariates is valid
#' @param ingest_covariates the ingest_covariates parameter in the config
#' @return ingest_covariates if valid
#' @export
check_ingest_covariates <- function(ingest_covariates) {
  if(is.null(ingest_covariates)){return("no")}
  if(!tolower(ingest_covariates) %in% c("yes", "no")){
    if(!tolower(ingest_covariates) %in% c("true", "false")){
      stop("The ingest_covariates parameter has to be true/false or yer/no. ")
    }
    return(as.logical(ingest_covariates))
  }
  return(ingest_covariates)
}

#' @include file_name_functions.R
#' @title check_ingest_new_covariates
#' @description Checks whether the ingest_new_covariates is valid
#' @param ingest_new_covariates the ingest_new_covariates parameter in the config
#' @return ingest_new_covariates if valid
#' @export
check_ingest_covariates <- function(ingest_new_covariates) {
  if(is.null(ingest_new_covariates)){return("no")}
  if(!tolower(ingest_new_covariates) %in% c("yes", "no")){
    if(!tolower(ingest_new_covariates) %in% c("true", "false")){
      stop("The ingest_new_covariates parameter has to be true/false or yer/no. ")
    }
    return(as.logical(ingest_new_covariates))
  }
  return(ingest_new_covariates)
}

#' @include file_name_functions.R

#' @title Check time resolution
#' @description Checks whether the time resolution input is valid
#'
#' @param res_time the time resolution of the model
#'
#' @return res_time if valid
#' @export
check_time_res <- function(res_time) {
  
  err_message <- "Time resolution should be specified as a string in the from '<n> <period>' where n is the number of time units of period <period>. Example: '1 year'."
  
  parts <- stringr::str_split(res_time, " ")[[1]]
  if (length(parts) != 2)
    stop(err_message)
  
  n_units <- as.numeric(parts[1])
  if (is.na(n_units))
    stop(paste(err_message, "[Units not valid]"))
  
  allowed_time_periods <- c("month", "year")
  allowed_time_periods <- c(allowed_time_periods, paste0(allowed_time_periods, "s"))
  time_period <- parts[2]
  if (!(time_period %in% allowed_time_periods))
    stop(paste(err_message, "[Time period not valid]"))
  
  # Standardize to always have an s at the end
  if (!stringr::str_detect(res_time, "s$")) {
    res_time <- stringr::str_c(res_time, "s")
  }
  
  cat("-- Running with valid time resolution:", res_time, "\n")
  return(res_time)
}

#' @title Check the number of grid-level random effect time slices (formerly, "smoothing period")
#' @description Checks whether the smoothing period (number of time slices of the spatial random effects) is 1. Default is 1.
#'
#' @param grid_rand_effects_N number of grid-level random effect time slices
#'
#' @return grid_rand_effects_N if valid
#' @export
check_grid_rand_effects_N <- function(grid_rand_effects_N) {
  if(is.null(grid_rand_effects_N)){
    grid_rand_effects_N <- 1
    cat("-- By default, running with grid_rand_effects_N: 1 \n'")
  } else if (!(is.numeric(grid_rand_effects_N))){
    stop("Grid_rand_effects_N must be numeric.")
  } else if(grid_rand_effects_N != 1){
    stop("Grid_rand_effects_N must be 1. Other values still need to be tested and validated.")
  } else{
    cat("-- Running with grid_rand_effects_N:", grid_rand_effects_N, "\n", sep = "")
  }
  
  return(grid_rand_effects_N)
}

#' @title Check case definition
#' @description Checks whether the column name for case definition is valid
#'
#' @param case_column the name of the case column to use in the database
#'
#' @return case column if valid
#' @export
check_case_definition <- function(case_col) {
  if (!(case_col %in% c("suspected", "confirmed")))
    stop("Cholera case definition not in allowed options (suspected or confirmed).")
  
  cat("-- Running with valid case defitinion: ", case_col, "\n", sep = "")
  return(case_col)
}


#' @title Check modeling date range
#' @description Checks if input date ranges are correct
#'
#' @param start_time the start time of the modeling time rante
#' @param end_time the end time of the modeling time range
#' @param time_change_func function to change dates to the aggregation level
#' determined by the time resolution of the model
#' @param aggregate_to_start function to aggregate dates to the start date of modeling time slices
#' @param aggregate_to_end function to aggregate dates to the end of the modeling time slices
#'
#' @return return
#' @export
check_model_date_range <- function(start_time,
                                   end_time,
                                   time_change_func,
                                   aggregate_to_start,
                                   aggregate_to_end) {
  
  if(any(c(!lubridate::is.Date(start_time), !lubridate::is.Date(end_time))))
    stop("Start and end times need to be in date format")
  
  if (start_time > end_time)
    stop("Start time is after end time")
  
  model_TL <- aggregate_to_start(time_change_func(start_time))
  model_TR <- aggregate_to_end(time_change_func(end_time))
  
  if (start_time != model_TL | end_time != model_TR) {
    warning("--- User-defined modeling time range does not cover the whole range",
            "as defined with the time resoltion: \n user-defined range:\t",
            start_time, " - ", end_time, "\n ",
            "model range:\t\t", model_TL, " - ", model_TR)
  } else {
    cat("---- Running with model date range:", as.character(model_TL),
        "-", as.character(model_TR), "\n")
  }
}

#' @title Check covariate choices
#' @description Verifies whether user-defined covariate choices are in the available
#' set
#'
#' @param covar_choices vector of user-defined covariate choices
#' @param available_choices vector of available covaraites choices
#'
#' @return if valid the vector of covariate choices
#' @export
check_covariate_choices <- function(covar_choices,
                                    available_choices){
  
  if (any(purrr::map_lgl(covar_choices, ~ !(. %in% available_choices))))
    stop("Covariate choices [", stringr::str_c(setdiff(covar_choices, available_choices), collapse = ", "), "] not available. \n",
         "Choose among: [", stringr::str_c(available_choices, collapse = ", "),"]")
  
  cat("---- Running with covariates:", stringr::str_c(covar_choices, collapse = ", "), "\n")
  
  return(covar_choices)
}


#' @title Check stan model
#' @description Checks whether the stan model file exists
#'
#' @param stan_model_path the path to the stan model to use
#' @param stan_dir the directory to the directory with all available stan models
#'
#' @return if valid the stan model path
#' @export
check_stan_model <- function(stan_model_path,
                             stan_dir) {
  
  if (!file.exists(stan_model_path))
    stop("Could not find stan model. Choose among:\n",
         stringr::str_c(dir(stan_dir), collapse = "\n"))
  cat("---- Running with stan model:", stringr::str_replace(stan_model_path, stan_dir, ""), "\n")
  
  return(stan_model_path)
}

#' @title Check set tfrac
#' @description Checks whether tfrac should be set to 1. If not set_tfrac is specified, the default is FALSE.
#'
#' @param set_tfrac set_tfrac parameter
#'
#' @return if valid the stan model path
#' @export
check_set_tfrac <- function(set_tfrac) {
  if (!is.null(set_tfrac)) {
    if (!is.logical(set_tfrac)){
      stop("set_tfrac must be a logical value")
    }
    if (set_tfrac){
      cat("---- Non-censored observations will have tfrac = 1 \n")
    } else {
      cat("---- Non-censored observations will keep their original tfrac values \n")
    }
    
  } else{
    set_tfrac <- FALSE
    cat("---- By default, non-censored observations will keep their original tfrac values \n")
  }
  
  
  return(set_tfrac)
}


#' @title Check snap tolerance
#' @description Checks that snap_tol is valid, with a default of 7/365.
#' @param snap_tol
#'
#' @export
#'
check_snap_tol <- function(snap_tol,
                           res_time) {
  if (!is.null(snap_tol)) {
    if (snap_tol < 0) {
      stop("Cannot specifiy negative snap tolerance values")
    }
    if (snap_tol > 1) {
      warning("---- Running with a tfrac value larger than 1")
    } else {
      cat("---- Running with user-specified value of snap tolerance:", snap_tol, "[", res_time, "]\n")
    }
  } else {
    snap_tol <- 7/365
    cat("---- By default, running with value of snap tolerance 7/365:", snap_tol, "[", res_time, "]\n")
  }
  return(snap_tol)
}


#' @title Check tfrac threshold
#' @description Checks what tfrac threshold should be applied, with a default of 0.
#' @param tfrac_thresh tfrac_thresh parameter
#'
#' @return if valid the stan model path
#' @export
check_tfrac_thresh <- function(tfrac_thresh) {
  if (!is.null(tfrac_thresh)) {
    if (!is.numeric(tfrac_thresh) | tfrac_thresh < 0 | tfrac_thresh > 1){
      stop("tfrac_thresh must be a numeric value between 0 and 1")
    }
    
    cat("---- Observations with tfrac greater than ", tfrac_thresh, " will be kept \n")
    
  } else{
    tfrac_thresh <- 0
    cat("---- By default, observations with tfrac greater than ", tfrac_thresh, " will be kept \n")
  }
  
  return(tfrac_thresh)
}


#' @title Check aggregate
#' @description Checks what aggregate setting should be applied, with a default of true.
#'
#' @param aggregate_param aggregate parameter
#'
#' @return if valid the stan model path
#' @export
check_aggregate <- function(aggregate_param) {
  if (!is.null(aggregate_param)) {
    if (!is.logical(aggregate_param)){
      stop("aggregate parameter must be a logical value")
    }
    
    if (aggregate_param){
      cat("---- Observations will be aggregated \n")
    } else{
      cat("---- Observations will not be aggregated \n")
    }
    
    
  } else{
    aggregate_param <- TRUE
    cat("---- By default, observations will be aggregated \n")
  }
  
  return(aggregate_param)
}


#' @title check_sfrac_thresh
#' @param sfrac_thresh sfrac_thresh parameter
#'
#' @return
#' @export
#'
check_sfrac_thresh <- function(sfrac_thresh) {
  
  if (is.null(sfrac_thresh)) {
    cat("---- Sfrac thresh not specified, setting to default: 1e-3 \n")
    sfrac_thresh <- 1e-3
  }
  
  if (sfrac_thresh < 0 | sfrac_thresh > 1) {
    stop("---- sfract thresh cannot be < 0 or > 1, value passed: ", sfrac_thresh)
  }
  
  sfrac_thresh
}

#' @title Modeling time slices
#' @description Defines the modeling time slicez
#'
#' @param start_time modeling start date (in date format)
#' @param end_time   modeling end date (in date format)
#' @param res_time   modeling time resolution
#' @param time_change_func function to get the time unit of the date
#' @param aggregate_to_start function to get the first date of a time slice
#' @param aggregate_to_end function to get the last date of a time slices
#' @return a dataframe left and right bounds of the modeling time slices
#' @export
modeling_time_slices <- function(start_time,
                                 end_time,
                                 res_time,
                                 time_change_func,
                                 aggregate_to_start,
                                 aggregate_to_end) {
  
  left_bounds <- seq.Date(start_time, end_time, by = res_time)
  left_bounds <- aggregate_to_start(time_change_func(left_bounds))
  right_bounds <- aggregate_to_end(time_change_func(left_bounds))
  
  time_slices <- tibble::tibble(TL = left_bounds[left_bounds <= end_time],
                                TR = right_bounds[right_bounds <= end_time])
  
  cat("-- Model consists of", nrow(time_slices), "time slices of duration", res_time,":\n")
  print(time_slices)
  cat("\n")
  return(time_slices)
}


#' Title
#'
#' @param debug 
#'
#' @return
#' @export
#'
#' @examples
check_stan_debug <- function(debug) {
  if (is.null(debug)) {
    # Default behavior
    debug <- FALSE
  }
  
  if (!is.logical(debug)) {
    stop("Debug needs to be boolean.")
  }
  
  debug
}


#' Get all config options
#'
#' @return
#' @export
#'
get_all_config_options <- function() {
  config_options <- list(
    name = "no-check", 
    countries = "no-check", 
    countries_name = "no-check", 
    aoi = as.function(check_aoi), 
    res_space = as.function(check_res_space), 
    res_time = as.function(check_time_res), 
    grid_rand_effects_N = as.function(check_grid_rand_effects_N), 
    case_definition = as.function(check_case_definition), 
    start_time = as.function(check_time),   
    end_time = as.function(check_time),
    data_source = as.function(check_data_source), 
    ovrt_metadata_table = as.function(check_ovrt_metadata_table), 
    OCs = "no-check", 
    taxonomy = as.function(check_taxonomy),  
    covariate_choices = as.function(check_covariate_choices), 
    obs_model = as.function(check_obs_model), 
    od_param = as.function(check_od_param), 
    time_effect = "stan-check", 
    time_effect_autocorr = "stan-check", 
    use_intercept = "stan-check", 
    covariate_transformations = "no-check", 
    beta_sigma_scale = "stan-check", 
    sigma_eta_scale = "stan-check", 
    exp_prior = "stan-check", 
    do_infer_sd_eta = "stan-check", 
    do_zerosum_cnst = "stan-check", 
    use_weights = "stan-check", 
    use_rho_prior = "stan-check", 
    covar_warmup = "stan-check", 
    warmup = "stan-check", 
    aggregate = as.function(check_aggregate), 
    tfrac_thresh = as.function(check_tfrac_thresh), 
    censoring = as.function(check_censoring), 
    censoring_thresh = as.function(check_censoring_thresh), 
    set_tfrac = as.function(check_set_tfrac),
    snap_tol = as.function(check_snap_tol),
    use_pop_weight = as.function(check_use_pop_weight), 
    sfrac_thresh = as.function(check_sfrac_thresh), 
    ingest_covariates = as.function(check_ingest_covariates),
    ingest_new_covariates = as.function(check_ingest_covariates),
    stan = c("ncores", "model", "genquant", "niter", "recompile"), 
    file_names = list(output_directory = "output_directory", data = "observations_filename", covar = "covariate_filename", 
                      stan_input = "stan_input_filename", initial_values = "initial_values_filename", 
                      stan_output = "stan_output_filename", stan_genquant = "stan_genquant_filename", 
                      country_data_report_filename = "country_data_report_filename", 
                      data_comparison_report_filename = "data_comparison_report_filename")
  )
  config_options
}

