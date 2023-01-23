#' @include config_helpers.R file_name_functions.R run_stan_helpers.R
#' @title check_update_config
#' @description Check whether parameter values exist or are valid and then update them 
#' @param config_fname pathway to the config 
#' @return the actual config file 
#' @export
check_update_config <- function(config_fname){
  
  ### Read in the config file first 
  cholera_testing <- as.logical(Sys.getenv("CHOLERA_TESTING", "FALSE"))
  config_file <- yaml::read_yaml(config_fname)
  
  ### Make the check list 
  check_list <- list(
    name = "no-check", 
    countries = "no-check", 
    countries_name = "no-check", 
    aoi = function(param){
        if(cholera_testing){return(param)}
        return("raw")
      }, 
    res_space = "no-check", 
    res_time = as.function(check_time_res), 
    grid_rand_effects_N = as.function(check_grid_rand_effects_N), 
    case_definition = as.function(check_case_definition), 
    start_time = function(param){
                if(stringr::str_count(param, " ") == nchar(param)){stop("The start_time parameter should not be blank because there is no default")}
                return(param)
              },   
    end_time = function(param){
              if(stringr::str_count(param, " ") == nchar(param)){stop("The end_time parameter should not be blank because there is no default")}
              return(param)
            }, 
    data_source = function(param){
                if(stringr::str_count(param, " ") == nchar(param)){return("sql")}
                return(param)
              }, 
    ovrt_metadata_table = function(param){
                        if(stringr::str_count(param, " ") == nchar(param)){return("no")}
                        return(param)
                      }, 
    OCs = "no-check", 
    taxonomy = function(param){
              if(stringr::str_count(param, " ") == nchar(param)){return("taxonomy-working/working-entry1")}
              return(param)
            },  
    covariate_choices = as.function(check_covariate_choices), 
    obs_model = function(param){
              if(!as.numeric(param) %in% 1:3){return(1)}
              return(as.numeric(param))
            }, 
    od_param = function(param1, param2){
              if(as.numeric(param1) == 2 | as.numeric(param1) == 3){return(as.numeric(param2))}
              return(NULL)
            }, 
    time_effect = "stan-check", 
    time_effect_autocor = "stan-check", 
    use_intercept = "stan-check", 
    covariate_transformations = "no-check", 
    beta_sigma_scale = "stan-check", 
    sigma_eta_scale = "stan-check", 
    exp_prior = "stan-check", 
    do_infer_sd_eta = "stan-check", 
    do_zerosum_cnst = "stan-check", 
    use_weights = "stan-check", 
    covar_warmup = "stan-check", 
    warmup = "stan-check", 
    aggregate = as.function(check_aggregate), 
    tfrac_thresh = as.function(check_tfrac_thresh), 
    censoring = function(param){
              if(stringr::str_count(param, " ") == nchar(param)){return("no")}
              return(param)
            }, 
    censoring_thresh = function(param){
                      if(stringr::str_count(param, " ") == nchar(param)){return(0.95)}
                      return(as.numeric(param))
                    }, 
    set_tfrac = as.function(check_set_tfrac),
    snap_tol = as.function(check_snap_tol),
    use_pop_weight = function(param){
                    if(stringr::str_count(param, " ") == nchar(param)){return("yes")}
                    return(param)
                  }, 
    sfrac_thresh = as.function(check_sfrac_thresh), 
    ingest_covariates = function(param){
                      if(stringr::str_count(param, " ") == nchar(param)){return("no")}
                      return(param)
                    },
    ingest_new_covariates = function(param){
                          if(stringr::str_count(param, " ") == nchar(param)){return("no")}
                          return(param)
                        },
    stan = function(param_list){
          param_list[["ncores"]] <- 4
          param_list[["model"]] <- ifelse(stringr::str_count(param_list[["model"]], " ") == nchar(param_list[["model"]]), 
                                          "mapping_model_inference.stan", param_list[["model"]])
          param_list[["genquant"]] <- ifelse(stringr::str_count(param_list[["genquant"]], " ") == nchar(param_list[["genquant"]]), 
                                            "mapping_model_generate.stan", param_list[["genquant"]])
          param_list[["niter"]] <- ifelse(stringr::str_count(param_list[["niter"]], " ") == nchar(param_list[["niter"]]), 
                                          2000, as.numeric(param_list[["niter"]]))
          param_list[["recompile"]] <- ifelse(stringr::str_count(param_list[["recompile"]], " ") == nchar(param_list[["recompile"]]), 
                                              "yes", param_list[["recompile"]])
          return(param_list)
        }
  )

  ### First make sure it has all the parameters and then update the values 
  for(nm in names(check_list)){
    if(taxdat::unspecified_parameter_check(config_file[[nm]])){
      eval(parse(text = paste0("tmp_list <- list(", nm, " = '')")))
      config_file <- append(config_file, tmp_list, after = match(nm, names(check_list)))
    } 

    if(nm == "od_param"){
      config_file[[nm]] <- check_list[[nm]](config_file[["obs_model"]], config_file[[nm]])
    }else if(is.function(check_list[[nm]])){
      config_file[[nm]] <- check_list[[nm]](config_file[[nm]])
    }else if(check_list[[nm]] != "no-check" & check_list[[nm]] != "stan-check"){
      config_file[[nm]] <- check_list[[nm]]
    }     

    if(identical(config_file[[nm]], '')){config_file[[nm]] <- NULL}             
  }

  ### The stan check
  config_file <- get_stan_parameters(config_file)

  ### Reorder all the parameters 
  names(config_file) <- names(check_list)

  ### Remove certain optional parameters if they're not specified (taxonomy, covariate_transformations, use_weights, set_tfrac)
  for(param in c("taxonomy", "covariate_transformations", "use_weights", "set_tfrac")){
    if(taxdat::unspecified_parameter_check(config_file[[param]])){
      rm(param, envir = config_file)
    }
  }

  ### Save the config file 
  yaml::write_yaml(config_file, config_fname)

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
