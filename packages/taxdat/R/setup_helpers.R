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

  cat("-- Running with valid case defitinion: '", case_col, "'\n", sep = "")
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
#' @description Checks whether the user-specified value of tfrac is valid
#'
#' @param config the config
#'
#' @return if valid the stan model path
#' @export
check_set_tfrac <- function(set_tfrac) {
  if (!is.null(set_tfrac)) {
    if (set_tfrac < 0) {
      stop("Cannot specifiy negative tfrac values")
    }
    if (set_tfrac > 1) {
      warning("-- Running with a tfrac value larger than 1")
    } else {
      cat("---- Running with user-specified value of tfrac:", set_tfrac, "\n")
    }
  }
  return(set_tfrac)
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


#' @title Make observations filename
#' @name make_observations_filename
#' @description Make string for observations Rdata file name
#'
#' @param cholera_directory cholera mapping directory
#' @param map_name map name
#' @return a string with the observation file name
#' @export
make_observations_filename <- function(cholera_directory,
                                       map_name) {
  paste(cholera_directory, "/Analysis/", "data/",
        map_name, '.preprocess', '.rdata', sep = '')
}


#' @title Make covariate filename
#' @name make_covar_filename
#' @description Make string for covariate Rdata file name
#'
#' @param cholera_directory cholera mapping directory
#' @param map_name map name
#' @param covariate_name_part name of covariate
#' @return a string with the covariate file name
#' @export
make_covar_filename <- function(cholera_directory,
                                map_name,
                                covariate_name_part) {
  paste(cholera_directory, "/Analysis/", "data/", map_name, ".",
        covariate_name_part, '.covar', '.rdata', sep = '')
}

#' @title Make Stan input filename
#' @name make_stan_input_filename
#' @description Make string for Stan input Rdata file name
#'
#' @param cholera_directory cholera mapping directory
#' @param map_name map name
#' @param covariate_name_part name of covariate
#' @param stan_model name of stan model
#' @param niter number of iterations
#' @return a string with the Stan input file name
#' @export
make_stan_input_filename <- function(cholera_directory,
                                     map_name,
                                     covariate_name_part,
                                     stan_model,
                                     niter) {
  paste(cholera_directory, "/Analysis/", "data/", map_name, '.',
        covariate_name_part, '.', stan_model, '.', niter, '.stan_input',
        '.rdata',sep='')
}

#' @title Make Stan output filename
#' @name make_stan_output_filename
#' @description Make string for Stan output Rdata file name
#'
#' @param cholera_directory cholera mapping directory
#' @param map_name map name
#' @param covariate_name_part name of covariate
#' @param stan_model name of stan model
#' @param niter number of iterations
#' @return a string with the Stan output file name
#' @export
make_stan_output_filename <- function(cholera_directory,
                                      map_name,
                                      covariate_name_part,
                                      stan_model,
                                      niter) {
  paste(cholera_directory, "/Analysis/", "data/", map_name, '.',
        covariate_name_part, '.', stan_model, '.', niter, '.stan_output',
        '.rdata',sep='')
}


#' @title Make map output filename
#' @name make_map_output_filename
#' @description Make string for map pdf file name
#'
#' @param cholera_directory cholera mapping directory
#' @param map_name map name
#' @param covariate_name_part name of covariate
#' @param stan_model name of stan model
#' @param niter number of iterations
#' @return a string with the map output file name
#' @export
make_map_output_filename <- function(cholera_directory,
                                     map_name,
                                     covariate_name_part,
                                     stan_model,
                                     niter) {
  paste(cholera_directory, "/Analysis/", "output/", map_name, '.',
        covariate_name_part, '.', stan_model, '.', niter, '.pdf', sep = '')
}

#' @title Make map name
#' @name make_map_name
#' @description Make string for map name used for all filenames
#'
#' @param config the configuration file
#' @param .f other functions to apply to the config to append to map name
#' @export
make_map_name <- function(config, .f = NULL) {
  
  # km by km resolution of analysis
  res_space <- as.numeric(config$res_space)
  # temporal resolution of analysis
  res_time <- suppressMessages(check_time_res(config$res_time))
  # Modeling start and end times
  start_time <- lubridate::ymd(config$start_time)
  end_time <- lubridate::ymd(config$end_time)
  # Suspected or confirmed cases
  suspected_or_confirmed <- suppressMessages(check_case_definition(config$case_definition))
  
  map_name <- paste(paste(config$countries_name, collapse = '-'),
                    stringr::str_replace(res_time, " ", "_"),
                    paste(start_time, end_time, sep = '-'),
                    paste(res_space, 'km', sep = ''),
                    suspected_or_confirmed,
                    sep = '_')
  
  if(!is.null(config$tfrac_thresh)) {
    map_name <- paste0(map_name, "_tfracthresh", config$tfrac_thresh)
  }
  
  if(!is.null(config$set_tfrac)) {
    map_name <- paste0(map_name, "_tfracset", config$set_tfrac)
  }
  
  if (!is.null(.f)){
    map_name <- paste(map_name, .f(config), sep = "_")
    # str_replace(last(str_split(opt$config, "/")[[1]]), "\\.yml", "")
  }
  return(map_name)
}