#' @title Check time resolution
#' @description Checks whether the time resolution input is valid
#'
#' @param res_time
#'
#' @return res_time if valid
checkTimeRes <- function(res_time) {
  err_message <- "Time resolution should be specified as a string in the from '<n> <period>' where n is the number of time units of period <period>. Example: '1 year'."

  parts <- str_split(res_time, " ")[[1]]
  if (length(parts) != 2) {
    stop(err_message)
  }

  n_units <- as.numeric(parts[1])
  if (is.na(n_units)) {
    stop(paste(err_message, "[Units not valid]"))
  }

  allowed_time_periods <- c("month", "year")
  allowed_time_periods <- c(allowed_time_periods, paste0(allowed_time_periods, "s"))
  time_period <- parts[2]
  if (!(time_period %in% allowed_time_periods)) {
    stop(paste(err_message, "[Time period not valid]"))
  }

  # Standardize to always have an s at the end
  if (!str_detect(res_time, "s$")) {
    res_time <- str_c(res_time, "s")
  }

  cat("-- Running with valid time resolution:", res_time, "\n")
  return(res_time)
}

#' @title Check case definition
#' @description Checks whether the column name for case definition is valid
#'
#' @param case_column
#'
#' @return case column if valid
checkCaseDefinition <- function(case_col) {
  if (!(case_col %in% c("suspected", "confirmed"))) {
    stop("Cholera case definition not in allowed options (suspected or confirmed).")
  }

  cat("-- Running with valid case defitinion: '", case_col, "'\n", sep = "")
  return(case_col)
}


#' @title Check modeling date range
#' @description Checks if input date ranges are correct
#'
#' @param start_time
#' @param end_time
#' @param time_change_func
#' @param aggregate_to_start
#' @param aggregate_to_end
#'
#' @return return
checkModelDateRange <- function(start_time,
                                end_time,
                                time_change_func,
                                aggregate_to_start,
                                aggregate_to_end) {
  if (any(c(!is.Date(start_time), !is.Date(end_time)))) {
    stop("Start and end times need to be in date format")
  }

  if (start_time > end_time) {
    stop("Start time is after end time")
  }

  model_TL <- aggregate_to_start(time_change_func(start_time))
  model_TR <- aggregate_to_end(time_change_func(end_time))

  if (start_time != model_TL | end_time != model_TR) {
    warning(
      "--- User-defined modeling time range does not cover the whole range",
      "as defined with the time resoltion: \n user-defined range:\t",
      start_time, " - ", end_time, "\n ",
      "model range:\t\t", model_TL, " - ", model_TR
    )
  } else {
    cat(
      "---- Running with model date range:", as.character(model_TL),
      "-", as.character(model_TR), "\n"
    )
  }
}

#' @title Check covariate choices
#' @description Verifies whether user-defined covariate choices are in the available
#' set
#'
#' @param covar_choices
#' @param available_choices
#'
#' @return if valid the vector of covariate choices
checkCovariateChoices <- function(covar_choices,
                                  available_choices) {
  if (any(map_lgl(covar_choices, ~ !(. %in% available_choices)))) {
    stop(
      "Covariate choices [", str_c(setdiff(covar_choices, available_choices), collapse = ", "), "] not available. \n",
      "Choose among: [", str_c(available_choices, collapse = ", "), "]"
    )
  }

  cat("---- Running with covariates:", str_c(covar_choices, collapse = ", "), "\n")

  return(covar_choices)
}

#' @title Check stan model
#' @description Checks whether the stan model file exists
#'
#' @param stan_model_path
#' @param stan_dir
#'
#' @return if valid the stan model path
checkStanModel <- function(stan_model_path,
                           stan_dir) {
  if (!file.exists(stan_model_path)) {
    stop(
      "Could not find stan model. Choose among:\n",
      str_c(dir(stan_dir), collapse = "\n")
    )
  }
  cat("---- Running with stan model:", str_replace(stan_model_path, stan_dir, ""), "\n")

  return(stan_model_path)
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
#'
modelingTimeSlices <- function(start_time,
                               end_time,
                               res_time,
                               time_change_func,
                               aggregate_to_start,
                               aggregate_to_end) {
  left_bounds <- seq.Date(start_time, end_time, by = res_time)
  left_bounds <- aggregate_to_start(time_change_func(left_bounds))
  right_bounds <- aggregate_to_end(time_change_func(left_bounds))

  time_slices <- tibble(
    TL = left_bounds[left_bounds <= end_time],
    TR = right_bounds[right_bounds <= end_time]
  )

  cat("-- Model consists of", nrow(time_slices), "time slices of duration", res_time, ":\n")
  print(time_slices)
  cat("\n")
  return(time_slices)
}



makeObservationsFilename <- function(cholera_directory,
                                     map_name) {
  paste(cholera_directory, "/Analysis/", "data/",
    map_name, ".preprocess", ".rdata",
    sep = ""
  )
}

makeCovarFilename <- function(cholera_directory,
                              map_name,
                              covariate_name_part) {
  paste(cholera_directory, "/Analysis/", "data/", map_name, ".",
    covariate_name_part, ".covar", ".rdata",
    sep = ""
  )
}

makeStanInputFilename <- function(cholera_directory,
                                  map_name,
                                  covariate_name_part,
                                  stan_model,
                                  niter) {
  paste(cholera_directory, "/Analysis/", "data/", map_name, ".",
    covariate_name_part, ".", stan_model, ".", niter, ".stan_input",
    ".rdata",
    sep = ""
  )
}

makeStanOutputFilename <- function(cholera_directory,
                                   map_name,
                                   covariate_name_part,
                                   stan_model,
                                   niter) {
  paste(cholera_directory, "/Analysis/", "data/", map_name, ".",
    covariate_name_part, ".", stan_model, ".", niter, ".stan_output",
    ".rdata",
    sep = ""
  )
}

makeMapOutputFilename <- function(cholera_directory,
                                  map_name,
                                  covariate_name_part,
                                  stan_model,
                                  niter) {
  paste(cholera_directory, "/Analysis/", "output/", map_name, ".",
    covariate_name_part, ".", stan_model, ".", niter, ".pdf",
    sep = ""
  )
}
