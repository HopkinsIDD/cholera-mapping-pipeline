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
    allowed_time_periods <- c(allowed_time_periods, paste0(allowed_time_periods, 
        "s"))
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
check_model_date_range <- function(start_time, end_time, time_change_func, aggregate_to_start, 
    aggregate_to_end) {

    if (any(c(!lubridate::is.Date(start_time), !lubridate::is.Date(end_time)))) 
        stop("Start and end times need to be in date format")

    if (start_time > end_time) 
        stop("Start time is after end time")

    model_TL <- aggregate_to_start(time_change_func(start_time))
    model_TR <- aggregate_to_end(time_change_func(end_time))

    if (start_time != model_TL | end_time != model_TR) {
        warning("--- User-defined modeling time range does not cover the whole range", 
            "as defined with the time resoltion: \n user-defined range:\t", start_time, 
            " - ", end_time, "\n ", "model range:\t\t", model_TL, " - ", model_TR)
    } else {
        cat("---- Running with model date range:", as.character(model_TL), "-", as.character(model_TR), 
            "\n")
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
check_covariate_choices <- function(covar_choices, available_choices) {

    if (any(purrr::map_lgl(covar_choices, ~!(. %in% available_choices)))) 
        stop("Covariate choices [", stringr::str_c(setdiff(covar_choices, available_choices), 
            collapse = ", "), "] not available. \n", "Choose among: [", stringr::str_c(available_choices, 
            collapse = ", "), "]")

    cat("---- Running with covariates:", stringr::str_c(covar_choices, collapse = ", "), 
        "\n")

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
check_stan_model <- function(stan_model_path, stan_dir) {

    if (!file.exists(stan_model_path)) 
        stop("Could not find stan model. Choose among:\n", stringr::str_c(dir(stan_dir), 
            collapse = "\n"))
    cat("---- Running with stan model:", stringr::str_replace(stan_model_path, stan_dir, 
        ""), "\n")

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
modeling_time_slices <- function(start_time, end_time, res_time, time_change_func, 
    aggregate_to_start, aggregate_to_end) {

    left_bounds <- seq.Date(start_time, end_time, by = res_time)
    left_bounds <- aggregate_to_start(time_change_func(left_bounds))
    right_bounds <- aggregate_to_end(time_change_func(left_bounds))

    time_slices <- tibble::tibble(TL = left_bounds[left_bounds <= end_time], TR = right_bounds[right_bounds <= 
        end_time])

    cat("-- Model consists of", nrow(time_slices), "time slices of duration", res_time, 
        ":\n")
    print(time_slices)
    cat("\n")
    return(time_slices)
}

config_checks <- list()
config_checks[["stan"]] <- list()
config_checks[["stan"]][["directory"]] <- function(value, config) {
    if (length(value) != 1) {
        warning(paste("config[['stan']][['ncores']] should be of length 1, but is of length", 
            length(value), "with value", value))
        return(FALSE)
    }
    if (!(dir.exists(value))) {
        warning(paste("config[['stan']][['directory']] should be a directory containing stan files, but is", 
            value))
        return(FALSE)
    }
    return(TRUE)
}
config_checks[["stan"]][["recompile"]] <- function(value, config) {
    if (length(value) != 1) {
        warning(paste("config[['stan']][['recompile']] should be of length 1, but is of length", 
            length(value), "with value", value))
        return(FALSE)
    }
    if (is.na(value)) {
        warning("config[['stan']][['recompile']] is NA")
        return(FALSE)
    }
    if (!is.logical(value)) {
        warning("config[['stan']][['recompile']] should be logical, but is", value)
        return(FALSE)
    }
    return(TRUE)
}
config_checks[["stan"]][["niter"]] <- function(value, config) {
    if (length(value) != 1) {
        warning(paste("config[['stan']][['niter']] should be of length 1, but is of length", 
            length(value), "with value", value))
        return(FALSE)
    }
    if (is.na(value)) {
        warning("config[['stan']][['niter']] is NA")
        return(FALSE)
    }
    if (!is.numeric(value)) {
        warning("config[['stan']][['niter']] should be numeric, but is", value)
        return(FALSE)
    }
    return(TRUE)
}
config_checks[["stan"]][["ncores"]] <- function(value, config) {
    if (length(value) != 1) {
        warning(paste("config[['stan']][['ncores']] should be of length 1, but is of length", 
            length(value), "with value", value))
        return(FALSE)
    }
    if (is.na(value)) {
        warning("config[['stan']][['ncores']] is NA")
        return(FALSE)
    }
    if (!is.numeric(value)) {
        warning("config[['stan']][['ncores']] should be numeric, but is", value)
        return(FALSE)
    }
    return(TRUE)
}
config_checks[["stan"]][["nchain"]] <- function(value, config) {
    if (length(value) != 1) {
        warning(paste("config[['stan']][['nchain']] should be of length 1, but is of length", 
            length(value), "with value", value))
        return(FALSE)
    }
    if (is.na(value)) {
        warning("config[['stan']][['nchain']] is NA")
        return(FALSE)
    }
    if (!is.numeric(value)) {
        warning("config[['stan']][['nchain']] should be numeric, but is", value)
        return(FALSE)
    }
    return(TRUE)
}

config_checks[["initial_values"]] <- list()
config_checks[["initial_values"]][["warmup"]] <- function(value, config) {
    if (length(value) != 1) {
        warning(paste("config[['initial_values']][['warmup']] should be of length 1, but is of length", 
            length(value), "with value", value))
        return(FALSE)
    }
    if (is.na(value)) {
        warning("config[['initial_values']][['warmup']] is NA")
        return(FALSE)
    }
    if (!is.logical(value)) {
        warning("config[['initial_values']][['warmup']] should be logical, but is", 
            value)
        return(FALSE)
    }
    return(TRUE)
}


#' @name check_config
#' @description Check the config to make sure the fields are valid
#' @param config The config to check
#' @param config_checks A named list of checks, where each element is either a named list of checks, or a function which takes the field value and the config.  The function should return TRUE if the config is valid, or FALSE and emit a warning if the config is invalid.
#' @param original_config In case of recursion, the original config this was part of.  This is what is passed to the functions in the defaults
#' @export
check_config <- function(config, checks = config_checks, original_config = config) {
    config_is_valid <- TRUE
    for (field_name in names(checks)) {
        if (class(checks[[field_name]]) == "list") {
            if (class(config[[field_name]]) %in% c("NULL", "list")) {
                subconfig_valid <- check_config(config[[field_name]], checks[[field_name]], 
                  original_config)
                config_is_valid <- config_is_valid && subconfig_valid
            } else {
                warning(paste("config field", field_name, "should be a list, but was of type", 
                  class(config[[field_name]]), "with value", config[[field_name]]))
                config_is_valid <- FALSE
            }
        } else {
            field_valid <- checks[[field_name]](config[[field_name]], original_config)
            config_is_valid <- config_is_valid && field_valid
        }
    }
    if (!all(names(config) %in% names(checks))) {
        missing_fields <- names(config)[!(names(config) %in% names(checks))]
        for (field in missing_fields) {
            warning(paste(field, "is not a known config field, and is unused"))
            config_is_valid <- FALSE
        }
    }

    return(config_is_valid)
}

config_defaults <- list()
config_defaults[["initial_values"]] <- list()
config_defaults[["initial_values"]][["warmup"]] <- function(config) {
    return(TRUE)
}

config_defaults[["stan"]] <- list()
config_defaults[["stan"]][["nchain"]] <- function(config) {
    return(pmax(config[["stan"]][["ncores"]], 2))
}
config_defaults[["stan"]][["nccores"]] <- function(config) {
    return(2)
}
config_defaults[["stan"]][["recompile"]] <- function(config) {
    return(TRUE)
}
config_defaults[["stan"]][["directory"]] <- function(config) {
    return(rprojroot::find_root_file("Analysis/Stan", criterion = rprojroot::has_file(".choldir")))
}

#' @name complete_config
#' @description Makes default values in the config explicit recursively
#' @param config A config (or section of the config) to apply defaults to
#' @param defaults A named list of defaults, where each element is either a named list of defaults, or a function which takes the full config, and uses it to provide a default for that field
#' @param original_config In case of recursion, the original config this was part of.  This is what is passed to the functions in the defaults
#' @export
complete_config <- function(config, defaults = config_defaults, original_config = config) {
    for (field_name in names(defaults)) {
        if (class(defaults[[field_name]]) == "list") {
            if (!(class(config[[field_name]]) %in% c("NULL", "list"))) {
                stop(paste("config field", field_name, "should be a list, but was of type", 
                  class(config[[field_name]]), "with value", config[[field_name]]))
            }
            config[[field_name]] <- complete_config(config[[field_name]], defaults[[field_name]], 
                original_config)
        } else if (is.null(config[[field_name]])) {
            config[[field_name]] <- defaults[[field_name]](original_config)
        }
    }
    return(config)
}
