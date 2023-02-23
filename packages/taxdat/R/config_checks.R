config_docstrings <- list()
config_docstrings[["general"]] <- list()
config_docstrings[["general"]][["location_name"]] <- "Qualified name (e.g., AFR::TZA) of the top level location you want to model."
config_docstrings[["general"]][["time_scale"]] <- "The units for the size of gridcell in time. Currently supports either 'year' or 'month'."
config_docstrings[["general"]][["start_date"]] <- "The first date (inclusive) to include in the model."
config_docstrings[["general"]][["end_date"]] <- "The last date (inclusive) to include in the model."
config_docstrings[["general"]][["width_in_km"]] <- "The width of each gridcell in space. Currently supports any numeric value."
config_docstrings[["general"]][["height_in_km"]] <- "The height of each gridcell in space. Currently supports any numeric value."
config_docstrings[["general"]][["covariates"]] <- list()
config_docstrings[["general"]][["covariates"]][["::"]] <- list()
config_docstrings[["general"]][["covariates"]][["::"]][["name"]] <- "The name of the covariate as stored in the postgres database."
config_docstrings[["general"]][["covariates"]][["::"]][["transform_name"]] <- "A string representing the name of the transformation applied to the covariate. Defaults to identity."
config_docstrings[["general"]][["covariates"]][["::"]][["transform_function"]] <- "A function to use to transform the covariate after loading.  Uses expr! syntax.  Should be a function of one variable. Default is expr! function(x){return(x)}."
config_docstrings[["stan"]] <- list()
config_docstrings[["stan"]][["enable_debug_logging"]] <- "Logical. Whether or not to print debug information from stan.  Generally used only to determine why the probability is log(0). Defaults to FALSE"
config_docstrings[["stan"]][["directory"]] <- "Directory (either absolute or relative to cholera-mapping-pipeline) containing stan files."
config_docstrings[["stan"]][["recompile"]] <- "Whether to force the stan model to recompile if it has already been compiled. logical. Defaults to FALSE."
config_docstrings[["stan"]][["niter"]] <- "The number of iterations to run on each chain. Results will have only have this number of iterations due to warmup. integer. Defaults to 2000."
config_docstrings[["stan"]][["nchain"]] <- "The number of chains to use when running stan. integer. Defaults to 4."
config_docstrings[["stan"]][["ncores"]] <- "The number of parallel processes to use when running stan. integer. Should not be greater than the number of chains. Defaults to 4 or the number of chains, whichever is smaller."
config_docstrings[["stan"]][["beta_sigma_scale"]] <- "This value is used to rescale the standard deviation associated with the covariate contributions (beta). numeric. Defaults to 1."
config_docstrings[["stan"]][["sigma_eta_scale"]] <- "This value is used to rescale the eta values. numeric. Defaults to 5."
config_docstrings[["stan"]][["sigma_eta_scale"]] <- "This value is used to rescale the eta values. numeric. Defaults to 5."
config_docstrings[["stan"]][["do_time_slice"]] <- list()
config_docstrings[["stan"]][["do_time_slice"]][["eta_simplex"]] <- "Should we compute initial conditions as though the eta values are part of a simplex. logical. Defaults to FALSE."
config_docstrings[["stan"]][["do_time_slice"]][["perform"]] <- "Should we compute values for the temporal random effect eta. logical. Defaults to TRUE."
config_docstrings[["stan"]][["do_time_slice"]][["autocorrelated_prior"]] <- "Should the prior associated with the temporal random effect (eta) be autocorrelated. logical. Defaults to TRUE."
config_docstrings[["stan"]][["model"]] <- "Filename (no directory) of the stan model to use."
config_docstrings[["stan"]][["exp_prior"]] <- "Should we use a double exponential prior for covariate contributions (beta) to help unused covariates tend toward 0. logical. Defaults to FALSE."
config_docstrings[["stan"]][["obs_model"]] <- "Which model should we use? Numeric. Defaults to 1 (basic poisson model)"
config_docstrings[["stan"]][["use_intercept"]] <- "Should we include an intercept in the model. logical. Defaults to FALSE."
config_docstrings[["stan"]][["do_zerosum_cnst"]] <- "Should we enforce a 0-sum constraint on the yearly random effects. logical. Defaults to FALSE."
config_docstrings[["stan"]][["do_infer_sd_eta"]] <- "nd toward 0. logical. Defaults to FALSE."
config_docstrings[["stan"]][["od_param"]] <- "Overdispersion parameter that accompanies the obs_model selection. Numeric. Defaults to 0."
config_docstrings[["stan"]][["tfrac_censoring"]] <- "tfrac accounting for censoring. Numeric. Defaults to 0."
config_docstrings[["initial_values"]] <- list()
config_docstrings[["initial_values"]][["warmup"]] <- "Should we use the gam model to try to find better starting conditions for the stan model. logical. Defaults to TRUE."
config_docstrings[["processing"]] <- list()
config_docstrings[["processing"]][["remove_short_time_observations"]] <- list()
config_docstrings[["processing"]][["remove_short_time_observations"]][["perform"]] <- "Should we remove all observations with low tfrac. logical. Defaults to FALSE"
config_docstrings[["processing"]][["remove_short_time_observations"]][["threshold"]] <- "Fraction of one time period below which we should remove observations."

config_docstrings[["processing"]][["reorder_adjacency_matrix"]] <- list()
config_docstrings[["processing"]][["reorder_adjacency_matrix"]][["perform"]] <- "Should we permute the order of the gridcells so that the (assymetric) adjacency matrix has better properties for DAGAR. logical. Defaults to TRUE."
config_docstrings[["processing"]][["remove_overlaps"]] <- list()
config_docstrings[["processing"]][["remove_overlaps"]][["perform"]] <- "Should observations completely covered by another observation from the same observation collection which covers the same gridcells be dropped. logical. Defaults to TRUE."
config_docstrings[["processing"]][["aggregate"]] <- "Should non-overlapping observations from the same observation collection covering the same gridcells be combined together. logical. Defaults to TRUE."
config_docstrings[["processing"]][["average_inconsistent_duplicates"]] <- "Should observations from the same observation collection covering the same gridcells with the same time bounds be combined together. logical. Defaults to TRUE."
config_docstrings[["processing"]][["censor_incomplete_observations"]] <- list()
config_docstrings[["processing"]][["censor_incomplete_observations"]][["perform"]] <- "Should observations covering a small subset of the time dimension of a gridcell be converted to partial observations. logical. Defaults to TRUE."
config_docstrings[["processing"]][["censor_incomplete_observations"]][["threshold"]] <- "How small is a small subset (as a fraction) of the time dimension. numeric. Defaults to 0.95."
config_docstrings[["processing"]][["remove_unobserved_time_slices"]] <- "Should time slices which are not observed be dropped from the model. logical. Defaults to TRUE."
config_docstrings[["processing"]][["remove_unobserved_space_slices"]] <- "Should space slices which are not observed be dropped from the model. logical. Defaults to TRUE."
config_docstrings[["file_names"]] <- list()
config_docstrings[["file_names"]][["stan_input"]] <- "Filename to save input data to. Defaults to a unique string based on the config."
config_docstrings[["file_names"]][["minimal_grid_population"]] <- "Filename to save 1km population data to. Defaults to a unique string based on the config."
config_docstrings[["file_names"]][["stan_output"]] <- "Filename to save stan output data to. Defaults to a unique string based on the config."
config_docstrings[["file_names"]][["generated_quantities"]] <- "Filename to save generated quantities data to. Defaults to a unique string based on the config."
config_docstrings[["file_names"]][["report"]] <- "Filename to save report to. Defaults to a unique string based on the config."
config_docstrings[["generated"]] <- list()
config_docstrings[["generated"]][["location_name"]] <- "Qualified name (e.g., AFR::TZA) of the top level location you want to generate modeled values for."
config_docstrings[["generated"]][["time_scale"]] <- "The units for the size of gridcell in time. Currently supports either 'year' or 'month'."
config_docstrings[["generated"]][["start_date"]] <- "The first date (inclusive) to include in the generate modeled values for."
config_docstrings[["generated"]][["end_date"]] <- "The last date (inclusive) to include in the generate modeled values for."
config_docstrings[["generated"]][["width_in_km"]] <- "The width of each gridcell in space. Currently supports any numeric value."
config_docstrings[["generated"]][["height_in_km"]] <- "The height of each gridcell in space. Currently supports any numeric value."
config_docstrings[["generated"]][["perform"]] <- "Whether to generate additional results from the model based on different information."





config_checks <- list()
config_checks[["general"]] <- list()
config_checks[["general"]][["location_name"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['location_name']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['general']][['location_name']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['general']][['location_name']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["general"]][["time_scale"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['time_scale']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['general']][['time_scale']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['general']][['time_scale']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (!value %in% c("year", "month")) {
    warning(paste(
      "config[['general']][['time_scale']] should be either 'year' or 'month', but is instead",
      value
    ))
  }
  return(TRUE)
}
config_checks[["general"]][["start_date"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['start_date']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['general']][['start_date']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['general']][['start_date']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (is.na(lubridate::ymd(value))) {
    warning(paste(
      "config[['general']][['start_date']] should be a date which can be parsed by lubridate::ymd",
      "but is instead", value
    ))
  }
  return(TRUE)
}
config_checks[["general"]][["end_date"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['end_date']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['general']][['end_date']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['general']][['end_date']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (is.na(lubridate::ymd(value))) {
    warning(paste(
      "config[['general']][['end_date']] should be a date which can be parsed by lubridate::ymd",
      "but is instead", value
    ))
  }
  return(TRUE)
}
config_checks[["general"]][["width_in_km"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['width_in_km']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['general']][['width_in_km']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['general']][['width_in_km']] should be numeric, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["general"]][["height_in_km"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['height_in_km']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['general']][['height_in_km']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['general']][['height_in_km']] should be numeric, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}

config_checks[["general"]][["covariates"]] <- list()
config_checks[["general"]][["covariates"]][["::"]] <- list()
config_checks[["general"]][["covariates"]][["::"]][["name"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['covariates']][[", index, "]][['name']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['general']][['covariates']][[", index, "]][['name']] should be character or NULL, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (any(is.na(value))) {
    warning(paste(
      "config[['general']][['covariates']][[", index, "]][['name']] is NA at least once"
    ))
    return(FALSE)
  }
  if (!is.null(config[["test_metadata"]])) {
    covariate_names <- sapply(
      config[["test_metadata"]][["covariates"]],
      function(x) {
        return(x[["name"]])
      }
    )
    if (!(value %in% covariate_names)) {
      warning(paste(
        "config[['general']][['covariates']][[", index, "]][['name']] should be one of the covariates, but",
        value, "was not found in", paste(covariate_names, collapse = ", ")
      ))
      return(FALSE)
    }
    if (!config[["test_metadata"]][["covariates"]][[which(value == covariate_names)]][["include_in_model"]]) {
      warning(paste(
        "config[['general']][['covariates']][[", index, "]][['name']] should be included in the model, but",
        value, "was not."
      ))
      return(FALSE)
    }
  }
  return(TRUE)
}
config_checks[["general"]][["covariates"]][["::"]][["transform_name"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['covariates']][[", index, "]][['transform_name']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['general']][['covariates']][[", index, "]][['transform_name']] should be a character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (any(is.na(value))) {
    warning("config[['general']][['covariates']][[", index, "]][['transform_name']] is NA.")
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["general"]][["covariates"]][["::"]][["transform_function"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['general']][['covariates']][[", index, "]][['transform_function']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (!is.function(value)) {
    warning(paste(
      "config[['general']][['covariates']][[", index, "]][['transform_function']] should be a function, but is of mode", mode(value)
    ))
    return(FALSE)
  }
  tryCatch(
    {
      tmp <- value(1:10)
    },
    error = function(e) {
      warning(paste("config[['general']][['covariates']][[", index, "]][['transform_function']] should take a single numeric vector argument, but does not run on 1:10"))
    }
  )
  tmp <- value(1:10)
  if (length(tmp) != 10) {
    warning(paste("config[['general']][['covariates']][[", index, "]][['transform_function']] should return a vector the same size as the input, but returns", tmp, "when run on 1:10"))
  }
  if (!is.numeric(tmp)) {
    warning(paste(
      "config[['general']][['covariates']][[", index, "]][['transform_function']] should return a numeric vector, but is",
      tmp, "of mode", mode(tmp), "when applied to 1:10"
    ))
    return(FALSE)
  }
  return(TRUE)
}

config_checks[["stan"]] <- list()
config_checks[["stan"]][["enable_debug_logging"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['enable_debug_logging']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['enable_debug_logging']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning("config[['stan']][['enable_debug_logging']] should be logical, but is", value)
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["stan"]][["directory"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['directory']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (!(dir.exists(value))) {
    warning(paste(
      "config[['stan']][['directory']] should be a directory containing stan files, but is",
      value
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["stan"]][["recompile"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['recompile']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
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
config_checks[["stan"]][["niter"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['niter']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
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
config_checks[["stan"]][["nchain"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['nchain']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
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
config_checks[["stan"]][["ncores"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['ncores']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
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
config_checks[["stan"]][["beta_sigma_scale"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['beta_sigma_scale']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['beta_sigma_scale']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['stan']][['beta_sigma_scale']] should be numeric, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["stan"]][["sigma_eta_scale"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['sigma_eta_scale']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['sigma_eta_scale']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['stan']][['sigma_eta_scale']] should be numeric, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
# QZ added: od_param
config_checks[["stan"]][["od_param"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['od_param']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['od_param']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['stan']][['od_param']] should be numeric, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
# QZ added: tfrac_censoring
config_checks[["stan"]][["tfrac_censoring"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['tfrac_censoring']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['tfrac_censoring']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['stan']][['tfrac_censoring']] should be numeric, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
# QZ added: obs_model
config_checks[["stan"]][["obs_model"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['obs_model']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['obs_model']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['stan']][['obs_model']] should be 1,2 or 3, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["stan"]][["do_time_slice"]] <- list()
config_checks[["stan"]][["do_time_slice"]][["eta_simplex"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['do_time_slice']][['eta_simplex']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['do_time_slice']][['eta_simplex']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(
      "config[['stan']][['do_time_slice']][['eta_simplex']] should be logical, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["stan"]][["do_time_slice"]][["perform"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['do_time_slice']][['perform']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['do_time_slice']][['perform']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(
      "config[['stan']][['do_time_slice']][['perform']] should be logical, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["stan"]][["do_time_slice"]][["autocorrelated_prior"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['do_time_slice']][['autocorrelated_prior']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['do_time_slice']][['autocorrelated_prior']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(
      "config[['stan']][['do_time_slice']][['autocorrelated_prior']] should be logical, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["stan"]][["model"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['model']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['model']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['stan']][['model']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (!file.exists(file.path(config[["stan"]][["directory"]], value))) {
    warning(paste(
      "config[['stan']][['model']] should be a path to a file, but",
      value, "is not a file in", config[["stan"]][["directory"]]
    ))
    return(FALSE)
  }
  tryCatch(
    {
      cmdstanr::cmdstan_model(file.path(config[["stan"]][["directory"]], value))
      return(TRUE)
    },
    error = function(e) {
      warning(paste("Could not compile the stan model", value, "with the above error message(s)"))
    }
  )
  return(FALSE)
}
config_checks[["stan"]][["exp_prior"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['exp_prior']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['exp_prior']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(
      "config[['stan']][['exp_prior']] should be logical, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}

## QZ: added use_intercept
config_checks[["stan"]][["use_intercept"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['use_intercept']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['use_intercept']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(
      "config[['stan']][['use_intercept']] should be logical, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}

## QZ: added do_zerosum_cnst
config_checks[["stan"]][["do_zerosum_cnst"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['do_zerosum_cnst']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['do_zerosum_cnst']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(
      "config[['stan']][['do_zerosum_cnst']] should be logical, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}

## QZ: added do_zerosum_cnst
config_checks[["stan"]][["do_infer_sd_eta"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['stan']][['do_infer_sd_eta']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['stan']][['do_infer_sd_eta']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(
      "config[['stan']][['do_infer_sd_eta']] should be logical, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["initial_values"]] <- list()
config_checks[["initial_values"]][["warmup"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['initial_values']][['warmup']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['initial_values']][['warmup']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['initial_values']][['warmup']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}

config_checks[["processing"]] <- list()
config_checks[["processing"]][["remove_short_time_observations"]] <- list()
config_checks[["processing"]][["remove_short_time_observations"]][["perform"]] <- function(value,
                                                                                           config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['remove_short_time_observations']][['perform']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['remove_short_time_observations']][['perform']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['processing']][['remove_short_time_observations']][['perform']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["remove_short_time_observations"]][["threshold"]] <- function(value,
                                                                                             config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['remove_short_time_observations']][['threshold']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['remove_short_time_observations']][['threshold']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(paste(
      "config[['processing']][['remove_short_time_observations']][['threshold']] should be numeric, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["reorder_adjacency_matrix"]] <- list()
config_checks[["processing"]][["reorder_adjacency_matrix"]][["perform"]] <- function(value,
                                                                                     config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['reorder_adjacency_matrix']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['reorder_adjacency_matrix']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['processing']][['reorder_adjacency_matrix']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["remove_overlaps"]] <- list()
config_checks[["processing"]][["remove_overlaps"]][["perform"]] <- function(value,
                                                                            config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['remove_overlaps']][['perform']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['remove_overlaps']][['perform']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['processing']][['remove_overlaps']][['perform']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["aggregate"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['aggregate']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['aggregate']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['processing']][['aggregate']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["average_inconsistent_duplicates"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['average_inconsistent_duplicates']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['average_inconsistent_duplicates']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['processing']][['average_inconsistent_duplicates']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["censor_incomplete_observations"]] <- list()
config_checks[["processing"]][["censor_incomplete_observations"]][["perform"]] <- function(value,
                                                                                           config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['censor_incomplete_observations']][['perform']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['censor_incomplete_observations']][['perform']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['processing']][['censor_incomplete_observations']][['perform']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["censor_incomplete_observations"]][["threshold"]] <- function(value,
                                                                                             config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['censor_incomplete_observations']][['threshold']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['censor_incomplete_observations']][['threshold']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(paste(
      "config[['processing']][['censor_incomplete_observations']][['threshold']] should be numeric, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["remove_unobserved_time_slices"]] <- function(value,
                                                                             config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['remove_unobserved_time_slices']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['remove_unobserved_time_slices']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['processing']][['remove_unobserved_time_slices']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["processing"]][["remove_unobserved_space_slices"]] <- function(value,
                                                                              config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['processing']][['remove_unobserved_space_slices']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['processing']][['remove_unobserved_space_slices']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['processing']][['remove_unobserved_space_slices']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["file_names"]] <- list()
config_checks[["file_names"]][["stan_input"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['file_names']][['stan_input']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['file_names']][['stan_input']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['file_names']][['stan_input']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (!dir.exists(dirname(value))) {
    warning(paste(
      "config[['file_names']][['stan_input']] should be a path to a file in an existing directory, but",
      value, "implies a directory of", dirname(value), "which does not exist"
    ))
    return(FALSE)
  }
  if (!(tolower(tools::file_ext(value)) == "rdata")) {
    warning(paste(
      "config[['file_names']][['stan_input']] should be an rdata file, but is actually",
      value, "with extension", tools::file_ext(value)
    ))
    return(FALSE)
  }
  if (sum(value == config$file_names) > 1) {
    warning(paste(
      "config[['file_names']][['stan_input']] should be a unique file name, but",
      value, "appears more than once"
    ))
    return(FALSE)
  }
  if (suppressWarnings(normalizePath(value)) != value) {
    warning(paste(
      "config[['file_names']][['stan_input']] should be a normalized path, but ",
      value,
      "normalizes to",
      suppressWarnings(normalizePath(value))
    ))
    return(FALSE)
  }

  return(TRUE)
}
config_checks[["file_names"]][["minimal_grid_population"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['file_names']][['minimal_grid_population']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['file_names']][['minimal_grid_population']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['file_names']][['minimal_grid_population']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (!dir.exists(dirname(value))) {
    warning(paste(
      "config[['file_names']][['minimal_grid_population']] should be a path to a file in an existing directory, but",
      value, "implies a directory of", dirname(value), "which does not exist"
    ))
    return(FALSE)
  }
  if (!(tolower(tools::file_ext(value)) == "csv")) {
    warning(paste(
      "config[['file_names']][['minimal_grid_population']] should be an csv file, but is actually",
      value, "with extension", tools::file_ext(value)
    ))
    return(FALSE)
  }
  if (sum(value == config$file_names) > 1) {
    warning(paste(
      "config[['file_names']][['minimal_grid_population']] should be a unique file name, but",
      value, "appears more than once"
    ))
    return(FALSE)
  }
  if (suppressWarnings(normalizePath(value)) != value) {
    warning(paste(
      "config[['file_names']][['minimal_grid_population']] should be a normalized path, but ",
      value,
      "normalizes to",
      suppressWarnings(normalizePath(value))
    ))
    return(FALSE)
  }

  return(TRUE)
}
config_checks[["file_names"]][["stan_output"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['file_names']][['stan_output']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['file_names']][['stan_output']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['file_names']][['stan_output']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (!dir.exists(dirname(value))) {
    warning(paste(
      "config[['file_names']][['stan_output']] should be a path to a file in an existing directory, but",
      value, "implies a directory of", dirname(value), "which does not exist"
    ))
    return(FALSE)
  }
  if (!(tolower(tools::file_ext(value)) == "rds")) {
    warning(paste(
      "config[['file_names']][['stan_output']] should be an rds file, but is actually",
      value, "with extension", tools::file_ext(value)
    ))
    return(FALSE)
  }
  if (sum(value == config$file_names) > 1) {
    warning(paste(
      "config[['file_names']][['stan_output']] should be a unique file name, but",
      value, "appears more than once"
    ))
    return(FALSE)
  }
  if (suppressWarnings(normalizePath(value)) != value) {
    warning(paste(
      "config[['file_names']][['stan_output']] should be a normalized path, but ",
      value,
      "normalizes to",
      suppressWarnings(normalizePath(value))
    ))
    return(FALSE)
  }

  return(TRUE)
}
config_checks[["file_names"]][["report"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['file_names']][['report']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['file_names']][['report']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['file_names']][['report']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (!dir.exists(dirname(value))) {
    warning(paste(
      "config[['file_names']][['report']] should be a path to a file in an existing directory, but",
      value, "implies a directory of", dirname(value), "which does not exist"
    ))
    return(FALSE)
  }
  if (!(tolower(tools::file_ext(value)) == "html")) {
    warning(paste(
      "config[['file_names']][['report']] should be an html file, but is actually",
      value, "with extension", tools::file_ext(value)
    ))
    return(FALSE)
  }
  if (sum(value == config$file_names) > 1) {
    warning(paste(
      "config[['file_names']][['report']] should be a unique file name, but",
      value, "appears more than once"
    ))
    return(FALSE)
  }
  if (suppressWarnings(normalizePath(value)) != value) {
    warning(paste(
      "config[['file_names']][['report']] should be a normalized path, but ",
      value,
      "normalizes to",
      suppressWarnings(normalizePath(value))
    ))
    return(FALSE)
  }

  return(TRUE)
}
config_checks[["file_names"]][["generated_quantities"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['file_names']][['generated_quantities']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['file_names']][['generated_quantities']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['file_names']][['generated_quantities']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (!dir.exists(dirname(value))) {
    warning(paste(
      "config[['file_names']][['generated_quantities']] should be a path to a file in an existing directory, but",
      value, "implies a directory of", dirname(value), "which does not exist"
    ))
    return(FALSE)
  }
  if (!(tolower(tools::file_ext(value)) == "rdata")) {
    warning(paste(
      "config[['file_names']][['generated_quantities']] should be an rdata file, but is actually",
      value, "with extension", tools::file_ext(value)
    ))
    return(FALSE)
  }
  if (sum(value == config$file_names) > 1) {
    warning(paste(
      "config[['file_names']][['generated_quantities']] should be a unique file name, but",
      value, "appears more than once"
    ))
    return(FALSE)
  }
  if (suppressWarnings(normalizePath(value)) != value) {
    warning(paste(
      "config[['file_names']][['generated_quantities']] should be a normalized path, but ",
      value,
      "normalizes to",
      suppressWarnings(normalizePath(value))
    ))
    return(FALSE)
  }

  return(TRUE)
}

config_checks[["generated"]] <- list()
config_checks[["generated"]][["location_name"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['generated']][['location_name']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['generated']][['location_name']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['generated']][['location_name']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["generated"]][["time_scale"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['generated']][['time_scale']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['generated']][['time_scale']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['generated']][['time_scale']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (!value %in% c("year", "month")) {
    warning(paste(
      "config[['generated']][['time_scale']] should be either 'year' or 'month', but is instead",
      value
    ))
  }
  return(TRUE)
}
config_checks[["generated"]][["start_date"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['generated']][['start_date']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['generated']][['start_date']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['generated']][['start_date']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (is.na(lubridate::ymd(value))) {
    warning(paste(
      "config[['generated']][['start_date']] should be a date which can be parsed by lubridate::ymd",
      "but is instead", value
    ))
  }
  return(TRUE)
}
config_checks[["generated"]][["end_date"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['generated']][['end_date']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['generated']][['end_date']] is NA")
    return(FALSE)
  }
  if (!is.character(value)) {
    warning(paste(
      "config[['generated']][['end_date']] should be character, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  if (is.na(lubridate::ymd(value))) {
    warning(paste(
      "config[['generated']][['end_date']] should be a date which can be parsed by lubridate::ymd",
      "but is instead", value
    ))
  }
  return(TRUE)
}
config_checks[["generated"]][["width_in_km"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['generated']][['width_in_km']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['generated']][['width_in_km']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['generated']][['width_in_km']] should be numeric, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["generated"]][["height_in_km"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['generated']][['height_in_km']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['generated']][['height_in_km']] is NA")
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(
      "config[['generated']][['height_in_km']] should be numeric, but is",
      value
    )
    return(FALSE)
  }
  return(TRUE)
}
config_checks[["generated"]][["perform"]] <- function(value, config, index) {
  if (length(value) != 1) {
    warning(paste(
      "config[['generated']][['perform']] should be of length 1, but is of length",
      length(value), "with value", value
    ))
    return(FALSE)
  }
  if (is.na(value)) {
    warning("config[['generated']][['perform']] is NA")
    return(FALSE)
  }
  if (!is.logical(value)) {
    warning(paste(
      "config[['generated']][['perform']] should be logical, but is",
      value, "of mode", mode(value)
    ))
    return(FALSE)
  }
  return(TRUE)
}

config_checks[["seeds"]] <- list()
config_checks[["seeds"]][["::"]] <- function(value, config, index) {
  if (length(value) != 626) {
    warning(paste0(
      "config[['seeds']][[", index, "]] should be of length 626, but is of length ",
      length(value)
    ))
    return(FALSE)
  }
  if (any(is.na(value))) {
    warning(paste0("config[['seeds']][[", index, "]] has at least one NA"))
    return(FALSE)
  }
  if (!is.numeric(value)) {
    warning(paste0("config[['seeds']][[", index, "]] should be numeric but is", mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

config_ignore_checks <- c("test_metadata")

#' @name check_config
#' @description Check the config to make sure the fields are valid
#' @param config The config to check
#' @param config_checks A named list of checks, where each element is either a named list of checks, or a function which takes the field value and the config.  The function should return TRUE if the config is valid, or FALSE and emit a warning if the config is invalid.
#' @param original_config In case of recursion, the original config this was part of.  This is what is passed to the functions in the defaults
#' @export
check_config <- function(config, checks = config_checks, docstrings = config_docstrings, original_config = config,
                         name_prefix = NULL, no_check_fields = config_ignore_checks, index = NULL) {
  config_is_valid <- TRUE
  subconfig_valid <- TRUE
  for (field_name in names(checks)) {
    if (field_name == "::") {
      for (new_index in seq_len(length(config))) {
        if (class(config[[field_name]]) %in% c("NULL", "list")) {
          subconfig_valid <- check_config(
            config[[new_index]], checks[[field_name]], docstrings[[field_name]],
            original_config, paste0(name_prefix, ifelse(is.null(name_prefix),
              "", "::"
            ), c(index, new_index)),
            index = c(index, new_index)
          )
        } else {
          subconfig_valid <- checks[[field_name]](config[[new_index]], original_config, c(index, new_index))
        }
        config_is_valid <- config_is_valid && subconfig_valid
      }
      next
    }
    if (class(checks[[field_name]]) == "list") {
      if (class(config[[field_name]]) %in% c("NULL", "list")) {
        subconfig_valid <- check_config(
          config[[field_name]], checks[[field_name]], docstrings[[field_name]],
          original_config, paste0(name_prefix, ifelse(is.null(name_prefix),
            "", "::"
          ), field_name)
        )
        config_is_valid <- config_is_valid && subconfig_valid
      } else {
        warning(paste(
          "config field", paste0(name_prefix, ifelse(is.null(name_prefix),
            "", "::"
          ), field_name), "should be a list, but was of type",
          class(config[[field_name]]), "with value", config[[field_name]]
        ))
        config_is_valid <- FALSE
      }
    } else {
      field_valid <- checks[[field_name]](config[[field_name]], original_config, index)
      field_documented <- field_name %in% names(docstrings)
      if (!field_documented) {
        warning(paste("config field", paste0(name_prefix, ifelse(is.null(name_prefix),
          "", "::"
        ), field_name), "does not have a documentation string"))
      }
      config_is_valid <- config_is_valid && field_valid && field_documented
    }
  }
  if ((!all(names(config) %in% names(checks))) && (!all(names(checks) == "::"))) {
    missing_fields <- names(config)[!(names(config) %in% names(checks))]
    missing_fields <- missing_fields[!(missing_fields %in% no_check_fields)]
    for (field in missing_fields) {
      warning(paste(paste0(name_prefix, ifelse(is.null(name_prefix),
        "", "::"
      ), field), "is not a known config field, and is unused"))
      config_is_valid <- FALSE
    }
  }

  return(config_is_valid)
}

config_defaults <- list()

config_defaults[["general"]] <- list()
config_defaults[["general"]][["covariates"]] <- list()
config_defaults[["general"]][["covariates"]][["::"]] <- list()
config_defaults[["general"]][["covariates"]][["::"]][["transform_name"]] <- function(config, index) {
  return("identity")
}
config_defaults[["general"]][["covariates"]][["::"]][["transform_function"]] <- function(config, index) {
  transform_name <- "identity"
  try(
    {
      transform_name <- config[["general"]][["covariates"]][[index]][["transform_name"]]
    },
    silent = TRUE
  )
  if (is.null(transform_name)) {
    transform_name <- "identity"
  }
  if (transform_name == "identity") {
    return(function(x) {
      return(x)
    })
  }
  if (transform_name == "log") {
    return(function(x) {
      return(log(x))
    })
  }
  return("identity")
}

config_defaults[["initial_values"]] <- list()
config_defaults[["initial_values"]][["warmup"]] <- function(config, index) {
  return(TRUE)
}

config_defaults[["processing"]] <- list()
config_defaults[["processing"]][["aggregate"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["processing"]][["average_inconsistent_duplicates"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["processing"]][["reorder_adjacency_matrix"]] <- list()
config_defaults[["processing"]][["reorder_adjacency_matrix"]][["perform"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["processing"]][["remove_short_time_observations"]] <- list()
config_defaults[["processing"]][["remove_short_time_observations"]][["perform"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["processing"]][["remove_short_time_observations"]][["threshold"]] <- function(config, index) {
  return(0.0)
}
config_defaults[["processing"]][["remove_overlaps"]] <- list()
config_defaults[["processing"]][["remove_overlaps"]][["perform"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["processing"]][["censor_incomplete_observations"]] <- list()
config_defaults[["processing"]][["censor_incomplete_observations"]][["perform"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["processing"]][["censor_incomplete_observations"]][["threshold"]] <- function(config, index) {
  return(0.95)
}
config_defaults[["processing"]][["remove_unobserved_time_slices"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["processing"]][["remove_unobserved_space_slices"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["stan"]] <- list()
config_defaults[["stan"]][["nchain"]] <- function(config, index) {
  return(4)
}
config_defaults[["stan"]][["ncores"]] <- function(config, index) {
  return(pmin(config[["stan"]][["nchain"]], parallel::detectCores()))
}
config_defaults[["stan"]][["niter"]] <- function(config, index) {
  return(2000)
}
config_defaults[["stan"]][["enable_debug_logging"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["stan"]][["recompile"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["stan"]][["directory"]] <- function(config, index) {
  return(rprojroot::find_root_file("Analysis/Stan", criterion = rprojroot::has_file(".choldir")))
}
config_defaults[["stan"]][["beta_sigma_scale"]] <- function(config, index) {
  return(1)
}
config_defaults[["stan"]][["sigma_eta_scale"]] <- function(config, index) {
  return(5)
}
config_defaults[["stan"]][["od_param"]] <- function(config, index) {
  return(1)
}
config_defaults[["stan"]][["tfrac_censoring"]] <- function(config, index) {
  return(0)
}
config_defaults[["stan"]][["do_time_slice"]] <- list()
config_defaults[["stan"]][["do_time_slice"]][["perform"]] <- function(config, index) {
  return(TRUE)
}
config_defaults[["stan"]][["do_time_slice"]][["eta_simplex"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["stan"]][["do_time_slice"]][["autocorrelated_prior"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["stan"]][["exp_prior"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["stan"]][["use_intercept"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["stan"]][["do_zerosum_cnst"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["stan"]][["do_infer_sd_eta"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["stan"]][["obs_model"]] <- function(config, index) {
  return(1)
}
config_defaults[["generated"]] <- list()
config_defaults[["generated"]][["perform"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["generated"]][["location_name"]] <- function(config, index) {
  return(config[["general"]][["location_name"]])
}
config_defaults[["generated"]][["start_date"]] <- function(config, index) {
  return(config[["general"]][["start_date"]])
}
config_defaults[["generated"]][["end_date"]] <- function(config, index) {
  return(config[["general"]][["end_date"]])
}
config_defaults[["generated"]][["width_in_km"]] <- function(config, index) {
  return(config[["general"]][["width_in_km"]])
}
config_defaults[["generated"]][["height_in_km"]] <- function(config, index) {
  return(config[["general"]][["height_in_km"]])
}
config_defaults[["generated"]][["time_scale"]] <- function(config, index) {
  return(config[["general"]][["time_scale"]])
}

config_defaults[["file_names"]] <- list()
config_defaults[["file_names"]][["stan_input"]] <- function(config, index) {
  file_name <- paste0(paste(unlist(config[["general"]]), collapse = "_"), ".stan_input.rdata")
  file_path <- rprojroot::find_root_file(paste0("Analysis/data/", file_name), criterion = rprojroot::has_file(".choldir"))
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))
  }
  return(suppressWarnings(normalizePath(file_path)))
}
config_defaults[["file_names"]][["minimal_grid_population"]] <- function(config, index) {
  file_name <- paste0(paste(unlist(config[["general"]]), collapse = "_"), ".minimal_grid_population.csv")
  file_path <- rprojroot::find_root_file(paste0("Analysis/data/", file_name), criterion = rprojroot::has_file(".choldir"))
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))
  }
  return(suppressWarnings(normalizePath(file_path)))
}
config_defaults[["file_names"]][["stan_output"]] <- function(config, index) {
  file_name <- paste0(paste(c(unlist(config[["general"]]), gsub(".*[/]", "", unlist(config[["stan"]]))),
    collapse = "_"
  ), ".stan_output.rds")
  file_path <- rprojroot::find_root_file(paste0("Analysis/data/", file_name), criterion = rprojroot::has_file(".choldir"))
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))
  }
  return(suppressWarnings(normalizePath(file_path)))
}
config_defaults[["file_names"]][["report"]] <- function(config, index) {
  file_name <- paste0(paste(c(unlist(config[["general"]]), gsub(".*[/]", "", unlist(config[["stan"]]))),
    collapse = "_"
  ), ".report.html")
  file_path <- rprojroot::find_root_file(paste0("Analysis/output/", file_name), criterion = rprojroot::has_file(".choldir"))
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))
  }
  return(suppressWarnings(normalizePath(file_path)))
}
config_defaults[["file_names"]][["generated_quantities"]] <- function(config, index) {
  file_name <- paste0(paste(c(
    unlist(config[["general"]]), gsub(".*[/]", "", unlist(config[["stan"]])),
    unlist(config[["generated"]])
  ), collapse = "_"), ".generated_quantities.rdata")
  file_path <- rprojroot::find_root_file(paste0("Analysis/data/", file_name), criterion = rprojroot::has_file(".choldir"))
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))
  }
  return(suppressWarnings(normalizePath(file_path)))
}
config_defaults[["test_metadata"]] <- list()
config_defaults[["test_metadata"]][["file_names"]] <- list()
config_defaults[["test_metadata"]][["file_names"]][["simulation_covariates"]] <- function(config, index) {
  file_name <- paste0(paste(c(
    unlist(config[["general"]]), rlang::hash(config[["test_metadata"]]), rlang::hash(config[["seeds"]])
  ), collapse = "_"), ".data_simulation_covariates.rds")
  file_path <- rprojroot::find_root_file(paste0("Analysis/data/", file_name), criterion = rprojroot::has_file(".choldir"))
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))
  }
  return(suppressWarnings(normalizePath(file_path)))
}
config_defaults[["test_metadata"]][["file_names"]][["true_grid_cases"]] <- function(config, index) {
  file_name <- paste0(paste(c(
    unlist(config[["general"]]), rlang::hash(config[["test_metadata"]]), rlang::hash(config[["seeds"]])
  ), collapse = "_"), ".true_grid_cases.rs")
  file_path <- rprojroot::find_root_file(paste0("Analysis/data/", file_name), criterion = rprojroot::has_file(".choldir"))
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))
  }
  return(suppressWarnings(normalizePath(file_path)))
}

config_defaults[["test_metadata"]][["raster"]] <- list()
config_defaults[["test_metadata"]][["raster"]][["nrow"]] <- function(config, index) {
  return(10)
}
config_defaults[["test_metadata"]][["raster"]][["ncol"]] <- function(config, index) {
  return(10)
}
config_defaults[["test_metadata"]][["raster"]][["nlayer"]] <- function(config, index) {
  interval <- lubridate::interval(
    lubridate::ymd(config[["general"]][["start_date"]]),
    lubridate::ymd(config[["general"]][["end_date"]]) + 1
  )
  period <- lubridate::as.period(interval, units = config[["test_metadata"]][["raster"]][["units"]])
  nperiods <- as.numeric(period, units = config[["test_metadata"]][["raster"]][["units"]])
  return(nperiods)
}
config_defaults[["test_metadata"]][["covariates"]] <- list()
config_defaults[["test_metadata"]][["covariates"]][["::"]] <- list()
config_defaults[["test_metadata"]][["covariates"]][["::"]][["nonspatial"]] <- function(config, index) {
  return(grepl("nonspatial", config[["test_metadata"]][["covariates"]][[index]][["template"]]))
}
config_defaults[["test_metadata"]][["covariates"]][["::"]][["nontemporal"]] <- function(config, index) {
  return(grepl("nontemporal", config[["test_metadata"]][["covariates"]][[index]][["template"]]))
}
config_defaults[["test_metadata"]][["covariates"]][["::"]][["spatially_smooth"]] <- function(config, index) {
  return(grepl("spatially_smooth", config[["test_metadata"]][["covariates"]][[index]][["template"]]))
}
config_defaults[["test_metadata"]][["covariates"]][["::"]][["temporally_smooth"]] <- function(config, index) {
  return(grepl("temporally_smooth", config[["test_metadata"]][["covariates"]][[index]][["template"]]))
}
config_defaults[["test_metadata"]][["covariates"]][["::"]][["constant"]] <- function(config, index) {
  return(grepl("constant", config[["test_metadata"]][["covariates"]][[index]][["template"]]))
}
config_defaults[["test_metadata"]][["covariates"]][["::"]][["polygonal"]] <- function(config, index) {
  return(grepl("polygonal", config[["test_metadata"]][["covariates"]][[index]][["template"]]))
}
config_defaults[["test_metadata"]][["covariates"]][["::"]][["radiating"]] <- function(config, index) {
  return(grepl("radiating", config[["test_metadata"]][["covariates"]][[index]][["template"]]))
}
config_defaults[["test_metadata"]][["grid_observation"]] <- list()
config_defaults[["test_metadata"]][["grid_observation"]][["proportion_observed"]] <- function(config, index) {
  return(1)
}
config_defaults[["test_metadata"]][["grid_observation"]][["number_draws"]] <- function(config, index) {
  return(1)
}
config_defaults[["test_metadata"]][["grid_observation"]][["spatial_observation_bias"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["test_metadata"]][["grid_observation"]][["temporal_observation_bias"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["test_metadata"]][["grid_observation"]][["value_observation_bias"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["test_metadata"]][["grid_observation"]][["noise"]] <- function(config, index) {
  return(FALSE)
}
config_defaults[["test_metadata"]][["observations"]] <- list()
config_defaults[["test_metadata"]][["observations"]]
config_defaults[["test_metadata"]][["observations"]][["::"]] <- list()
config_defaults[["test_metadata"]][["observations"]][["::"]][["proportion_observed"]] <- function(config, index) {
  return(1)
}
config_defaults[["test_metadata"]][["observations"]][["::"]][["start_date"]] <- function(config, index) {
  return(config[["general"]][["start_date"]])
}
config_defaults[["test_metadata"]][["observations"]][["::"]][["end_date"]] <- function(config, index) {
  return(config[["general"]][["end_date"]])
}






#' @name complete_config
#' @description Makes default values in the config explicit recursively
#' @param config A config (or section of the config) to apply defaults to
#' @param defaults A named list of defaults, where each element is either a named list of defaults, or a function which takes the full config, and uses it to provide a default for that field
#' @param original_config In case of recursion, the original config this was part of.  This is what is passed to the functions in the defaults
#' @export
complete_config <- function(config, defaults = config_defaults, original_config = config, index = NULL) {
  for (field_name in names(defaults)) {
    if (class(defaults[[field_name]]) == "list") {
      # Special case for array like members
      if (field_name == "::") {
        for (new_index in seq_len(length(config))) {
          config[[new_index]] <- complete_config(
            config[[new_index]], defaults[[field_name]],
            original_config,
            index = c(index, new_index)
          )
        }
        next
      } else if (!(class(config[[field_name]]) %in% c("NULL", "list"))) {
        stop(paste(
          "config field", field_name, "should be a list, but was of type",
          class(config[[field_name]]), "with value", config[[field_name]]
        ))
      } else if ((field_name == "test_metadata") && (is.null(config[[field_name]]))) {
        next
      }
      config[[field_name]] <- complete_config(
        config[[field_name]], defaults[[field_name]],
        original_config
      )
    } else if (is.null(config[[field_name]])) {
      config[[field_name]] <- defaults[[field_name]](original_config, index)
    }
  }
  return(config)
}

document_config <- function(docstrings = config_docstrings, prefix = "", verbose = FALSE) {
  rc <- ""
  for (field_name in names(docstrings)) {
    if (class(docstrings[[field_name]]) == "list") {
      rc <- paste(rc, field_name, document_config(docstrings[[field_name]], prefix = paste0("  ", prefix), verbose = verbose), sep = "\n")
    } else {
      rc <- paste(rc, paste0(prefix, field_name, ":"), paste0(prefix, docstrings[[field_name]]), sep = "\n")
    }
  }
  return(rc)
}

document_config_options <- function(name_prefix = NULL, docstrings = config_docstrings, defaults = config_defaults, checks = config_checks, no_check_fields = config_ignore_checks, verbose = FALSE) {
  config_documentation <- ""
  subconfig_documentation <- ""
  for (field_name in unique(c(names(docstrings), names(checks), names(defaults)))) {
    if (field_name == "::") {
      subconfig_documentation <- document_config_options(
        name_prefix = paste0(name_prefix, ifelse(is.null(name_prefix), "", "::"), "ARRAY"),
        docstrings = docstrings[[field_name]],
        defaults = defaults[[field_name]],
        checks = checks[[field_name]],
        no_check_fields = no_check_fields,
        verbose = verbose
      )
      config_documentation <- paste(config_documentation, subconfig_documentation, sep = ifelse(nchar(config_documentation) > 0, "\n\n\n\n", ""))
      next
    }
    if ((class(checks[[field_name]]) == "list") || (class(defaults[[field_name]]) == "list")) {
      subconfig_documentation <- document_config_options(
        name_prefix = paste0(name_prefix, ifelse(is.null(name_prefix), "", "::"), field_name),
        docstrings = docstrings[[field_name]],
        defaults = defaults[[field_name]],
        checks = checks[[field_name]],
        no_check_fields = no_check_fields,
        verbose = verbose
      )
      config_documentation <- paste(config_documentation, subconfig_documentation, sep = ifelse(nchar(config_documentation) > 0, "\n\n\n\n", ""))
    } else {
      config_documentation <- paste(config_documentation, document_single_field(field_name, docstrings[[field_name]], checks[[field_name]], defaults[[field_name]], name_prefix, verbose = verbose), sep = ifelse(nchar(config_documentation) > 0, "\n\n", ""))
    }
  }
  return(config_documentation)
}

document_single_field <- function(name, docstring, check, default, name_prefix = NULL, verbose = FALSE) {
  if ((class(docstring) == "list") || (class(default) == "list") || (class(check) == "list")) {
    stop(paste("The default, docstring, or check for field", paste0(name_prefix, ifelse(is.null(name_prefix), "", "::"), name), "was a list instead of a value"))
  }
  name_string <- paste("Name is", paste0(name_prefix, ifelse(is.null(name_prefix), "", "::"), name))
  doc_string <- paste("Explanation is", docstring)
  check_string <- paste("Check function is", yaml::as.yaml(check))
  default_string <- paste("Default is", yaml::as.yaml(default))
  rc <- paste(name_string, doc_string, sep = "\n")
  if (verbose) {
    rc <- paste(rc, check_string, default_string, sep = "\n")
  }
  return(rc)
}

#' @description Print documentation for part of the config by field name.
#' @param field_name character The part of the config you want to see documentation for. For nested fields, separate by "::"
#' @export
get_config_documentation <- function(field_name, verbose = FALSE) {
  my_docstrings <- config_docstrings
  my_defaults <- config_defaults
  my_checks <- config_checks
  if (missing(field_name)) {
    cat(document_config_options(name_prefix = NULL, docstrings = my_docstrings, defaults = my_defaults, checks = my_checks, no_check_fields = no_check_fields, verbose = verbose))
    cat("\n")
    invisible(NULL)
  }
  all_field_names <- stringr::str_split(field_name, pattern = "::")[[1]]
  for (name in all_field_names) {
    if (!is.null(my_docstrings)) {
      if (name %in% names(my_docstrings)) {
        my_docstrings <- my_docstrings[[name]]
      } else {
        my_docstrings <- NULL
      }
    }
    if (!is.null(my_defaults)) {
      if (name %in% names(my_defaults)) {
        my_defaults <- my_defaults[[name]]
      } else {
        my_defaults <- NULL
      }
    }
    if (!is.null(my_checks)) {
      if (name %in% names(my_checks)) {
        my_checks <- my_checks[[name]]
      } else {
        my_checks <- NULL
      }
    }
  }
  name_prefix <- field_name
  if (class(my_docstrings) == "character") {
    my_docstrings <- setNames(list(my_docstrings), field_name)
    name_prefix <- NULL
  }
  if (class(my_defaults) == "function") {
    my_defaults <- setNames(list(my_defaults), field_name)
    name_prefix <- NULL
  }
  if (class(my_checks) == "function") {
    my_checks <- setNames(list(my_checks), field_name)
    name_prefix <- NULL
  }
  cat(document_config_options(name_prefix = name_prefix, docstrings = my_docstrings, defaults = my_defaults, checks = my_checks, no_check_fields = no_check_fields, verbose = verbose))
  cat("\n")
  invisible(NULL)
}
