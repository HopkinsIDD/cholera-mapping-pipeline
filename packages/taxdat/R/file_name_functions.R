#' @title Make observations filename
#' @name make_observations_filename
#' @description Make string for observations Rdata file name
#'
#' @param cholera_directory cholera mapping directory
#' @param map_name map name
#' @return a string with the observation file name
#' @export

make_observations_filename <- function(cholera_directory,
                                       map_name,
                                       config = NULL) {
  if (!is.null(config)) {
    if ("file_names" %in% names(config)) {
      filename <- paste(cholera_directory, "Analysis", "data", sep = "/")
      if ("output_directory" %in% names(config[["file_names"]])) {
        filename <- config[["file_names"]][["output_directory"]]
      }
      if ("observations_filename" %in% names(config[["file_names"]])) {
        return(paste(filename, config[["file_names"]][["observations_filename"]], sep = "/"))
      }
    }
  }
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
#' @param config A config object (not path), used to obtain filenames from
#' @return a string with the covariate file name
#' @export

make_covar_filename <- function(cholera_directory,
                                map_name,
                                covariate_name_part,
                                config = NULL) {
  if (!is.null(config)) {
    if ("file_names" %in% names(config)) {
      filename <- paste(cholera_directory, "Analysis", "data", sep = "/")
      if ("output_directory" %in% names(config[["file_names"]])) {
        filename <- config[["file_names"]][["output_directory"]]
      }
      if ("covariate_filename" %in% names(config[["file_names"]])) {
        return(paste(filename, config[["file_names"]][["covariate_filename"]], sep = "/"))
      }
    }
  }
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
#' @param config config object (not path)
#' @param config_dict dictionary object (not path) with abbreviationas of config
#' @return a string with the Stan input file name
#' @export

make_stan_input_filename <- function(cholera_directory,
                                     map_name,
                                     covariate_name_part,
                                     config,
                                     config_dict) {
  
  if ("file_names" %in% names(config)) {
    filename <- paste(cholera_directory, "Analysis", "data", sep = "/")
    if ("output_directory" %in% names(config[["file_names"]])) {
      filename <- config[["file_names"]][["output_directory"]]
    }
    if ("stan_input_filename" %in% names(config[["file_names"]])) {
      return(paste(filename, config[["file_names"]][["stan_input_filename"]], sep = "/"))
    }
  }
  
  # Processing configs
  to_add <- "pc"
  for (par in c("smoothing_period", "aggregate", "tfrac_thresh", "set_tfrac")) {
    if(!is.null(config[[par]])) {
      to_add <- paste0(to_add, "-", config_dict[[par]]$abbreviation, config[[par]])
    } else {
      to_add <- paste0(to_add, "-", config_dict[[par]]$abbreviation, "NULL")
    }
  }
  
  to_add <- stringr::str_replace_all(to_add, "TRUE", "T")
  to_add <- stringr::str_replace_all(to_add, "FALSE", "F")
  to_add <- stringr::str_replace_all(to_add, "NULL", "N")
  
  paste(cholera_directory, "/Analysis/", "data/", map_name, '.',
        covariate_name_part, '.', to_add, ".stan_input", '.rdata',sep='')
}

#' @title Make initial values filename
#' @name make_initial_values_filename
#' @description Make string for Stan output Rdata file name
#'
#' @param cholera_directory cholera mapping directory
#' @param map_name map name
#' @param covariate_name_part name of covariate
#' @param config config object (not path)
#' @param config_dict dictionary of configuration options
#' @return a string with the Stan output file name
#' @export

make_initial_values_filename <- function(cholera_directory,
                                         map_name,
                                         covariate_name_part,
                                         config,
                                         config_dict) {
  
    if ("file_names" %in% names(config)) {
        filename <- paste(cholera_directory, "Analysis", "data", sep = "/")
        if ("output_directory" %in% names(config[["file_names"]])) {
            filename <- config[["file_names"]][["output_directory"]]
        }
        if ("initial_values_filename" %in% names(config[["file_names"]])) {
            return(paste(filename, config[["file_names"]][["initial_values_filename"]],
                sep = "/"))
        }
    }
  
  # Get stan input filename
  base_filename <- make_stan_input_filename(cholera_directory,
                                            map_name,
                                            covariate_name_part,
                                            config,
                                            config_dict)
  # Base part of output filename
  base_filename <- stringr::str_remove(base_filename, "stan_input\\.rdata")
  
  # Get stan parameters
  stan_pars <- get_stan_parameters(config)
  
  # Modeling configs
  to_add <- "iv"
  for (par in c("warmup", "covar_warmup")) {
    if (!is.null(config[[par]])) {
      to_add <- paste0(to_add, "-", config_dict[[par]]$abbreviation, config[[par]])
    } else {
      to_add <- paste0(to_add, "-", config_dict[[par]]$abbreviation, "NULL")
    }
  }
  
  # Modeling configs
  for (par in c("censoring", "time_effect", "time_effect_autocorr", "use_weights", "use_rho_prior")) {
    if(!is.null(stan_pars[[par]])) {
      to_add <- paste0(to_add, "-", config_dict[[par]]$abbreviation, stan_pars[[par]])
    } else {
      to_add <- paste0(to_add, "-", config_dict[[par]]$abbreviation, "NULL")
    }
  }
  
  # Add stan filename and iterations
  
  to_add <- stringr::str_replace_all(to_add, "TRUE", "T")
  to_add <- stringr::str_replace_all(to_add, "FALSE", "F")
  to_add <- stringr::str_replace_all(to_add, "NULL", "N")
  
  paste0(base_filename, to_add, ".initial_values.rdata")
}

#' @title Make Stan output filename
#' @name make_stan_output_filename
#' @description Make string for Stan output Rdata file name
#'
#' @param cholera_directory cholera mapping directory
#' @param map_name map name
#' @param covariate_name_part name of covariate
#' @param config configuration file
#' @param config_dict dictionary of configuration options
#' @return a string with the Stan output file name
#' @export

make_stan_output_filename <- function(cholera_directory,
                                      map_name,
                                      covariate_name_part,
                                      config,
                                      config_dict) {
  
  if ("file_names" %in% names(config)) {
    filename <- paste(cholera_directory, "Analysis", "data", sep = "/")
    if ("output_directory" %in% names(config[["file_names"]])) {
      filename <- config[["file_names"]][["output_directory"]]
    }
    if ("stan_output_filename" %in% names(config[["file_names"]])) {
      return(paste(filename, config[["file_names"]][["stan_output_filename"]], sep = "/"))
    }
  }
  
  # Get stan input filename
  base_filename <- make_initial_values_filename(cholera_directory,
                                                map_name,
                                                covariate_name_part,
                                                config,
                                                config_dict)
  # Base part of output filename
  base_filename <- stringr::str_remove(base_filename, "initial_values\\.rdata")
  
  # Get stan parameters
  stan_pars <- get_stan_parameters(config)
  
  # Modeling configs
  to_add <- "mc"
  for (par in names(stan_pars)) {
    if(!is.null(stan_pars[[par]])) {
      to_add <- paste0(to_add, "-", config_dict[[par]]$abbreviation, stan_pars[[par]])
    } else {
      to_add <- paste0(to_add, "-", config_dict[[par]]$abbreviation, "NULL")
    }
  }
  
  # Add stan filename and iterations
  to_add <- paste0(to_add, "-model:", stringr::str_remove(config$stan$model, "\\.stan"))
  to_add <- paste0(to_add, "-niter", config$stan$niter)
  
  to_add <- stringr::str_replace_all(to_add, "TRUE", "T")
  to_add <- stringr::str_replace_all(to_add, "FALSE", "F")
  to_add <- stringr::str_replace_all(to_add, "NULL", "N")
  
  paste0(base_filename, to_add, ".stan_output.rdata")
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
  
  # Subset of OCs
  OCs <- config$OCs
  if (is.null(OCs)) {
    OCs <- "allOCs"
  } else {
    OCs <- hashids::encode_hex(paste(OCs, collapse = ""),
                               settings = hashids::hashid_settings(
                                 salt = ifelse(is.null(config$name), "chol", config$name)
                               )
    ) 
  }
  
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
                    OCs,
                    stringr::str_replace(res_time, " ", "_"),
                    paste(start_time, end_time, sep = '-'),
                    paste(res_space, 'km', sep = ''),
                    suspected_or_confirmed,
                    sep = '_')
  
  return(map_name)
}

#' @title get_filenames
#' @name get_filenames
#' @description add
#' @param config object representing the imported YAML config file for the model
#' @param cholera_directory path to cholera directory
#' @return
#' @export

get_filenames <- function (config, cholera_directory) {
  
  if (is.null(config$covariate_choices)) {
    covariate_name_part <- "nocovar"
  } else {
    # Covariate names
    covariate_dict <- yaml::read_yaml(paste0(cholera_directory, "/Layers/covariate_dictionary.yml"))
    all_covariate_choices <- names(covariate_dict)
    short_covariate_choices <- purrr::map_chr(covariate_dict, "abbr")
    covariate_choices <- check_covariate_choices(covar_choices = config$covariate_choices,
                                                 available_choices = all_covariate_choices)
    short_covariates <- short_covariate_choices[covariate_choices]
    covariate_name_part <- paste(short_covariates, collapse = '-')
  }
  
  map_name <- make_map_name(config)
  
  # Load dictionary of configuration options
  config_dict <- yaml::read_yaml(paste0(cholera_directory, "/Analysis/configs/config_dictionary.yml"))
  
  preprocessed_data_fname <- make_observations_filename(cholera_directory = cholera_directory,
                                                        map_name = map_name)
  
  preprocessed_covar_fname <- make_covar_filename(cholera_directory = cholera_directory,
                                                  map_name = map_name,
                                                  covariate_name_part = covariate_name_part)
  
  stan_input_fname <- make_stan_input_filename(cholera_directory = cholera_directory,
                                               map_name = map_name,
                                               covariate_name_part = covariate_name_part,
                                               config = config,
                                               config_dict = config_dict)
  
  initial_values_fname <- make_initial_values_filename(cholera_directory = cholera_directory,
                                                       map_name = map_name,
                                                       covariate_name_part = covariate_name_part,
                                                       config = config,
                                                       config_dict = config_dict)
  
  stan_output_fname <- make_stan_output_filename(cholera_directory = cholera_directory,
                                                 map_name = map_name,
                                                 covariate_name_part = covariate_name_part,
                                                 config = config,
                                                 config_dict = config_dict)
  
  rc <- list(
    data = preprocessed_data_fname,
    covar = preprocessed_covar_fname,
    stan_input = stan_output_fname,
    initial_values = initial_values_fname,
    stan_output = stan_output_fname
  )
  rc <- setNames(c(preprocessed_data_fname, preprocessed_covar_fname,
                   stan_input_fname, initial_values_fname, stan_output_fname), c("data", "covar",
                                                                                 "stan_input", "initial_values", "stan_output"))
  return(rc)
}

#' @title filename_to_stubs
#' @name filename_to_stubs
#' @description Parse filename string to get model run information
#' @param x filename string or vector of strings
#' @return
#' @export

filename_to_stubs <- function(x){
  if(length(x)==0){return(x)}
  print(x)
  x <- strsplit(x, "/")
  x <- sapply(
    x,
    function(y) {
      y[[length(y)]]
    }
  )
  x <- strsplit(x, ".", fixed = TRUE)
  x <- sapply(
    x,
    function(y) {
      if (y[[1]] == "testing") {
        y[[1]] <- paste(y[1:2], collapse = ".")
        y <- y[-2]
      }
      y <- y[-4]
      y <- y[-2]
      if(length(y) > 3){y <- y[-length(y)]}
      return(y)
    })
  return(x)
}


#' @title Add File Names to Existing Config
#' @name add_explicit_file_names_to_config
#' @export
#' @description Take a config, read it, determine it's default filenames, and add them explicitly to it's config.  WARNING : This function will overwrite the config in question
#' @param config_path File name of config
#' @param cholera_directory Path to cholera-mapping-pipeline checkout

add_explicit_file_names_to_config <- function(config_path, cholera_directory) {
  
  in_config <- yaml::read_yaml(config_path)
  default_file_names <- taxdat::get_filenames(config=in_config, cholera_directory = cholera_directory)
  out_config <- in_config
  
  default_file_dir <- unique(dirname(default_file_names))
  if(length(default_file_dir) != 1){ stop("All output files expected in a single directory") }
  default_file_names <- setNames(basename(default_file_names), names(default_file_names))
  
  out_config$file_names <- default_file_names
  out_config$file_names$output_path <- default_file_dir
  
  yaml::write_yaml(out_config, config_path)
  
  return
}

