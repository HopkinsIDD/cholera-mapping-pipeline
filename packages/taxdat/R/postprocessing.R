# Scripts to postprocess results for continent-level and global-level stiching


# File management ---------------------------------------------------------

#' Title
#'
#' @param config 
#'
#' @return
#' @export
#'
#' @examples
parse_run_name <- function(config) {
  yaml::read_yaml(config)$file_names$observations_filename %>% 
    stringr::str_remove("\\.preprocess\\.rdata")
}

#' save_file_generic
#'
#' @param res 
#' @param res_file 
#' @param file_type 
#'
#' @return
#' @export
#'
#' @examples
#' 
save_file_generic <- function(res,
                              res_file,
                              file_type = "rds") {
  
  if (file_type == "rds") {
    if (!stringr::str_detect(res_file, "\\.rds$")) {
      stop("File name is not .rds")
    }
    saveRDS(res, file = res_file)
  } else if (file_type == "csv") {
    if (!stringr::str_detect(res_file, "\\.csv$")) {
      stop("File name is not .csv")
    }
    readr::write_csv(res, file = res_file)
  }
}

#' read_file_generic
#'
#' @param res_file 
#' @param file_type 
#'
#' @return
#' @export
#'
#' @examples
#' 
read_file_generic <- function(res_file,
                              file_type = "rds") {
  
  if (file_type == "rds") {
    if (!stringr::str_detect(res_file, "\\.rds$")) {
      stop("File name is not .rds")
    }
    readRDS(file = res_file)
  } else if (file_type == "csv") {
    if (!stringr::str_detect(res_file, "\\.csv$")) {
      stop("File name is not .csv")
    }
    readr::read_csv(file = res_file)
  }
}


#' make_std_output_name
#'
#' @param output_dir 
#' @param fun_name 
#' @param prefix 
#' @param suffix 
#' @param file_type 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
#' 
make_std_output_name <- function(output_dir,
                                 fun_name,
                                 prefix = NULL,
                                 suffix = NULL,
                                 file_type = "csv",
                                 verbose = F) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  if (!is.null(prefix)) {
    resname <- paste(prefix, fun_name, sep = "_")
  } else {
    resname <- fun_name
  }
  
  if (!is.null(suffix)) {
    resname <- paste(resname, suffix, sep = "_")
  }
  
  filename <- paste(output_dir,
                    paste(
                      resname,
                      stringr::str_remove(file_type, "\\."), sep = "."),
                    sep = "/")
  
  if (verbose) {
    cat("File name:", filename, "\n")
  }
  
  filename
}


#' read_yaml_for_data
#'
#' @param config 
#' @param data_dir 
#'
#' @return
#' @export
#'
#' @examples
read_yaml_for_data <- function(config,
                               data_dir) {
  config_list <- yaml::read_yaml(config)
  # Add output directory to file names
  config_list$file_names <- purrr::map(config_list$file_names, ~ paste(data_dir, ., sep = "/"))
  config_list
}



#' make_prefix_from_config_dir
#'
#' @param prefix 
#' @param config_dir 
#'
#' @return
#' @export
#'
#' @examples
make_prefix_from_config_dir <- function(prefix = NULL,
                                        config_dir) {
  # Add config directory to prefix
  prefix_add <- config_dir %>% 
    # Remove tailing / to ensure non-empty string
    stringr::str_remove("/$") %>% 
    stringr::str_split("/") %>% 
    .[[1]] %>% 
    last()
  
  prefix_dir <- ifelse(is.null(prefix), prefix_add, paste(prefix, prefix_add, sep = "_"))
  prefix_dir
}

#' Title
#'
#' @param fun_name 
#' @param prefix 
#' @param suffix 
#' @param output_dir 
#' @param output_file_type 
#'
#' @return
#' @export
#'
#' @examples
read_output <- function(fun_name, 
                        prefix = NULL,
                        suffix = NULL,
                        output_dir = "./",
                        output_file_type = "rds") {
  
  res_file <- make_std_output_name(output_dir = output_dir,
                                   fun_name = fun_name,
                                   prefix = prefix,
                                   suffix = suffix,
                                   file_type = output_file_type,
                                   verbose = FALSE)
  
  if (!file.exists(res_file)) {
    stop("Output file ", res_file, " does not exist.")
  }
  
  read_file_generic(res_file = res_file,
                    file_type = output_file_type)
  
}

# Postprocessing wrapper --------------------------------------------------

postprocess_wrapper <- function(config,
                                redo = TRUE, 
                                redo_aux = FALSE,
                                fun_name = "mai",
                                fun = NULL, 
                                fun_opts = NULL,
                                prefix = NULL,
                                suffix = NULL,
                                data_dir = "cholera-mapping-output",
                                output_dir,
                                output_file_type = "rds",
                                verbose = FALSE) {
  
  if (verbose) {
    cat("-- Running function", fun_name, "on", config, "\n")
  }
  
  # Get the run name from the data file name
  run_name <- parse_run_name(config)
  
  if (!is.null(prefix)) {
    prefix <- paste(prefix, run_name, sep = "_")
  } else {
    prefix <- run_name
  }
  
  # Result filename
  res_file <- make_std_output_name(output_dir = output_dir,
                                   fun_name = fun_name,
                                   prefix = prefix,
                                   suffix = suffix,
                                   file_type = output_file_type,
                                   verbose = verbose)
  
  if (!file.exists(res_file) | redo) {
    # Read config adding the path to the data folder
    config_list <- read_yaml_for_data(config = config,
                                      data_dir = data_dir)
    
    # Run post-processinf function
    res <- do.call(fun, 
                   c(list(config_list = config_list,
                          redo_aux = redo_aux), 
                     fun_opts)) %>% 
      dplyr::mutate(postproc_var = fun_name)
    
    # Save result
    save_file_generic(res = res,
                      res_file = res_file, 
                      file_type = output_file_type)
    
    if (verbose) {
      cat("-- Saved result to", res_file, "\n")
    }
  } else {
    res <- read_file_generic(res_file = res_file, 
                             file_type = output_file_type)
    if (verbose) {
      cat("-- Found pre-computed result in", res_file, "\n")
    }
  }
  
  res
}

#' @title Run all
#'
#' @description Runs a function over a set of combinations of country,
#' run level, and, identifier
#'
#' @param countries
#' @param run_levels
#' @param identifiers
#' @param models
#' @param times_left
#' @param times_right
#' @param fun function to run, must take in arguments country, run_level,
#' identifier, model, time_left and time_right
#' @param redo
#'
#' @return a dataframe
#' @export
run_all <- function(
    config_dir = NULL,
    fun,
    fun_name,
    postprocess_fun = NULL,
    fun_opts = NULL,
    postprocess_fun_opts = NULL,
    prefix = NULL,
    suffix = NULL,
    error_handling = "remove",
    redo = FALSE,
    redo_interm = FALSE,
    redo_aux = FALSE,
    interm_dir = "./",
    output_dir = "./",
    data_dir = "./",
    output_file_type = "rds",
    verbose = FALSE,
    ...) {
  
  res_file <- make_std_output_name(output_dir = output_dir,
                                   fun_name = fun_name,
                                   prefix = make_prefix_from_config_dir(config_dir = config_dir,
                                                                        prefix = prefix),
                                   suffix = suffix,
                                   file_type = output_file_type,
                                   verbose = verbose)
  
  if (file.exists(res_file) & !redo) {
    all_res <- read_file_generic(res_file = res_file, 
                                 file_type = output_file_type)
    
    if (verbose) {
      cat("-- Found pre-computed file", res_file, "\n")
    }
    
  } else {
    
    # Get all configs
    configs <- dir(config_dir, pattern = "yml", full.names = T)
    
    if (length(configs) == 0) {
      stop("No configs found in directory ", config_dir)
    } else if (verbose) {
      cat("-- Running", fun_name, "for", length(configs), "config(s) in", config_dir, "\n")
    }
    
    # This is for parallel computation
    export_packages <- c("tidyverse", "magrittr", "foreach", "rstan", "cmdstanr",
                         "lubridate", "sf", "taxdat")
    
    if (!is.null(postprocess_fun_opts) & all(names(postprocess_fun_opts) == "col")) {
      if (postprocess_fun_opts$col == "pop_high_risk") {
        
        ## bug fix 29 Oct 2026 CA
        # Old code (commented out for reference) had two compounding bugs:
        #  1. `new_configs` was overwritten each loop iteration instead of
        #     accumulating exclusions, so only the LAST flagged config was
        #     ever dropped.
        #  2. `which(risk_cat_dict == ">100")` never matched anything --
        #     get_risk_cat_dict() actually returns "\u2265100" (U+2265) --
        #     so high_risk_var was malformed and every config spuriously
        #     "failed" the check, triggering a false
        #     "No countries ... have high risk population." stop().
        # Fixed by using a `keep` logical vector (correct accumulation) and
        # the correct "\u2265100" label.
        #
        #   new_configs= NULL
        #   for (config_idx in 1:length(configs)) {
        #     configs_tmp <- read_yaml_for_data(configs[config_idx],data_dir)
        #
        #     genquant <- readRDS(configs_tmp$file_names$stan_genquant_filename)
        #
        #     # Get dictionnary of risk categories
        #     risk_cat_dict <- get_risk_cat_dict()
        #     high_risk_ind <- which(risk_cat_dict == ">100")
        #     high_risk_var <- stringr::str_glue("tot_pop_risk[{high_risk_ind},3]")
        #
        #     tot_pop_risk <- genquant$draws("tot_pop_risk") %>%
        #       draws_to_df(var_name = "tot_pop_risk",
        #                   to_name = "variable",
        #                   to_value = "tot_pop_risk")
        #
        #     if(!any(tot_pop_risk$variable==high_risk_var)){
        #       new_configs <- configs[-config_idx]
        #       print(paste0("no high risk population for ", configs_tmp$countries_name))
        #     }
        #
        #   }
        #   if(is.null(new_configs)){
        #     stop("No countries in this config list have high risk population.")
        #   } else {
        #
        #     configs <- new_configs
        #   }
        
        keep <- rep(TRUE, length(configs))
        for (config_idx in seq_along(configs)) {
          configs_tmp <- read_yaml_for_data(configs[config_idx], data_dir)
          genquant <- readRDS(configs_tmp$file_names$stan_genquant_filename)
          risk_cat_dict <- get_risk_cat_dict()
          high_risk_ind <- which(risk_cat_dict == "\u2265100")
          high_risk_var <- stringr::str_glue("tot_pop_risk[{high_risk_ind},3]")
          tot_pop_risk <- genquant$draws("tot_pop_risk") %>%
            draws_to_df(var_name = "tot_pop_risk",
                        to_name = "variable",
                        to_value = "tot_pop_risk")
          if (!any(tot_pop_risk$variable == high_risk_var)) {
            keep[config_idx] <- FALSE
            print(paste0("no high risk population for ", configs_tmp$countries_name))
          }
        }
        if (!any(keep)) {
          warning("No countries in this config list have high risk population -- skipping ",
                  fun_name, " and returning NULL.")
          configs <- character(0)
        } else {
          configs <- configs[keep]
        }
        ## end bug fix
      }
    }
    
    # BUGFIX 29 Oct 2026 CA: configs can now legitimately come back as
    # character(0) from the pop_high_risk filtering above, so skip foreach()
    # entirely rather than let it error on an empty iteration set.
    #
    # Old code (commented out for reference) called foreach() unconditionally:
    #
    #   all_res <- foreach(
    #     config = configs,
    #     .combine = dplyr::bind_rows,
    #     .errorhandling = error_handling,
    #     .packages = export_packages) %do% {
    #
    #       args <- list(config = config,
    #                    redo = redo_interm,
    #                    redo_aux = redo_aux,
    #                    prefix = prefix,
    #                    suffix = suffix,
    #                    fun_name = fun_name,
    #                    fun = fun,
    #                    output_dir = interm_dir,
    #                    data_dir = data_dir,
    #                    verbose = verbose,
    #                    fun_opts = fun_opts)
    #
    #       res <- do.call(postprocess_wrapper, args)
    #
    #       # Set country name
    #       res$country <- get_country_from_string(config)
    #
    #       res
    #     }
    #
    #   if (!is.null(postprocess_fun)) {
    #     args <- c(
    #       list(df = all_res),
    #       postprocess_fun_opts
    #     )
    #
    #     all_res <- do.call(postprocess_fun, args)
    #   }
    
    if (length(configs) == 0) {
      all_res <- NULL
    } else {
      all_res <- foreach(
        config = configs,
        .combine = dplyr::bind_rows,
        .errorhandling = error_handling,
        .packages = export_packages) %do% {
          
          args <- list(config = config,
                       redo = redo_interm,
                       redo_aux = redo_aux,
                       prefix = prefix,
                       suffix = suffix,
                       fun_name = fun_name,
                       fun = fun,
                       output_dir = interm_dir,
                       data_dir = data_dir,
                       verbose = verbose,
                       fun_opts = fun_opts)
          
          res <- do.call(postprocess_wrapper, args)
          
          # Set country name
          res$country <- get_country_from_string(config)
          
          res
        }
      
      if (!is.null(postprocess_fun)) {
        args <- c(
          list(df = all_res),
          postprocess_fun_opts
        )
        
        all_res <- do.call(postprocess_fun, args)
      }
    }
    
    save_file_generic(res = all_res, 
                      res_file = res_file,
                      file_type = output_file_type)
  }
  
  all_res
}

# Postprocessing functions ------------------------------------------------

#' postprocess_mean_annual_incidence
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_mean_annual_incidence <- function(config_list,
                                              redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get mean annual incidence summary
  mai_summary <- genquant$summary("location_total_rates_output", custom_summaries())
  
  # Get the output shapefiles and join
  output_shapefiles <- get_output_sf_reload(config_list = config_list,
                                            redo = redo_aux)
  
  res <- join_output_shapefiles(output = mai_summary, 
                                output_shapefiles = output_shapefiles,
                                var_col = "variable") %>% 
    dplyr::select(-variable)
  
  res
}

#' postprocess_mean_annual_incidence_draws
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_mean_annual_incidence_draws <- function(config_list,
                                                    redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get mean annual incidence summary
  mai_draws <- genquant$draws("location_total_rates_output") %>% 
    draws_to_df(var_name = "location_total_rates_output")
  
  
  # # Get the output shapefiles and join
  output_shapefiles <- get_output_sf_reload(config_list = config_list,
                                            redo = redo_aux)
  
  res <- join_output_shapefiles(output = mai_draws,
                                output_shapefiles = output_shapefiles %>% 
                                  sf::st_drop_geometry(),
                                var_col = "variable") %>%
    dplyr::select(-variable) 
  
  res
}


#' postprocess_mai_adm0_cases
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_adm0_cases <- function(config_list,
                                   redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get index of national-level space output
  adm0_ind <- get_adm0_index(config_list = config_list)
  
  # This assumes that the first output shapefile is always the national-level shapefile
  cases_adm0 <- genquant$draws(stringr::str_glue("location_mean_cases_output[{adm0_ind}]")) %>% 
    posterior::as_draws() %>% 
    posterior::as_draws_df() %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(country_cases = `location_mean_cases_output[1]`) %>% 
    dplyr::mutate(country = get_country_from_string(config_list$file_names$stan_genquant_filename))
  
  
  cases_adm0
}

#' postprocess_mai_adm0_simulated_cases
#' This differs from postprocess_adm0_cases in that it accounts for eventual 
#' over-dispersion in the observation in the model
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_mai_adm0_simulated_cases <- function(config_list,
                                                 redo_aux = FALSE) {
  
  # First get the mean of the cases
  mean_cases_adm0 <- postprocess_adm0_cases(config_list,
                                            redo_aux = redo_aux)
  
  # Get the observation model
  stan_input <- read_file_of_type(config_list$file_names$stan_input_filename, "stan_input") 
  obs_model <- stan_input$stan_data$obs_model
  adm0_od <- stan_input$stan_data$adm0_od
  
  # Simulate
  res <- simulate_observations(mean_cases_adm0$country_cases,
                               obs_model = obs_model,
                               od_param = adm0_od)
  
  
  mean_cases_adm0 %>% 
    mutate(country_cases = res) %>% 
    rename(sim_country_cases = country_cases)
}


#' postprocess_adm_mean_cases
#' Mean number of cases for all output locations across time slices
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_mean_annual_cases <- function(config_list,
                                          redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # This assumes that the first output shapefile is always the national-level shapefile
  cases <- genquant$summary("location_mean_cases_output", custom_summaries()) %>% 
    dplyr::mutate(country = get_country_from_string(config_list$file_names$stan_genquant_filename))
  
  # Get output shapefiles for admin level informaiton
  output_shapefiles <- get_output_sf_reload(config_list = config_list,
                                            redo = redo_aux) %>% 
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::select(-country)
  
  res <- join_output_shapefiles(output = cases, 
                                output_shapefiles = output_shapefiles,
                                var_col = "variable") %>% 
    dplyr::select(-variable)
  
  res
}

#' postprocess_annual_adm0_cases
#' Mean number of cases for all output locations
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_annual_adm0_cases <- function(config_list,
                                          redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  stan_input <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")

  # This assumes that the first output shapefile is always the national-level shapefile
  cases <- genquant$summary(variables = "location_cases_output", custom_summaries())  %>% 
    dplyr::mutate(id = str_extract(variable, "[0-9]+") %>% as.numeric(),
           location_period_id = stan_input$fake_output_obs$locationPeriod_id[id],
           TL = stan_input$fake_output_obs$TL[id])

  # Get output shapefiles for admin level informaiton
  output_shapefiles <- get_output_sf_reload(config_list = config_list,
                                            redo = redo_aux) %>% 
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::select(-country)
  
  res <- join_output_shapefiles_by_time(output = cases, 
                                output_shapefiles = output_shapefiles,
                                var_col = "variable") %>% 
    dplyr::select(-variable)
  
  res
}


#' postprocess_mai_adm0_cases
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_adm0_rates <- function(config_list,
                                   redo_aux = FALSE) {
  
  # Get total cases over modeled time peroid
  cases_adm0 <- postprocess_adm0_cases(config_list = config_list,
                                       redo_aux = redo_aux)
  
  # Get total population over modeled time period
  pop_adm0 <- postprocess_adm0_pop(config_list = config_list,
                                   redo_aux = redo_aux,
                                   total_pop = FALSE)
  
  # Add population and compute rates
  cases_adm0 %>% 
    dplyr::mutate(country_pop = pop_adm0$country_pop[1],
                  country_rates = country_cases/country_pop)
}

#' postprocess_mai_adm0_pop
#' The object pop_loc_output in generated quantites contains the average 
#' population in the location, computed as the sum over time slices divided 
#' by the number of time slices. The function enables to return either the average
#' or the total population. 
#' 
#' @param config_list config list
#' @param redo_aux redo auxiliary files
#' @param total_pop whether to return total population over modeling period or average population
#'
#' @return
#' @export
#'
postprocess_adm0_pop <- function(config_list,
                                 redo_aux = FALSE,
                                 total_pop = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get index of national-level output
  adm0_ind <- get_adm0_index(config_list = config_list)
  
  # Get population
  pop_adm0 <- genquant$summary(stringr::str_glue("pop_loc_output[{adm0_ind}]"), mean) %>% 
    dplyr::rename(country_pop = mean)
  
  
  if (total_pop) {
    stan_input <- read_file_of_type(config_list$file_names$stan_input_filename, "stan_input") 
    n_time_slices <- stan_input$stan_data[["T"]]
    pop_adm0$country_pop <- pop_adm0$country_pop * n_time_slices
  }
  
  pop_adm0
}

#' postprocess_coef_of_variation
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_coef_of_variation <- function(config_list,
                                          redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get mean annual incidence summary
  cov_summary <- genquant$summary("location_cov_cases_output", custom_summaries())
  
  # Get the output shapefiles and join
  output_shapefiles <- get_output_sf_reload(config_list = config_list,
                                            redo = redo_aux)
  
  res <- join_output_shapefiles(output = cov_summary, 
                                output_shapefiles = output_shapefiles,
                                var_col = "variable") %>% 
    dplyr::select(-variable)
  
  res
}

#' postprocess_od_param
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_od_param <- function(config_list,
                                          redo_aux = FALSE) {
  
  # Get stan_output data
  load(config_list$file_names$stan_output_filename)

  # Get mean annual incidence summary
  param_names <- grep("^od_param", model.rand@sim$fnames_oi, value = TRUE)
  param_summary <- summary(model.rand, pars = param_names)$summary 
  res <- as.data.frame(param_summary) %>% 
    dplyr::mutate(
      admin_level = as.numeric(str_extract(rownames(.), "\\d+"))-1
    ) %>% 
    dplyr::select(admin_level,mean,`2.5%`,`97.5%`)

  # Remove row names
  rownames(res) <- NULL
  
  res
}


#' postprocess_adm0_sf
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_adm0_sf <- function(config_list,
                                redo_aux = FALSE) {
  
  res <- get_output_sf_reload(config_list = config_list,
                              redo = redo_aux) %>% 
    dplyr::filter(admin_level == "ADM0")
  
  res
}

#' postprocess_lp_shapefiles
#' Extracts the unique shapefiles available in the dataset
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_lp_shapefiles <- function(config_list,
                                      redo_aux = FALSE) {
  
  stan_input <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")
  
  stan_input$sf_cases_resized %>% 
    dplyr::group_by(locationPeriod_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(locationPeriod_id, location_name, admin_level) %>% 
    dplyr::arrange(admin_level, location_name)
}


#' postprocess_observations
#' Extracts observations
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_observations <- function(config_list,
                                     redo_aux = FALSE) {
  
  stan_input <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")
  
  cases_column <- taxdat::check_case_definition(config_list$case_definition) %>% 
    taxdat::case_definition_to_column_name(database = T)
  
  stan_input$sf_cases_resized %>% 
    sf::st_drop_geometry()
}

#' postprocess_lp_obs_counts
#' Extracts observation counts by location period
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_lp_obs_counts <- function(config_list,
                                      redo_aux = FALSE) {
  
  stan_input <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")
  
  cases_column <- taxdat::check_case_definition(config_list$case_definition) %>% 
    taxdat::case_definition_to_column_name(database = T)
  
  stan_input$sf_cases_resized %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(imputed = stringr::str_detect(OC_UID, "impute")) %>% 
    dplyr::group_by(locationPeriod_id, location_name, admin_level, imputed) %>% 
    dplyr::mutate(cases = !!rlang::sym(cases_column)) %>% 
    dplyr::summarise(n_obs = n(),
                     n_cases = sum(cases),
                     mean_cases = mean(cases)) %>% 
    dplyr::ungroup()
}

#' postprocess_risk_category
#'
#' @param config_list 
#' @param redo_aux 
#'
#' @return
#' @export
#'
#' @examples
postprocess_risk_category <- function(config_list,
                                      redo_aux = FALSE,
                                      cum_prob_thresh = .95) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get dictionnary of risk categories
  risk_cat_dict <- get_risk_cat_dict()
  
  # Get the population per output
  output_location_pop <- genquant$summary("pop_loc_output")
  
  # Get proportion
  risk_cat <- genquant$summary("location_risk_cat",
                               compute_cumul_proportion_thresh,
                               .args = list(thresh = cum_prob_thresh)
  ) %>% 
    dplyr::mutate(risk_cat = risk_cat_dict[risk_cat],
                  risk_cat = factor(risk_cat, levels = risk_cat_dict),
                  pop = output_location_pop$mean)
  
  # Get the output shapefiles and join
  output_shapefiles <- get_output_sf_reload(config_list = config_list,
                                            redo = redo_aux)
  res <- join_output_shapefiles(output = risk_cat, 
                                output_shapefiles = output_shapefiles,
                                var_col = "variable") %>% 
    dplyr::select(-variable)
  
  res
}

#' postprocess_pop_at_risk
#'
#' @param config_list 
#' @param redo_aux 
#'
#' @return
#' @export
#'
#' @examples
postprocess_pop_at_risk <- function(config_list,
                                    redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get dictionnary of risk categories
  risk_cat_dict <- get_risk_cat_dict()
  
  # Get mean annual incidence summary
  pop_at_risk <- genquant$summary("tot_pop_risk", custom_summaries()) %>% 
    dplyr::mutate(risk_cat = risk_cat_dict[as.numeric(stringr::str_extract(variable, "(?<=\\[)[0-9]+(?=,)"))],
                  risk_cat = factor(risk_cat, levels = risk_cat_dict),
                  admin_level = str_c("ADM", as.numeric(str_extract(variable, "(?<=,)[0-9]+(?=\\])")) - 1),
                  country = taxdat::get_country_isocode(config_list))
  
  pop_at_risk
}


#' postprocess_pop_at_risk
#'
#' @param config_list 
#' @param redo_aux 
#'
#' @return
#' @export
#'
#' @examples
postprocess_pop_at_risk_draws <- function(config_list,
                                          redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get dictionnary of risk categories
  risk_cat_dict <- get_risk_cat_dict()
  
  # Get mean annual incidence summary
  pop_at_risk <- genquant$draws("tot_pop_risk") %>% 
    draws_to_df(var_name = "tot_pop_risk",
                to_name = "variable",
                to_value = "tot_pop_risk") %>% 
    dplyr::mutate(risk_cat = risk_cat_dict[as.numeric(stringr::str_extract(variable, "(?<=\\[)[0-9]+(?=,)"))],
                  risk_cat = factor(risk_cat, levels = risk_cat_dict),
                  admin_level = str_c("ADM", as.numeric(str_extract(variable, "(?<=,)[0-9]+(?=\\])")) - 1),
                  country = taxdat::get_country_isocode(config_list))
  
  pop_at_risk
}

#' postprocess_pop_at_high_risk
#' This is to match computations in the Lancet paper of > 100 cases/100'000
#'
#' @param config_list 
#' @param redo_aux 
#'
#' @return
#' @export
#'
#' @examples
postprocess_pop_at_high_risk <- function(config_list,
                                         redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get dictionnary of risk categories
  risk_cat_dict <- get_risk_cat_dict()
  #high_risk_ind <- which(risk_cat_dict == ">100")
  # BUGFIX: get_risk_cat_dict() returns "\u2265100" (Unicode "greater-or-equal",
  # U+2265), not the ASCII ">100". The old ">100" never matched anything, so
  # `which(...)` silently returned integer(0) and high_risk_var downstream was
  # malformed -- this is what ultimately produced the
  # "object 'pop_high_risk' not found" error several steps later.
  high_risk_ind <- which(risk_cat_dict == "\u2265100")
  ##end bug fix
  high_risk_var <- stringr::str_glue("tot_pop_risk[{high_risk_ind},3]")                       
  
  # Get mean annual incidence summary
  pop_at_hihgh_risk <- genquant$draws(high_risk_var) %>% 
    posterior::as_draws() %>% 
    posterior::as_draws_df() %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(c("pop_high_risk" =  high_risk_var)) %>% 
    dplyr::mutate(country = get_country_from_string(config_list$file_names$stan_genquant_filename))
  
  
  pop_at_hihgh_risk
}

#' postprocess_mean_annual_incidence
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_grid_mai_rates <- function(config_list,
                                       redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get mean annual incidence summary
  mai_summary <- genquant$summary("space_grid_rates", custom_summaries())
  
  # Get the output shapefiles and join
  res <- get_space_grid(config_list = config_list,
                        redo = redo_aux) %>% 
    dplyr::bind_cols(mai_summary) %>% 
    dplyr::select(-variable)
  
  res
}

#' postprocess_grid_mai_rates_draws
#' 
#' @param config_list config list
#' @param redo_aux 
#'
#' @return
#' @export
#'
postprocess_grid_mai_rates_draws <- function(config_list,
                                             redo_aux = FALSE,
                                             filter_draws = 4000) {
  
  cat("---- Extracting", filter_draws, "draws of mean annual rate grid\n")
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get mean annual incidence summary
  rate_draws <- genquant$draws("space_grid_rates") %>% 
    draws_to_df(var_name = "space_grid_rates",
                filter_draws = filter_draws) %>% 
    dplyr::mutate(grid_id = stringr::str_extract(variable, "[0-9]+") %>% as.integer())
  
  
  # Get the output shapefiles and join
  res <- get_space_grid(config_list = config_list,
                        redo = redo_aux) %>% 
    dplyr::mutate(grid_id = dplyr::row_number()) %>% 
    dplyr::inner_join(rate_draws) %>% 
    dplyr::select(-variable)
  
  res
}


#' postprocess_mean_annual_incidence
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_grid_mai_cases <- function(config_list,
                                       redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get mean annual incidence rates at space grid level
  mai_summary <- genquant$summary("space_grid_rates", custom_summaries())
  
  # Get population and average over space grid
  mean_pop_sf <- get_mean_pop_grid(config_list = config_list,
                                   redo = redo_aux)
  
  # Get the output shapefiles and join
  res <- mean_pop_sf %>% 
    dplyr::bind_cols(mai_summary) %>% 
    dplyr::select(-variable) %>% 
    dplyr::mutate(dplyr::across(.cols = c("mean", "q2.5", "q97.5"),
                                ~ . * pop))
  
  res
}

#' postprocess_grid_mai_cases_draws
#' 
#' @param config_list config list
#' @param redo_aux 
#'
#' @return
#' @export
#'
postprocess_grid_mai_cases_draws <- function(config_list,
                                             redo_aux = FALSE,
                                             filter_draws = 4000) {
  
  
  cat("---- Extracting", filter_draws, "draws of mean annual case grid\n")
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get mean annual incidence summary
  rate_draws <- genquant$draws("space_grid_rates") %>% 
    draws_to_df(var_name = "space_grid_rates",
                filter_draws = filter_draws) %>% 
    dplyr::mutate(grid_id = stringr::str_extract(variable, "[0-9]+") %>% as.integer())
  
  # Get population and average over space grid
  mean_pop_sf <- get_mean_pop_grid(config_list = config_list,
                                   redo = redo_aux) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(grid_id = dplyr::row_number()) %>% 
    dplyr::ungroup()
  
  # Compute mai cases by grid cll
  res <- mean_pop_sf %>% 
    dplyr::inner_join(rate_draws) %>% 
    dplyr::select(-variable) %>%
    dplyr::mutate(value = value * pop)
  
  # Get the output shapefiles and join
  res <- get_space_grid(config_list = config_list,
                        redo = redo_aux) %>% 
    dplyr::mutate(grid_id = dplyr::row_number()) %>% 
    dplyr::inner_join(res) %>% 
    dplyr::select(-pop)
  
  res
}


#' postprocess_gen_obs
#' Get summaries for generated observations
#'
#' @param config_list 
#' @param redo_aux 
#'
#' @return
#' @export
#'
#' @examples
postprocess_gen_obs <- function(config_list,
                                redo_aux = FALSE) {
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get quantiles of generated observations for unique combinations of
  # location-times
  gen_obs <- dplyr::inner_join(
    genquant$summary("gen_obs_loctime_combs", mean),
    genquant$summary("gen_obs_loctime_combs", 
                     ~ posterior::quantile2(., probs = c(0.005, seq(0.025, 0.975, by = .025), .995)))
  )
  
  
  # Join with data
  load(config_list$file_names$stan_input_filename)
  
  mapped_gen_obs <- gen_obs %>% 
    .[stan_input$stan_data$map_obs_loctime_combs, ] %>% 
    dplyr::bind_cols(stan_input$sf_cases_resized %>% 
                       sf::st_drop_geometry() %>% 
                       dplyr::filter(!is.na(loctime)) %>% 
                       dplyr::select(observation = attributes.fields.suspected_cases,
                                     censoring,
                                     admin_level,
                                     locationPeriod_id,
                                     TL, 
                                     TR) %>% 
                       dplyr::mutate(loctime_comb = stan_input$stan_data$map_obs_loctime_combs)) %>% 
    dplyr::mutate(obs_gen_id = stringr::str_extract(variable, "[0-9]+") %>% as.numeric()) %>% 
    dplyr::add_count(obs_gen_id)
  
  
  mapped_gen_obs
  
}


#' postprocess_mean_population
#' Mean number of cases for all output locations across time slices
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_mean_population <- function(config_list,
                                        redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # This assumes that the first output shapefile is always the national-level shapefile
  population <- genquant$summary("pop_loc_output") %>% 
    dplyr::mutate(country = get_country_from_string(config_list$file_names$stan_genquant_filename))
  
  # Get output shapefiles for admin level informaiton
  output_shapefiles <- get_output_sf_reload(config_list = config_list,
                                            redo = redo_aux) %>% 
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::select(-country)
  
  res <- join_output_shapefiles(output = population, 
                                output_shapefiles = output_shapefiles,
                                var_col = "variable") %>% 
    dplyr::select(-variable)
  
  res
}

# Output shapefiles -------------------------------------------------------

#' get_output_sf_wrapper
#' This function enables loaded pre-extracted the subset of output shapefiles 
#' for which generated quantities were computed.
#'
#' @param config 
#' @param data_dir 
#'
#' @return
#' @export
#'
#' @examples
get_output_sf_reload <- function(config_list,
                                 redo = FALSE) {
  
  # Make file name
  output_space_sf_file <- stringr::str_replace(config_list$file_names$observations_filename,
                                               ".preprocess.rdata",
                                               "_output_space_sf.rds")
  
  if (file.exists(output_space_sf_file) & !redo) {
    res <- readRDS(output_space_sf_file)
  } else {
    output_shapefiles <- taxdat::read_file_of_type(config_list$file_names$observations_filename, "output_shapefiles")
    stan_input <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")
    
    res <- output_shapefiles %>% 
      dplyr::inner_join(stan_input$output_lps %>% 
                          dplyr::select(locationPeriod_id, shp_id),
                        by = c("location_period_id" = "locationPeriod_id")) %>% 
      dplyr::arrange(shp_id)
    
    saveRDS(res, file = output_space_sf_file)
  }
  
  res
}

#' Title
#'
#' @param config_list 
#'
#' @return
#' @export
#'
#' @examples
#' 
get_adm0_index <- function(config_list) {
  
  stan_input <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")
  
  stan_input$output_lps %>%
    dplyr::filter(admin_lev == 0) %>%
    dplyr::pull(shp_id)
}

# get regions for countries -------------------------------------------------------

#' get_AFRO_region
#' @param data data frame with country name
#' @param ctry_col colname of country name
#' @return 
#' @export
#' @examples
get_AFRO_region <- function(data, ctry_col) {
  
  data_with_AFRO_region <- data %>% 
    dplyr::mutate(
      AFRO_region = dplyr::case_when(
        !!rlang::sym(ctry_col) %in% c("BDI","ETH","KEN","MDG","RWA","SDN","SSD","UGA","TZA","ERI","","DJI","SOM","SDN") ~ "Eastern Africa",
        !!rlang::sym(ctry_col) %in% c("BWA","MOZ","MWI","NAM","SWZ","ZMB","ZWE","ZAF","LSO") ~ "Southern Africa",
        !!rlang::sym(ctry_col) %in% c("AGO","CMR","CAF","TCD","COG","COD","GNQ","GAN","GAB") ~ "Central Africa",
        !!rlang::sym(ctry_col) %in% c("BEN","BFA","CIV","GHA","GIN","GNB","LBR","MLI","MRT","NER","NGA","SEN","SLE","TGO","DJI","SOM","SDN") ~ "Western Africa",
      ) 
    )
  
  return(data_with_AFRO_region)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_AFRO_region_levels <- function() {
  c("Western Africa", "Central Africa", "Eastern Africa", "Southern Africa")
}


# Get grids ---------------------------------------------------------------


#' get_smooth_grid
#'
#' @param config_list 
#'
#' @return
#' @export
#'
#' @examples
get_space_grid <- function(config_list,
                           redo = FALSE) {
  
  # Make file name
  space_grid_sf_file <- stringr::str_replace(config_list$file_names$observations_filename,
                                             ".preprocess.rdata",
                                             "_space_grid_sf.rds")
  
  if (file.exists(space_grid_sf_file) & !redo) {
    res <- readRDS(space_grid_sf_file)
  } else {
    res <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")$sf_grid %>% 
      dplyr::group_by(rid, x, y) %>% 
      dplyr::slice(1) %>% 
      dplyr::select(rid, x, y, geom)
    
    saveRDS(res, file = space_grid_sf_file)
  }
  
  res %>% 
    dplyr::ungroup()
}

#' get_smooth_grid
#'
#' @param config_list 
#'
#' @return
#' @export
#'
#' @examples
get_spacetime_grid <- function(config_list,
                               redo = FALSE) {
  
  # Make file name
  spacetime_grid_sf_file <- stringr::str_replace(config_list$file_names$observations_filename,
                                                 ".preprocess.rdata",
                                                 "_spacetime_grid_sf.rds")
  
  if (file.exists(spacetime_grid_sf_file) & !redo) {
    res <- readRDS(spacetime_grid_sf_file)
  } else {
    res <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")$sf_grid
    
    saveRDS(res, file = spacetime_grid_sf_file)
  }
  
  res
}

get_mean_pop_grid <- function(config_list,
                              redo = FALSE) {
  
  # Make file name
  mean_pop_grid_sf_file <- stringr::str_replace(config_list$file_names$observations_filename,
                                                ".preprocess.rdata",
                                                "_mean_pop_grid_sf.rds")
  
  if (file.exists(mean_pop_grid_sf_file) & !redo) {
    res <- readRDS(mean_pop_grid_sf_file)
  } else {
    res <- get_spacetime_grid(config_list = config_list) %>% 
      dplyr::mutate(pop = taxdat::read_file_of_type(config_list$file_names$stan_input_filename, variable = "stan_input")$stan_data$pop) %>% 
      sf::st_drop_geometry() %>% 
      dplyr::group_by(rid, x, y, id) %>% 
      dplyr::summarise(pop = mean(pop)) %>% 
      dplyr::inner_join(get_space_grid(config_list = config_list), .)
    
    saveRDS(res, file = mean_pop_grid_sf_file)
  }
  
  res
}


# Postprocess functions ---------------------------------------------------

#' collapse_grid
#' Function to collapse space grid to single non-overlapping cells
#' 
#' @param grid_obj 
#'
#' @return
#' @export
#'
#' @examples
collapse_grid <- function(df,
                          by_draw = FALSE) {
  
  u_grid <- df  %>%
    dplyr::group_by(rid, x, y) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(rid, x, y, geom)
  
  
  if (!by_draw) {
    res <- df %>%
      sf::st_drop_geometry() %>% 
      dplyr::group_by(rid, x, y) %>% 
      dplyr::summarise(mean = mean(mean),
                       q2.5 = min(q2.5),
                       q97.5 = max(q97.5),
                       overlap = n() > 1) %>% 
      dplyr::inner_join(u_grid, .)
  } else {
    res <- df %>%
      sf::st_drop_geometry() %>% 
      dplyr::group_by(rid, x, y, .draw) %>% 
      dplyr::summarise(value = mean(value),
                       overlap = n() > 1) %>% 
      dplyr::inner_join(u_grid, .)
  }
  
  res
}


# Auxilliary funcitons ----------------------------------------------------

#' Title
#' https://www.tutorialspoint.com/r/r_mean_median_mode.htm
#' @param v 
#'
#' @return
#' @export
#'
#' @examples
compute_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#' compute_proportions
#'
#' @param v 
#'
#' @return
#' @export
#'
#' @examples
compute_cumul_proportion_thresh <- function(v, thresh = .95) {
  
  counts <- table(v)
  
  # Complete with all cases
  u_cats <- seq_along(get_risk_cat_dict())
  all_counts <- rep(0, length(u_cats))
  names(all_counts) <- u_cats
  all_counts[names(counts)] <- counts
  
  # Compute cumulative probability of being in a risk category larger or equal
  res <- all_counts/sum(all_counts)
  cum_prob <- cumsum(rev(res))
  cum_prob_thresh <- cum_prob[dplyr::first(which(cum_prob >= thresh))]
  names(cum_prob_thresh) <- NULL
  
  c(
    "risk_cat" = dplyr::first(rev(names(all_counts))[which(cum_prob >= thresh)]) %>% as.numeric(),
    "cumul_prob" = cum_prob_thresh
  )
}


#' Title
#'
#' @param draws 
#' @param var_name 
#' @param to_name 
#' @param to_value 
#'
#' @return
#' @export
#'
#' @examples
draws_to_df <- function(draws,
                        var_name,
                        to_name = "variable",
                        to_value = "value",
                        filter_draws = 4000) {
  
  draws <- draws %>% 
    posterior::as_draws()
  
  # Subsample draws if asked for
  if (!is.null(filter_draws)) {
    
    # Get all draws
    all_draws <- prod(dim(draws)[1:2])
    
    if (filter_draws > all_draws) {
      stop("Asked for ", filter_draws, " draws but only ", all_draws, " available.")
    }
    
    draws_subset <- sample(seq_len(all_draws), filter_draws, replace = FALSE)
    
    draws <- draws %>% 
      posterior::subset_draws(draw = draws_subset) 
  } 
  
  draws %>% 
    posterior::as_draws_df() %>% 
    dplyr::as_tibble() %>% 
    tidyr::pivot_longer(cols = contains(var_name),
                        names_to = to_name,
                        values_to = to_value) %>% 
    dplyr::select(-.iteration, -.chain)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_country_from_string <- function(x) {
  stringr::str_extract(x, "[A-Z]{3}")
}

#' custom_summaries
#' Custom summaries to get the 95% CrI
#'
#' @return
#' @export
#'
#' @examples
custom_summaries <- function() {
  
  c(
    "mean", "median", "custom_quantile2",
    posterior::default_convergence_measures(),
    posterior::default_mcse_measures()
  )
}

#' cri_interval
#' The Credible interval to report in summaries
#' @return
#' @export
#'
#' @examples
cri_interval <- function() {
  c(0.025, 0.975)
}

#' custom_quantile2
#' quantile functoin with custom cri
#'
#' @param x 
#' @param cri 
#'
#' @return
#' @export
#'
#' @examples
custom_quantile2 <- function(x, cri = cri_interval()) {
  posterior::quantile2(x, probs = cri)
}


#' get_coverage
#'
#' @param df 
#' @param widths 
#'
#' @return
#' @export
#'
#' @examples
get_coverage <- function(df, 
                         widths = c(seq(.05, .95, by = .1), .99),
                         with_period = FALSE){
  
  purrr::map_df(widths, function(w) {
    bounds <- str_c("q", c(.5 - w/2, .5 + w/2)*100)
    
    df %>% 
      {
        x <- .
        if (!with_period){
          dplyr::select(x, country, admin_level, observation, censoring, 
                        dplyr::one_of(bounds))
        } else {
          dplyr::select(x, country, admin_level, observation, censoring, 
                        period,
                        dplyr::one_of(bounds))
        }
      } %>%
      dplyr::mutate(in_cri = observation >= !!rlang::sym(bounds[1]) &
                      observation <= !!rlang::sym(bounds[2])) %>% 
      {
        x <- .
        if (!with_period){
          dplyr::group_by(x, country, admin_level)
        } else {
          dplyr::group_by(x, period, country, admin_level)
        }
      } %>% 
      dplyr::summarise(frac_covered = sum(in_cri)/n()) %>% 
      dplyr::mutate(cri = w) %>% 
      dplyr::ungroup()
  })
}

#' aggregate_and_summarise_case_draws
#'
#' @param df 
#' @param col 
#' @param grouping_variables 
#' @param weights_col 
#'
#' @return
#' @export
#'
#' @examples
aggregate_and_summarise_draws <- function(df, 
                                          col = "country_cases",
                                          grouping_variables = NULL,
                                          weights_col = NULL,
                                          do_summary = TRUE) {
  
  df %>% 
    dplyr::group_by_at(c(".draw", grouping_variables)) %>% 
    {
      x <- .
      if (is.null(weights_col)) {
        dplyr::summarise(x, tot = sum(!!rlang::sym(col))) 
      } else {
        dplyr::summarise(x, tot = sum(!!rlang::sym(col) * !!rlang::sym(weights_col))/sum(!!rlang::sym(weights_col))) 
      }
    } %>% 
    {
      x <- .
      if (is.null(grouping_variables)) {
        x
      } else {
        x %>% dplyr::ungroup() %>% 
          tidyr::pivot_wider(names_from = grouping_variables,
                             values_from = "tot") %>% 
          janitor::clean_names() %>% 
          dplyr::select(-draw) %>% 
          magrittr::set_names(stringr::str_c(col, colnames(.), sep = "_") %>% 
                                stringr::str_remove("x"))
      }
    }  %>% 
    posterior::as_draws() %>%
    {
      x <- .
      if(do_summary) {
        posterior::summarise_draws(x, custom_summaries())
      } else {
        posterior::as_draws_df(x) %>% 
          dplyr::as_tibble()
      }
    }
}

#' aggregate_and_summarise_case_draws_by_region
#'
#' @param df 
#' @param col 
#' @param grouping_variables 
#' @param weights_col 
#'
#' @return
#' @export
#'
#' @examples
aggregate_and_summarise_draws_by_region <- function(df, 
                                                    col = "country_cases",
                                                    grouping_variables = NULL,
                                                    weights_col = NULL,
                                                    do_summary = TRUE) {
  
  # Define columns from which to extract names
  if (!is.null(grouping_variables)) {
    name_cols <-   c("AFRO_region", grouping_variables)
  } else {
    name_cols <- "AFRO_region"
  }
  
  df %>% 
    get_AFRO_region(ctry_col = "country") %>% 
    dplyr::group_by_at(c(".draw", "AFRO_region", grouping_variables)) %>% 
    {
      x <- .
      if (is.null(weights_col)) {
        dplyr::summarise(x, tot = sum(!!rlang::sym(col))) 
      } else {
        dplyr::summarise(x, tot = sum(!!rlang::sym(col) * !!rlang::sym(weights_col))/sum(!!rlang::sym(weights_col))) 
      }
    } %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = name_cols,
                       values_from = "tot") %>% 
    janitor::clean_names() %>% 
    dplyr::select(-draw) %>% 
    magrittr::set_names(stringr::str_c(col, colnames(.), sep = "_")) %>% 
    posterior::as_draws() %>% 
    {
      x <- .
      if(do_summary) {
        posterior::summarise_draws(x, custom_summaries())
      } else {
        posterior::as_draws_df(x) %>% 
          dplyr::as_tibble()
      }
    }
}


#' tidy_shapefiles
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
tidy_shapefiles <- function(df) {
  
  df %>% 
    # Remove holes 
    nngeo::st_remove_holes() %>% 
    # Remove small islands 
    rmapshaper::ms_filter_islands(min_area = 1e9) %>% 
    rmapshaper::ms_simplify(keep = 0.05,
                            keep_shapes = FALSE) 
  
}

#' join_output_shapefiles
#'
#' @param output 
#' @param output_shapefiles 
#' @param var_col
#'
#' @return
#' @export
#'
#' @examples
join_output_shapefiles <- function(output,
                                   output_shapefiles,
                                   var_col = "variable") {
  
  output %>% 
    dplyr::mutate(shp_id = str_extract(!!rlang::sym(var_col), "[0-9]+") %>% as.numeric()) %>% 
    dplyr::inner_join(output_shapefiles, ., by = "shp_id")
  
}

#' join_output_shapefiles_by_time
#'
#' @param output 
#' @param output_shapefiles 
#' @param var_col
#'
#' @return
#' @export
#'
#' @examples
join_output_shapefiles_by_time <- function(output,
                                   output_shapefiles,
                                   var_col = "variable") {
  
  output %>% 
    dplyr::filter(stringr::str_detect(location_period_id, "ADM0")) %>% 
    dplyr::left_join(output_shapefiles %>% dplyr::filter(admin_level == "ADM0"), ., by = c("location_period_id")) 
  
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
get_no_w_runs <- function() {
  # TODO
  c("RWA-2011_2015")
}


#' get_intended_runs
#'
#' List of all countries with intended run
#' Sheet Data Entry Tracking Feb 2022 from spreadsheet that can be downloaded from
#' https://docs.google.com/spreadsheets/d/17MtTdUlC2tNLk3QPYdTzgFqiPacUg4rbi8cVTYoOaZE/edit#gid=1264119518
#'
#' @param csv_path 
#'
#' @return
#' @export
#'
#' @examples
get_intended_runs <- function(csv_path = "Analysis/output/Data Entry Coordination - Data Entry Tracking - Feb 2022.csv") {
  readr::read_csv(csv_path) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(country != "GMB") %>% 
    dplyr::mutate(isocode = dplyr::case_when(
      stringr::str_detect(country, "TZA") ~ "TZA",
      T ~ country))
}

#' simulate_observations
#' Simulated observations based on observation model
#' 
#' @param mu 
#' @param obs_model 
#' @param od_param 
#'
#' @return
#' @export
#'
#' @examples
simulate_observations <- function(mu, 
                                  obs_model, 
                                  od_param = NULL) {
  if (obs_model == 1) {
    # Poisson
    rpois(length(mu), mu)
  } else if (obs_model == 2) {
    # Quasi-poisson
    rnbinom(length(mu), mu = mu, size = od_param * mu)
  } else {
    # Negative binomial
    rnbinom(length(mu), mu = mu, size = od_param)
  }
}

# Postprocessing functions for global-level stiching ---------------------------------------------------------

#' get global regions
#' get_global_region
#' @param data data frame with country name
#' @param ctry_col colname of country name
#' @return 
#' @export
#' @examples
get_global_region <- function(data, ctry_col) {
  
  data_with_global_region <- data %>% 
    dplyr::mutate(
      global_region = dplyr::case_when(
        
        # !!rlang::sym(ctry_col) %in% c("BDI","COM","ETH","KEN","MDG","RWA","SSD","UGA","TZA","ERI") ~ "Eastern Africa",
        # !!rlang::sym(ctry_col) %in% c("BWA","MOZ","MWI","NAM","SWZ","ZMB","ZWE","ZAF","LSO") ~ "Southern Africa",
        # !!rlang::sym(ctry_col) %in% c("AGO","CMR","CAF","TCD","COG","COD","GNQ","GNA","GAB") ~ "Central Africa",
        # !!rlang::sym(ctry_col) %in% c("BEN","BFA","CIV","GHA","GIN","GMB","GNB","LBR","MLI","MRT","NER","NGA","SEN","SLE","TGO") ~ "Western Africa",
        !!rlang::sym(ctry_col) %in% c("BDI","COM","ETH","KEN","MDG","RWA","SSD","UGA","TZA","ERI",
                                      "BWA","MOZ","MWI","NAM","SWZ","ZMB","ZWE","ZAF","LSO",
                                      "AGO","CMR","CAF","TCD","COG","COD","GNQ","GNA","GAB",
                                      "BEN","BFA","CIV","GHA","GIN","GMB","GNB","LBR","MLI","MRT","NER","NGA","SEN","SLE","TGO") ~ "Africa",
        !!rlang::sym(ctry_col) %in% c("DOM","HTI") ~ "Americas",
        !!rlang::sym(ctry_col) %in% c("BGD","MMR","NPL","THA","IND") ~ "South-East Asia",
        !!rlang::sym(ctry_col) %in% c("AFG","IRQ","LBN","PAK","SAU","SYR","ARE","YEM","SDN","SOM","DJI") ~ "Eastern Mediterranean",
        !!rlang::sym(ctry_col) %in% c("CHN","PHL") ~ "Western Pacific",
        !!rlang::sym(ctry_col) %in% c("MYT") ~ "Europe",
        TRUE ~ !!rlang::sym(ctry_col)
      ) 
    )
  
  return(data_with_global_region)
}


#' get_global_region_levels
#'
#' @return
#' @export
#'
#' @examples
get_global_region_levels <- function() {
  c("Eastern Mediterranean",
    #"Western Africa", "Central Africa", "Eastern Africa", "Southern Africa",
    "Africa",
    "South-East Asia","Western Pacific","Americas","Europe")
}


#' aggregate_and_summarise_case_draws_by_global_region
#'
#' @param df 
#' @param col 
#' @param grouping_variables 
#' @param weights_col 
#'
#' @return
#' @export
#'
#' @examples
aggregate_and_summarise_draws_by_global_region <- function(df, 
                                                           col = "country_cases",
                                                           grouping_variables = NULL,
                                                           weights_col = NULL,
                                                           do_summary = TRUE) {
  
  # Define columns from which to extract names
  if (!is.null(grouping_variables)) {
    name_cols <-   c("global_region", grouping_variables)
  } else {
    name_cols <- "global_region"
  }
  
  df %>% 
    get_global_region(ctry_col = "country") %>% 
    dplyr::group_by_at(c(".draw", "global_region", grouping_variables)) %>% 
    {
      x <- .
      if (is.null(weights_col)) {
        dplyr::summarise(x, tot = sum(!!rlang::sym(col))) 
      } else {
        dplyr::summarise(x, tot = sum(!!rlang::sym(col) * !!rlang::sym(weights_col))/sum(!!rlang::sym(weights_col))) 
      }
    } %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = name_cols,
                       values_from = "tot") %>% 
    janitor::clean_names() %>% 
    dplyr::select(-draw) %>% 
    magrittr::set_names(stringr::str_c(col, colnames(.), sep = "_")) %>% 
    posterior::as_draws() %>% 
    {
      x <- .
      if(do_summary) {
        posterior::summarise_draws(x, custom_summaries())
      } else {
        posterior::as_draws_df(x) %>% 
          dplyr::as_tibble()
      }
    }
}

#' postprocess_WHO_est
#' get the modeled WHO annual cases 
#' who_annual_report_OCs(World).csv is a file including country and its corresponding OC UID of the WHO annual reports
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_WHO_est <- function(config_list,
                                redo_aux = FALSE,who_path = 'Analysis/output/who_annual_report_OCs(World).csv') {
  # Get WHO annual report OCs
  who_OCs <- read.csv(who_path)
  
  # Get preprocess data
  load(config_list$file_names$observations_filename) 
  
  # Get who annual estimates
  who_ests <- sf_cases %>% dplyr::filter(OC_UID %in% who_OCs$Related.WHO.Annual.Report.OC)
  
  who_ests
}

# Case burden adjustment (cCh) scaling functions ---------------------------
# Added to support scaling country-level case draws from the main
# pipeline by (1) a fixed reporting-completeness ratio, (2) a
# global test-positivity posterior, (3) country-level under-5 proportion
# posteriors from the case age-distribution model
# (cad_model4_country_random_endemicity_random.stan, fit in
# fit_cholera_age_model.R, not part of this pipeline's per-country loop),
# and (4) global care-seeking posteriors -- all from a systematic review.
# See postprocess_results.R section K ("Case burden adjustment") for how
# these are chained together.

#' align_draws_to_reference
#'
#' Resample a posterior to exactly n_draws draws, re-indexed .draw = 1:n_draws,
#' so it can be paired via inner_join() with draws from an independently-fit
#' model or an externally-sourced posterior. Works for both country-level
#' draws (grouped by country, e.g. p_u5 from the age-distribution model) and
#' global/scalar-source draws with no country column (e.g. positivity,
#' care-seeking, sourced from a systematic review and shared across every
#' country). Sampling is with replacement only when the source has fewer
#' draws than n_draws (genuine upsampling, e.g. stretching p_u5 to match a
#' larger n_draws) -- when the source has more draws than n_draws (e.g. the
#' systematic-review posteriors, typically ~8000 draws, being thinned down
#' to match the geospatial model's smaller case-draw count), sampling is
#' without replacement, so no artificial duplicate draws are introduced
#' where the real posterior has plenty of distinct draws to spare.
#'
#' @param draws_df a tibble with a .draw column (and, optionally, a country column)
#' @param n_draws number of draws to resample to
#'
#' @return draws_df resampled to n_draws rows (per country, if a country
#' column is present), with .draw re-indexed 1:n_draws
#' @export
#'
#' @examples
align_draws_to_reference <- function(draws_df, n_draws) {

  if ("country" %in% names(draws_df)) {
    draws_df %>%
      dplyr::group_by(country) %>%
      dplyr::group_modify(~ dplyr::slice_sample(.x, n = n_draws, replace = nrow(.x) < n_draws)) %>%
      dplyr::mutate(.draw = dplyr::row_number()) %>%
      dplyr::ungroup()
  } else {
    draws_df %>%
      dplyr::slice_sample(n = n_draws, replace = nrow(draws_df) < n_draws) %>%
      dplyr::mutate(.draw = dplyr::row_number())
  }
}

#' combine_draws
#'
#' Join two draws tibbles and combine two columns with a binary operator.
#' Covers positivity scaling, severity-split multiplication, care-seeking
#' scaling, and the final mild + severe summation -- call it with different
#' inputs/op rather than writing a separate function for each step.
#'
#' df1 may be at any granularity that includes a "country" and ".draw"
#' column -- in particular, per-admin-unit draws (one row per
#' location_period_id/admin_level/draw, several rows per country), not just
#' one row per country/draw. df2 supplies a country-level (or fully global)
#' scaling factor, joined on "country" (when df2 has one) and ".draw"; every
#' row of df1 sharing that (country, .draw) -- e.g. every admin unit of that
#' country -- receives the same scaling value. If df2 has no "country"
#' column at all (a global posterior, e.g. from a systematic review), the
#' join is on .draw only, so that draw's value is broadcast to every row of
#' df1 sharing that .draw index, country or admin unit alike.
#'
#' All of df1's columns (country, .draw, and anything else identifying the
#' row -- location_period_id, admin_level, shp_id, etc.) are kept as-is;
#' only the two input value columns are consumed, so admin-unit identity
#' survives the whole scaling chain unless the caller explicitly drops it.
#'
#' @param df1 draws tibble with at least country and .draw columns -- may
#' have one row per country/draw or one row per admin-unit/draw
#' @param col1 name of the column in df1 to combine
#' @param df2 country x .draw (or global x .draw) tibble supplying the
#' scaling factor
#' @param col2 name of the column in df2 to combine
#' @param out_col name of the output column
#' @param op binary operator to apply (default `*`)
#'
#' @return df1 with col1/col2 replaced by out_col, all other df1 columns kept
#' @export
#'
#' @examples
combine_draws <- function(df1, col1, df2, col2, out_col, op = `*`) {

  join_cols <- setdiff(intersect(names(df1), names(df2)), c(col1, col2))

  # Only pull the join keys + the one value column from df2, so df1's own
  # columns (admin_level, location_period_id, shp_id, ...) are never
  # touched or dropped by the join.
  df2_slim <- df2 %>% dplyr::select(dplyr::all_of(join_cols), dplyr::all_of(col2))

  result <- dplyr::inner_join(df1, df2_slim, by = join_cols) %>%
    dplyr::mutate(!!out_col := op(!!rlang::sym(col1), !!rlang::sym(col2))) %>%
    dplyr::select(-dplyr::any_of(setdiff(c(col1, col2), out_col)))

  # ADDED: fail loudly if join_cols does not uniquely identify rows in df2 
  if (nrow(result) != nrow(df1)) {
    stop("combine_draws(): row count changed from ", nrow(df1), " to ", nrow(result),
        " after joining on (", paste(join_cols, collapse = ", "), ") -- df2 is not unique on ",
        "those columns, so this join was many-to-many rather than many-to-one. Check whether ",
        "df1/df2 have overlapping keys across separate configs (e.g. multiple time-period ",
        "configs for the same country sharing .draw indices).")
  }

  result
}

#' scale_by_reporting_ratio
#'
#' Inflate reported case draws to an estimate of all medically-attended
#' cases, by dividing by the reporting-completeness ratio (reported / all
#' medically-attended sCh) -- a single GLOBAL scalar (see
#' get_config_reporting_ratio()), applied identically to every country and
#' every admin unit. Unlike severity_u5/severity_o5, this is not a
#' per-country value, so there is no join here at all -- just a division
#' applied to every row. Divides, rather than multiplies, since the ratio
#' is defined as reported/all (<= 1 under underreporting), so
#' all = reported / ratio.
#'
#' @param cases_draws country x .draw (or admin-unit x .draw) tibble of case draws
#' @param value_col name of the column in cases_draws to scale
#' @param reporting_ratio single numeric scalar (see get_config_reporting_ratio())
#'
#' @return cases_draws with value_col divided by reporting_ratio
#' @export
#'
#' @examples
scale_by_reporting_ratio <- function(cases_draws, value_col, reporting_ratio) {

  cases_draws %>%
    dplyr::mutate(!!value_col := !!rlang::sym(value_col) / reporting_ratio)
}

#' get_config_reporting_ratio
#'
#' The reporting-completeness ratio (reported / all medically-attended
#' sCh) -- a single scalar shared by every country, unlike severity_u5/
#' severity_o5 (see get_config_severity_scalars(), a deliberately separate
#' function: this one returns one global number, not a country-keyed
#' tibble, and scale_by_reporting_ratio() uses it without any join). Still
#' read from each config's scaling: section (reporting_ratio is broadcast
#' identically to every config by write_batch_mapping_config_general.R's
#' single shared params_df row), but every config in config_dir is checked
#' to actually agree -- it errors loudly, listing the distinct values
#' found, rather than silently using whichever config happened to be read
#' first, if any config disagrees (e.g. one was hand-edited and the others
#' weren't).
#'
#' @param config_dir directory of per-country config ymls (same as run_all()'s config_dir)
#'
#' @return single numeric scalar: the reporting_ratio value common to every config in config_dir
#' @export
#'
#' @examples
get_config_reporting_ratio <- function(config_dir) {

  config_files <- list.files(config_dir, pattern = "\\.yml$", full.names = TRUE)

  ratios <- purrr::map_dbl(config_files, function(f) {
    cfg <- yaml::read_yaml(f)
    cfg$scaling$reporting_ratio
  })

  if (dplyr::n_distinct(ratios) > 1) {
    stop("reporting_ratio disagrees across configs in ", config_dir,
        " -- it is meant to be a single global scalar, but found: ",
        paste(unique(ratios), collapse = ", "))
  }

  ratios[1]
}

#' get_config_severity_scalars
#'
#' The severity-by-age-class scalars (proportion of cCh cases that are
#' moderate-to-severe, among under-5s and among 5-and-overs) -- two GLOBAL
#' scalars shared by every country, the same pattern as
#' get_config_reporting_ratio(): read from every config's scaling: section
#' (severity_u5/severity_o5 are broadcast identically to every config by
#' write_batch_mapping_config_general.R's single shared params_df row),
#' checked for agreement across every config file, and returned as two
#' plain numbers rather than a country-keyed tibble. Errors loudly, listing
#' the distinct values found, if any config disagrees with the others (e.g.
#' one time window was hand-edited).
#'
#' Deliberately a separate function from get_config_reporting_ratio(), not
#' a shared/reused one, even though both now follow the same
#' read-and-verify-a-global-scalar pattern.
#'
#' @param config_dir directory of per-country config ymls (same as run_all()'s config_dir)
#'
#' @return named list: severity_u5, severity_o5 -- each a single numeric
#' scalar common to every config in config_dir
#' @export
#'
#' @examples
get_config_severity_scalars <- function(config_dir) {

  config_files <- list.files(config_dir, pattern = "\\.yml$", full.names = TRUE)

  per_config <- purrr::map_dfr(config_files, function(f) {
    cfg <- yaml::read_yaml(f)
    tibble::tibble(severity_u5 = cfg$scaling$severity_u5,
                   severity_o5 = cfg$scaling$severity_o5)
  })

  if (dplyr::n_distinct(per_config$severity_u5) > 1 | dplyr::n_distinct(per_config$severity_o5) > 1) {
    stop("severity_u5/severity_o5 disagree across configs in ", config_dir,
        " -- they are meant to be single global scalars, but found severity_u5: ",
        paste(unique(per_config$severity_u5), collapse = ", "),
        "; severity_o5: ", paste(unique(per_config$severity_o5), collapse = ", "))
  }

  list(severity_u5 = per_config$severity_u5[1], severity_o5 = per_config$severity_o5[1])
}

#' compute_severity_proportions
#'
#' Country-level, per-draw proportion of medically-attended cCh that are
#' mild vs. moderate-to-severe: a weighted average of the two global
#' severity-by-age-class scalars (see get_config_severity_scalars()),
#' weighted by that country's (and that draw's) posterior proportion of
#' cases under 5 -- so the result varies by country and draw even though
#' severity_u5/severity_o5 themselves do not. No join against a severity
#' table is needed here (severity_u5/severity_o5 are plain scalars, not a
#' per-country tibble), so this is a straight mutate() on p_u5_draws.
#'
#' @param p_u5_draws country x .draw tibble with column p_u5 (proportion of cases under 5)
#' @param severity_u5 single numeric scalar: proportion of under-5 cases that are moderate-to-severe
#' @param severity_o5 single numeric scalar: proportion of 5-and-over cases that are moderate-to-severe
#' (both from get_config_severity_scalars())
#'
#' @return tibble with country, .draw, prop_severe, prop_mild
#' @export
#'
#' @examples
compute_severity_proportions <- function(p_u5_draws, severity_u5, severity_o5) {

  p_u5_draws %>%
    dplyr::mutate(
      prop_severe = p_u5 * severity_u5 + (1 - p_u5) * severity_o5,
      prop_mild = 1 - prop_severe
    ) %>%
    dplyr::select(country, .draw, prop_severe, prop_mild)
}

#' postprocess_country_p_u5
#'
#' Per-country, per-draw posterior of the proportion of cases under 5, from
#' the fitted case age-distribution model (model 4:
#' cad_model4_country_random_endemicity_random.stan, fit by
#' fit_cholera_age_model.R, saved to scaling_input_dir). Draw-level
#' counterpart of compute_country_pbar_summary() in
#' cholera_age_model_fits.qmd -- averages a country's own per-observation
#' pbar draws across whatever years/endemic-statuses it had, rather than
#' collapsing to quantiles.
#'
#' @param scaling_input_dir directory containing the saved model4 fit,
#' country_lookup.csv, and country_id_vec.csv
#' @param country_lookup tibble with columns country, country_id (defaults to
#' reading country_lookup.csv from scaling_input_dir)
#' @param country_id_vec integer vector, length = number of age-model rows,
#' giving each row's country_id (defaults to reading country_id_vec.csv from
#' scaling_input_dir)
#'
#' @return tibble with country, .draw, p_u5 -- one row per country present in
#' country_lookup, per draw
#' @export
#'
#' @examples
postprocess_country_p_u5 <- function(scaling_input_dir,
                                     country_lookup = readr::read_csv(
                                       file.path(scaling_input_dir, "country_lookup.csv")),
                                     country_id_vec = readr::read_csv(
                                       file.path(scaling_input_dir, "country_id_vec.csv"))$country_id) {

  fit <- readRDS(file.path(scaling_input_dir, "fit_cad_model4_country_random_endemicity_random.rds"))
  pbar_mat <- unclass(fit$draws("pbar", format = "matrix"))
  storage.mode(pbar_mat) <- "double"

  country_ids_present <- sort(unique(country_id_vec))

  purrr::map_dfr(country_ids_present, function(cid) {
    cols <- which(country_id_vec == cid)
    draws <- if (length(cols) == 1) pbar_mat[, cols] else rowMeans(pbar_mat[, cols, drop = FALSE])
    tibble::tibble(
      country = country_lookup$country[match(cid, country_lookup$country_id)],
      .draw = seq_along(draws),
      p_u5 = draws
    )
  })
}

#' postprocess_country_p_u5_fallback
#'
#' Fallback per-draw proportion of cases under 5, for countries with no
#' age-split data at all (absent from country_lookup, so
#' postprocess_country_p_u5() has nothing to compute for them). Uses model
#' 4's p_epidemic_posterior -- a single scalar per draw, identical for every
#' no-data country, computed in the model's generated quantities block from
#' gamma0 + w[epidemic] with the country term dropped (see
#' cholera_age_distribution.qmd, "Predicting for countries with no data").
#'
#' @param scaling_input_dir directory containing the saved model4 fit
#' @param missing_countries character vector of country codes with no
#' age-split data (i.e. absent from country_lookup)
#'
#' @return tibble with country, .draw, p_u5 -- one row per missing country, per draw
#' @export
#'
#' @examples
postprocess_country_p_u5_fallback <- function(scaling_input_dir, missing_countries) {

  if (length(missing_countries) == 0) {
    return(tibble::tibble(country = character(0), .draw = integer(0), p_u5 = numeric(0)))
  }

  fit <- readRDS(file.path(scaling_input_dir, "fit_cad_model4_country_random_endemicity_random.rds"))
  p_fallback <- as.numeric(unclass(fit$draws("p_epidemic_posterior", format = "matrix")))

  tidyr::expand_grid(country = missing_countries, .draw = seq_along(p_fallback)) %>%
    dplyr::mutate(p_u5 = rep(p_fallback, times = length(missing_countries)))
}

#' postprocess_admin_cases_draws
#'
#' Case draws for every output admin unit (ADM0 and all subnational levels),
#' for one country's config -- the draw-level counterpart of
#' postprocess_mean_annual_cases() (which reads the same
#' "location_mean_cases_output" generated quantity but collapses it to a
#' posterior summary via genquant$summary(), discarding draws), built the
#' same way postprocess_mean_annual_incidence_draws() already does for
#' rates rather than cases -- that function is the existing template this
#' one mirrors. Used as the base input to the case burden adjustment
#' (postprocess_results.R section K) so that country-level scaling factors
#' (reporting ratio, positivity, age-distribution, care-seeking) can be
#' broadcast, via combine_draws(), to every admin unit of that country at
#' once.
#'
#' filter_draws is NOT left NULL here (unlike an earlier version of this
#' function): postprocess_adm0_cases()/postprocess_adm0_rates() keep every
#' raw draw a country's fit produced, with no cap, and different countries'
#' fits can have different total draw counts (e.g. different chains/
#' iter_sampling settings). Left uncapped, run_all()'s bind_rows() across
#' countries would let n_draws (computed downstream in section K as the
#' number of distinct .draw values present anywhere) be set by whichever
#' country's fit produced the MOST draws -- every other country would then
#' silently lose admin units in every combine_draws() inner_join() for any
#' .draw index beyond its own real draw count, with no warning. Capping
#' here instead makes every country contribute the same, deliberately
#' chosen number of real draws.
#'
#' @param config_list config list
#' @param redo_aux whether to rebuild the output shapefile join
#' @param filter_draws number of draws to keep per country (default 1000 --
#' set this to the smallest raw draw count actually produced across your
#' country configs' Stan fits, not left to an arbitrary default, since a
#' value larger than some country's real draw count will error via
#' draws_to_df()'s own "Asked for X draws but only Y available" check)
#'
#' @return tibble with one row per admin unit per draw: .draw, admin_cases,
#' location_period_id, admin_level, country, and any other columns carried
#' by output_shapefiles
#' @export
#'
#' @examples
postprocess_admin_cases_draws <- function(config_list,
                                          redo_aux = FALSE,
                                          filter_draws = 1000) {

  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename)

  # location_mean_cases_output is indexed over every output location for
  # this country's config (ADM0 and subnational alike) -- not filtered to
  # a single adm0_ind the way postprocess_adm0_cases() is.
  cases_draws <- genquant$draws("location_mean_cases_output") %>%
    draws_to_df(var_name = "location_mean_cases_output", filter_draws = filter_draws)

  # Get the output shapefiles (admin_level, location_period_id, country,
  # shp_id, ...) for every admin unit and join
  output_shapefiles <- get_output_sf_reload(config_list = config_list,
                                            redo = redo_aux)

  res <- join_output_shapefiles(output = cases_draws,
                                output_shapefiles = output_shapefiles %>%
                                  sf::st_drop_geometry(),
                                var_col = "variable") %>%
    dplyr::select(-variable) %>%
    dplyr::rename(admin_cases = value)

  # ADDED: tag every row with which config (time window) produced it.
  # location_period_id alone does NOT disambiguate two time-period configs
  # for the same country when the underlying admin boundaries are stable
  # across both windows (e.g. BDI 2011-2015 vs. 2016-2020 sharing the same
  # location_period_id for a given admin unit) -- without this, downstream
  # combine_draws() calls that join two admin-level tables together (e.g.
  # the final mild + severe sum) have no column left to distinguish the
  # two configs' draws, and silently fan out many-to-many. .draw indices
  # are independent per config (separate Stan fits), so this is not
  # optional metadata -- it is the only thing that makes (country, .draw)
  # actually unique per admin unit once a country has multiple configs.
  res <- res %>%
    dplyr::mutate(period_start = config_list$start_time,
                  period_end = config_list$end_time)

  res
}
