# Scripts to postprocess results for continent-level stiching


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
  
  # Add config directory to prefix
  prefix_add <- config_dir %>% 
    # Remove tailing / to ensure non-empty string
    stringr::str_remove("/$") %>% 
    stringr::str_split("/") %>% 
    .[[1]] %>% 
    last()
  
  prefix_dir <- ifelse(is.null(prefix), prefix_add, paste(prefix, prefix_add, sep = "_"))
  
  res_file <- make_std_output_name(output_dir = output_dir,
                                   fun_name = fun_name,
                                   prefix = prefix_dir,
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
  res <- get_output_sf_reload(config_list = config_list,
                              redo = redo_aux) %>% 
    dplyr::bind_cols(mai_summary) %>% 
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
postprocess_mai_adm0_cases <- function(config_list,
                                       redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get index of national-level space output
  adm0_ind <- get_adm0_index(config_list = config_list)
  
  # This assumes that the first output shapefile is always the national-level shapefile
  cases_adm0 <- genquant$draws(stringr::str_glue("location_total_cases_output[{adm0_ind}]")) %>% 
    posterior::as_draws() %>% 
    posterior::as_draws_df() %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(country_cases = `location_total_cases_output[1]`) %>% 
    dplyr::mutate(country = get_country_from_string(config_list$file_names$stan_genquant_filename))
  
  
  cases_adm0
}


#' postprocess_mai_adm0_pop
#' 
#' @param config_list config list
#'
#' @return
#' @export
#'
postprocess_mai_adm0_pop <- function(config_list,
                                     redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get index of national-level output
  adm0_ind <- get_adm0_index(config_list = config_list)
  
  # This assumes that the first output shapefile is always the national-level shapefile
  pop_adm0 <- genquant$summary(stringr::str_glue("pop_loc_output[{adm0_ind}]", mean)) %>% 
    dplyr::rename(country_pop = `pop_loc_output[1]`)
  
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
  res <- get_output_sf_reload(config_list = config_list,
                              redo = redo_aux) %>% 
    dplyr::bind_cols(cov_summary) %>% 
    dplyr::select(-variable)
  
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
                  pop = output_location_pop$mean) %>% 
    dplyr::bind_cols(get_output_sf_reload(config_list = config_list,
                                          redo = redo_aux), .)
  
  risk_cat
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
    dplyr::mutate(risk_cat = risk_cat_dict[as.numeric(str_extract(variable, "(?<=\\[)[0-9]+(?=,)"))],
                  risk_cat = factor(risk_cat, levels = risk_cat_dict),
                  admin_level = as.numeric(str_extract(variable, "(?<=,)[0-9]+(?=\\])")) - 1,
                  country = taxdat::get_country_isocode(config_list))
  
  pop_at_risk
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
  
  mapped_gen_obs <- gen_obs[stan_input$stan_data$map_obs_loctime_combs, ] %>% 
    dplyr::bind_cols(stan_input$sf_cases_resized %>% 
                       sf::st_drop_geometry() %>% 
                       dplyr::filter(!is.na(loctime)) %>% 
                       dplyr::select(observation = attributes.fields.suspected_cases,
                                     censoring,
                                     admin_level)) %>% 
    dplyr::mutate(obs_gen_id = stringr::str_extract(variable, "[0-9]+") %>% as.numeric()) %>% 
    dplyr::add_count(obs_gen_id)
  
  
  mapped_gen_obs
  
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
    res <- taxdat::read_file_of_type(config_list$file_names$observations_filename, "output_shapefiles")
    stan_input <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")
    
    # Keep only output shapefiles with data
    res <- res %>% 
      dplyr::filter(location_period_id %in% stan_input$fake_output_obs$locationPeriod_id)
    
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
  
  # stan_input <- taxdat::read_file_of_type(config_list$file_names$stan_input_filename, "stan_input")
  # 
  # stan_input$output_lps %>% 
  #   dplyr::filter(admin_level == 0) %>% 
  #   dplyr::pull(shp_id)
  1
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
        !!rlang::sym(ctry_col) %in% c("BDI","ETH","KEN","MDG","RWA","SDN","SSD","UGA","TZA") ~ "Eastern Africa",
        !!rlang::sym(ctry_col) %in% c("MOZ","MWI","NAM","SWZ","ZMB","ZWE","ZAF") ~ "Southern Africa",
        !!rlang::sym(ctry_col) %in% c("AGO","CMR","CAF","TCD","COG","COD","GNQ","GAN") ~ "Middle Africa",
        !!rlang::sym(ctry_col) %in% c("BEN","BFA","CIV","GHA","GIN","GNB","LBR","MLI","MRT","NER","NGA","SEN","SLE","TGO") ~ "Western Africa",
        !!rlang::sym(ctry_col) %in% c("DJI","SOM","SDN") ~ "Eastern Mediterranean"
      ) 
    )
  
  return(data_with_AFRO_region)
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
  
  res
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
collapse_grid <- function(df) {
  
  u_grid <- df  %>%
    dplyr::group_by(rid, x, y) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(rid, x, y, geom)
  
  
  res <- df %>%
    sf::st_drop_geometry() %>% 
    dplyr::group_by(rid, x, y) %>% 
    dplyr::summarise(mean = mean(mean),
                     q2.5 = min(q2.5),
                     q97.5 = max(q97.5),
                     overlap = n() > 1) %>% 
    dplyr::inner_join(u_grid, .)
  
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
  
  c(
    "risk_cat" = dplyr::first(rev(names(all_counts))[which(cum_prob >= thresh)]) %>% as.numeric(),
    "cumul_prob" = cum_prob[dplyr::first(which(cum_prob >= thresh))]
  )
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
    "mean", "custom_quantile2",
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
                         widths = c(seq(.05, .95, by = .1), .99)){
  
  purrr::map_df(widths, function(w) {
    bounds <- str_c("q", c(.5 - w/2, .5 + w/2)*100)
    
    df %>% 
      dplyr::select(country, admin_level, observation, censoring, 
                    dplyr::one_of(bounds)) %>%
      dplyr::mutate(in_cri = observation >= !!rlang::sym(bounds[1]) &
                      observation <= !!rlang::sym(bounds[2])) %>% 
      dplyr::group_by(country, admin_level) %>% 
      dplyr::summarise(frac_covered = sum(in_cri)/n()) %>% 
      dplyr::mutate(cri = w)
  })
}

#' aggregate_and_summarise_case_draws
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
aggregate_and_summarise_draws <- function(df, 
                                          col = "country_cases") {
  df %>% 
    dplyr::group_by(.draw) %>% 
    dplyr::summarise(tot = sum(!!rlang::sym(col))) %>% 
    posterior::as_draws() %>% 
    posterior::summarise_draws()
}


#' aggregate_and_summarise_case_draws_by_region
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
aggregate_and_summarise_draws_by_region <- function(df, 
                                                    col = "country_cases") {
  df %>% 
    get_AFRO_region(ctry_col = "country") %>% 
    dplyr::group_by(.draw, AFRO_region) %>% 
    dplyr::summarise(tot = sum(!!rlang::sym(col))) %>%
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = "AFRO_region",
                       values_from = "tot") %>% 
    janitor::clean_names() %>% 
    dplyr::select(-draw) %>% 
    magrittr::set_names(stringr::str_c(col, colnames(.), sep = "_")) %>% 
    posterior::as_draws() %>% 
    posterior::summarise_draws()
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
