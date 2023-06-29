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
    res <- fun(config_list = config_list,
               redo_aux = redo_aux) %>% 
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
        
        args <- c(
          list(config = config,
               redo = redo_interm,
               redo_aux = redo_aux,
               prefix = prefix,
               suffix = suffix,
               fun_name = fun_name,
               fun = fun,
               output_dir = interm_dir,
               data_dir = data_dir,
               verbose = verbose),
          fun_opts
        )
        
        res <- do.call(postprocess_wrapper, args)
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
  mai_summary <- genquant$summary("location_total_rates_output")
  
  # Get the output shapefiles and join
  res <- get_output_sf_reaload(config_list = config_list,
                               redo = redo_aux) %>% 
    dplyr::bind_cols(mai_summary) %>% 
    dplyr::select(-variable, -median, -sd, -mad)
  
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
  
  res <- get_output_sf_reaload(config_list = config_list,
                               redo = redo_aux) %>% 
    dplyr::filter(admin_level == "ADM0")
  
  res
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
                                      redo_aux = FALSE) {
  
  # Get genquant data
  genquant <- readRDS(config_list$file_names$stan_genquant_filename) 
  
  # Get dictionnary of risk categories
  risk_cat_dict <- get_risk_cat_dict()
  
  # Get the population per output
  output_location_pop <- genquant$summary("pop_loc_output")
  
  # Get mean annual incidence summary
  risk_cat <- genquant$summary("location_risk_cat", mode) %>% 
    dplyr::mutate(risk_cat = risk_cat_dict[mode],
                  risk_cat = factor(risk_cat, levels = risk_cat_dict),
                  pop = output_location_pop$mean) %>% 
    dplyr::bind_cols(get_output_sf_reaload(config_list = config_list,
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
  pop_at_risk <- genquant$summary("tot_pop_risk") %>% 
    dplyr::mutate(risk_cat = risk_cat_dict[as.numeric(str_extract(variable, "(?<=\\[)[0-9]+(?=,)"))],
                  risk_cat = factor(risk_cat, levels = risk_cat_dict),
                  admin_level = as.numeric(str_extract(variable, "(?<=,)[0-9]+(?=\\])")) - 1,
                  country = get_country_from_filename(config_list$file_names$stan_genquant_filename))
  
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
  mai_summary <- genquant$summary("space_grid_rates")
  
  # Get the output shapefiles and join
  res <- get_space_grid(config_list = config_list,
                        redo = redo_aux) %>% 
    dplyr::bind_cols(mai_summary) %>% 
    dplyr::select(-variable, -median, -sd, -mad)
  
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
  mai_summary <- genquant$summary("space_grid_rates")
  
  # Get population and average over space grid
  mean_pop_sf <- get_mean_pop_grid(config_list = config_list,
                                   redo = redo_aux)
  
  # Get the output shapefiles and join
  res <- mean_pop_sf %>% 
    dplyr::bind_cols(mai_summary) %>% 
    dplyr::select(-variable, -median, -sd, -mad) %>% 
    dplyr::mutate(dplyr::across(.cols = c("mean", "q5", "q95"),
                                ~ . * pop))
  
  res
}


# Output shapefiles -------------------------------------------------------

#' get_output_sf_wrapper
#' This function enables loaded pre-extracted output shapefiles
#'
#' @param config 
#' @param data_dir 
#'
#' @return
#' @export
#'
#' @examples
get_output_sf_reaload <- function(config_list,
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

# get regions for countries -------------------------------------------------------

#' get_AFRO_region
#' @param data data frame with country name
#' @param ctry_col colname of country name
#' @return 
#' @export
#' @examples
get_AFRO_region <- function(data, ctry_col) {
  
  data_with_AFRO_region <- data %>% 
    mutate(
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
      mutate(pop = taxdat::read_file_of_type(config_list$file_names$stan_input_filename, variable = "stan_input")$stan_data$pop) %>% 
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
    group_by(rid, x, y) %>% 
    slice(1) %>% 
    select(rid, x, y, geom)
  
  
  res <- df %>%
    st_drop_geometry() %>% 
    group_by(rid, x, y) %>% 
    summarise(mean = mean(mean),
              q5 = min(q5),
              q95 = max(q95),
              overlap = n() > 1) %>% 
    inner_join(u_grid, .)
  
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
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_country_from_filename <- function(x) {
  stringr::str_extract(x, "[A-Z]{3}")
}

