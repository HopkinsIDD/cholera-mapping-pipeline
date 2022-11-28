#' @title Perapre stan input
#' @description Prepares the data for the Stan code
#'
#' @param dbuser
#' @param cholera_directory
#' @param smooth_covariate_number_timesteps
#' @param ncore
#' @param res_time
#' @param res_space
#' @param time_slices
#' @param cases_column
#' @param sf_cases
#' @param non_na_gridcells
#' @param sf_grid
#' @param location_periods_dict
#' @param covar_cube
#'
#' @return A list with the data
#'
prepare_stan_input <- function(
    dbuser,
    cholera_directory,
    smooth_covariate_number_timesteps,
    ncore,
    res_time,
    res_space,
    time_slices,
    cases_column,
    sf_cases,
    non_na_gridcells,
    sf_grid,
    location_periods_dict,
    covar_cube
) {
  
  library(sf)
  
  # Get covariate choices from covar_cube slice names
  covariate_choices <- dimnames(covar_cube)[[3]][-1]
  
  # Adjacency --------------------------------------------------------------------
  
  cat("Computing adjacency \n")
  
  grid_changer <- taxdat::make_changer(x = non_na_gridcells)
  
  # make the smooth grid
  smooth_grid_obj <- taxdat::make_smooth_grid(sf_grid = sf_grid,
                                              smooth_covariate_number_timesteps = smooth_covariate_number_timesteps)
  
  # Unpack
  sf_grid <- smooth_grid_obj$sf_grid            # updated sf_grid with column "s" for smooth grid index
  smooth_grid <- smooth_grid_obj$smooth_grid    # smooth grid sf object
  rm(smooth_grid_obj)
  
  # Define model time slices
  model_time_slices <- sort(unique(sf_grid$s))
  
  adjacency <- taxdat::make_adjacency(smooth_grid = smooth_grid,
                                      model_time_slices = model_time_slices)
  
  # Stan inputs ------------------------------------------------------------------
  cat("---- Preparing Stan input data \n")
  
  # ---- A. Adjacency ----
  stan_data <- list()
  
  stan_data$rho <- 0.999
  stan_data$N <-  length(non_na_gridcells)
  stan_data$N_edges <- nrow(adjacency$adjacency_list)
  stan_data$node1 <- adjacency$adjacency_list[, 1]
  stan_data$node2 <- adjacency$adjacency_list[, 2]
  stan_data$diag <- adjacency$number_of_neighbors
  
  # ---- B. Covariates ----
  stan_data$pop <- taxdat::extract_population(covar_cube = covar_cube,
                                              non_na_gridcells = non_na_gridcells)
  
  if (length(covariate_choices) > 0) {
    # Case when covariates are used
    # Flatten covariate cube to 2d array: [n_pix * n_time_units] * [n_cov]
    # Here the first covariate corresponds to the population raster, so needs to be
    # excluded. Data flattened by pixels first, meaning that
    # stan_data$covar[1:10] = covar_cube[1:10, 1, 2]
    # TODO check if the index removing the first covarcub column which should correspond
    # to population is correct
    
    stan_data$ncovar <- length(covariate_choices)
    stan_data$covar <- matrix(
      apply(covar_cube, 3, function(x) x[non_na_gridcells])[, -1], 
      nrow = length(non_na_gridcells)
    )
    
    for (i in rev(seq_len(stan_data$ncovar:1))) {
      # Throw out constant covariates since they mess up the model
      # TODO check whether the indexing in the if is correct (no indexing before)
      if ((max(stan_data$covar[, i]) - min(stan_data$covar[, i])) < 1e-6) {
        stan_data$covar <- stan_data$covar[, -i]
        stan_data$ncovar <- stan_data$ncovar - 1
        print(paste("Threw out covariate", covariate_choices[i]))
      } else {
        print(paste("Kept covariate", covariate_choices[i]))
      }
    }
    
    for (i in seq_len(stan_data$ncovar)) {
      # standardize
      stan_data$covar <- taxdat::standardize_covar(stan_data$covar)
    }
    
  } else {
    # Case when no covariates are used
    stan_data$covar <- array(0, dim = c(length(non_na_gridcells), 0))
    stan_data$ncovar <- 0
  }
  
  # ---- C. Aggregation ----
  
  # Mapping between observations to location periods and between
  # location periods and grid cells
  ind_mapping <- taxdat::get_space_time_ind_speedup(
    df = sf_cases,
    lp_dict = location_periods_dict,
    model_time_slices = time_slices,
    res_time = res_time,
    n_cpus = ncore,
    do_parallel = FALSE)
  
  non_na_obs <- sort(unique(ind_mapping$map_obs_loctime_obs))
  sf_cases_resized <- sf_cases[non_na_obs, ]
  
  
  if (config$aggregate) {
    
    print("---- AGGREGATING CHOLERA DATA TO MODELING TIME RES ----")
    
    sf_cases_resized <- taxdat::aggregate_observations(sf_cases_resized = sf_cases_resized,
                                                       non_na_obs = non_na_obs,
                                                       ind_mapping = ind_mapping,
                                                       cases_column = cases_column,
                                                       verbose = opt$verbose)
    
    # Re-compute space-time indices based on aggretated data
    ind_mapping_resized <- taxdat::get_space_time_ind_speedup(
      df = sf_cases_resized, 
      lp_dict = location_periods_dict,
      model_time_slices = time_slices,
      res_time = res_time,
      n_cpus = ncore,
      do_parallel = F)
    
  } else {
    print("---- USING RAW CHOLERA DATA ----")
    ind_mapping_resized <- ind_mapping
  }
  
  
  #  ---- D. Drop tfrac threshold ----
  
  # If specified threshold of minimum tfrac filter out data
  if (!is.null(config$tfrac_thresh)) {
    # Which observations to remove
    obs_remove_thresh <- unique(ind_mapping_resized$obs[ind_mapping_resized$tfrac < as.numeric(config$tfrac_thresh)])
    
    if (length(obs_remove_thresh) == 0){
      cat("---- FOUND none of", nrow(sf_cases_resized), "observations that are under the tfrac threshold of", config$tfrac_thresh, "\n")
    } else {
      cat("---- REMOVING", length(obs_remove_thresh), "of", nrow(sf_cases_resized), "observations that are under the tfrac threshold of", config$tfrac_thresh, "\n")
      
      # Remove observations
      sf_cases_resized <- sf_cases_resized[-c(obs_remove_thresh), ]
      
      # Re-compute space-time indices based on aggretated data
      ind_mapping_resized <- taxdat::get_space_time_ind_speedup(
        df = sf_cases_resized, 
        lp_dict = location_periods_dict,
        model_time_slices = time_slices,
        res_time = res_time,
        n_cpus = ncore,
        do_parallel = F)
    }
  }
  
  stan_data$M <- nrow(sf_cases_resized)
  non_na_obs_resized <- sort(unique(ind_mapping_resized$map_obs_loctime_obs))
  obs_changer <- taxdat::make_changer(x = non_na_obs_resized) 
  stan_data$map_obs_loctime_obs <- taxdat::get_map_obs_loctime_obs(x = ind_mapping_resized$map_obs_loctime_obs,
                                                                   obs_changer = obs_changer)
  stan_data$map_obs_loctime_loc <- as.array(ind_mapping_resized$map_obs_loctime_loc) 
  
  # ---- E. Censoring ----
  
  # First define censored observations
  stan_data$censored  <- as.array(ind_mapping_resized$tfrac <= stan_params$censoring_thresh)
  
  # Extract censoring information
  censoring_inds <- taxdat::get_censoring_inds(M = stan_data$M,
                                               ind_mapping_resized = ind_mapping_resized,
                                               censoring_thresh = stan_params$censoring_thresh)
  
  # Then overwrite tfrac with user-specified value
  if (!is.null(set_tfrac) && (set_tfrac)) {
    cat("-- Overwriting tfrac with user-specified value of ", set_tfrac)
    ind_mapping_resized$tfrac <- rep(1.0, length(ind_mapping_resized$tfrac))
  }
  
  stan_data$tfrac <- as.array(ind_mapping_resized$tfrac)
  stan_data$map_loc_grid_loc <- as.array(ind_mapping_resized$map_loc_grid_loc)
  stan_data$map_loc_grid_grid <- as.array(ind_mapping_resized$map_loc_grid_grid)
  stan_data$u_loctime <- ind_mapping_resized$u_loctimes
  stan_data$L <- length(ind_mapping_resized$u_loctimes)
  
  # ---- F. Spatial fraction ----
  # Add 1km population fraction (this is deprecated in new stan model)
  stan_data$use_pop_weight <- stan_params$use_pop_weight
  
  if (stan_params$use_pop_weight) {
    # Make sure that all observations for have a pop_loctime > 0
    stan_data$pop_weight <- ind_mapping_resized$u_loc_grid_weights
    
    pop_loctimes <- taxdat::compute_pop_loctimes(stan_data = stan_data)
    
    if (any(pop_loctimes == 0)) {
      # Remove pop_loctimes == 0
      cat("-- Found", sum(pop_loctimes == 0), "location/times with weighted population == 0. \n")
      nopop_loctimes <- which(pop_loctimes == 0)
      nopop_obs <- purrr::map(nopop_loctimes, ~ stan_data$map_obs_loctime_obs[which(stan_data$map_obs_loctime_loc == .)]) %>% 
        unlist() %>% 
        unique()
      
      cat("---- REMOVING", length(nopop_obs), "of", nrow(sf_cases_resized), "observations for which the location/time population is 0. \n")
      
      # Remove observations
      sf_cases_resized <- sf_cases_resized[-c(nopop_obs), ]
      
      # Re-compute space-time indices based on aggregated data
      ind_mapping_resized <- taxdat::get_space_time_ind_speedup(
        df = sf_cases_resized, 
        lp_dict = location_periods_dict,
        model_time_slices = time_slices,
        res_time = res_time,
        n_cpus = ncore,
        do_parallel = F)
      
      # Reset stan_data
      non_na_obs_resized <- sort(unique(ind_mapping_resized$map_obs_loctime_obs))
      obs_changer <- taxdat::make_changer(x = non_na_obs_resized)
      stan_data$map_obs_loctime_obs <- as.array(obs_changer[as.character(ind_mapping_resized$map_obs_loctime_obs)])
      stan_data$map_obs_loctime_loc <- as.array(ind_mapping_resized$map_obs_loctime_loc)
      
      # First define censored observations
      stan_data$censored  <- as.array(ind_mapping_resized$tfrac <= stan_params$censoring_thresh)
      stan_data$M <- nrow(sf_cases_resized)
      
      # Extract censoring information
      censoring_inds <-  taxdat::get_censoring_inds(M = stan_data$M,
                                                    ind_mapping_resized = ind_mapping_resized)
      
      # Then overwrite tfrac with user-specified value
      if (!is.null(set_tfrac) && (set_tfrac)) {
        cat("-- Overwriting tfrac with user-specified value of ", set_tfrac)
        ind_mapping_resized$tfrac <- rep(1.0, length(ind_mapping_resized$tfrac))
      }
      
      stan_data$tfrac <- as.array(ind_mapping_resized$tfrac)
      stan_data$map_loc_grid_loc <- as.array(ind_mapping_resized$map_loc_grid_loc)
      stan_data$map_loc_grid_grid <- as.array(ind_mapping_resized$map_loc_grid_grid)
      stan_data$u_loctime <- ind_mapping_resized$u_loctimes
      stan_data$pop_weight <- ind_mapping_resized$u_loc_grid_weights
    }
    
  } else {
    stan_data$pop_weight <- array(data = 0, dim = 0)
  }
  
  #  ---- G. Observations ----
  stan_data$y <- as.array(sf_cases_resized[[cases_column]])
  
  # Get censoring indexes 
  stan_data$ind_full <- which(censoring_inds == "full") %>% array()
  stan_data$M_full <- length(stan_data$ind_full)
  # TODO Left-censoring is not implemented for now
  stan_data$ind_left <- which(censoring_inds == "left-censored") %>% array()
  stan_data$M_left <- length(stan_data$ind_left)
  stan_data$ind_right <- which(censoring_inds == "right-censored") %>% array()
  stan_data$M_right <- length(stan_data$ind_right)
  stan_data$censoring_inds <- censoring_inds
  
  # ---- H. Mean rate ----
  stan_data$meanrate <- taxdat::compute_mean_rate(stan_data = stan_data)
  
  # ---- I. Mappings ----
  stan_data$K1 <- length(stan_data$map_obs_loctime_obs)
  stan_data$K2 <- length(stan_data$map_loc_grid_loc)
  stan_data$L <- length(ind_mapping_resized$u_loctimes)
  
  full_grid <- sf::st_drop_geometry(sf_grid) %>% 
    dplyr::left_join(sf::st_drop_geometry(smooth_grid)) %>% 
    dplyr::select(upd_id, smooth_id, t)
  
  stan_data$smooth_grid_N <- nrow(smooth_grid)
  stan_data$map_smooth_grid <- full_grid$smooth_id
  stan_data$map_grid_time <- full_grid$t
  stan_data['T'] <- nrow(time_slices)
  stan_data$map_full_grid <- full_grid$upd_id
  
  # ---- J. Data for output summaries ----
  
  # Set user-specific name for location_periods table to use
  output_lp_name <- taxdat::make_output_locationperiods_table_name(dbuser = dbuser, map_name = map_name)
  
  # Add population 1km weights
  output_intersections_table <- taxdat::make_output_grid_intersections_table_name(dbuser = dbuser,
                                                                                  map_name = map_name)
  
  # Add population 1km weights
  output_cntrds_table <- taxdat::make_output_grid_centroids_table_name(dbuser = dbuser,
                                                                       map_name = map_name)
  
  # Connect to database
  conn_pg <- taxdat::connect_to_db(dbuser)
  
  output_location_periods_table <- taxdat::make_location_periods_dict(
    conn_pg = conn_pg,
    lp_name = output_lp_name,
    intersections_table = output_intersections_table,
    cntrd_table = output_cntrds_table,
    res_space = res_space,
    sf_grid = sf_grid,
    grid_changer = grid_changer)
  
  # Make fake data to compute output location periods mappings
  fake_output_obs <- output_location_periods_table %>% 
    dplyr::inner_join(time_slices %>% 
                        dplyr::mutate(t = dplyr::row_number())) %>% 
    dplyr::distinct(location_period_id, TL, TR) %>% 
    dplyr::rename(locationPeriod_id = location_period_id) %>% 
    dplyr::mutate(admin_lev = stringr::str_extract(locationPeriod_id, "ADM[0-9]{1}"),
                  admin_lev = stringr::str_remove_all(admin_lev, "ADM") %>% as.integer())
  
  # Mapping from fake observations to location-periods 
  ind_mapping_output <- taxdat::get_space_time_ind_speedup(
    df = fake_output_obs, 
    lp_dict = output_location_periods_table,
    model_time_slices = time_slices,
    res_time = res_time,
    n_cpus = ncore,
    do_parallel = F)
  
  # Space-only location periods
  output_lps_space <- fake_output_obs %>% 
    dplyr::distinct(locationPeriod_id, admin_lev) %>% 
    dplyr::arrange(locationPeriod_id)
  
  # Set data for output in stan object
  stan_data$M_output <- nrow(fake_output_obs)
  stan_data$map_output_obs_loctime_obs <- as.array(ind_mapping_output$map_obs_loctime_obs)
  stan_data$map_output_obs_loctime_loc <- as.array(ind_mapping_output$map_obs_loctime_loc)
  stan_data$map_output_loc_grid_loc <- as.array(ind_mapping_output$map_loc_grid_loc)
  stan_data$map_output_loc_grid_grid <- as.array(ind_mapping_output$map_loc_grid_grid)
  stan_data$u_output_loctime <- ind_mapping_output$u_loctimes
  
  stan_data$K1_output <- length(stan_data$map_output_obs_loctime_obs)
  stan_data$K2_output <- length(stan_data$map_output_loc_grid_loc)
  stan_data$L_output <- length(ind_mapping_output$u_loctimes)
  stan_data$L_output_space <- nrow(output_lps_space)
  stan_data$map_output_loctime_loc <- purrr::map_dbl(fake_output_obs$locationPeriod_id, 
                                                     ~ which(output_lps_space$locationPeriod_id == .))
  stan_data$map_output_loc_adminlev <- output_lps_space$admin_lev
  
  if (stan_params$use_pop_weight) {
    stan_data$pop_weight_output <- ind_mapping_output$u_loc_grid_weights
  } else {
    stan_data$pop_weight_output <- array(data = 0, dim = 0)
  }
  
  # ---- K. Population at risk ----
  # Data for people at risk
  risk_cat_low <- c(0, 1, 10, 100)*1e-5
  risk_cat_high <- c(risk_cat_low[-1], 1e6)
  
  stan_data$N_cat <- length(risk_cat_low)
  stan_data$risk_cat_low <- risk_cat_low
  stan_data$risk_cat_high <- risk_cat_high
  
  # Map from space x time grid to space grid
  sf_grid <- sf_grid %>% 
    dplyr::group_by(rid, x, y) %>% 
    dplyr::mutate(space_id = min(upd_id)) %>% 
    dplyr::ungroup()
  
  stan_data$N_space <- length(unique(sf_grid$space_id))
  stan_data$map_spacetime_space_grid <- sf_grid$space_id[sf_grid$upd_id]
  
  # ---- L. Observation model ----
  stan_data$lambda <- stan_params$lambda
  
  
  cat("**** FINISHED PREPARING STAN INPUT \n")
  
  return(
    list(stan_data = stan_data,
         sf_cases_resized = sf_cases_resized,
         sf_grid = sf_grid,
         smooth_grid  = smooth_grid,
         fake_output_obs = fake_output_obs)
  )
}
