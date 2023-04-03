#' @title Perapre stan input
#' @description Prepares the data for the Stan code
#'
#' @param dbuser
#' @param cholera_directory
#' @param grid_rand_effects_N
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
    grid_rand_effects_N,
    ncore,
    res_time,
    res_space,
    time_slices,
    cases_column,
    sf_cases,
    non_na_gridcells,
    sf_grid,
    location_periods_dict,
    covar_cube,
    set_tfrac,
    tfrac_thresh,
    snap_tol,
    opt,
    stan_params,
    aggregate,
    debug,
    config
) {
  
  library(sf)
  
  # Get covariate choices from covar_cube slice names
  covariate_choices <- dimnames(covar_cube)[[3]][-1]
  
  # Adjacency --------------------------------------------------------------------
  
  cat("Computing adjacency \n")
  
  grid_changer <- taxdat::make_changer(x = non_na_gridcells)
  
  # make the smooth grid
  smooth_grid_obj <- taxdat::make_smooth_grid(sf_grid = sf_grid,
                                              non_na_gridcells = non_na_gridcells,
                                              grid_rand_effects_N = grid_rand_effects_N)
  
  # Unpack
  sf_grid <- smooth_grid_obj$sf_grid            # updated sf_grid with column "s" for smooth grid index
  smooth_grid <- smooth_grid_obj$smooth_grid    # smooth grid sf object
  rm(smooth_grid_obj)
  
  # Add country information
  conn_pg <- taxdat::connect_to_db(dbuser)
  map_grid_to_country_df <- taxdat::map_gridcell_to_country(
    conn_pg = conn_pg,
    output_intersections_table = taxdat::make_output_grid_intersections_table_name(config = config)
  ) 
  DBI::dbDisconnect(conn_pg)
  
  sf_grid <- dplyr::left_join(sf_grid, map_grid_to_country_df)
  smooth_grid <- dplyr::left_join(smooth_grid, map_grid_to_country_df)
  
  if (any(is.na(sf_grid$country))) {
    cat("-- No country found for ", sum(is.na(sf_grid$country)), "spacetime gridcells. Dropping them. \n")
  }
  
  # Define model time slices
  model_time_slices <- sort(unique(sf_grid$s))
  
  adjacency <- taxdat::make_adjacency(smooth_grid = smooth_grid,
                                      model_time_slices = model_time_slices,
                                      non_na_gridcells = non_na_gridcells)
  
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
  
  
  if (aggregate) {
    
    print("---- AGGREGATING CHOLERA DATA TO MODELING TIME RES ----")
    
    sf_cases_resized <- taxdat::aggregate_observations(sf_cases_resized = sf_cases_resized,
                                                       non_na_obs = non_na_obs,
                                                       ind_mapping = ind_mapping,
                                                       cases_column = cases_column,
                                                       verbose = opt$verbose)
    
    # Snap to time period after aggregation
    sf_cases_resized <- taxdat::snap_to_time_period_df(df = sf_cases_resized,
                                                       TL_col = "TL",
                                                       TR_col = "TR",
                                                       res_time = res_time,
                                                       tol = snap_tol)
    
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
    sf_cases_resized <- sf_cases
  }
  
  
  #  ---- D. Drop tfrac threshold ----
  
  # If specified threshold of minimum tfrac filter out data
  if (tfrac_thresh > 0) {
    # Which observations to remove
    obs_remove_thresh <- unique(ind_mapping_resized$obs[ind_mapping_resized$tfrac < as.numeric(tfrac_thresh)])
    
    if (length(obs_remove_thresh) == 0){
      cat("---- FOUND none of", nrow(sf_cases_resized), "observations that are under the tfrac threshold of", tfrac_thresh, "\n")
    } else {
      cat("---- REMOVING", length(obs_remove_thresh), "of", nrow(sf_cases_resized), "observations that are under the tfrac threshold of", tfrac_thresh, "\n")
      
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
  stan_data$censored  <- as.array(ind_mapping_resized$tfrac <= config$censoring_thresh)
  
  # Extract censoring information
  censoring_inds <- taxdat::get_censoring_inds(stan_data = stan_data,
                                               ind_mapping_resized = ind_mapping_resized,
                                               censoring_thresh = config$censoring_thresh)
  
  # Drop data that are censored and for which the observations are 0
  if (config$censoring) {
    
    cat("-- Checking for 0 censored observations \n")
    
    y <- sf_cases_resized[[cases_column]]
    censored_zero_obs <- which(y == 0 & censoring_inds == "right-censored")
    
    if (length(censored_zero_obs) > 0) {
      
      cat("-- Dropping", length(censored_zero_obs), "observations that are 0 and censored.\n")
      
      sf_cases_resized <- sf_cases_resized[-c(censored_zero_obs), ]
      
      ind_mapping_resized <- taxdat::get_space_time_ind_speedup(
        df = sf_cases_resized, 
        lp_dict = location_periods_dict,
        model_time_slices = time_slices,
        res_time = res_time,
        n_cpus = ncore,
        do_parallel = F)
      
      # First define censored observations
      stan_data$censored  <- as.array(ind_mapping_resized$tfrac <= config$censoring_thresh)
      
      # Extract censoring information
      censoring_inds <- taxdat::get_censoring_inds(stan_data = stan_data,
                                                   ind_mapping_resized = ind_mapping_resized,
                                                   censoring_thresh = config$censoring_thresh)
      
      
      # Update data
      stan_data$M <- nrow(sf_cases_resized)
      non_na_obs_resized <- sort(unique(ind_mapping_resized$map_obs_loctime_obs))
      obs_changer <- taxdat::make_changer(x = non_na_obs_resized) 
      stan_data$map_obs_loctime_obs <- taxdat::get_map_obs_loctime_obs(x = ind_mapping_resized$map_obs_loctime_obs,
                                                                       obs_changer = obs_changer)
      stan_data$map_obs_loctime_loc <- as.array(ind_mapping_resized$map_obs_loctime_loc) 
    }
  }
  
  # Then overwrite tfrac with user-specified value
  if (set_tfrac) {
    cat("-- Overwriting tfrac for non-censored observations with 1")
    ind_mapping_resized$tfrac <- rep(1.0, length(ind_mapping_resized$tfrac))
  }
  
  stan_data$tfrac <- as.array(ind_mapping_resized$tfrac)
  stan_data$map_loc_grid_loc <- as.array(ind_mapping_resized$map_loc_grid_loc)
  stan_data$map_loc_grid_grid <- as.array(ind_mapping_resized$map_loc_grid_grid)
  stan_data$u_loctime <- ind_mapping_resized$u_loctimes
  stan_data$L <- length(ind_mapping_resized$u_loctimes)
  
  # ---- F. Spatial fraction ----
  # Add 1km population fraction (this is deprecated in new stan model)
  stan_data$use_pop_weight <- config$use_pop_weight
  
  if (config$use_pop_weight) {
    # Make sure that all observations for have a pop_loctime > 0
    stan_data$map_loc_grid_sfrac <- ind_mapping_resized$u_loc_grid_weights
    
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
      stan_data$censored  <- as.array(ind_mapping_resized$tfrac <= config$censoring_thresh)
      stan_data$M <- nrow(sf_cases_resized)
      
      # Extract censoring information
      censoring_inds <-  taxdat::get_censoring_inds(stan_data = stan_data,
                                                    ind_mapping_resized = ind_mapping_resized,
                                                    censoring_thresh = config$censoring_thresh)
      
      # Then overwrite tfrac with user-specified value
      if (!is.null(set_tfrac) && (set_tfrac)) {
        cat("-- Overwriting tfrac with user-specified value of ", set_tfrac)
        ind_mapping_resized$tfrac <- rep(1.0, length(ind_mapping_resized$tfrac))
      }
      
      stan_data$tfrac <- as.array(ind_mapping_resized$tfrac)
      stan_data$map_loc_grid_loc <- as.array(ind_mapping_resized$map_loc_grid_loc)
      stan_data$map_loc_grid_grid <- as.array(ind_mapping_resized$map_loc_grid_grid)
      stan_data$u_loctime <- ind_mapping_resized$u_loctimes
      stan_data$map_loc_grid_sfrac <- ind_mapping_resized$u_loc_grid_weights
    }
    
  } else {
    stan_data$map_loc_grid_sfrac <- array(data = 0, dim = 0)
  }
  
  if (config$use_pop_weight) {
    # Check if sfrac is valid
    if (any(stan_data$map_loc_grid_sfrac > 1.01)) {
      
      warning("Invalid sfrac values > 1", " Maximum value of ", 
              max(stan_data$map_loc_grid_sfrac), ".",
              "Caping all values to 1.")
      
      stan_data$map_loc_grid_sfrac <- pmin(stan_data$map_loc_grid_sfrac, 1) 
    }
    
    # Make sure all values are <= 1 (possible rounding errors)
    stan_data$map_loc_grid_sfrac <- pmin(1, stan_data$map_loc_grid_sfrac)
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
  
  # What observation model to use
  stan_data$obs_model <- config$obs_model
  
  # Grid cells to country
  u_countries <- unique(sf_grid$country) %>% sort()
  stan_data$N_countries <- length(u_countries)
  
  stan_data$map_grid_country <- purrr::map_dbl(
    1:nrow(sf_grid), 
    ~ which(u_countries == sf_grid$country[.])
  )
  
  # Observation to country
  stan_data$map_obs_country <- sf_cases_resized$location_name %>% 
    taxdat::get_country() %>% 
    purrr::map_dbl(~ which(u_countries == .)) %>% 
    as.array()
  
  # Administrative levels for observation model
  admin_levels <- sf_cases_resized$location_name %>% 
    taxdat::get_admin_level() %>% 
    as.array()
  
  n_na_admin <- sum(is.na(admin_levels))
  
  if (n_na_admin > 0) {
    cat("---- Replacing unknown admin level for ", n_na_admin, " observations corresponding to",
        sum(stan_data$y[is.na(admin_levels)]), "cases. \n")
  }
  
  # Make sure all admin levels are specified
  admin_levels[is.na(admin_levels)] <- max(admin_levels, na.rm = T)
  
  # Get unique levels, this is necessary if not all admin levels are present
  # in the data
  u_admin_levels <- sort(unique(admin_levels))
  
  # index staring at 1
  stan_data$map_obs_admin_lev <- purrr::map_dbl(admin_levels, ~ which(u_admin_levels == .))
  
  # Indices of adm0 level
  stan_data$ind_obs_admin_lev0 <- which(admin_levels == 1)
  
  # Add unique number of admin levels for use in observation model
  stan_data$N_admin_lev <- length(u_admin_levels)
  
  # Unique map of country/admin levels
  u_country_admin_lev <- expand.grid(
    country = 1:stan_data$N_coutries,
    admin_lev = 1:stan_data$N_admin_lev 
  ) %>% 
    dplyr::mutate(row = dplyr::row_number())
  
  stan_data$comb_country_admin_lev_country <- u_country_admin_lev$country
  stan_data$comb_country_admin_lev_admin_lev <- u_country_admin_lev$admin_lev
  
  # Map from od to inv_od parameters (this is because we do not have parameters for admin level 0 observations)
  comb_inv_od <- u_country_admin_lev %>% 
    # !! note that here 1 is in the sorted vector of unique admin levels
    # this will break if the first admin level is not admin level 0
    dplyr::filter(admin_lev != 1)   
  
  stan_data$map_od_inv_od_param <- purrr::map_dbl(
    u_country_admin_lev$row,
    function(x){
      ind <- which(comb_inv_od$row == x)
      if (length(ind) == 0) {
        return(0)
      } else {
        return(ind)
      }
    })
  
  
  stan_data$map_obs_country_admin_lev <- map_dbl(
    1:nrow(sf_cases_resized),
    function(x) {
      
      dplyr::filter(u_country_admin_lev,
                    country == taxdat::get_country(sf_cases_resized$location_name[x]),
                    admin_lev == admin_levels[x]) %>% 
        dplyr::pull(row)
    })
  
  # ---- J. Data for output summaries ----
  
  # Set user-specific name for location_periods table to use
  output_lp_name <- taxdat::make_output_locationperiods_table_name(
    config = config
  )
  
  # Add population 1km weights
  output_intersections_table <- taxdat::make_output_grid_intersections_table_name(
    config = config
  )
  
  # Add population 1km weights
  output_cntrds_table <- taxdat::make_output_grid_centroids_table_name(
    config = config
  )
  
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
  
  if (config$use_pop_weight) {
    stan_data$map_loc_grid_sfrac_output <- ind_mapping_output$u_loc_grid_weights
    
    # Check if sfrac is valid
    if (any(stan_data$map_loc_grid_sfrac_output > 1.01)) {
      warning("Invalid sfrac values > 1 in outputs.", " Maximum value of ", 
              max(stan_data$map_loc_grid_sfrac_output), ".",
              "Caping all values to 1.")
      stan_data$map_loc_grid_sfrac_output <- pmin(stan_data$map_loc_grid_sfrac_output, 1) 
    }
    # Make sure all values are <= 1 (possible rounding errors)
    stan_data$map_loc_grid_sfrac_output <- pmin(1, stan_data$map_loc_grid_sfrac_output)
    
  } else {
    stan_data$map_loc_grid_sfrac_output <- array(data = 0, dim = 0)
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
  
  # Option for debug mode
  if (debug) {
    stan_data$debug <- 0
  } else {
    stan_data$debug <- debug
  }
  
  # ---- L. Covariates ----
  
  # Option for double-exponential prior on betas
  stan_data$exp_prior <- config$exp_prior
  
  
  # ---- M. Other options for stan ----
  # Use intercept
  stan_data$use_intercept <- config$use_intercept
  
  # 0-sum constraint on yerly random effects
  stan_data$do_zerosum_cnst <- config$do_zerosum_cnst
  
  # Infer the sd of the prior on yearly random effects
  stan_data$do_infer_sd_eta <- config$do_infer_sd_eta
  
  
  # ---- N. Priors ----
  # Set sigma_eta_scale for all models (not used for models without time effect)
  stan_data$sigma_eta_scale <- config$sigma_eta_scale
  
  # Add scale of prior on the sd of regression coefficients
  stan_data$beta_sigma_scale <- config$beta_sigma_scale
  
  # Priors for intercept
  stan_data$mu_alpha <- config$mu_alpha
  stan_data$sd_alpha <- config$sd_alpha
  
  # Priors for observation model overdispersion parameters in negative-binomial model
  # We model the od parameter on the 1/tau constrained to be positive scale to facilitate setting priors
  # We assume that the largest admin level (admin level 0 for national) has
  # an informative prior so as to produce little overdispersion. The over-dispersion for other
  # admin levels are allowed to have more prior support for larger amount of over-dispersion.
  stan_data$mu_inv_od <- rep(0, stan_data$N_admin_lev)   # center at 0 (note that this is on the scale of 1/tau)
  stan_data$sd_inv_od <- c(config$inv_od_sd_adm0, 
                           rep(config$inv_od_sd_nopool,
                               stan_data$N_admin_lev - 1))
  
  # Also save for hierarchical model
  stan_data$h_mu_mean_inv_od <- 0     # the mean of hierarchical inverse over-dispersion parameters
  stan_data$h_mu_sd_inv_od <- config$h_mu_sd_inv_od       # the sds  of hierarchical inverse over-dispersion parameters
  stan_data$h_sd_sd_inv_od <- config$h_sd_sd_inv_od       # the sd of the hierarchical sd of inverse over-dispersion parameters
  stan_data$mu_inv_od_lev0 <- 0       # mean of the inv od param for national level
  stan_data$sd_inv_od_lev0 <- config$inv_od_sd_adm0    # sd of the inv od param for national level
  
  
  # Prior on the std_dev_w
  stan_data$mu_sd_w <- config$mu_sd_w
  stan_data$sd_sd_w <- config$sd_sd_w
  
  cat("**** FINISHED PREPARING STAN INPUT \n")
  
  return(
    list(stan_data = stan_data,
         stan_params = stan_params,
         sf_cases_resized = sf_cases_resized,
         sf_grid = sf_grid,
         smooth_grid  = smooth_grid,
         fake_output_obs = fake_output_obs,
         config = config,
         u_admin_levels = u_admin_levels,
         u_countries = u_countries)
  )
}
