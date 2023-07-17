#' @title Prepare stan input
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
#' @param opt
#' @param stan_params
#' @param aggregate
#' @param debug
#' @param config
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
    opt,
    stan_params,
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
  # JPS: this is not ideal, could consider making to separate functions
  sf_grid <- smooth_grid_obj$sf_grid            # updated sf_grid with column "s" for smooth grid index
  smooth_grid <- smooth_grid_obj$smooth_grid    # smooth grid sf object
  rm(smooth_grid_obj)

  # Define model time slices
  # JPS: Could make this into a function
  model_time_slices <- sort(unique(sf_grid$s))

  adjacency <- taxdat::make_adjacency(smooth_grid = smooth_grid,
                                      model_time_slices = model_time_slices,
                                      non_na_gridcells = non_na_gridcells)

  # Stan inputs ------------------------------------------------------------------
  cat("---- Preparing Stan input data \n")

  # ---- A. Adjacency ----
  stan_data <- list()
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
  
  # ---- C. Pre-Aggregation Duplicates Removal in sf_cases ----
  # JPS: could package this into a function for unit testing
  sf_cases_dup_obs <- sf_cases %>% 
    sf::st_drop_geometry() %>%
    # don't decide on duplicates based on these variables
    dplyr::select(-dplyr::one_of("id", "deaths")) %>%
    duplicated()

  sf_cases <- sf_cases[!sf_cases_dup_obs, ]

  
  # ---- D. Aggregation ----
  
  # Mapping between observations to location periods and between
  # location periods and grid cells
  ind_mapping <- taxdat::get_space_time_ind_speedup(
    df = sf_cases,
    lp_dict = location_periods_dict,
    model_time_slices = time_slices,
    res_time = res_time,
    n_cpus = config$ncpus_parallel_prep,
    do_parallel = config$do_parallel_prep)

  non_na_obs <- sort(unique(ind_mapping$map_obs_loctime_obs))
  sf_cases_resized <- sf_cases[non_na_obs, ]
  
  
  if (config$aggregate) {
    
    # Should standardize the way we print messages
    print("---- AGGREGATING CHOLERA DATA TO MODELING TIME RES ----")

    sf_cases_resized <- taxdat::aggregate_observations(sf_cases_resized = sf_cases_resized,
                                                       non_na_obs = non_na_obs,
                                                       ind_mapping = ind_mapping,
                                                       cases_column = cases_column,
                                                       verbose = opt$verbose,
                                                       n_cpus = config$ncpus_parallel_prep,
                                                       do_parallel = config$do_parallel_prep)

    # Snap to time period after aggregation
    sf_cases_resized <- taxdat::snap_to_time_period_df(df = sf_cases_resized,
                                                       TL_col = "TL",
                                                       TR_col = "TR",
                                                       res_time = res_time,
                                                       tol = config$snap_tol)
    
    # Re-compute space-time indices based on aggretated data
    ind_mapping_resized <- taxdat::get_space_time_ind_speedup(
      df = sf_cases_resized,
      lp_dict = location_periods_dict,
      model_time_slices = time_slices,
      res_time = res_time,
      n_cpus = config$ncpus_parallel_prep,
      do_parallel = config$do_parallel_prep)

  } else {
    print("---- USING RAW CHOLERA DATA ----")
    ind_mapping_resized <- ind_mapping
    sf_cases_resized <- sf_cases
  }

  # Clean unused objects
  rm(ind_mapping)
  
  #  ---- Ea. Drop tfrac threshold ----
  
  # If specified threshold of minimum tfrac filter out data
  if (config$tfrac_thresh > 0) {
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
        n_cpus = config$ncpus_parallel_prep,
        do_parallel = config$do_parallel_prep)
    }
  }
  
  # ---- Eb. Drop multi-year ----
  
  # Set admin level
  sf_cases_resized <- sf_cases_resized %>% 
    dplyr::mutate(admin_level = purrr::map_dbl(location_name, ~ taxdat::get_admin_level(.)))
  
  # Drop multi-year observations if present
  if (config$drop_multiyear_adm0) {
    sf_cases_resized <- taxdat::drop_multiyear(df = sf_cases_resized,
                                               admin_levels = 0)
    
    # Re-compute space-time indices based on aggretated data
    ind_mapping_resized <- taxdat::get_space_time_ind_speedup(
      df = sf_cases_resized, 
      lp_dict = location_periods_dict,
      model_time_slices = time_slices,
      res_time = res_time,
      n_cpus = config$ncpus_parallel_prep,
      do_parallel = config$do_parallel_prep)
  }
  
  # ---- F. Censoring ----
  

  # Set stan_data at this point
  stan_data <- taxdat::update_stan_data_indexing(stan_data = stan_data,
                                                 ind_mapping_resized = ind_mapping_resized,
                                                 config = config)
  
  # Extract censoring information
  sf_cases_resized$censoring <- taxdat::get_censoring_inds(ind_mapping_resized = ind_mapping_resized,
                                                           censoring_thresh = config$censoring_thresh)

  sf_cases_resized <- sf_cases_resized %>% 
    dplyr::mutate(ref_TL = taxdat::get_start_timeslice(TL, res_time),
                  ref_TR = taxdat::get_end_timeslice(TR, res_time),
                  obs_id = dplyr::row_number()) 
  

  # Drop data that are censored and for which the observations are 0
  if (config$censoring) {

    cat("-- Checking for 0 censored observations \n")

    y <- sf_cases_resized[[cases_column]]

    censored_zero_obs <- which(y == 0 & sf_cases_resized$censoring == "right-censored")
    
    if (length(censored_zero_obs) > 0) {

      cat("-- Dropping", length(censored_zero_obs), "observations that are 0 and censored.\n")

      sf_cases_resized <- sf_cases_resized[-c(censored_zero_obs), ]

      
      # Re-compute indices
      ind_mapping_resized <- taxdat::get_space_time_ind_speedup(
        df = sf_cases_resized,
        lp_dict = location_periods_dict,
        model_time_slices = time_slices,
        res_time = res_time,
        n_cpus = config$ncpus_parallel_prep,
        do_parallel = config$do_parallel_prep)

    }
  }
  
  
  # Drop ADM0 observations that do not inform the model
  if (config$drop_censored_adm0) {
    
    sf_cases_resized <- taxdat::drop_censored_adm0(sf_cases_resized = sf_cases_resized,
                                                   thresh = config$drop_censored_adm0_thresh,
                                                   res_time = res_time,
                                                   cases_column = cases_column)
    
    # Re-compute indices
    ind_mapping_resized <- taxdat::get_space_time_ind_speedup(
      df = sf_cases_resized, 
      lp_dict = location_periods_dict,
      model_time_slices = time_slices,
      res_time = res_time,
      n_cpus = config$ncpus_parallel_prep,
      do_parallel = config$do_parallel_prep)
  }
  
  
  # First define censored observations 
  stan_data <- taxdat::update_stan_data_indexing(stan_data = stan_data,
                                                 ind_mapping_resized = ind_mapping_resized,
                                                 config = config)
  

  # ---- G. Spatial fraction ----
  # Add 1km population fraction (this is deprecated in new stan model)
  stan_data$use_pop_weight <- config$use_pop_weight

  if (config$use_pop_weight) {
 
    # Make sure that all observations for have a pop_loctime > 0
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
        n_cpus = config$ncpus_parallel_prep,
        do_parallel = config$do_parallel_prep)

      
      # First define censored observations
      stan_data <- taxdat::update_stan_data_indexing(stan_data = stan_data,
                                                     ind_mapping_resized = ind_mapping_resized,
                                                     config = config)
    }


    stan_data$map_loc_grid_sfrac <- taxdat::check_pop_weight_validity(stan_data$map_loc_grid_sfrac)
    
  } else {
    stan_data$map_loc_grid_sfrac <- array(data = 0, dim = 0)
  }

  #  ---- H. Observations ----
  stan_data$y <- as.array(sf_cases_resized[[cases_column]])
  
  # Get censoring indexes 
  censoring_inds <- taxdat::get_censoring_inds(ind_mapping_resized = ind_mapping_resized,
                                               censoring_thresh = config$censoring_thresh)
  
  stan_data$ind_full <- which(censoring_inds == "full") %>% array()
  stan_data$M_full <- length(stan_data$ind_full)
  # TODO Left-censoring is not implemented for now
  stan_data$ind_left <- which(censoring_inds == "left-censored") %>% array()
  stan_data$M_left <- length(stan_data$ind_left)
  stan_data$ind_right <- which(censoring_inds == "right-censored") %>% array()
  stan_data$M_right <- length(stan_data$ind_right)
  stan_data$censoring_inds <- censoring_inds
  
  # ---- I. Mappings ----
  stan_data$K1 <- length(stan_data$map_obs_loctime_obs)
  stan_data$K2 <- length(stan_data$map_loc_grid_loc)
  
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

  # First define admin levels to get data ad upper admin levels
  # Administrative levels for observation model
  admin_levels <- sf_cases_resized$location_name %>%
    purrr::map_dbl(~ taxdat::get_admin_level(.)) %>%
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

  # Add unique number of admin levels for use in observation model
  stan_data$N_admin_lev <- length(u_admin_levels)

  # Map from space x time grid to space grid
  sf_grid <- sf_grid %>%
    dplyr::group_by(rid, x, y) %>%
    dplyr::mutate(space_id = min(upd_id)) %>%
    dplyr::ungroup()

  stan_data$N_space <- length(unique(sf_grid$space_id))
  stan_data$map_spacetime_space_grid <- sf_grid$space_id[sf_grid$upd_id]

  # ---- I. Mean rate ----
  stan_data$meanrate <- taxdat::compute_mean_rate(stan_data = stan_data,
                                                  res_time = res_time)

  #  ---- J. Imputation ----

  sf_cases_resized$admin_level <- admin_levels
  sf_cases_resized$censoring <- censoring_inds
  sf_cases_resized <- sf_cases_resized %>% 
    dplyr::mutate(obs_id = dplyr::row_number())
  
  
  sf_cases_resized <- taxdat::impute_adm0_obs(sf_cases_resized = sf_cases_resized,
                                              stan_data = stan_data,
                                              time_slices = time_slices,
                                              res_time = res_time,
                                              cases_column = cases_column,
                                              frac_coverage_thresh = 0.1)

  
  ## ECL why don't we need to remap indices after imputing observations? 
  ## JPS: This is taken care of in taxdat::update_stan_data_imputation

  stan_data <- taxdat::update_stan_data_imputation(sf_cases_resized = sf_cases_resized,
                                                   stan_data = stan_data,
                                                   time_slices = time_slices,
                                                   res_time = res_time,
                                                   cases_column = cases_column)

  # Update censoring inds in sf_cases_resized
  sf_cases_resized$censoring <- stan_data$censoring_inds

  # ---- K. Data for output summaries ----

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
    grid_changer = grid_changer,
    res_time = res_time)

  # Get pixels with low sfrac
  output_low_sfrac <- output_location_periods_table %>% 
    dplyr::group_by(rid, x, y) %>% 
    dplyr::slice_max(pop_weight) %>% 
    dplyr::filter(pop_weight < config$sfrac_thresh_conn) %>% 
    dplyr::select(rid, x, y) %>% 
    dplyr::inner_join(sf_grid %>% sf::st_drop_geometry())

  # Drop from gridcells with low sfrac from output_location_periods_table
  output_location_periods_table <- output_location_periods_table %>%
    dplyr::filter(!(long_id %in% output_low_sfrac$long_id))

  # Drop grid cells to output location periods connections
  output_location_periods_table <- output_location_periods_table %>%
    dplyr::mutate(connect_id = dplyr::row_number())
  
  output_low_sfrac_connections <- output_location_periods_table %>% 
    dplyr::filter(pop_weight < config$sfrac_thresh_conn)
  
  cat("Dropping", nrow(output_low_sfrac_connections), "/", nrow(output_location_periods_table),
      "connections between grid cells",
      "and output location periods which have sfrac <", config$sfrac_thresh_conn,  "\n")
  
  output_location_periods_table <- output_location_periods_table %>% 
    dplyr::filter(!(connect_id %in% output_low_sfrac_connections$connect_id))

  # Make fake data to compute output location periods mappings
  fake_output_obs <- output_location_periods_table %>%
    dplyr::inner_join(time_slices %>%
                        dplyr::mutate(t = dplyr::row_number())) %>%
    dplyr::distinct(location_period_id, TL, TR) %>%
    dplyr::rename(locationPeriod_id = location_period_id) %>%
    dplyr::mutate(admin_lev = stringr::str_extract(locationPeriod_id, "ADM[0-9]{1}"),
                  admin_lev = stringr::str_remove_all(admin_lev, "ADM") %>% as.integer())

  
  # Check that all fake observations appear in all time slices, drop if not
  fake_output_obs <- fake_output_obs %>% 
    dplyr::add_count(locationPeriod_id) %>% 
    dplyr::mutate(missing_time_slices = n != nrow(time_slices))
  
  if (any(fake_output_obs$missing_time_slices)) {
    dropped_output_lps <- fake_output_obs %>% 
      dplyr::filter(missing_time_slices) %>% 
      dplyr::distinct(locationPeriod_id)
    
    cat("---- Dropping ", nrow(dropped_output_lps),
        "output location period shapfiles due to inconsitent time slice coverage:",
        paste(dropped_output_lps$locationPeriod_id, collapse = ", "), "\n")
    
    fake_output_obs <- fake_output_obs %>% 
      dplyr::filter(!missing_time_slices) %>%
      dplyr::select(-missing_time_slices)
    
  } else {
    fake_output_obs <- fake_output_obs %>% dplyr::select(-missing_time_slices)
  }
  
  # Mapping from fake observations to location-periods 
  ind_mapping_output <- taxdat::get_space_time_ind_speedup(
    df = fake_output_obs,
    lp_dict = output_location_periods_table,
    model_time_slices = time_slices,
    res_time = res_time,
    n_cpus = config$ncpus_parallel_prep,
    do_parallel = config$do_parallel_prep)

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
    stan_data$map_loc_grid_sfrac_output <- taxdat::check_pop_weight_validity(ind_mapping_output$u_loc_grid_weights)
  } else {
    stan_data$map_loc_grid_sfrac_output <- array(data = 0, dim = 0)
  }

  # ---- L. Population at risk ----
  # Data for people at risk
  risk_cat_low <- c(0, 1, 10, 20, 50, 100)*1e-5
  risk_cat_high <- c(risk_cat_low[-1], 1e6)

  stan_data$N_cat <- length(risk_cat_low)
  stan_data$risk_cat_low <- risk_cat_low
  stan_data$risk_cat_high <- risk_cat_high

  # Option for debug mode
  if (debug) {
    stan_data$debug <- 0
  } else {
    stan_data$debug <- debug
  }

  # ---- M. Covariates ----

  # Option for double-exponential prior on betas
  stan_data$exp_prior <- config$exp_prior


  # ---- N. Other options for stan ----
  # Use intercept
  stan_data$use_intercept <- config$use_intercept

  # 0-sum constraint on yerly random effects
  stan_data$do_zerosum_cnst <- config$do_zerosum_cnst

  # Infer the sd of the prior on yearly random effects
  stan_data$do_infer_sd_eta <- config$do_infer_sd_eta


  # ---- O. Priors ----
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

  # Prior on the std_dev_w
  stan_data$mu_sd_w <- config$mu_sd_w
  stan_data$sd_sd_w <- config$sd_sd_w

  # ---- P. Data Structure Check ----
  taxdat::check_stan_input_objects(censoring_thresh = config$censoring_thresh,
                                   sf_cases = sf_cases,
                                   stan_data = stan_data,
                                   sf_cases_resized = sf_cases_resized)

  rm(sf_cases)

  cat("**** FINISHED PREPARING STAN INPUT \n")
  taxdat::close_parallel_setup()

  return(
    list(stan_data = stan_data,
         stan_params = stan_params,
         sf_cases_resized = sf_cases_resized,
         sf_grid = sf_grid,
         smooth_grid  = smooth_grid,
         fake_output_obs = fake_output_obs,
         config = config,
         u_admin_levels = u_admin_levels)
  )
}
