# Preamble ---------------------------------------------------------------------

#' @title Perapre stan input
#' @description Prepares the data for the Stan code
#'
#' @param dbuser
#' @param cholera_directory
#' @param smooth_covariate_number_timesteps
#' @param ncore
#' @param res_time
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
  time_slices,
  cases_column,
  sf_cases,
  non_na_gridcells,
  sf_grid,
  location_periods_dict,
  covar_cube
) {


  # Get covariate choices from covar_cube slice names
  covariate_choices <- dimnames(covar_cube)[[3]][-1]

  # Adjacency --------------------------------------------------------------------
  cat("Computing adjacency \n")
  # extract the adjacency matrix
  grid_changer <- setNames(seq_len(length(non_na_gridcells)), non_na_gridcells) #(already in prepare_map_data ?)
  # sf_grid <- filter(sf_grid, as.logical(st_intersects(sf_grid, st_read("/home/perez/temporary/tmp.shp")))) %>%
  #   dplyr::mutate(long_id = row_number())
  # sf_grid <-  dplyr::mutate(sf_grid, long_id = row_number())

  sf_grid$s <- (sf_grid$t - 1) %% smooth_covariate_number_timesteps + 1
  model_time_slices <- sort(unique(sf_grid$s))

  temporally_inconsistant_cells <- sf_grid %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(bad_percentage = sum(!(id %in% non_na_gridcells))/length(id)) %>%
    dplyr::filter(bad_percentage != 0, bad_percentage != 1)

  if (nrow(temporally_inconsistant_cells)) {
    warning("The following cells were included at some time points, but not others. See output for details")
    print(temporally_inconsistant_cells)
  }

  sf_grid <- sf_grid %>% dplyr::filter(long_id %in% non_na_gridcells)
  sf_grid$upd_id <- grid_changer[as.character(sf_grid$long_id)]
  smooth_grid <- sf_grid %>% dplyr::group_by(id,s) %>% dplyr::summarize() %>% dplyr::ungroup() %>% dplyr::mutate(smooth_id = dplyr::row_number())
  nearest_neighbor_matrices <- list()
  # this is the mapping between the grid ids (from 1 to number of cells that intersect
  # location_periods) and the model ids in space (from 1 to the number of non-NA cells,
  # i.e. cells for which all covariates are non-NA). Note that model ids here do not
  # consider time slices, this is done when computing the adjacency list
  cell_id_mapping <- data.frame()

  for (it in model_time_slices) {

    poly_adj <- smooth_grid %>%
      dplyr::filter(s == it) %>%
      dplyr::select(id) %>%
      spdep::poly2nb(.)

    adj_dat <- taxdat::nb2graph(poly_adj) #[non_na_gridcells, ]))

    N <- adj_dat$N    # number of spatial units at the level of spatial interactions
    node1 <- adj_dat$node1    # "origine" node list
    node2 <- adj_dat$node2    # "destination" node
    N_edges <- adj_dat$N_edges # number of edges

    nn_mat <- Matrix::sparseMatrix(i = node1, j = node2, x = 1, symmetric = TRUE)

    isolated_vertices <- which(Matrix::rowSums(nn_mat) == 0)

    if(length(isolated_vertices) > 1){
      cat("Found", length(isolated_vertices), "isolated vertices in time slice", it ,", adding edges until only one remains.\n")

      for(vertex in isolated_vertices[-1]){
        dist_to_main <- as.vector(sf::st_distance(smooth_grid[vertex, ], smooth_grid[seq_len(vertex-1), ] ))
        new_neighbor <- which.min(dist_to_main)
        nn_mat[vertex,new_neighbor] <- 1
      }
    }
    # Create graph to extract disconnected islands
    ng <- igraph::graph_from_adjacency_matrix(nn_mat)
    # Get the clusters
    ng_cl <- igraph::clusters(ng)

    if (ng_cl$no > 1) {
      cat("Found", ng_cl$no, "clusters of sizes {", paste(ng_cl$csize, collapse = ","), "} in time slice", it ,", adding edges from islands to mainland. \n")

      cluster_ids <- seq_len(ng_cl$no)
      mainland <- which(ng_cl$csize == max(ng_cl$csize))[1]
      mainland_ids <- which(ng_cl$membership == mainland)

      # Loop over island
      smooth_centroids <- sf::st_geometry(sf::st_centroid(smooth_grid))
      for (i in cluster_ids[-mainland]) {
        island_ids <- which(ng_cl$membership == i)
        # find closest mainland pixels (n_pix_islands x n_pix_mainland matrix)
        dist_to_main <- sf::st_distance(smooth_centroids[island_ids], smooth_centroids[mainland_ids])
        # get nearest ids for each island pixel
        nearest_main_ids <- apply(dist_to_main, 1, function(x) which(x == min(x))[1])
        nearest_dist <- unlist(mapply(x = 1:length(island_ids), y = nearest_main_ids, function(x, y) dist_to_main[x, y]))
        # get overall nearest mainland pixel
        nearest_isl_id <- which(nearest_dist == min(nearest_dist))[1]
        # connect the nearest island to the mainland (symetry)
        nn_mat[island_ids[nearest_isl_id], mainland_ids[nearest_main_ids[nearest_isl_id] ] ] <- 1
        nn_mat[mainland_ids[nearest_main_ids[nearest_isl_id] ], island_ids[nearest_isl_id] ] <- 1
      }

      # Check that everything is connected now
      if (igraph::clusters(igraph::graph_from_adjacency_matrix(nn_mat))$no > 1) {
        print(unique(igraph::clusters(igraph::graph_from_adjacency_matrix(nn_mat))$no > 1))
        stop("Something went wrong with island connection.")
      } else {
        cat("Done island connection for time slice", it, "\n")
      }
    }
    nn_mat <- Matrix::tril(nn_mat)

    # if (!Matrix::isSymmetric(nn_mat)) {
    #   stop("This should not happen")
    # }
    nearest_neighbor_matrices <- append(nearest_neighbor_matrices, list(nn_mat))
    ids <- smooth_grid %>%
      dplyr::filter(s == it, id %in% non_na_gridcells)

    cell_id_mapping <- rbind(cell_id_mapping,
                             data.frame(s = it,
                                        id = ids$id,
                                        upd_id = grid_changer[as.character(ids$id)]))
  }

  # Bind all matrices
  N <- sum(unlist(lapply(nearest_neighbor_matrices, nrow)))
  # Initialize adjacency list (list of row, col indices for non-0 entries)
  adjacency_list <- matrix(nrow = 0, ncol = 2)
  cnt <- 0
  for (i in seq_along(nearest_neighbor_matrices)) {
    n_entries <- nrow(nearest_neighbor_matrices[[i]])
    nonzero_ind <- as.matrix(Matrix::which(nearest_neighbor_matrices[[i]]>0, arr.ind = T)) + cnt
    adjacency_list <- rbind(adjacency_list, nonzero_ind)
    # Update the counter to account for the time slices. Note that n_entries for now
    # is the same for all time slices, since the non-NA cells are determined accross
    # time slices of covariates
    cnt <- cnt + n_entries
  }
  # adjacency_list <- adjacency_list[!is.na(adjacency_list[, 1]), ]

  # flag for time adjacency
  flag_adj <- F
  if (flag_adj) {
    stop("Time Adjacency has not been implemented")
    # Add time adjacency
    # time_adj_inds <- matrix(ncol = 2)
    # for (i in seq_along(grid_changer)) {
    #   # Get all overall grid ids
    #   ids <- sf_grid$long_id[sf_grid$id == grid_changer[i]]
    #   # ! THIS CONNECTS ALL TIMES, should change this
    #   for (j in ids) {
    #     for (k in ids) {
    #       from_id <- cell_id_mapping$long_id[cell_id_mapping$ids == j]
    #       to_id <- cell_id_mapping$long_id[cell_id_mapping$ids == k]
    #       if (length(from_id) > 0 & length(to_id) > 0)
    #         if (from_id != to_id)
    #           time_adj_inds <- rbind(time_adj_inds, matrix(c(from_id, to_id), ncol = 2))
    #     }
    #   }
    # }
    # time_adj_inds <- time_adj_inds[!is.na(time_adj_inds[, 1]), ]
    # adjacency_list <- rbind(adjacency_list, time_adj_inds)
  }

  adjacency_list <- adjacency_list[order(adjacency_list[, 1]), ]
  # Get number of neighboors (here both in space AND time)
  positive_neighbors <- dplyr::mutate(
    dplyr::summarize(
      dplyr::group_by(
        as.data.frame(adjacency_list),
        row
      ),
      n = length(col)
    )
  )
  number_of_neighbors <- rep(0,times = nrow(smooth_grid))
  number_of_neighbors[positive_neighbors$row] <- positive_neighbors$n

  ## create fundamental period
  for (it in model_time_slices) {

  }
  #Matrix::rowSums(nearest_neighbor_matrix)

  # Stan inputs ------------------------------------------------------------------
  cat("---- Preparing Stan input data \n")

  stan_data <- list()

  stan_data$rho <- 0.999
  stan_data$N <-  length(non_na_gridcells)
  stan_data$N_edges <- nrow(adjacency_list)
  stan_data$node1 <- adjacency_list[, 1]
  stan_data$node2 <- adjacency_list[, 2]
  stan_data$diag <- number_of_neighbors

  # the first covariate is always the population (n_grid_cells x n_obs_t_units x n_covar)
  stan_data$pop <- as.numeric(covar_cube[, , 1])[non_na_gridcells]

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
    ## aggregate observations:
    sf_cases_resized$loctime <- ""
    for(i in seq_len(length(ind_mapping$map_obs_loctime_obs))) {
      sf_cases_resized$loctime[which(non_na_obs == ind_mapping$map_obs_loctime_obs[i])] <- paste(sf_cases_resized$loctime[which(non_na_obs == ind_mapping$map_obs_loctime_obs[i])] , 
                                                                                                 ind_mapping$map_obs_loctime_loc[i])
    }
    sf_cases_resized$loctime <- gsub("^ ","",sf_cases_resized$loctime)
    ocrs <- sf::st_crs(sf_cases_resized)
    sf_cases_resized <- sf_cases_resized %>%
      dplyr::group_by(loctime, OC_UID, locationPeriod_id) %>%
      dplyr::group_modify(function(.x,.y){
        cat("iter", unlist(.y), "\n")
        if(nrow(.x) <= 1 ){return(.x %>% dplyr::select(TL,TR,!!rlang::sym(cases_column)))}
        ## combine non-adjacent but overlapping observations
        .x <- dplyr::arrange(dplyr::mutate(.x, set=as.integer(NA)),desc(TR))
        .x$set[[1]] <- 0
        current_set <- 1
        something_changed <- TRUE
        while(any(is.na(.x$set))){
          # print("LOOPING")
          # print(.x$set)
          new_set_indices <- (rev(cummax(rev(!is.na(.x$set)))) == 1) & is.na(.x$set)
          if(any(new_set_indices) & (!something_changed)){
            .x$set[[which(new_set_indices)[[1]] ]] <- current_set
            # print("Assigning from new_set_indices")
            # print(.x$set)
            current_set <- current_set + 1
            something_changed <- TRUE
          } else if(!something_changed){
            .x$set[[which(is.na(.x$set))[[1]] ]] <- current_set
            # print("Starting a new set")
            # print(.x$set)
            current_set <- current_set + 1
            something_changed <- TRUE
          }
          something_changed <- FALSE
          for(set_idx in (seq_len(current_set) - 1) ){
            possible_extensions <- (.x$TR < .x$TL[max(which((.x$set == set_idx) & !is.na(.x$set)))]) & is.na(.x$set)
            if(any(possible_extensions)){
              .x$set[[min(which(possible_extensions))]] <- set_idx
              # print(paste("Extending an existing set",set_idx))
              # print(.x$set)
              something_changed <- TRUE
            }
          }
        }
        ## The TL calculation here is made up
        .ox <- .x
        .x$duration <- .x$TR - .x$TL + 1
        .x <- .x %>% group_by(set) %>% summarize(TL = min(TL), TR = min(TL) + sum(duration) - 1 , !!cases_column := sum(!!rlang::sym(cases_column),na.rm=TRUE)) %>% ungroup() %>% dplyr::select(-set)
        return(.x)
      }) %>% 
      ungroup() 
    # sf_cases_resized$geom <- sf::st_as_sfc(sf_cases_resized$geom)
    sf_cases_resized <- sf::st_as_sf(sf_cases_resized)
    sf::st_crs(sf_cases_resized) <- ocrs
    
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
  
  
  
  non_na_obs_resized <- sort(unique(ind_mapping_resized$map_obs_loctime_obs))
  
  obs_changer <- setNames(seq_len(length(non_na_obs_resized)),non_na_obs_resized)
  stan_data$map_obs_loctime_obs <- obs_changer[as.character(ind_mapping_resized$map_obs_loctime_obs)]
  stan_data$map_obs_loctime_loc <- ind_mapping_resized$map_obs_loctime_loc
  stan_data$tfrac <- ind_mapping_resized$tfrac
  stan_data$map_loc_grid_loc <- ind_mapping_resized$map_loc_grid_loc
  stan_data$map_loc_grid_grid <- ind_mapping_resized$map_loc_grid_grid
  stan_data$u_loctime <- ind_mapping_resized$u_loctimes
  
  y_tfrac <- tibble(tfrac = stan_data$tfrac, 
                    map_obs_loctime_obs = stan_data$map_obs_loctime_obs) %>% 
    dplyr::group_by(map_obs_loctime_obs) %>% 
    dplyr::summarize(tfrac = mean(tfrac)) %>%  # We take the average over time so we can sum population over time
    .[["tfrac"]]
  
  stan_data$M <- nrow(sf_cases_resized)
  stan_data$y <- sf_cases_resized[[cases_column]]
  
  # Extract censoring information
  censoring_inds <- map_chr(
    1:stan_data$M, 
    function(x) {
      # Get all tfracs for the given observation
      tfracs <- stan_data$tfrac[stan_data$map_obs_loctime_obs == x]
      # Define right-censored if any tfrac is smaller than 95% of the time slice
      ifelse(any(tfracs < 0.95), "right-censored", "full")
    })
  
  # Get censoring indexes 
  stan_data$ind_full <- which(censoring_inds == "full") %>% array()
  stan_data$M_full <- length(stan_data$ind_full)
  # TODO Left-censoring is not implemented for now
  stan_data$ind_left <- which(censoring_inds == "left-censored") %>% array()
  stan_data$M_left <- length(stan_data$ind_left)
  stan_data$ind_right <- which(censoring_inds == "right-censored") %>% array()
  stan_data$M_right <- length(stan_data$ind_right)
  stan_data$censoring_inds <- censoring_inds
  
  bad_data <- as.data.frame(sf_cases)[
    !(seq_len(nrow(sf_cases)) %in% non_na_obs),
    c('id','TL','TR',cases_column,'valid','attributes.location_period_id')
  ]
  if (nrow(bad_data) > 0) {
    cat(paste(
      "The following observations were thrown out because they did not cover any gridcells\n ",
      "id,TL,TR,cases,valid,location_period_id\n ",
      paste(
        bad_data$id,
        bad_data$TL,
        bad_data$TR,
        bad_data[[cases_column]],
        bad_data$valid,
        bad_data$attributes.location_period_id,
        sep=',',
        collapse='\n  '
      ),
      "\n"
    ))
  } else {
    cat("All observations cover modeling grid cells, none were dropped \n")
  }

  stan_data$K1 <- length(stan_data$map_obs_loctime_obs)
  stan_data$K2 <- length(stan_data$map_loc_grid_loc)
  stan_data$L <- length(ind_mapping_resized$u_loctimes)
  stan_data$ncovar <- length(covariate_choices)

  print(stan_data$ncovar)

  if (stan_data$ncovar > 0) {
    # Flatten covariate cube to 2d array: [n_pix * n_time_units] * [n_cov]
    # Here the first covariate corresponds to the population raster, so needs to be
    # excluded. Data flattened by pixels first, meaning that
    # stan_data$covar[1:10] = covar_cube[1:10, 1, 2]
    # TODO check if the index removing the first covarcub column which should correspond
    # to population is correct
    stan_data$covar <- matrix(apply(covar_cube, 3, function(x) x[non_na_gridcells])[, -1], nrow = length(non_na_gridcells))

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

    # normalize data
    for (i in seq_len(stan_data$ncovar)) {
      # standardize
      stan_data$covar[, i] <- stan_data$covar[ , i] - mean(stan_data$covar[, i])
      stan_data$covar[,  i] <- stan_data$covar[ , i] / max(abs(stan_data$covar[, i]))
    }
  }
  
  full_grid <- dplyr::left_join(sf::st_drop_geometry(sf_grid),sf::st_drop_geometry(smooth_grid))[,c('upd_id','smooth_id', 't')]
  nobs <- length(unique(stan_data$map_obs_loctime_obs))
  # Compute population corresponding to each observation
  aggpop <- rep(0, nobs)
  for(i in 1:nobs) {
    lps <- stan_data$map_obs_loctime_loc[which(stan_data$map_obs_loctime_obs==i)]
    for (lp in lps) { # This is summed over years so tfrac is averaged over years
      aggpop[i] <- aggpop[i] + sum(stan_data$pop[stan_data$map_loc_grid_grid[stan_data$map_loc_grid_loc == lp]])
    }
  }

  # Compute the mean incidence
  # Note that this is in the model's temporal resolution
  stan_data$meanrate <- sum(stan_data$y * y_tfrac)/sum(aggpop)
  if(stan_data$meanrate < 1e-10){
    stan_data$meanrate <- 1e-10
    print("The mean rate was less than 1e-10, so increased it to 1e-10")
    warning("The mean rate was less than 1e-10, so increased it to 1e-10")
  }
  cat("----\nMean cholera incidence is of", formatC(stan_data$meanrate*1e5, format = "e", digits = 2),
      "cases per 100'000 people per", res_time, "\n----")

  stan_data$smooth_grid_N <- nrow(smooth_grid)
  stan_data$map_smooth_grid <- full_grid$smooth_id
  stan_data$map_grid_time <- full_grid$t
  stan_data['T'] <- nrow(time_slices)
  
  stan_data$map_full_grid <- full_grid$upd_id

  cat("**** FINISHED PREPARING STAN INPUT \n")

  return(list(stan_data = stan_data,
              sf_cases_resized = sf_cases_resized,
              sf_grid = sf_grid,
              smooth_grid  = smooth_grid))
}
