
#' @name nb2graph
#' @title nb2graph
#' @param x nb_object
#'
#' @return dataframe containing num nodes, num edges, and a list of graph edges from node1 to node2. from: https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R
#' @export
nb2graph <- function(x) {
  N <- length(x)
  n_links <- 0
  for (i in 1:N) {
    if (length(x[[i]]) > 0) {
      if (x[[i]][1] != 0) {
        n_links <- n_links + length(x[[i]])
      }
    }
  }
  N_edges <- n_links / 2
  node1 <- vector(mode = "numeric", length = N_edges)
  node2 <- vector(mode = "numeric", length = N_edges)
  idx <- 0
  for (i in 1:N) {
    if (length(x[[i]]) > 0) {
      if (x[[i]][1] > 0) {
        for (j in 1:length(x[[i]])) {
          n2 <- unlist(x[[i]][j])
          if (i < n2) {
            idx <- idx + 1
            node1[idx] <- i
            node2[idx] <- n2
          }
        }
      }
    }
  }
  return(list("N" = N, "N_edges" = N_edges, "node1" = node1, "node2" = node2))
}


#' @title Get space time index
#'
#' @description Compute the space-time index of observations based on a reference grid and its location-period and time
#'
#' @param df the dataframe of the cholera cases
#' @param lp_dict the location-periods dictionary
#' @param model_time_slices dataframe of left and right bounds of the modeling time slices
#' @param res_time the time resolution of the model
#' @param do_parallel whether to compute indices in parallel or not
#' @param n_cpus the number of cpus to do parellel computatoin
#'
#' @details the space-time indices are a replication of the grid indices with an offset of the number of pixels
#'
#' @return a list with the mapping to observations, the mapping to grid cells and the fraction of modelling time step covered by each observation
#' @export
get_space_time_ind <- function(df, 
                               lp_dict, 
                               model_time_slices, 
                               res_time,
                               do_parallel = T, 
                               n_cpus = parallel::detectCores() - 2) {
  
  
  if(do_parallel) {
    if(n_cpus == 0)
      stop("Specify the number of CPUS to use")
    
    # Parallel setup
    cl <- parallel::makeCluster(n_cpus)
    doParallel::registerDoParallel(cl)
  }
  
  doFun <- ifelse(do_parallel, foreach::`%dopar%`, foreach::`%do%`)
  
  nchunk <- 1000
  df <- dplyr::select(as.data.frame(df), TL, TR, locationPeriod_id)
  
  res <-  doFun(foreach::foreach(
    rs = itertools::ichunk(iterators::iter(df, by = "row"), nchunk), 
    is = itertools::ichunk(iterators::icount(nrow(df)), nchunk),
    .packages = c("lubridate", "foreach", "iterators", "magrittr", "dplyr"),
    .combine = rbind,
    .inorder = F),
    {
      foreach::`%do%`(foreach::foreach(
        r = rs, 
        i = is,
        .packages = c("lubridate", "foreach", "iterators"),
        .combine = rbind,
        .inorder = F), {
          
          # Get the modeling time slices covered by the data
          temporal_bands <- seq(
            dplyr::last(which(model_time_slices$TL <= r$TL[1])),
            which(model_time_slices$TR >= r$TR[1])[1]
          )
          
          # Get indices of spatio-temporal modeling grid covered by the observation
          inds <- which(lp_dict$location_period_id == r$locationPeriod_id[1] & lp_dict$t %in% temporal_bands)
          
          if (length(inds) > 0) {
            cell_ind <- lp_dict$upd_long_id[inds]
            time_slices_ind <- lp_dict$t[inds]
            
            ## proportion of time captured by the observation, normalized by number of 
            ## modeling time steps that the observation spans
            tf <- unlist(
              lapply(
                temporal_bands,
                function(x) {
                  suppressMessages(
                    lubridate::days(lubridate::days(1) + min(r$TR[1], model_time_slices$TR[x]) - max(r$TL[1], model_time_slices$TL[x])) /
                      lubridate::days(lubridate::days(1) + model_time_slices$TR[x] - model_time_slices$TL[x])
                  )
                })
            ) %>% 
              setNames(temporal_bands)
            
            timefracs <- tf[as.character(time_slices_ind)]
            
            tibble::tibble(map_obs = rep(i, length(cell_ind)), 
                           map_cell = cell_ind, 
                           tfrac = timefracs)
          }
        }
      )
    }
  ) 
  
  if (do_parallel) 
    parallel::stopCluster(cl)
  
  # Reorder based on observation
  res <- dplyr::arrange(res, map_obs, map_cell)
  return(res)
}

#' @title Reorder for single source
#'
#' @description Reorder adjacency matrix to have single source
#'
#' @param A
#' @param coords  
#'
#' @return the reodered adjacency matrix
#' @export
reorder_single_source <- function(A, 
                                  coords) {
  
  # Check if the matrix is symetric
  if (!Matrix::isSymmetric(A)) {
    stop("Adjacency matrix not symetric fed to re-ordering")
  }
  
  print("Reordering adjacency to have single source")
  
  # Number of vertices
  N <- nrow(A)
  
  # Reoder with first top-left
  topleftorder <- order(coords[,1]-coords[,2])
  Atopleft <- A[topleftorder, topleftorder]
  
  # Undirected graph
  g_un <- igraph::graph_from_adjacency_matrix(Atopleft, mode = "undirected")
  
  # Determine vertex ordering using the breadth-first algorithm
  bfs_ordering <- igraph::bfs(g_un, root = 1)
  bfs_order <- as.vector(bfs_ordering$order)
  
  # Extract directed reodered matrix
  A_bfs <- Atopleft[bfs_order, bfs_order]
  A_bfs_directed <- Matrix::triu(A_bfs)
  
  return(list(A = A_bfs_directed, 
              reordering = topleftorder[bfs_order]))
}

#' @title Get space time index
#'
#' @description Compute the space-time index of observations based on a reference grid and its location-period and time
#' This version of the code is to run the speedup Stan code which maps observations to location periods and 
#' location periods to grid cells
#'
#' @param df the dataframe of the cholera cases
#' @param lp_dict the location-periods dictionary
#' @param model_time_slices dataframe of left and right bounds of the modeling time slices
#' @param do_parallel 
#' @param do_parallel whether to compute indices in parallel or not
#' @param n_cpus the number of cpus to do parellel computatoin
#'
#' @details the space-time indices are a replication of the grid indices with an offset of the number of pixels.
#' In this version we compute to mappings: from observations to location periods and from
#' location periods to grid cells
#'
#' @return a list with the mapping to observations, the mapping to grid cells
#'  and the fraction of modelling time step covered by each observation
#' @export
get_space_time_ind_speedup <- function(df, 
                                       lp_dict, 
                                       model_time_slices, 
                                       res_time,
                                       do_parallel = T, 
                                       n_cpus = parallel::detectCores() - 2) {
  
  
  if(do_parallel) {
    if(n_cpus == 0)
      stop("Specify the number of CPUS to use")
    
    # Parallel setup
    cl <- parallel::makeCluster(n_cpus)
    doParallel::registerDoParallel(cl)
  }
  
  doFun <- ifelse(do_parallel, foreach::`%dopar%`, foreach::`%do%`)
  
  # Number of chunks for parllel computation
  nchunk <- 20
  # Keep only relevant information
  df <- dplyr::select(as.data.frame(df), TL, TR, locationPeriod_id)
  
  res <-  doFun(foreach::foreach(
    rs = itertools::ichunk(iterators::iter(df, by = "row"), nchunk), 
    is = itertools::ichunk(iterators::icount(nrow(df)), nchunk),
    .packages = c("lubridate", "foreach", "iterators", "magrittr", "dplyr"),
    .combine = rbind,
    .inorder = F),
    {
      foreach::`%do%`(foreach::foreach(
        r = rs, 
        i = is,
        .packages = c("lubridate", "foreach", "iterators"),
        .combine = rbind,
        .inorder = F), {
          
          # Get the modeling time slices covered by the data
          temporal_bands <- seq(
            dplyr::last(which(model_time_slices$TL <= r$TL[1])),
            which(model_time_slices$TR >= r$TR[1])[1]
          )
          
          # Get indices of spatio-temporal modeling grid covered by the observation
          inds <- which(lp_dict$location_period_id == r$locationPeriod_id[1] & lp_dict$t %in% temporal_bands)
          
          if (length(inds) > 0) {
            # Unique 3D grid cell indexes
            cell_ind <- lp_dict$upd_long_id[inds]
            lp_inds <- lp_dict$loctime_id[inds]
            # Unique modeling time slice indexes
            time_slices_ind <- lp_dict$t[inds]
            # Unique location period ids (accounting for modeling time slices)
            # and modeling time slices
            lp_loctime_t <- distinct(lp_dict[inds, ], loctime_id, t)
            
            ## proportion of time captured by the observation, normalized by number of 
            ## modeling time steps that the observation spans
            tf <- unlist(
              lapply(
                temporal_bands,
                function(x) {
                  suppressMessages(
                    compute_tfrac(TL = r$TL[1],
                                  TR = r$TR[1],
                                  TL_ref = model_time_slices$TL[x],
                                  TR_ref =  model_time_slices$TR[x])
                  )
                })
            ) %>% 
              setNames(temporal_bands)
            
            # Outputs:
            # K1: length of the mapping from observation to location periods
            # K2: length of the mapping from location periods to cells
            
            timefracs <- tf[as.character(lp_loctime_t$t)]
            
            tibble::tibble(obs = i,
                           map_obs_loctime_obs = list(rep(i, nrow(lp_loctime_t))), 
                           map_obs_loctime_loc = list(lp_loctime_t$loctime_id),
                           map_loc_grid_loc = list(lp_inds),
                           map_loc_grid_grid = list(cell_ind), 
                           tfrac = list(timefracs))
          }
        }
      )
    }
  ) 
  
  if (do_parallel) 
    parallel::stopCluster(cl)
  
  # Reorder based on observation
  res <- dplyr::arrange(res, obs)
  
  # Unpack results
  map_obs_loctime_obs <- unlist(res$map_obs_loctime_obs)
  map_obs_loctime_loc <- unlist(res$map_obs_loctime_loc)
  map_loc_grid_loc_all <- unlist(res$map_loc_grid_loc)
  map_loc_grid_grid_all <- unlist(res$map_loc_grid_grid)
  tfrac <- unlist(res$tfrac)
  obs <- unlist(purrr::map(1:nrow(res), function(i) rep(res$obs[i], length(res$tfrac[[i]])))) 
  
  # Get unique location periods
  u_loctimes <- sort(unique(map_obs_loctime_loc))
  # Create new index of unique location periods
  u_loctimes_ind <- 1:length(u_loctimes)
  names(u_loctimes_ind) <- as.character(u_loctimes)
  
  # Get unique set of mappings between location periods and grid cells
  map_loc_grid_loc <- vector("integer", 0)
  map_loc_grid_grid <- vector("integer", 0)
  
  for (i in u_loctimes_ind) {
    # Get set of grid cells that cover the location period
    cell_ind <- map_loc_grid_grid_all[map_loc_grid_loc_all == u_loctimes[i]] %>% 
      unique() %>% 
      sort()
    
    # Append results
    map_loc_grid_loc <- c(map_loc_grid_loc, rep(i, length(cell_ind)))
    map_loc_grid_grid <- c(map_loc_grid_grid, cell_ind)
  }
  
  # Get set of population 1km weights
  u_loc_grid_weights <- purrr::map_dbl(
    1:length(map_loc_grid_loc),
    ~ with(lp_dict, pop_weight[upd_long_id == map_loc_grid_grid[.] & 
                                 loctime_id == u_loctimes[map_loc_grid_loc[.]]]))
  
  # Adjust location period mapping ids to reindexed ids
  map_obs_loctime_loc <- u_loctimes_ind[as.character(map_obs_loctime_loc)]
  
  return(list(obs = obs, 
              map_obs_loctime_obs = map_obs_loctime_obs, 
              map_obs_loctime_loc = map_obs_loctime_loc,
              map_loc_grid_loc = map_loc_grid_loc,
              map_loc_grid_grid = map_loc_grid_grid,
              u_loctimes = u_loctimes,
              u_loctimes_ind = u_loctimes_ind,
              u_loc_grid_weights = u_loc_grid_weights,
              tfrac = tfrac))
}


# Functions to move to taxdat
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
make_changer <- function(x) {
  setNames(seq_len(length(x)), x)
}

#' Title
#'
#' @param sf_grid 
#' @param smooth_covariate_number_timesteps 
#'
#' @return
#' @export
#'
make_smooth_grid <- function(sf_grid,
                             non_na_gridcells,
                             smooth_covariate_number_timesteps) {
  
  grid_changer <- make_changer(x = non_na_gridcells)
  
  # Set the index of smooth grid time slices
  sf_grid <- sf_grid %>% 
    dplyr::mutate(s = (t-1) %% smooth_covariate_number_timesteps + 1)
  
  temporally_inconsistant_cells <- sf_grid %>%
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::group_by(id) %>%
    dplyr::summarize(bad_percentage = sum(!(long_id %in% non_na_gridcells))/length(id)) %>%
    dplyr::filter(bad_percentage != 0, bad_percentage != 1)
  
  if (nrow(temporally_inconsistant_cells)) {
    warning("The following cells were included at some time points, but not others. See output for details")
    print(temporally_inconsistant_cells)
  }
  
  # Drop cells with NAs
  sf_grid_drop <- sf_grid %>% 
    dplyr::filter(!(long_id %in% non_na_gridcells))
  
  cat("---- Dropping", nrow(sf_grid_drop), "space-time cells corresponding to",
      length(unique(sf_grid_drop$id)), "space cells with space ids:",
      unique(sf_grid_drop$id))
  
  sf_grid <- sf_grid %>% 
    dplyr::filter(long_id %in% non_na_gridcells) %>% 
    dplyr::mutate(upd_id = grid_changer[as.character(long_id)])
  
  smooth_grid <- sf_grid %>% 
    dplyr::select(id, s) %>% 
    dplyr::group_by(id, s) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(smooth_id = dplyr::row_number())
  
  return(
    list(sf_grid = sf_grid,
         smooth_grid = smooth_grid)
  )
}

# First connect all vertices
#' Title
#'
#' @param nn_mat
#' @param smooth_grid_it
#'
#' @return
#' @export
#'
connect_vertices <- function(nn_mat,
                             smooth_grid_it) {
  
  isolated_vertices <- which(Matrix::rowSums(nn_mat) == 0)
  
  if(length(isolated_vertices) > 1){
    cat("Found", length(isolated_vertices), "isolated vertices in time slice", smooth_grid_it$s ,", adding edges until only one remains.\n")
    
    for(vertex in isolated_vertices[-1]){
      
      dist_to_main <- sf::st_distance(
        smooth_grid_it[vertex, ], 
        smooth_grid_it[seq_len(vertex-1), ]
      ) %>% 
        as.vector()
      
      new_neighbor <- which.min(dist_to_main)
      nn_mat[vertex, new_neighbor] <- 1
      nn_mat[new_neighbor, vertex] <- 1
    }
  }
  
  nn_mat
}


#' Title
#'
#' @param nn_mat 
#' @param smooth_grid_it
#' @param it
#' @return
#' @export
#'
connect_islands <- function(nn_mat,
                            smooth_grid_it, 
                            it) {
  
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
    smooth_centroids <- sf::st_geometry(sf::st_centroid(smooth_grid_it))
    
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
      nn_mat[mainland_ids[nearest_main_ids[nearest_isl_id]], island_ids[nearest_isl_id] ] <- 1
    }
    
    # Check that everything is connected now
    if (igraph::clusters(igraph::graph_from_adjacency_matrix(nn_mat))$no > 1) {
      print(unique(igraph::clusters(igraph::graph_from_adjacency_matrix(nn_mat))$no > 1))
      stop("Something went wrong with island connection.")
    } 
  }
  
  nn_mat
} 



#' Title
#'
#' @param adjacency_list 
#' @param sf_grid 
#' @param cell_id_mapping 
#' @param flag 
#'
#' @return
#' @export
#'
add_time_adjacency <- function(adjacency_list,
                               sf_grid,
                               cell_id_mapping,
                               flag = F) {
  
  if (flag) {
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
  
  adjacency_list
}


#' Title
#'
#' @param adjacency_list 
#' @param smooth_grid 
#'
#' @return
#' @export
#'
compute_n_neighbors <- function(adjacency_list,
                                smooth_grid) {
  
  # Get number of neighboors (here both in space AND time)
  positive_neighbors <- adjacency_list %>% 
    dplyr::as_tibble() %>% 
    dplyr::count(row)
  
  number_of_neighbors <- rep(0, times = nrow(smooth_grid))
  number_of_neighbors[positive_neighbors$row] <- positive_neighbors$n
  
  number_of_neighbors
}

#' Title
#'
#' @return
#' @export
#'
get_crs_africa <- function() {
  sf::st_crs("+proj=sinu +lon_0=15 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
}

#' Title
#'
#' @param smooth_grid 
#' @param model_time_slices 
#'
#' @return
#' @export
#'
make_adjacency <- function(smooth_grid,
                           model_time_slices,
                           non_na_gridcells) {
  
  grid_changer <- make_changer(x = non_na_gridcells)
  
  # this is the mapping between the grid ids (from 1 to number of cells that intersect
  # location_periods) and the model ids in space (from 1 to the number of non-NA cells,
  # i.e. cells for which all covariates are non-NA). Note that model ids here do not
  # consider time slices, this is done when computing the adjacency list
  
  cell_id_mapping <- data.frame()    # mapping from cell ids to adjacency
  adjacency_list <- matrix(nrow = 0, ncol = 2)    # Adjacency list
  cnt <- 0    # Counter for number of cells
  
  # Iterate over time slices of space-time grid-level random effects
  for (it in model_time_slices) {
    
    smooth_grid_it <- smooth_grid %>%
      dplyr::filter(s == it) %>%
      dplyr::select(id) 
    
    # Snap to grid to fix neighborhoods
    smooth_grid_it <- smooth_grid_it %>% 
      sf::st_transform(get_crs_africa()) %>% 
      lwgeom::st_snap_to_grid(1)
    
    # Extract neighborhood
    poly_adj <- spdep::poly2nb(smooth_grid_it)
    
    # Transform to adjacency
    adj_dat <- nb2graph(poly_adj) 
    
    # Extract info
    N <- adj_dat$N            # number of spatial units at the level of spatial interactions
    node1 <- adj_dat$node1    # "origin" node list
    node2 <- adj_dat$node2    # "destination" node
    N_edges <- adj_dat$N_edges    # number of edges
    
    # Transform to sparse matrix
    nn_mat <- Matrix::sparseMatrix(i = node1, j = node2, x = 1,
                                   symmetric = TRUE, dims = c(N, N))
    
    # Make fully connected graph
    nn_mat <- nn_mat %>% 
      # Connect isolated vertices
      connect_vertices(nn_mat = .,
                       smooth_grid_it = smooth_grid_it) %>%
      # Connect islands to mainland
      connect_islands(nn_mat = .,
                      smooth_grid_it = smooth_grid_it, 
                      it = it)
    
    cat("Done island connection for smooth time slice", it, "\n")
    
    # Reorder nodes in directed graph to have a single source using the breadth-
    # first algorithm. for DAGAR
    smooth_centroids_coord <- smooth_grid_it %>% 
      sf::st_geometry() %>% 
      sf::st_centroid() %>% 
      sf::st_coordinates()
    
    nn_mat_reordereding <- reorder_single_source(
      A = nn_mat, 
      coords = smooth_centroids_coord
    )
    
    # Update adjacency list
    n_entries <- nrow(nn_mat_reordereding$A)
    reorderered_adj_list <- Matrix::which(nn_mat_reordereding$A > 0, arr.ind = T)
    # Reorder using previous cell id
    nonzero_ind <- apply(reorderered_adj_list, 2, function(x) nn_mat_reordereding$reordering[x]) + cnt
    
    # Append to adjacency list
    adjacency_list <- rbind(adjacency_list, nonzero_ind)
    
    # Update the counter to account for the smooth time slices. Note that n_entries for now
    # is the same for all time slices, since the non-NA cells are determined across
    # time slices of covariates
    cnt <- cnt + n_entries
    
    ids <- smooth_grid %>%
      dplyr::filter(s == it, id %in% non_na_gridcells)
    
    cell_id_mapping <- cell_id_mapping %>% 
      rbind(
        data.frame(s = it,
                   id = ids$id,
                   upd_id = grid_changer[as.character(ids$id)])
      )
  }
  
  # Add time adjacency
  # Note: this is not implemented yet and this will not do anything
  adjacency_list <- add_time_adjacency(adjacency_list = adjacency_list,
                                       sf_grid = sf_grid,
                                       cell_id_mapping = cell_id_mapping,
                                       flag = F)
  
  # Reorder
  adjacency_list <- adjacency_list[order(adjacency_list[, 1]), ]
  
  # Compute number of neighbors
  number_of_neighbors <- compute_n_neighbors(adjacency_list = adjacency_list,
                                             smooth_grid = smooth_grid)
  
  
  list(
    adjacency_list = adjacency_list,
    number_of_neighbors = number_of_neighbors
  )
}

#' Title
#'
#' @param covar_cube 
#' @param non_na_gridcells 
#'
#' @return
#' @export
#'
#' @examples
extract_population <- function(covar_cube,
                               non_na_gridcells = NULL) {
  # the first covariate is always the population (n_grid_cells x n_obs_t_units x n_covar)
  if (is.null(non_na_gridcells)) {
    as.numeric(covar_cube[, , 1])
  } else {
    as.numeric(covar_cube[, , 1])[non_na_gridcells]
  }
}

#' Title
#'
#' @param .x 
#' @param .y 
#' @param cases_column 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
aggregate_single_lp <- function(.x, 
                                .y, 
                                cases_column,
                                verbose = F){
  
  if (verbose) {
    cat("iter", unlist(.y), "\n")
  }
  
  if(nrow(.x) <= 1){
    # If only single observation return time bounds and data
    res <- .x %>% dplyr::select(TL, TR, !!rlang::sym(cases_column))
    return(res)
  }
  
  # Combine non-adjacent but overlapping observations
  .x <- .x %>% 
    dplyr::mutate(set = as.integer(NA)) %>% 
    dplyr::arrange(dplyr::desc(TR))
  
  # Initialization
  .x$set[1] <- 0
  current_set <- 1
  something_changed <- TRUE
  
  while(any(is.na(.x$set))){
    
    if (verbose) {
      print("LOOPING")
      print(.x$set)
    }
    
    # Get indices of new set
    new_set_indices <- (rev(cummax(rev(!is.na(.x$set)))) == 1) & is.na(.x$set)
    
    if(any(new_set_indices) & (!something_changed)){
      
      # Add indices to current set if overlapping
      .x$set[[which(new_set_indices)[[1]]]] <- current_set
      
      if (verbose) {
        print("Assigning from new_set_indices")
        print(.x$set)
      }
      
      current_set <- current_set + 1
      something_changed <- TRUE
      
    } else if(!something_changed){
      .x$set[[which(is.na(.x$set))[[1]]]] <- current_set
      
      if (verbose) {
        print("Starting a new set")
        print(.x$set)
      }
      
      current_set <- current_set + 1
      something_changed <- TRUE
    }
    
    something_changed <- FALSE
    
    # Find possible extensions
    for(set_idx in (seq_len(current_set) - 1)) {
      
      # Get index of latest observation in current set 
      ind_max <- max(which((.x$set == set_idx) & !is.na(.x$set)))
      
      # Get data that is overlapping but which has not yet been assigned to set
      possible_extensions <- (.x$TR < .x$TL[ind_max]) & is.na(.x$set)
      
      if(any(possible_extensions)){
        # Add first extention
        .x$set[[min(which(possible_extensions))]] <- set_idx
        
        if (verbose) {
          print(paste("Extending an existing set",set_idx))
          print(.x$set)
        }
        
        something_changed <- TRUE
      }
    }
  }
  
  ## The TL calculation here is made up
  .ox <- .x
  .x <- .x %>% 
    dplyr::mutate(duration = TR - TL + 1) %>% 
    dplyr::group_by(set) %>% 
    dplyr::summarize(TL = min(TL), 
                     TR = min(TL) + sum(duration) - 1 , 
                     !!cases_column := sum(!!rlang::sym(cases_column), na.rm=TRUE)) %>%  
    dplyr::ungroup() %>% 
    dplyr::select(-set)
  
  # Remove duplicates
  res <- .x[!duplicated(.x),]
  
  return(res)
}

#' Title
#'
#' @param sf_cases_resized 
#' @param non_na_obs 
#' @param ind_mapping 
#' @param cases_column 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
aggregate_observations <- function(sf_cases_resized,
                                   non_na_obs,
                                   ind_mapping,
                                   cases_column,
                                   verbose = F) {
  
  # Get OCRS 
  ocrs <- sf::st_crs(sf_cases_resized)
  
  # Initialize loctime
  sf_cases_resized$loctime <- NA
  
  # Create string of unique lp identifiers for each observation 
  for(i in seq_len(length(non_na_obs))){
    sf_cases_resized$loctime[i] <- paste(
      ind_mapping$map_obs_loctime_loc[ind_mapping$map_obs_loctime_obs == non_na_obs[[i]]], 
      collapse = ', '
    )
  }
  
  
  sf_cases_resized <- sf_cases_resized %>%
    dplyr::group_by(loctime, OC_UID, locationPeriod_id) %>%
    dplyr::group_modify(.f = aggregate_single_lp, 
                        verbose = verbose, 
                        cases_column = cases_column) %>% 
    dplyr::ungroup() 
  
  # sf_cases_resized$geom <- sf::st_as_sfc(sf_cases_resized$geom)
  sf_cases_resized <- sf::st_as_sf(sf_cases_resized)
  sf::st_crs(sf_cases_resized) <- ocrs
  
  sf_cases_resized
}

#' Title
#'
#' @param x 
#' @param obs_changer 
#'
#' @return
#' @export
#'
#' @examples
get_map_obs_loctime_obs <- function(x, obs_changer) {
  x %>% 
    as.character() %>% 
    obs_changer[.] %>% 
    as.array()
}

#' Title
#'
#' @param M 
#' @param ind_mapping_resized 
#' @param censoring_thresh 
#'
#' @return
#' @export
#'
#' @examples
get_censoring_inds <- function(stan_data, 
                               ind_mapping_resized,
                               censoring_thresh = NULL) {
  
  if (is.null(censoring_thresh)) {
    cat("-- No censoring thresh specified, considering all observations as full.\n")
    censoring_thresh <- 1
  }
  
  purrr::map_chr(
    1:stan_data$M, 
    function(x) {
      # Get all tfracs for the given observation
      tfracs <- ind_mapping_resized$tfrac[stan_data$map_obs_loctime_obs == x]
      # Define right-censored if any tfrac is smaller than 95% of the time slice
      ifelse(any(tfracs <= censoring_thresh), "right-censored", "full")
    })
}


#' Title
#'
#' @param stan_data 
#'
#' @return
#' @export
#'
#' @examples
compute_mean_rate <- function(stan_data) {
  
  y_tfrac <- tibble::tibble(
    tfrac = stan_data$tfrac, 
    map_obs_loctime_obs = stan_data$map_obs_loctime_obs) %>% 
    dplyr::group_by(map_obs_loctime_obs) %>% 
    dplyr::summarize(tfrac = mean(tfrac)) %>%  # We take the average over time so we can sum population over time
    .[["tfrac"]]
  
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
  meanrate <- sum(stan_data$y * y_tfrac)/sum(aggpop)
  if(meanrate < 1e-10){
    meanrate <- 1e-10
    print("The mean rate was less than 1e-10, so increased it to 1e-10")
    warning("The mean rate was less than 1e-10, so increased it to 1e-10")
  }
  
  cat("---- Mean cholera incidence is of", formatC(meanrate*1e5, format = "e", digits = 2),
      "cases per 100'000 people per", res_time, "\n")
  
  meanrate
}
standardize <- function(x){
  (x-mean(x))/sd(x)
}

#' Title
#'
#' @param M 
#'
#' @return
#' @export
#'
#' @examples
standardize_covar <- function(M){
  cbind(
    M[, 1], 
    apply(M[, -1, drop=F], 2, standardize)
  )
}

#' Title
#'
#' @param stan_data 
#'
#' @return
#' @export
#'
#' @examples
compute_pop_loctimes <- function(stan_data) {
  # Initialize 
  K2 <- length(stan_data$map_loc_grid_loc)
  
  # Compute pop_loctimes
  pop_loctimes <- rep(0, stan_data$L)
  for (i in 1:K2) {
    pop_loctimes[stan_data$map_loc_grid_loc[i]] <- pop_loctimes[stan_data$map_loc_grid_loc[i]] + stan_data$pop[stan_data$map_loc_grid_grid[i]]  * stan_data$map_loc_grid_sfrac[i]
  }
  
  pop_loctimes
}

