
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
#' @param n_cpus the number of cpus to do parallel computatoin
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

#' check_paralle_setup
#'
#' @param do_parallel 
#' @param n_cpus 
#'
#' @export
#'
check_parallel_setup <- function(do_parallel, 
                                 n_cpus) {
  if(do_parallel) {
    
    library(foreach)
    
    if(n_cpus == 0)
      stop("Specify the number of CPUS to use")
    
    if (!exists("cl")) {
      cat("---- Initializing implicit cluster of", n_cpus, "cores \n")
      # Parallel setup for foreach
      doParallel::registerDoParallel(n_cpus)
      cl <<- TRUE
    }
  } else {
    cat("Parallel setup done \n")
  }
}

#' close_parallel_setup
#'
#' @return
#' @export
#'
#' @examples
close_parallel_setup <- function() {
  if (exists("cl")) {
    doParallel::stopImplicitCluster()
  }
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
#' @param n_cpus the number of cpus to do parallel computatoin
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
                                       do_parallel = F, 
                                       n_cpus = parallel::detectCores() - 2) {
  
  check_parallel_setup(do_parallel = do_parallel,
                       n_cpus = n_cpus)
  
  doFun <- ifelse(do_parallel, foreach::`%dopar%`, foreach::`%do%`)
  
  # Number of chunks for parallel computation
  if (!is.null(n_cpus)) {
    nchunk <- n_cpus * 3 
  } else {
    nchunk <- 1
  }
  
  # Keep only relevant information
  df <- dplyr::select(as.data.frame(df), TL, TR, locationPeriod_id)
  
  res <-  doFun(foreach::foreach(
    rs = itertools::ichunk(iterators::iter(df, by = "row"), nchunk), 
    is = itertools::ichunk(iterators::icount(nrow(df)), nchunk),
    .packages = c("taxdat", "lubridate", "foreach", "iterators", "magrittr", "dplyr"),
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



#' Make temporal grid consistency
#' This function enforces that all space gridcells are matched to 
#' space/time gridcells. This is to avoid that some space gridcells appear only 
#' in certain space/time cells
#'
#' @param sf_grid 
#' @param non_na_gridcells 
#'
#' @return
#' @export
#'
make_temporal_grid_consistency <- function(sf_grid,
                                           non_na_gridcells) {
  
  temporally_inconsistant_cells <- sf_grid %>%
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::group_by(id) %>%
    dplyr::summarize(bad_percentage = sum(!(long_id %in% non_na_gridcells))/length(id)) %>%
    dplyr::filter(bad_percentage != 0, bad_percentage != 1)
  
  if (nrow(temporally_inconsistant_cells)) {
    warning("The following cells were included at some time points, but not others. The corresponding space gridcells will be ecxluded.")
    print(temporally_inconsistant_cells)
  }
  
  rm_cells <- sf_grid %>% 
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(id %in% temporally_inconsistant_cells$id) %>% 
    dplyr::pull(long_id)
  
  non_na_gridcells <- setdiff(non_na_gridcells, rm_cells)
  
  return(non_na_gridcells)
}

#' Title
#'
#' @param sf_grid 
#' @param non_na_gridcells
#' @param grid_rand_effects_N
#'
#' @return
#' @export
#'
make_smooth_grid <- function(sf_grid,
                             non_na_gridcells,
                             grid_rand_effects_N) {
  
  grid_changer <- make_changer(x = non_na_gridcells)
  
  # Set the index of smooth grid time slices
  sf_grid <- sf_grid %>% 
    dplyr::mutate(s = (t-1) %% grid_rand_effects_N + 1)
  
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
                                   verbose = F,
                                   do_parallel = F,
                                   n_cpus = 0) {
  
  check_parallel_setup(do_parallel = do_parallel,
                       n_cpus = n_cpus)
  
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
  
  if (do_parallel) {
    
    df_split <- sf_cases_resized %>%
      dplyr::group_by(loctime, OC_UID, locationPeriod_id, location_name) %>%
      dplyr::group_split() 
    
    nchunk <- n_cpus * 5
    
    sf_cases_resized <- foreach::foreach(
      rs = itertools::ichunk(df_split, nchunk),
      .combine = "bind_rows",
      .inorder = F,
      .packages = c("tidyverse", "taxdat")
    ) %dopar% {
      rs %>% 
        dplyr::bind_rows() %>% 
        dplyr::group_by(loctime, OC_UID, locationPeriod_id, location_name) %>%
        dplyr::group_modify(.f = aggregate_single_lp,
                            verbose = verbose,
                            cases_column = cases_column) %>%
        dplyr::ungroup()
    }
  } else {
    sf_cases_resized <- sf_cases_resized %>%
      dplyr::group_by(loctime, OC_UID, locationPeriod_id, location_name) %>%
      dplyr::group_modify(.f = aggregate_single_lp, 
                          verbose = verbose,
                          cases_column = cases_column) %>%
      dplyr::ungroup()
  } %>% 
    dplyr::arrange(loctime, OC_UID, locationPeriod_id)
  
  
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
                               censoring_thresh) {
  
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
compute_mean_rate_subset <- function(stan_data, 
                                     subset_ind = NULL) {
  
  if (is.null(stan_data$y)) {
    stop("Missing y in stan_data for mean rate computation")
  }
  
  if (is.null(subset_ind)) {
    stop("Please provid non-null subset")
  }
  
  subset_ind <- sort(unique(subset_ind))
  nobs <- length(subset_ind)
  
  y_tfrac <- tibble::tibble(
    tfrac = stan_data$tfrac, 
    map_obs_loctime_obs = stan_data$map_obs_loctime_obs) %>% 
    dplyr::group_by(map_obs_loctime_obs) %>% 
    dplyr::filter(map_obs_loctime_obs %in% subset_ind) %>% 
    dplyr::summarize(tfrac = mean(tfrac)) %>%  # We take the average over time so we can sum population over time
    .[["tfrac"]]
  
  if (length(y_tfrac) == 0) {
    stop("No matching data to comute mean rate on")
  }
  
  # Compute population corresponding to each observation
  aggpop <- rep(0, nobs)
  for(i in 1:nobs) {
    j <- subset_ind[i]
    lps <- stan_data$map_obs_loctime_loc[which(stan_data$map_obs_loctime_obs == j)]
    for (lp in lps) { 
      # This is summed over years so tfrac is averaged over years
      aggpop[i] <- aggpop[i] + sum(stan_data$pop[stan_data$map_loc_grid_grid[stan_data$map_loc_grid_loc == lp]])
    }
  }
  
  # Compute the mean incidence
  # Note that this is in the model's temporal resolution
  meanrate <- sum(stan_data$y[subset_ind] / y_tfrac)/sum(aggpop)
  
  if(meanrate < 1e-7){
    meanrate <- 1e-7
    print("The mean rate was less than 1e-16 so increased it to 1e-7")
    warning("The mean rate was less than 1e-6, so increased it to 1e-7")
  }
  
  cat("---- Mean cholera incidence in subset is of", formatC(meanrate*1e5, format = "e", digits = 2),
      "cases per 100'000 people per", res_time, "\n")
  
  meanrate
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
  
  if (is.null(stan_data$y)) {
    stop("Missing y in stan_data for mean rate computation")
  }
  
  y_tfrac <- tibble::tibble(
    tfrac = stan_data$tfrac, 
    map_obs_loctime_obs = stan_data$map_obs_loctime_obs) %>% 
    dplyr::group_by(map_obs_loctime_obs) %>% 
    dplyr::summarize(tfrac = mean(tfrac)) %>%  # We take the average over time so we can sum population over time
    dplyr::arrange(map_obs_loctime_obs) %>% 
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
  
  # Compute the mean incidence using only full observations
  meanrate <- sum(stan_data$y[stan_data$ind_full] / y_tfrac[stan_data$ind_full])/sum(aggpop[stan_data$ind_full])
  
  if(meanrate < 1e-7){
    meanrate <- 1e-7
    print("The mean rate was less than 1e-7, so increased it to 1e-7")
    warning("The mean rate was less than 1e-7, so increased it to 1e-7")
  }
  
  cat("---- Mean cholera incidence is of", formatC(meanrate*1e5, format = "e", digits = 2),
      "cases per 100'000 people per", res_time, "\n")
  
  meanrate
}
standardize <- function(x){
  (x-mean(x))/sd(x)
}

#' standardize_covar
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

#' compute_pop_loctimes
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

#' compute_pop_loctime_obs
#' Computes the population corresponding to a given observation ID
#' @param stan_data 
#'
#' @return
#' @export
#'
#' @examples
compute_pop_loctime_obs <- function(stan_data,
                                    obs_id) {
  
  # Get loctime(s) of obs
  lt <- get_loctime_obs(stan_data = stan_data, obs_id = obs_id)
  
  # Get grid cells
  cells <- which(stan_data$map_loc_grid_loc %in% lt)
  
  pop <- sum(stan_data$pop[stan_data$map_loc_grid_grid[cells]]  * stan_data$map_loc_grid_sfrac[cells])
  
  return(pop)
}

#' get_loctime_obs
#'
#' @param stan_data 
#' @param obs_id 
#' @return
#' @export
#'
#' @examples
get_loctime_obs <- function(stan_data, obs_id) {
  stan_data$map_obs_loctime_loc[stan_data$map_obs_loctime_obs == obs_id]
}

#' get_grid_loctime
#'
#' @param stan_data 
#' @param loctime 
#'
#' @return
#' @export
#'
#' @examples
get_grid_loctime <- function(stan_data, loctime) {
  stan_data$map_loc_grid_grid[stan_data$map_loc_grid_loc == loctime]
}

#' get_sfrac_loctime
#'
#' @param stan_data 
#' @param loctime 
#'
#' @return
#' @export
#'
#' @examples
get_sfrac_loctime <- function(stan_data, loctime) {
  stan_data$map_loc_grid_sfrac[stan_data$map_loc_grid_loc == loctime]
}

#' get_admin_level
#'
#' @param location_name 
#'
#' @return
#' @export
#'
#' @examples
get_admin_level <- function(location_name) {
  stringr::str_count(location_name, "::") %>% 
    as.numeric() %>% 
    # Remove the continent nesting
    {.-1}
}

#' Q_sum_to_zero_QR
#' https://discourse.mc-stan.org/t/test-soft-vs-hard-sum-to-zero-constrain-choosing-the-right-prior-for-soft-constrain/3884/31
#' @param N 
#'
#' @return
#' @export
#'
Q_sum_to_zero_QR <- function(N) {
  Q_r = rep(0, 2*N);
  
  for(i in 1:N) {
    Q_r[i] = -sqrt((N-i)/(N-i+1.0));
    Q_r[i+N] = 1/sqrt((N-i) * (N-i+1));
    
    if (is.infinite(Q_r[i+N])) {
      Q_r[i+N] <- 0
    }
  }
  Q_r
}

#' sum_to_zero_QR
#'
#' @param x_raw 
#' @param Q_r 
#'
#' @return
#' @export
#'
sum_to_zero_QR <- function(x_raw, 
                           Q_r) {
  
  N = length(x_raw) + 1;
  x = rep(0, N);
  x_aux = 0;
  
  for(i in 1:(N-1)){
    x[i] = x_aux + x_raw[i] * Q_r[i];
    x_aux = x_aux + x_raw[i] * Q_r[i+N];
  }
  x[N] = x_aux;
  x;
}

#' check_stan_input_objects
#'
#' @param censoring_thresh
#' @param sf_cases 
#' @param stan_data 
#' @param sf_cases_resized
#' @export
#' 
check_stan_input_objects <- function(censoring_thresh, sf_cases, stan_data, sf_cases_resized){
  # Test 1: the same number of observations
  if(!identical(
    nrow(sf_cases_resized),
    length(stan_data$y),
    stan_data$M,
    length(stan_data$censoring_inds)
  )){
    stop("At least two objects among stan_input$sf_cases_resized, stan_input$stan_data$y, stan_input$stan_data$M, and stan_input$stan_data$censoring_inds 
    within the stan input do not agree on data dimensions (the number of observations). ")
  }
  
  # Test 2: the same number of cases
  if( sum(sf_cases_resized$attributes.fields.suspected_cases) != sum(stan_data$y) | 
      sum(sf_cases_resized$attributes.fields.suspected_cases != stan_data$y) > 0  ){
    stop("The stan_input$sf_cases_resized, stan_input$stan_data$y don't have the same number of total cases. ")
  }
  
  # Test 3: compare the sums of sf_cases, sf_cases_resized across each OC-locationPeriod-year combination
  sf_cases_cmb <- rbind(sf_cases %>% sf::st_drop_geometry() %>% dplyr::select(OC_UID, locationPeriod_id,
                                                                              attributes.fields.suspected_cases) %>% dplyr::mutate(token = "prior"), 
                        sf_cases_resized %>% sf::st_drop_geometry() %>% dplyr::select(OC_UID,
                                                                                      locationPeriod_id, attributes.fields.suspected_cases) %>% dplyr::mutate(token = "after"))
  sf_cases_cmb <- sf_cases_cmb %>%
    dplyr::group_by(OC_UID, locationPeriod_id, token) %>%
    dplyr::summarize(cases = sum(attributes.fields.suspected_cases)) %>%
    dplyr::group_by(OC_UID, locationPeriod_id) %>%
    dplyr::summarize(compare = ifelse(min(cases) == max(cases), "pass", "fail"))
  if(any(sf_cases_cmb$compare == "fail")){
    stop("For a given OC and a given location period, the sum of the cases in preprocess data and that in stan input data are not the same. ")
  }
  
  # Test 4: the censored observations are the same
  if(!all(stan_data$M_full == length(stan_data$censoring_inds[stan_data$censoring_inds == "full"]) | 
          stan_data$M_right == length(stan_data$censoring_inds[stan_data$censoring_inds == "right-censored"]) | 
          stan_data$M_full == length(stan_data$ind_full) | 
          stan_data$M_right == length(stan_data$ind_right))){
    stop("The censored observations are not the same within stan_data object. ")
  }
  ##newly computed tfracs and censoring 
  single_year_idx <- which(lubridate::year(sf_cases_resized$TL) == lubridate::year(sf_cases_resized$TR))
  tmp <- sf_cases_resized%>%
    dplyr::mutate(
      year = lubridate::year(TL), 
      total_days = dplyr::case_when(
        year%%4 == 0 ~ 366, 
        TRUE ~ 365
      ), 
      cmp_tfrac = as.numeric(TR - TL) / total_days, 
      cmp_censoring = dplyr::case_when(cmp_tfrac < censoring_thresh ~ "right-censored", 
                                       TRUE ~ "full")
    )
  if(!all(table(tmp[single_year_idx, ]$cmp_censoring) == table(stan_data$censoring_inds[single_year_idx]))){
    stop("The calculations of tfracs and censoring for single-year observations are wrong in the stan input preparation process. ")
  }
  
  # Test 5: the number of location/times 
  for(ids in names(table(sf_cases_resized$locationPeriod_id))){
    if(table(sf_cases$locationPeriod_id)[[ids]] < table(sf_cases_resized$locationPeriod_id)[[ids]]){
      stop(paste0("The number of observations in the stan input dataset given a location period id ", ids, " is more than the preprocess data. "))
    }
  }
  
  cat("--- All stan input checks have been passed. \n")
  
}

#' Get data for given administrative level
#'
#' @param data 
#' @param admin_level 
#' @param censoring 
#' @param res_time 
#'
#' @return
#' @export
#'
#' @examples
get_admin_level_data <- function(data,
                                 admin_levels = NULL,
                                 censorings = NULL,
                                 res_time) {
  
  # Check censoring
  if (!is.null(censorings)) {
    if (!(censorings %in% c("full", "right-censored"))) {
      stop("res_time needs to be one of {'full', 'right-censored'}")
    }
  }
  
  # Check res_time
  res_time <- purrr::quietly(check_res_time)(res_time)$result
  
  data %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(ref_TL = get_start_timeslice(TL, res_time),
                  ref_TR = get_end_timeslice(TR, res_time)) %>% 
    {
      if (is.null(admin_levels)) {
        .
      } else {
        dplyr::filter(., admin_level %in% admin_levels)
      }
    } %>% 
    {
      if (is.null(censorings)) {
        .
      } else {
        dplyr::filter(., censoring == censorings)
      }
    } %>% 
    dplyr::filter(ref_TL == get_start_timeslice(TR, res_time),
                  get_end_timeslice(TL, res_time) == ref_TR)
}


#' get_loctime_mappings
#'
#' @param stan_data 
#' @param loctime 
#'
#' @return
#' @export
#'
#' @examples
get_loctime_mappings <- function(stan_data, 
                                 loctime) {
  
  grid <- get_grid_loctime(stan_data = stan_data, 
                           loctime = loctime)
  sfrac <- get_sfrac_loctime(stan_data = stan_data, 
                             loctime = loctime)
  space <- stan_data$map_spacetime_space_grid[grid]
  
  list(
    loctime = loctime,
    grid = grid,
    sfrac = sfrac,
    space = space
  )
}


#' compute_population_coverage
#' Compute the population coverage of a datasate by admin level
#' @param data 
#' @param stan_data 
#' @param ref_pop 
#'
#' @return
#' @export
#'
#' @examples
compute_population_coverage <- function(data,
                                        stan_data,
                                        ref_pop) {
  # Compute fraction of population covered by these loctimes by admin level
  frac_coverages <- data %>% 
    # Keep one entry by loctime
    dplyr::group_by(loctime) %>% 
    dplyr::slice(1) %>% 
    dplyr::mutate(pop = purrr::map_dbl(obs_id, ~ compute_pop_loctime_obs(., stan_data = stan_data))) %>% 
    dplyr::group_by(admin_level) %>% 
    dplyr::summarise(frac_coverage = sum(pop)/ref_pop) %>% 
    tibble::deframe()
  
  frac_coverages
}

#' Compute population time slice location
#' This function is useful to compute the population for a location in an arbitrary
#' time slice
#' @param stan_data 
#' @param obs_id 
#' @param ts 
#'
#' @return
#' @export
#'
#' @examples
compute_pop_ts_loc <- function(stan_data,
                               loctime,
                               ts) {
  
  ref_mappings <- get_loctime_mappings(stan_data,
                                       loctime = loctime)
  
  # Get reference population for this missing time slice
  ts_subset_gridcells <- get_cells_ts(stan_data = stan_data,
                                      space_cells = ref_mappings$space,
                                      ts = ts)
  
  ref_pop <- sum(stan_data$pop[ts_subset_gridcells] * ref_mappings$sfrac)
  
  ref_pop
}

get_cells_ts <- function(stan_data,
                         space_cells,
                         ts) {
  
  # First all gridcells in time slices
  ts_gridcells <- which(stan_data$map_grid_time == ts)
  # Then subset that cover the location period
  ts_subset_gridcells <- ts_gridcells[which(stan_data$map_spacetime_space_grid[ts_gridcells] %in% space_cells)]
  ts_subset_gridcells
}

impute_adm0_obs_single <- function(sf_cases_resized,
                                   stan_data,
                                   ref_amd0_obs_id,
                                   time_slices,
                                   m_ts,
                                   cases_column,
                                   frac_coverage_thresh) {
  
  adm0_template <- sf_cases_resized %>% 
    dplyr::slice(ref_amd0_obs_id) %>% 
    dplyr::mutate(loctime = NA,
                  OC_UID = "imputed",
                  TL = NA,
                  TR = NA,
                  !!cases_column := NA)
  
  m_TL <- time_slices$TL[m_ts]
  m_TR <- time_slices$TR[m_ts]
  
  ref_pop <- compute_pop_ts_loc(stan_data = stan_data,
                                loctime = get_loctime_obs(stan_data, ref_amd0_obs_id),
                                ts = m_ts)
  
  # All data in the missing year
  ts_all <- sf_cases_resized %>% 
    get_admin_level_data(res_time = res_time) %>% 
    dplyr::filter(ref_TL == m_TL,
                  ref_TR == m_TR) 
  
  # All national level data (censored)
  ts_all_adm0 <- sf_cases_resized %>% 
    get_admin_level_data(res_time = res_time,
                         admin_levels = 0) %>%  
    dplyr::filter(ref_TL == m_TL,
                  ref_TR == m_TR) 
  
  # First try getting the full subnational level data corresponding to the year
  ts_subset <- sf_cases_resized %>% 
    get_admin_level_data(res_time = res_time,
                         admin_levels = c(1:10),
                         censorings = "full") %>% 
    dplyr::filter(ref_TL == m_TL,
                  ref_TR == m_TR) 
  
  if (nrow(ts_all) == 0) {
    # Condition 1: No data available in year, impute a 0 observation
    cat("-- No available data, imputating 0 in", 
        as.character(m_TL), "\n")
    impute_obs <- 0
    impute_type <- "0_nodata"
  } else {
    
    # If subnational data available comute the fraction of population coverage
    if (nrow(ts_subset) > 1) {
      
      ts_subset <- ts_subset %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(tfrac = compute_tfrac(TL, TR, ref_TL, ref_TR),
                      tfrac_cases = !!rlang::sym(cases_column)/tfrac) %>% 
        dplyr::group_by(loctime) %>% 
        # !! Keep only one observation per loctime to avoid double-counting population
        dplyr::slice_max(tfrac_cases) %>% 
        dplyr::ungroup() 
      
      # Get population coverage by admin level
      frac_coverages <- compute_population_coverage(data = ts_subset,
                                                    stan_data = stan_data,
                                                    ref_pop = ref_pop)
      
      frac_coverage <- max(frac_coverages)
      subset_admin_lev <- names(frac_coverages)[frac_coverages == frac_coverage]
      
      # Get obs_ids of admin level corresponding to the maximum coverage 
      subset_ind <- ts_subset %>% 
        dplyr::filter(admin_level == subset_admin_lev) %>% 
        dplyr::pull(obs_id)
      
    } else {
      frac_coverage <- 0
    }
    
    # !! If coverage OK compute mean rate (threshold of 10% is hardcoded)
    if (frac_coverage > frac_coverage_thresh) {
      # Compute mean rate for the subnational data
      meanrate_tmp <- compute_mean_rate_subset(stan_data = stan_data,
                                               subset_ind = subset_ind)
      # Imputed observation based on rates
      impute_obs <- round(meanrate_tmp * ref_pop)
      impute_type <- "subnat_rate"
      
      cat("-- Using subnational data for imputation in", 
          as.character(m_TL),
          "imputed obs =", impute_obs, 
          "cases. \n")
      
    } else if (frac_coverage > 0) {
      # Compute sum of non-overlapping subnational level data
      subnat_sums <- ts_subset %>% 
        dplyr::group_by(OC_UID, admin_level) %>% 
        dplyr::summarise(sum_cases = sum(!!rlang::sym(cases_column)))
      
      impute_obs <- max(subnat_sums$sum_cases)
      impute_type <- "subnat_sum"
      
      cat("-- Using sum of subnational data for imputation in", 
          as.character(m_TL),
          "imputed obs =", impute_obs, 
          "cases. \n")
      
    } else if (frac_coverage == 0 & nrow(ts_all_adm0) > 0) {
      # No subnational-level data use censored national level data if available
      
      impute_obs <- max(ts_all_adm0[[cases_column]])
      impute_type <- "nat_max"
      
      cat("-- Using censored national data for imputation in",
          as.character(m_TL),
          "imputed obs =", impute_obs, 
          "cases. \n")
      
      
    } else {
      stop("No imputation conditions met in times slice ", as.character(m_TL), "_", as.character(m_TR))
    }
  }
  
  # Update stan_data and sf_cases_resized with new data
  if (nrow(ts_all_adm0) > 0) {
    loctime <- get_loctime_obs(stan_data = stan_data, obs_id =  ts_all_adm0$obs_id[1])
  } else {
    loctime <- NA
  }
  
  # Add observation of cases corresponding to the mean rate
  imputed_obs_df <- adm0_template %>% 
    dplyr::mutate(loctime = loctime,
                  OC_UID = stringr::str_glue("imputed_{impute_type}"),
                  TL = m_TL,
                  TR = m_TR,
                  !!cases_column := impute_obs)
  
  imputed_obs_df
}

#' Imputation of missing ADM0 level data
#'
#' @param sf_cases_resized 
#' @param stan_data 
#' @param time_slices 
#' @param res_time 
#' @param cases_column 
#' @param frac_coverage_thresh 
#'
#' @return
#' @export
#'
#' @examples
impute_adm0_obs <- function(sf_cases_resized,
                            stan_data,
                            time_slices,
                            res_time,
                            cases_column,
                            frac_coverage_thresh = 0.1) {
  
  # Get national level data with full observations
  y_adm0_full <- get_admin_level_data(data = sf_cases_resized,
                                      admin_levels = 0 ,
                                      censorings = "full",
                                      res_time = res_time)
  
  if (nrow(y_adm0_full) == 0) {
    if (nrow(time_slices) == 1) {
      cat("-- Imputation is not developed in the case a single modeling time slice \n")
      return(
        list(sf_cases_resized = sf_cases_resized,
             stan_data = stan_data)
      )
    }
    stop("No full national-level observations in modeling time slices")
  }
  
  # Get years with no full national level observations that cover a single time slice
  missing_adm0 <- time_slices  %>% 
    dplyr::mutate(ts = row_number()) %>% 
    dplyr::left_join(y_adm0_full %>% 
                       dplyr::distinct(ref_TL, ref_TR) %>% 
                       dplyr::mutate(in_set = TRUE),
                     by = c("TL" = "ref_TL", "TR" = "ref_TR"))
  
  # Get missing time slices
  missing_ts <- missing_adm0 %>% dplyr::filter(is.na(in_set))
  
  if (nrow(missing_ts) == 0) {
    cat("-- No missing times slices with missing full ADM0 data. \n")
    return(sf_cases_resized)
  } else {
    missing_adm0 %>% 
      dplyr::filter(is.na(in_set)) %>% 
      dplyr::pull(TL) %>% 
      stringr::str_c(collapse = ", ") %>% 
      cat("-- Missing national level full data in time slices", ., ". Trying imputation. \n")
  }
  
  # Load loctime populations
  if (!exists("pop_loctimes")) {
    pop_loctimes <- compute_pop_loctimes(stan_data = stan_data)
  }
  
  # Get reference national observation to use as template for imputed data
  imputed_obs_df <- purrr::map_df(
    1:nrow(missing_ts),
    function(i) {
      
      cat("-- Imputing data for", as.character(missing_ts$TL[i]), "\n")
      
      impute_adm0_obs_single(sf_cases_resized = sf_cases_resized,
                             stan_data = stan_data,
                             ref_amd0_obs_id = y_adm0_full$obs_id[1], 
                             time_slices = time_slices,
                             m_ts = missing_ts$ts[i],
                             cases_column = cases_column,
                             frac_coverage_thresh = frac_coverage_thresh)
    })
  
  # Return sf_cases_resized with new data
  sf_cases_resized %>%
    dplyr::bind_rows(imputed_obs_df) %>% 
    dplyr::mutate(obs_id = dplyr::row_number())
}


#' Title
#'
#' @param sf_cases_resized 
#' @param obs_id 
#' @param res_time 
#' @param time_slices 
#'
#' @return
#' @export
#'
#' @examples
get_time_slices_obs <- function(sf_cases_resized,
                                oid,
                                res_time,
                                time_slices) {
  
  obs <- sf_cases_resized %>% 
    sf::st_drop_geometry() %>%
    dplyr::slice(oid) %>% 
    dplyr::mutate(ref_TL = get_start_timeslice(TL, res_time),
                  ref_TR = get_end_timeslice(TR, res_time)) %>% 
    dplyr::select(ref_TL, ref_TR) %>% 
    unlist()
  
  which(time_slices$TL == obs["ref_TL"] | time_slices$TR == obs["ref_TR"])
}

#' update_stan_data_imputation
#'
#' @param sf_cases_resized 
#' @param stan_data 
#' @param time_slices 
#' @param res_time 
#'
#' @return
#' @export
#'
#' @examples
update_stan_data_imputation <- function(sf_cases_resized,
                                        stan_data,
                                        time_slices,
                                        res_time,
                                        cases_column) {
  
  # Checks
  n_imputed <- sum(stringr::str_detect(sf_cases_resized$OC_UID, "imputed"))
  
  if (n_imputed == 0) {
    cat("-- No imputed data found ('imputed' in OC_UID), skipping update of stan_data \n.")
    return(stan_data)
  } else {
    ind_imputed <- which(stringr::str_detect(sf_cases_resized$OC_UID, "imputed"))
  }
  
  # Get national level data with full observations
  y_adm0_full <- get_admin_level_data(data = sf_cases_resized,
                                      admin_levels = 0 ,
                                      censorings = "full",
                                      res_time = res_time) %>% 
    # Remove imputed loctimes
    dplyr::filter(!stringr::str_detect(OC_UID, "imputed"))
  
  # Get mapping objects for imputation
  ref_cntry_mappings <- get_loctime_mappings(stan_data = stan_data,
                                             loctime = get_loctime_obs(stan_data, y_adm0_full$obs_id[1]))
  
  
  # Numbers in stan data that may be updated due to imputation
  n_loc <- stan_data$L
  n_obs <- stan_data$M
  n_obs_full <- stan_data$M_full
  
  for (i in ind_imputed) {
    
    # Get time slice of observation
    m_ts <- get_time_slices_obs(sf_cases_resized = sf_cases_resized,
                                oid = i,
                                time_slices = time_slices,
                                res_time = res_time)
    
    # Update stan_data and sf_cases_resized with new data
    if (!is.na(sf_cases_resized$loctime[i])) {
      loctime <- sf_cases_resized$loctime[i]
      new_loctime <- F
    } else {
      ts_cntry_gridcells <- get_cells_ts(stan_data = stan_data,
                                         space_cells = ref_cntry_mappings$space,
                                         ts = m_ts)
      ref_cntry_sfrac <- ref_cntry_mappings$sfrac
      
      # Create a new loctime for this year
      loctime <- n_loc + 1
      # Increment total number of loctimes
      n_loc <- n_loc + 1
      new_loctime <- T
      cells <- ts_cntry_gridcells
      cells_sfrac <- ref_cntry_sfrac
    }
    
    # Update counters
    n_obs <- n_obs + 1
    n_obs_full <- n_obs_full + 1
    
    # Update stan_data
    stan_data$y <- c(stan_data$y, sf_cases_resized[[cases_column]])
    stan_data$tfrac <- c(stan_data$tfrac, 1)
    stan_data$censored <- c(stan_data$censored, 0)
    stan_data$map_obs_loctime_loc <- c(stan_data$map_obs_loctime_loc, loctime)
    stan_data$map_obs_loctime_obs <- c(stan_data$map_obs_loctime_obs, n_obs)
    stan_data$map_obs_admin_lev <- c(stan_data$map_obs_admin_lev, 1)
    stan_data$ind_full <- c(stan_data$ind_full, n_obs)
    
    # If necessary update grid mappings
    if (new_loctime) {
      stan_data$map_loc_grid_grid <- c(stan_data$map_loc_grid_grid, cells)
      stan_data$map_loc_grid_loc <- c(stan_data$map_loc_grid_loc, rep(loctime, length(cells)))
      stan_data$map_loc_grid_sfrac <- c(stan_data$map_loc_grid_sfrac, cells_sfrac)
    }
  }
  
  # Update stan data counters
  stan_data$K1 <- length(stan_data$map_obs_loctime_loc) 
  stan_data$K2 <- length(stan_data$map_loc_grid_grid) 
  stan_data$M <- n_obs
  stan_data$M_full <- n_obs_full
  stan_data$L <- n_loc
  
  # Update censoring inds (use stan_data instead of ind_mapping resized to avoid recomputing it)
  stan_data$censoring_inds <-  c(stan_data$censoring_inds, 
                                 rep("full", n_imputed))
  
  stan_data
}