
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
#' 
reoder_single_source <- function(A, 
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
  A_bfs <- A[bfs_order, bfs_order]
  A_bfs_directed <- matrix(0, nrow = N, ncol = N)
  A_bfs_directed[upper.tri(A_bfs)] <- A_bfs[upper.tri(A_bfs)]
  
  return(list(A = A_bfs_directed, 
              reordering = bfs_order))
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
                    lubridate::days(lubridate::days(1) + min(r$TR[1], model_time_slices$TR[x]) - max(r$TL[1], model_time_slices$TL[x])) /
                      lubridate::days(lubridate::days(1) + model_time_slices$TR[x] - model_time_slices$TL[x])
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
  
  # Adjust location period mapping ids to reindexed ids
  map_obs_loctime_loc <- u_loctimes_ind[as.character(map_obs_loctime_loc)]
  
  return(list(obs = obs, 
              map_obs_loctime_obs = map_obs_loctime_obs, 
              map_obs_loctime_loc = map_obs_loctime_loc,
              map_loc_grid_loc = map_loc_grid_loc,
              map_loc_grid_grid = map_loc_grid_grid,
              u_loctimes = u_loctimes,
              u_loctimes_ind = u_loctimes_ind,
              tfrac = tfrac))
}
