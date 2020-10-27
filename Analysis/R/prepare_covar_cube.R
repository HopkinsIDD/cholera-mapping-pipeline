# Preamble ---------------------------------------------------------------------

#' @title Prepare covariate cube
#' @description Extracts the covariates for each model gridcell
#'
#' @param covar_list the list of covariate names to extract from (in the cholera_covariates database)
#' @param dbuser
#' @param cholera_directory
#' @param full_grid_name
#' @param start_time
#' @param end_time
#' @param temporal_aggregate_time_unit
#' @param res_space
#' @param res_time
#'
#' @return a list with the outputs
prepare_covar_cube <- function(
  covar_list,
  dbuser,
  cholera_directory,
  full_grid_name,
  start_time,
  end_time,
  temporal_aggregate_time_unit,
  res_space,
  res_time
) {
  
  
  # Extract data cube ------------------------------------------------------------
  # Database connection
  conn_pg <- taxdat::connect_to_db(dbuser)
  
  # Get various functions to convert between time units and dates
  time_change_func <- taxdat::time_unit_to_aggregate_function(temporal_aggregate_time_unit)
  aggregate_to_start <- taxdat::time_unit_to_start_function(temporal_aggregate_time_unit)
  aggregate_to_end <- taxdat::time_unit_to_end_function(temporal_aggregate_time_unit)
  
  # Define modeling time slices (set of time periods at which the data generating process occurs)
  time_slices <- taxdat::modeling_time_slices(start_time = start_time, 
                                    end_time = end_time, 
                                    res_time = res_time,
                                    time_change_func = time_change_func,
                                    aggregate_to_start = aggregate_to_start,
                                    aggregate_to_end = aggregate_to_end)
  
  # Start and end dates of the model
  # model_date_range <- seq.Date(start_time, end_time + 1, temporal_aggregate_time_unit)
  n_time_slices <- nrow(time_slices)
  n_covar <- length(covar_list)
  # Get the grid centroids corresponding to the location periods centroids table
  cntrd_table <- taxdat::make_grid_centroids_table_name(dbuser)
  n_grid_cells <- DBI::dbGetQuery(
    conn_pg, 
    glue::glue_sql("SELECT COUNT(*) FROM {`{DBI::SQL(cntrd_table)}`};", .con = conn_pg)
  ) %>% 
    .[["count"]] %>% 
    as.numeric()
  
  # Initialize the data cube
  covar_cube <- array(
    rep(0, times = n_time_slices * n_grid_cells * n_covar),
    c(n_grid_cells, n_time_slices, n_covar)
  )
  
  for (j in seq_along(covar_list)) {
    # covariate name
    covar <- strsplit(covar_list[j], "\\.")[[1]][2]
    
    covar_date_metadata <- DBI::dbGetQuery(
      conn_pg,
      glue::glue_sql("SELECT src_res_time, res_time, first_TL, last_TL
          FROM covariates.metadata
          WHERE covariate = {covar}",
                     .con = conn_pg)) 
    
    if (nrow(covar_date_metadata) == 0) {
      stop("Couldn't find covariate ", covar, "in metadata table")
    }
    
    if (covar_date_metadata$src_res_time != "static") {
      # Define the left and right bounds of the temporal slices covered by the covariates
      covar_TL_seq <- seq.Date(as.Date(covar_date_metadata$first_tl[1]),
                               as.Date(covar_date_metadata$last_tl[1]),
                               by = res_time)
      covar_TR_seq <- aggregate_to_end(time_change_func(covar_TL_seq))
      
      cat("---- Extracting ", covar, "\n")
      covar_bands <- taxdat::get_temporal_bands(model_time_slices = time_slices, 
                                      covar_TL_seq = covar_TL_seq,
                                      covar_TR_seq = covar_TR_seq)
      
      if (length(covar_bands$ind) != nrow(time_slices))
        stop("Failed to match modeling and covariate time slices for covariate: ",
             covar, ". N model time slices: ", nrow(time_slices),
             "; N covar time slices: ", length(covar_bands$ind))
    } else {
      covar_bands <- list(ind = 1)
    }
    
    tmp <- DBI::dbGetQuery(
      conn_pg,
      glue::glue_sql("
      SELECT g.rid, g.x, g.y, {`{DBI::SQL(
      paste0(
        paste0('ST_Value(rast, ', {covar_bands$ind}, ', geom) as values_', {covar_bands$ind}),
        collapse = ',')
        )}`}
      FROM {`{DBI::SQL(covar_list[j])}`} r, {`{DBI::SQL(cntrd_table)}`} g
      WHERE ST_Intersects(rast, geom)",
                     .con = conn_pg)
    )
    
    if (length(tmp) > 0) {
      dat <- tmp %>%
        # !! Arrange by pixel id for consistency with the dictionary
        dplyr::arrange(rid, x, y) %>% 
        dplyr::select(dplyr::contains("value")) %>%
        as.matrix()
      
      if (covar_date_metadata$src_res_time == "static") {
        if ((n_time_slices - 1) > 1) {
          dat <- cbind(dat, matrix(rep(dat, n_time_slices - 1), nrow = n_grid_cells))
        } 
      }
      
      # Verify the dimensions of the extracted covariate
      if (!identical(dim(dat), dim(covar_cube[, , j])))
        stop("Dimensions of extraction for covariate ", covar, " do no match the ones of covar_cube:\n",
             "dim extraction: ", paste(dim(dat), collapse = " x "), 
             "\ndim covar_cube: ", paste(dim(covar_cube[, , j]), collapse = " x "))
      
      # Insert data in data cube
      covar_cube[, , j] <- dat
    } else {
      stop("Couldn't find data to extract from covariate ", covar)
    }
    
    # Set covariate names in covar_cube
    dimnames(covar_cube)[[3]] <- stringr::str_split(covar_list, "\\.") %>% purrr::map_chr(~ .[2])
    
    cat("---- Done ", covar, "\n")
  } 
  
  # determine the grids cells that have full data
  non_na_gridcells <- which(apply(covar_cube, 1:2, function(x) {(x[1] >= 1) && sum(is.na(x)) == 0}))
  # non_na_gridcells <- which(as.numeric(apply(covar_cube, c(1,2), function(x) {(x[1] >= 1) && sum(is.na(x)) == 0})>0))
  grid_changer <- setNames(seq_len(length(non_na_gridcells)), non_na_gridcells)
  
  # Get the modelling grid
  sf_grid <- sf::st_read(conn_pg, 
                     query = glue::glue_sql(
                       "SELECT p.* FROM
                   {`{DBI::SQL(paste0(full_grid_name, '_polys'))}`} p 
                   INNER JOIN {`{DBI::SQL(cntrd_table)}`} c
                   ON p.rid = c.rid AND p.x = c.x AND p.y = c.y", .con = conn_pg)) %>% 
    dplyr::arrange(rid, x, y) %>% 
    # Create a unique cell id
    dplyr::mutate(id = dplyr::row_number())
  
  # Create a dataframe of grids for each time slice
  sf_grid <- do.call(rbind, 
                     lapply(seq_len(ncol(covar_cube)), 
                            function(t) {
                              sf_grid$t = t
                              sf_grid
                            }
                     )) %>% 
    dplyr::mutate(long_id = dplyr::row_number())  # this is the overall cell id (from 1 to n_space x n_times)
  
  # Table of correspondence between location periods and grid cells
  username <- Sys.getenv("USER")
  location_periods_table <- paste0("location_periods_", res_space, "_", res_space, "_dict_", username)
  
  # Get the dictionary of location periods to pixel ids
  location_periods_dict <- DBI::dbReadTable(conn_pg, location_periods_table)
  
  # Join the location periods dictionary with pixel ids
  location_periods_dict <- dplyr::inner_join(location_periods_dict, 
                                             as.data.frame(sf_grid) %>% 
                                               dplyr::select(-geom)) %>% 
    # arrange(location_period_id, id, t) %>% 
    dplyr::mutate(upd_long_id = grid_changer[as.character(long_id)]) %>% 
    dplyr::filter(!is.na(upd_long_id))
  
  # Create a unique location period id which also accounts for the modeling time slice
  location_periods_dict <- location_periods_dict %>% 
    dplyr::distinct(location_period_id, t) %>% 
    dplyr::mutate(loctime_id = dplyr::row_number()) %>% 
    dplyr::inner_join(location_periods_dict)
  
  cat("**** FINISHED EXTRACTING COVARITE CUBE OF DIMENSINONS", paste0(dim(covar_cube), collapse = "x"), "[n_pix x n_time x n_covar] \n")
  
  # close database
  DBI::dbDisconnect(conn_pg)
  
  # Assemble output
  output <- list(covar_cube = covar_cube,
                 sf_grid = sf_grid,
                 non_na_gridcells = non_na_gridcells, 
                 location_periods_dict = location_periods_dict)
  
  return(output)
}
