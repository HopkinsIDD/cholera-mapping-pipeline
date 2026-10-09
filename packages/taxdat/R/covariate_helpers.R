

#' @title Get temporal bands
#' @name get_temporal_bands
#' @description Get the band numbers corresponding of a temporal covariate
#' corresponding to the modelling time window
#'
#' @param model_time_slices dataframe of model time slices (TL and TR)
#' @param covar_TL_seq sequence of left time bounds of covariate
#' @param covar_TR_seq sequence of right time bounds of covariate
#'
#' @return a list with the band indices, and the left and right time bounds
#' @export
get_temporal_bands <- function(model_time_slices, covar_TL_seq, covar_TR_seq) {
  
  # Get the covariate stack band indices that correspond to the model time slices
  ind_vec <- purrr::map(1:nrow(model_time_slices), ~which(covar_TL_seq == model_time_slices$TL[.] &
                                                            covar_TR_seq == model_time_slices$TR[.]))
  
  for (i in 1:nrow(model_time_slices)) {
    if (length(ind_vec[[i]]) == 0)
      stop("Covariate not found for modeling time slice ", i)
  }
  
  ind_vec <- unlist(ind_vec)
  
  return(list(ind = ind_vec, tl = covar_TL_seq[ind_vec], tr = covar_TR_seq[ind_vec]))
}

#' Title
#'
#' @param conn_pg
#' @param covar
#'
#' @return
#' @export
#'
get_covariate_metadata <- function(conn_pg,
                                   covar) {
  
  if (stringr::str_detect(covar, "\\.")) {
    covar <- strsplit(covar, "\\.")[[1]][2]
  }
  
  DBI::dbGetQuery(
    conn_pg,
    glue::glue_sql("SELECT src_res_time, res_time, first_TL, last_TL
          FROM covariates.metadata
          WHERE covariate = {covar}",
                   .con = conn_pg))
}

#' Get covariate values
#'
#' @param covar_name the name of the covariate in the database (with schema)
#' @param cntrd_table the table of centroids at which to extract values
#' @param time_slices the time slices
#' @param res_time the time resolution
#' @param conn_pg connection to pg database
#'
#' @return a dataframe with pixel ids and values
#' @export
#'
get_covariate_values <- function(covar_name,
                                 cntrd_table,
                                 time_slices,
                                 res_time,
                                 conn_pg) {
  
  covar <- strsplit(covar_name, "\\.")[[1]][2]
  
  # Get covariate metadata then used to select temporal bands
  covar_date_metadata <-  get_covariate_metadata(conn_pg = conn_pg,
                                                 covar = covar)
  
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
  
  query <- glue::glue_sql("
      SELECT g.rid, g.x, g.y, {`{DBI::SQL(
      paste0(
        paste0('ST_Value(rast, ', {covar_bands$ind}, ', geom) as values_', {covar_bands$ind}),
        collapse = ',')
        )}`}
      FROM {`{DBI::SQL(covar_name)}`} r, {`{DBI::SQL(cntrd_table)}`} g
      WHERE ST_Intersects(rast, geom)",
                          .con = conn_pg
  )
  
  tmp <- DBI::dbGetQuery(
    conn_pg,
    query
  )
  
  return(tmp)
}

#' Get Population fractions
#'
#' @param cntrd_table the centroids table for the whole map
#' @param intersections_table the intersetctions table
#' @param lp_table the location periods dictionary
#'
#' @return a dataframe with data
#' @export
#'
get_pop_weights <- function(res_space,
                            cntrd_table,
                            intersections_table,
                            lp_table,
                            conn_pg,
                            res_time) {
  
  # First get the 20km populations in all cells
  pop_grid <- get_covariate_values(
    covar_name = stringr::str_glue("covariates.pop_1_years_{res_space}_{res_space}"),
    cntrd_table = cntrd_table,
    time_slices = time_slices, 
    conn_pg = conn_pg,
    res_time = res_time)
  
  # Get covariate metadata then used to select temporal bands
  covar_date_metadata <- DBI::dbGetQuery(
    conn_pg,
    glue::glue_sql("SELECT src_res_time, res_time, first_TL, last_TL
          FROM covariates.metadata
          WHERE covariate = 'pop_1_years_1_1'",
                   .con = conn_pg))
  
  # Define the left and right bounds of the temporal slices covered by the covariates
  covar_TL_seq <- seq.Date(as.Date(covar_date_metadata$first_tl[1]),
                           as.Date(covar_date_metadata$last_tl[1]),
                           by = res_time)
  covar_TR_seq <- aggregate_to_end(time_change_func(covar_TL_seq))
  
  pop_1km_bands <- get_temporal_bands(model_time_slices = time_slices,
                                      covar_TL_seq = covar_TL_seq,
                                      covar_TR_seq = covar_TR_seq)
  
  query <- glue::glue_sql("
      SELECT g.location_period_id, g.rid, g.x, g.y, g.lp_covered, g.area_ratio,
      {`{DBI::SQL(
      paste0(
        paste0('sum((ST_SummaryStats(ST_Clip(rast,', {pop_1km_bands$ind}, ', geom, true), 1, true)).sum) as values_', {pop_1km_bands$ind}),
        collapse = ','))}`}
      FROM covariates.pop_1_years_1_1 r, {`{DBI::SQL(intersections_table)}`} g
      WHERE ST_Intersects(rast, geom)
      GROUP BY g.location_period_id, g.rid, g.x, g.y, g.lp_covered, g.area_ratio",
                          .con = conn_pg
  )
  
  pop_1km_intersections <- DBI::dbGetQuery(conn_pg, query)
  
  # Compute population fractions
  pop_weights <- dplyr::full_join(
    pop_1km_intersections %>%
      tidyr::pivot_longer(cols = dplyr::contains("values"),
                          values_to = "pop_1km") %>% 
      # Fill with 0s where populations are NA due to very small overlap
      dplyr::mutate(pop_1km = ifelse(is.na(pop_1km), 0, pop_1km)), 
    pop_grid %>% 
      tidyr::pivot_longer(cols = dplyr::contains("values"),
                          values_to = "pop_grid"),
    by = c("rid", "x", "y", "name")) %>%
    dplyr::group_by(location_period_id, rid, x, y, lp_covered, area_ratio, name) %>%
    dplyr::summarise(pop_weight = pop_1km/pop_grid) %>%
    # Set fractions to 1 for all pixels contained within location periods
    # which were not selected by design in the intersections
    dplyr::mutate(pop_weight = ifelse(is.na(pop_weight), 1, pop_weight),
                  # Get time slice information to join to location periods dict
                  band = stringr::str_extract(name, "[0-9]+") %>% as.numeric(),
                  t = purrr::map_dbl(band, ~ which(pop_1km_bands$ind == .))) %>% 
    dplyr::select(-name, -band) 
  
  return(pop_weights)
}

#' Make location periods dictionary
#' Dictionary between location periods and grid cells. Also compute the population
#' weighted fractions for partial coverage
#'
#' @param conn_pg 
#' @param lp_name 
#' @param intersections_table 
#' @param cntrd_table 
#' @param res_space 
#' @param sf_grid 
#' @param res_time
#'
#' @export
#'
make_location_periods_dict <- function(conn_pg,
                                       lp_name,
                                       intersections_table,
                                       cntrd_table,
                                       res_space,
                                       sf_grid,
                                       grid_changer,
                                       res_time
) {
  
  location_periods_table <- paste0(lp_name, "_dict")
  
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
    # !! Arrange by location period id to ensure consistency of ordering throughout
    #   the pipeline
    dplyr::arrange(location_period_id, t) %>%
    dplyr::mutate(loctime_id = dplyr::row_number()) %>%
    dplyr::inner_join(location_periods_dict)
  
  pop_weights <- taxdat::get_pop_weights(res_space = res_space,
                                         cntrd_table = cntrd_table,
                                         intersections_table = intersections_table,
                                         lp_table = lp_name,
                                         conn_pg = conn_pg,
                                         res_time = res_time)
  
  location_periods_dict <- location_periods_dict %>% 
    dplyr::left_join(pop_weights,
                     by = c("location_period_id","rid", "x", "y", "t")) %>% 
    dplyr::mutate(pop_weight = ifelse(is.na(pop_weight), 1, pop_weight))
  
  # Stop if anything missing
  if (any(is.na(location_periods_dict$pop_weights))) {
    u_lps_missing <- unique(location_periods_dict$location_period_id[is.na(location_periods_dict$pop_weights)]) 
    stop("Missing pop_weights for ", length(u_lps_missing), " location periods:\n",
         str_c(u_lps_missing, collaspe = " - "))
  }
  
  # Keep distinct entries
  location_periods_dict <- location_periods_dict %>%
    dplyr::as_tibble() %>% 
    dplyr::distinct()
  
  return(location_periods_dict)
}
