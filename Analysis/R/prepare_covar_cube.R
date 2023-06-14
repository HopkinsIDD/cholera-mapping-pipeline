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
#' @param res_time
#' @param res_space
#' @param res_time
#' @param username
#' @param covariate_transformations
#' @param sfrac_thresh
#' @param sfrac_thresh_lp
#'
#' @return a list with the outputs
prepare_covar_cube <- function(
    covar_list,
    dbuser,
    map_name,
    cholera_directory,
    full_grid_name,
    start_time,
    end_time,
    res_space,
    res_time,
    username,
    covariate_transformations,
    sfrac_thresh,
    sfrac_thresh_lp
) {


  # Extract data cube ------------------------------------------------------------
  # Database connection
  conn_pg <- taxdat::connect_to_db(dbuser)

  # Get various functions to convert between time units and dates
  time_change_func <- taxdat::time_unit_to_aggregate_function(res_time)
  aggregate_to_start <- taxdat::time_unit_to_start_function(res_time)
  aggregate_to_end <- taxdat::time_unit_to_end_function(res_time)

  # Define modeling time slices (set of time periods at which the data generating process occurs)
  time_slices <- taxdat::modeling_time_slices(start_time = start_time,
                                              end_time = end_time,
                                              res_time = res_time,
                                              time_change_func = time_change_func,
                                              aggregate_to_start = aggregate_to_start,
                                              aggregate_to_end = aggregate_to_end)

  # Start and end dates of the model
  # model_date_range <- seq.Date(start_time, end_time + 1, res_time)
  n_time_slices <- nrow(time_slices)
  n_covar <- length(covar_list)
  # Get the grid centroids corresponding to the location periods centroids table
  cntrd_table <- taxdat::make_grid_centroids_table_name(config = config)
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

    covar_date_metadata <- taxdat::get_covariate_metadata(conn_pg = conn_pg,
                                                          covar = covar_list[j])

    tmp <- taxdat::get_covariate_values(covar_name = covar_list[j],
                                        cntrd_table = cntrd_table,
                                        time_slices = time_slices,
                                        conn_pg = conn_pg)

    if (length(tmp) > 0) {
      dat <- tmp %>%
        # !! Arrange by pixel id for consistency with the dictionary
        dplyr::arrange(rid, x, y) %>%
        dplyr::select(dplyr::contains("value")) %>%
        as.matrix()

      if (covar_date_metadata$src_res_time == "static") {
        dat <- matrix(rep(dat, n_time_slices), nrow = n_grid_cells)
      }

      # Verify the dimensions of the extracted covariate
      if (!identical(dim(dat)[1:2], dim(covar_cube[, , j,drop=FALSE])[1:2])){
        stop("Dimensions of extraction for covariate ", covar_list[j], " do no match the ones of covar_cube:\n",
             "dim extraction: ", paste(dim(dat), collapse = " x "),
             "\ndim covar_cube: ", paste(dim(covar_cube[, , j]), collapse = " x "))
      }

      # Insert data in data cube
      covar_cube[, , j] <- dat
    } else {
      stop("Couldn't find data to extract from covariate ", covar_list[j])
    }

    # Set covariate names in covar_cube
    dimnames(covar_cube)[[3]] <- stringr::str_split(covar_list, "\\.") %>% purrr::map_chr(~ .[2])

    cat("---- Done ", covar_list[j], "\n")
  }

  # determine the grids cells that have full data
  non_na_gridcells <- which(apply(covar_cube, 1:2, function(x) {(x[1] >= 1) && sum(is.na(x)) == 0}))
  grid_changer <- taxdat::make_changer(x = non_na_gridcells)

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

  # Set user-specific name for location_periods table to use
  lp_name <- taxdat::make_locationperiods_table_name(config = config)

  # Add population 1km weights
  intersections_table <- taxdat::make_grid_intersections_table_name(config = config)

  location_periods_dict <- taxdat::make_location_periods_dict(conn_pg = conn_pg,
                                                              lp_name = lp_name,
                                                              intersections_table = intersections_table,
                                                              cntrd_table = cntrd_table,
                                                              res_space = res_space,
                                                              sf_grid = sf_grid,
                                                              grid_changer = grid_changer)


  # Get cell ids in output summary shapefiles
  output_cntrd_table <- taxdat::make_output_grid_centroids_table_name(config = config)
  output_cells <- DBI::dbGetQuery(conn = conn_pg,
                                  statement = glue::glue_sql(
                                    "SELECT DISTINCT rid, x, y
                                    FROM {`{DBI::SQL(output_cntrd_table)}`}",
                                    .con = conn_pg
                                  ))


  # Drop cells from sf_grid that do not intersect the output summary shapefiles (rgeoboundaries)
  sf_grid_drop <- sf_grid %>%
    dplyr::left_join(output_cells %>%
                       dplyr::mutate(include = T),
                     by = c("rid", "x", "y")) %>%
    dplyr::filter(is.na(include))

  if (nrow(sf_grid_drop) > 0) {
    cat("---- Dropping", nrow(sf_grid_drop), "spacetime cells that do not overalp with output shapefiles.\n")

    # Redefine non_na_gridcells
    non_na_gridcells <- setdiff(non_na_gridcells, sf_grid_drop$long_id)

  } else {
    cat("---- All cells within output summary shapefiles.\n")
  }

  # Drop cells from location period dict that do not intersect the output summary shapefiles (rgeoboundaries)
  location_periods_dict <- location_periods_dict %>%
    dplyr::inner_join(output_cells, by = c("rid", "x", "y"))


  # Filter out pixels with pop_weight spatial fraction below threshold
  # Get pixels with low sfrac
  low_sfrac <- location_periods_dict %>%
    dplyr::group_by(rid, x, y) %>%
    dplyr::slice_max(pop_weight) %>%
    dplyr::filter(pop_weight < sfrac_thresh_lp) %>%
    dplyr::select(rid, x, y) %>%
    dplyr::inner_join(sf_grid %>% sf::st_drop_geometry())

  if (nrow(low_sfrac) > 0) {
    cat("---- Dropping", nrow(low_sfrac), "space grid cells because the max sfrac is below",
        sfrac_thresh_lp, ". \n")

    # Re-define grid cells to remove cells with low sf_frac
    non_na_gridcells <- setdiff(non_na_gridcells, low_sfrac$long_id)

  } else {
    cat("---- No cells with sfrac below", sfrac_thresh_lp, ".\n")
  }


  # Update which gridcells to keep to enforce temporal consistency
  non_na_gridcells <- taxdat::make_temporal_grid_consistency(sf_grid = sf_grid,
                                                             non_na_gridcells = non_na_gridcells)


  grid_changer <- taxdat::make_changer(x = non_na_gridcells)

  # Drop from gridcells with low sfrac from location_periods_dict
  location_periods_dict <- location_periods_dict %>%
    dplyr::filter(!(long_id %in% low_sfrac$long_id)) %>%
    # !! Reset upd_long_id with new grid_changer after removing gird cells with low pop_weight
    dplyr::mutate(upd_long_id = grid_changer[as.character(long_id)]) %>%
    dplyr::filter(!is.na(upd_long_id))


  # Drop grid cells to location periods connections
  location_periods_dict <- location_periods_dict %>%
    dplyr::mutate(connect_id = dplyr::row_number())

  low_sfrac_connections <- location_periods_dict %>%
    dplyr::filter(pop_weight < sfrac_thresh_lp)

  cat("Dropping", nrow(low_sfrac_connections), "/", nrow(location_periods_dict),
      "connections between grid cells",
      "and location periods which have sfrac <", sfrac_thresh_lp,  "\n")

  location_periods_dict <- location_periods_dict %>%
    dplyr::filter(!(connect_id %in% low_sfrac_connections$connect_id))

  cat("**** FINISHED EXTRACTING COVARIATE CUBE OF DIMENSIONS", paste0(dim(covar_cube), collapse = "x"), "[n_pix x n_time x n_covar] \n")

  # close database
  DBI::dbDisconnect(conn_pg)

  covar_cube <- taxdat::transform_covariates(covar_cube, covariate_transformations)

  # Assemble output
  output <- list(covar_cube = covar_cube,
                 sf_grid = sf_grid,
                 non_na_gridcells = non_na_gridcells,
                 location_periods_dict = location_periods_dict)

  return(output)
}
