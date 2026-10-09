#' @title Make covariate alias
#' @name make_covar_alias
#' @description Function makes the alias of for a given covariate
#'
#' @param alias covariate alias as specified in the dictionary
#' @param type covariate type, either 'temporal' or 'static'
#' @param res_time temporal resolution
#' @param res_space spatial resolution, vector with two elements for latitude and longitude
#'
#' @return a string with the alias
#' @export
make_covar_alias <- function(alias, type, res_time, res_space) {
  
  if (!(type %in% c("temporal", "static")))
    stop("Covariate type needs to be either 'temporal' or 'static'")
  
  covar_alias <- stringr::str_c(alias, ifelse(type == "temporal", stringr::str_replace(res_time,
                                                                                       " ", "_"), ""), res_space[1], res_space[2], sep = "_")
  return(covar_alias)
}

#' @title Table exists in multiple schemas
#' @name db_exists_table_multi
#' @description Checks whether a given table exists in multiple schemas
#'
#' @param conn connection to the databas
#' @param schemas schemas in which to look for the table
#' @param table_name name of the table to look for
#'
#' @return logical
#' @export
db_exists_table_multi <- function(conn, schemas, table_name) {
  check <- lapply(schemas, function(x, conn, table) {
    DBI::dbExistsTable(conn, DBI::Id(schema = x, table = table))
  }, conn = conn, table = table_name) %>%
    unlist(.)
  
  names(check) <- schemas
  return(check)
}

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

#' @title Ingest covariate
#' @name ingest_covariate
#' @description Processes a raster covariate at the desired temporal and spatial
#' resolution and adds it to the database.
#'
#' @param conn connection to the database, object of type DBI::dbConnect
#' @param covar_name name of the covariate
#' @param covar_alias alias of the covariate in the database
#' @param covar_dir directory where to find the covariate
#' @param covar_unit not sure
#' @param covar_type type of covariate, either 'temporal' or 'static'
#' @param covar_res_time default is NULL
#' @param covar_schema schema where to save the covariate
#' @param ref_grid name of the reference grid in the database
#' @param aoi_extent extent of the area of interest to process, default is NULL
#' @param res_time string with the input temporal resolution
#' @param time_aggregator string with the aggregator to use along the temporal
#' dimention
#' @param res_x longitudinal spatial resolution in km
#' @param res_y latitudinal spatial resolution in km
#' @param space_aggregator string with the spatial aggregator to use
#' @param transform default is NULL
#' @param path_to_cholera_covariates path to the trunk of cholera-covariates repository
#' @param write_to_db flag to write tables to database or not
#' @param do_parallel flag to perform map computations in parallel
#' @param n_cpus number of CPUS to run
#' @param dbuser User for database connections
#'
#' @details The covariates are assumed to be stored in separate folders, each with
#' potentially multiple multi-band rasters. Covariates of type 'static' will
#' not be processed temporally
#'
#' @return None
#' @export
ingest_covariate <- function(conn, covar_name, covar_alias, covar_dir, covar_unit,
                             covar_type, covar_res_time = NULL, covar_schema, ref_grid, aoi_extent = NULL,
                             aoi_name = "raw", res_time, time_aggregator = "sum", res_x, res_y, space_aggregator = "mean",
                             transform = NULL, path_to_cholera_covariates, write_to_db = F, do_parallel = F,
                             n_cpus = 0, dbuser) {
  
  # Checks
  if (do_parallel & write_to_db)
    stop("Cannot process in parallel and write to database at the same time, set
         either to FALSE")
  
  if (covar_type == "static" & do_parallel) {
    do_parallel <- F
    warning("Found parallel computation with static covariate, changing to do_parallel = FALSE")
  }
  
  t_start_covar <- Sys.time()  # timing covariate
  
  # What are we doing?
  action <- ifelse(write_to_db, "ingesting", "pre-computing")
  
  cat("---- ", paste(toupper(substr(action, 1, 1)), substr(action, 2, nchar(action)),
                     sep = ""), " ", stringr::str_to_upper(covar_name), " at resolution [", res_time,
      ", ", res_x, "x", res_y, "km] at ", format(t_start_covar), "\n", sep = "")
  
  if (covar_type == "temporal") {
    raster_files <- dir(covar_dir, pattern = "\\.", full.names = T)
    # Keep only raster files
    raster_files <- stringr::str_subset(raster_files, "nc|tif")
    if (length(raster_files) == 0) {
      stop("No raster files found in", covar_dir)
    }
  } else {
    if (!file.exists(covar_dir)) {
      stop("Couldn't find the file", covar_dir)
    }
    raster_files <- covar_dir
  }
  
  ref_schema <- strsplit(ref_grid, "\\.")[[1]][1]
  ref_table <- strsplit(ref_grid, "\\.")[[1]][2]
  cholera_password <- Sys.getenv("COVARIATE_DATABASE_PASSWORD", "")
  ref_grid_db <- glue::glue("PG:\"host=localhost dbname=cholera_covariates schema={ref_schema} table={ref_table} user={dbuser} password={cholera_password} mode=2\"")
  
  covar_table <- stringr::str_c(covar_schema, covar_alias, sep = ".")
  
  # Directory to which to write files
  proc_dir <- stringr::str_c(path_to_cholera_covariates, "/processed_covariates/",
                             covar_name, "/", aoi_name, "/")
  
  if (!dir.exists(proc_dir)) {
    cat("Couldn't find", proc_dir, ", creating it \n")
    # Create directory for processed data
    dir.create(proc_dir, recursive = T)
  }
  
  # Parallel setup
  if (do_parallel) {
    if (n_cpus == 0)
      stop("Specify the number of CPUS to use")
    
    # Parallel setup
    cl <- parallel::makeCluster(n_cpus)
    doParallel::registerDoParallel(cl)
    
    parallel::clusterExport(cl = cl, list("connect_to_db", "dbuser", "get_time_res",
                                          "generate_time_sequence", "write_ncdf"), envir = environment())
    
    parallel::clusterEvalQ(cl, {
      conn <- connect_to_db(dbuser)
      NULL
    })
  }
  
  if (do_parallel) {
    print(paste("Running in parallel over", n_cpus, "cores"))
  } else {
    print("Running in Serial")
  }
  doFun <- ifelse(do_parallel, foreach::`%dopar%`, foreach::`%do%`)
  no_export <- ifelse(do_parallel, "conn", "")
  export_funs <- c("extract_covariate_metadata", "parse_gdal_res", "parse_time_res",
                   "db_exists_table_multi", "build_geoms_query", "show_progress", "get_ncdf_metadata",
                   "time_aggregate", "space_aggregate", "gdalinfo2", "gdal_cmd_builder2", "gdalwarp2",
                   "align_rasters2")
  doFun(foreach::foreach(j = seq_along(raster_files),
                         .combine = rbind,
                         .inorder = T,
                         .export = export_funs,
                         .noexport = no_export,
                         .packages = c("dplyr", "stringr",
                                       "raster", "gdalUtils")),
        {
          t_start <- Sys.time()  # timing file
          
          full_path <- raster_files[j]  # full path to raster to process
          f_name <- strsplit(full_path, "/")[[1]] %>%
            .[length(.)]  # name of raster
          f_format <- stringr::str_extract(f_name, "(?<=\\.)[a-z]+$")
          
          # File of the temporal aggregation step
          res_file_time <- stringr::str_c(proc_dir, stringr::str_replace(f_name, stringr::str_c(".",
                                                                                                f_format), stringr::str_c("__", time_aggregator, "_", stringr::str_replace(res_time,
                                                                                                                                                                           " ", "-"), ".nc")))
          
          # Aggregate covariate in time
          if (!file.exists(res_file_time)) {
            cat("Aggregating", f_name, "in time at", res_time, "resolution \n")
            res_file_time <- time_aggregate(src_file = full_path, covar_name = covar_name,
                                            covar_unit = covar_unit, covar_type = covar_type, covar_res_time = covar_res_time,
                                            res_file = res_file_time, res_time = res_time, aggregator = time_aggregator,
                                            aoi_extent = aoi_extent)
          }
          
          # File of the spatial aggregation step
          res_file_space <- stringr::str_replace(res_file_time, "\\.nc", stringr::str_c("__resampled_",
                                                                                        res_x, "x", res_y, "km.nc")) %>%
            {
              str <- .
              if (!is.null(transform)) {
                stringr::str_replace(str, "\\.nc", stringr::str_c("_", transform,
                                                                  "_trans.nc"))
              } else {
                str
              }
            }
          
          # Aggregate covariate in space
          if (!file.exists(res_file_space)) {
            
            cat("Aggregating", f_name, "in space at", res_x, "x", res_y, "km resolution \n")
            
            space_aggregate(res_file_time, res_file = res_file_space, ref_grid_db = ref_grid_db,
                            covar_type = covar_type, aggregator = space_aggregator, dbuser = dbuser)
            
            if (!is.null(transform)) {
              # if specified, apply transform
              transform_raster(res_file_space, transform)
            }
          }
          
          # Progress
          t_end <- Sys.time()
          
          if (write_to_db) {
            dbuser <- Sys.getenv("USER")
            conn_string <- get_covariate_conn_string(dbuser)
            if (j == 1) {
              # Write to database
              r2psql_cmd <- stringr::str_c("raster2pgsql -s 4326:4326 -I -t auto -d ",
                                           res_file_space, covar_table, "| psql", conn_string, sep = " ")
              err <- system(r2psql_cmd)
              if (err != 0) {
                stop(paste("System command", r2psql_cmd, "failed"))
              }
            } else {
              # Write to database
              r2psql_cmd <- stringr::str_c("raster2pgsql -s 4326:4326 -I -t auto -d ",
                                           res_file_space, "tmprast | psql", conn_string, sep = " ")
              cat(paste0("Runing command: ", r2psql_cmd, "\n"))
              err <- system(r2psql_cmd)
              if (err != 0) {
                stop(paste("System command", r2psql_cmd, "failed"))
              }else{
                cat(paste0("Finish command: ", r2psql_cmd, "\n"))
              }
              n_bands <- DBI::dbGetQuery(conn, "SELECT ST_NumBands(rast)
                            FROM tmprast LIMIT 1;") %>%
                unlist()
              
              DBI::dbSendStatement(conn, "DROP TABLE IF EXISTS tmprast2;")
              
              # Update tmprast to have centroid of tiles
              DBI::dbSendStatement(conn,
                                   "
                                   CREATE TABLE tmprast2 AS (
                                   SELECT rid, rast, ST_Centroid(ST_Envelope(rast)) as centroid
                                   FROM tmprast);
                                   ")
              
              DBI::dbSendStatement(conn, "DROP TABLE IF EXISTS tmprast;")
              
              for (nb in 1:n_bands) {
                DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("
                UPDATE {`{DBI::SQL(covar_table)}`} a
                              SET rast = ST_AddBand(a.rast, b.rast, {nb})
                              FROM tmprast2 b
                              WHERE ST_Intersects(b.centroid, a.rast);",
                                                                             .con = conn)))
                
                show_progress(nb, n_bands, prefix = f_name)
              }
              
              DBI::dbClearResult(DBI::dbSendStatement(conn, "DROP TABLE IF EXISTS tmprast2;"))
            }
            cat("\n-- Done file ", j, "/", length(raster_files), " (took ", format(difftime(t_end,
                                                                                            t_start, units = "hours"), digits = 2), ")\n", sep = "")
          }
          
        })

  
  if (do_parallel) {
    parallel::clusterEvalQ(cl, {
      DBI::dbDisconnect(conn)
    })
    parallel::stopCluster(cl)
  }
  
  DBI::dbGetQuery(conn, glue::glue_sql("SELECT AddRasterConstraints({covar_schema}::name, {covar_alias}::name,
      'rast'::name);",
                                       .con = conn))
  DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("VACUUM ANALYZE {`DBI::SQL(covar_table)`};",
                                                               .con = conn)))
  
  t_end_covar <- Sys.time()
  
  cat("---- Done", action, stringr::str_to_upper(covar_name), "(took", format(difftime(t_end_covar,
                                                                                       t_start_covar, units = "hours"), digits = 2), ")\n")
}

#' @title Write metadata
#' @name write_metadata
#' @description writes the covariate metadata to the metadata table
#'
#' @param conn database connection
#' @param covar_dir directory where to find the covariate
#' @param covar_alias alias of the covariate in the database
#' @param res_x longitudinal spatial resolution in km
#' @param res_y latitudinal spatial resolution in km
#' @param res_time string with the input temporal resolution
#' @param space_aggregator string with the spatial aggregator to use
#' @param time_aggregator string with the aggregator to use along the temporal
#' dimention
#' @param dbuser user name
#' @return return
#' @export
write_metadata <- function(conn, covar_dir, covar_type, covar_alias, res_x, res_y,
                           res_time, space_aggregator, time_aggregator, dbuser) {
  
  if (covar_type == "temporal") {
    raster_files <- dir(covar_dir, pattern = "\\.", full.names = T)
    # Keep only raster files
    raster_files <- stringr::str_subset(raster_files, "nc|tif")
    if (length(raster_files) == 0) {
      stop("No raster files found in", covar_dir)
    }
  } else {
    if (!file.exists(covar_dir)) {
      stop("Couldn't find the file", covar_dir)
    }
    raster_files <- covar_dir
  }
  
  if (length(raster_files) == 0)
    stop("Didn't find any raster files for ", covar_alias, " in ", covar_dir)
  
  # Get covariate metadata for each layer
  covar_metadata <- foreach::`%do%`(foreach::foreach(full_path = raster_files,
                                                     .combine = rbind, .inorder = T), {
                                                       # Extract metadata
                                                       extract_covariate_metadata(covar_alias, full_path, covar_type)
                                                     }) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("date")), as.Date)
  
  # Extract metadata
  covar_metadata <- covar_metadata %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(src_res_x = src_res_x[1], src_res_y = src_res_y[1], src_res_time = src_res_time[1],
                     first_TL = min(first_TL), last_TL = max(last_TL)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(src_dir = covar_dir, res_x = res_x, res_y = res_y, res_time = ifelse(covar_type ==
                                                                                         "temporal", res_time, as.character(NA)), space_agg = space_aggregator,
                  time_agg = time_aggregator)
  
  # Temporary table to write metadata
  tmp_name <- stringr::str_c("tmp_", dbuser)
  
  DBI::dbWriteTable(conn = conn, name = tmp_name, covar_metadata, row.names = F,
                    apppend = F, overwrite = T)
  
  try(DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("INSERT INTO covariates.metadata
                                     SELECT * FROM {`DBI::SQL(tmp_name)`};",
                                                                   .con = conn))), silent = T)
  
  DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("DROP TABLE IF EXISTS {`DBI::SQL(tmp_name)`};",
                                                               .con = conn)))
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
