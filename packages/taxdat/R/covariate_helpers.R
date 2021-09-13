#' @title Connect to database
#' @name connect_to_db
#' @description Helper to connect to cholera_covariates database
#'
#' @param dbuser database user name
#'
#' @return a DBI database connection object
#' @export
connect_to_db <- function(dbuser) {
    #' @title Connect to database
    #' @description Connects to the postgres/postgis cholera_covariates database
    #' @return db connection object
    DBI::dbConnect(RPostgres::Postgres(), dbname = "cholera_covariates", user = dbuser)
}

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

#' @title make location periods table name
#' @name make_locationperiods_table_name
#' @description makes the table name for the requested location periods
#'
#' @param dbuser database username
#' @param map_name A string representing a somewhat unique name for this run
#'
#' @return the table name
#' @export
make_locationperiods_table_name <- function(dbuser, map_name) {
    md5hash <- digest::digest(stringr::str_c(dbuser, "_", map_name, algo = "md5"))
    cat("-- MD5 hash for location periods table is:", md5hash, "\n")
    glue::glue("location_periods_{md5hash}")
}

#' @title make grid centroids table name
#' @name make_grid_centroids_table_name
#' @description makes the table name for the centroids of the computation grid
#' corresponding to the requested location periods
#'
#' @param dbuser database username
#' @param map_name A string representing a somewhat unique name for this run
#'
#' @return the table name
#' @export
make_grid_centroids_table_name <- function(dbuser, map_name) {
    md5hash <- digest::digest(stringr::str_c(dbuser, "_", map_name, algo = "md5"))
    glue::glue("grid_cntrds_{md5hash}")
}

#' @title clean all tmp
#' @name clean_all_tmp
#' @description Delete all temporary tables in the database used to create this map
#' @param dbuser database username
#' @param map_name A string representing a somewhat unique name for this run
#' @export
clean_all_tmp <- function(dbuser, map_name) {
    print("-- Cleaning temporary tables")
    conn <- connect_to_db(dbuser)
    lp_name <- make_locationperiods_table_name(dbuser, map_name)
    DBI::dbSendStatement(conn, glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(lp_name)}`};", 
        .con = conn))
    DBI::dbSendStatement(conn, glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(paste0(lp_name, '_dict'))}`};", 
        .con = conn))
    cntrd_table <- make_grid_centroids_table_name(dbuser, map_name)
    DBI::dbSendStatement(conn, glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(cntrd_table)}`};", 
        .con = conn))
    DBI::dbDisconnect(conn)
}

#' @title Build geometry
#' @name build_geoms_query
#' @description Builds the centroids or the polygons corresponding to a given raster and writes the result to a new table in the database
#'
#' @param conn connection to the database, object of type DBI::dbConnect
#' @param schema schema of the database where the raster is stored
#' @param table_name name of the raster to process
#' @param type type of geometry to compute, needs to be either 'centroids' or
#' 'polygons'
#'
#' @return None
#' @export
build_geoms_query <- function(conn, schema = "public", table_name, type = "centroids") {


    if (!(type %in% c("centroids", "polygons"))) {
        stop("Type not in {centroids, polygons}")
    }

    grid_full_name <- paste(schema, table_name, sep = ".")
    grid <- DBI::SQL(grid_full_name)
    suffix <- c(centroids = "centroids", polygons = "polys")
    src_file <- c(centroids = "ST_PixelAsCentroids", polygons = "ST_PixelAsPolygons")
    table_full_name <- stringr::str_c(grid_full_name, suffix[type], sep = "_")
    table <- DBI::SQL(table_full_name)
    f_sql <- DBI::SQL(src_file[type])

    # Drop table
    suppressMessages(DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("DROP TABLE IF EXISTS {`table`};", 
        .con = conn))))
    # Create polygons or centrdois table
    query2 <- glue::glue_sql("
    CREATE TABLE {`table`} AS
    SELECT rid, x, y, geom
    FROM (
      SELECT rid, dp.*
      FROM {`grid`}, LATERAL {`f_sql`}(rast, 1) AS dp
    ) foo;", 
        .con = conn)
    suppressMessages(DBI::dbClearResult(DBI::dbSendStatement(conn, query2)))

    # Create indices
    suppressMessages(DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("
                    CREATE INDEX {`{str_c(table_full_name, '_gidx')}`}
    ON {`table`} USING GIST(geom);", 
        .con = conn))))
    suppressMessages(DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("
                    CREATE INDEX {`{str_c(table_full_name, '_idx')}`}
    ON {`table`} (rid, x, y);", 
        .con = conn))))

    DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("VACUUM ANALYZE {`table`};", 
        .con = conn)))
}


#' @title make grid file
#' @name make_grid_file
#' @description makes the table name for the grid file
#' @param dbuser database username
#' @return the table name
make_grid_file <- function(dbuser) {
    glue::glue("grid_name_{dbuser}.txt")
}


#' @title Show progress
#' @name show_progress
#' @description Shows a bar of the progess of a process based on a scalar iterator
#'
#' @param i current step
#' @param tot_length total number of steps to do
#' @param n_hyph_tot number of hyphens corresponding to the full completion
#' @param prefix string to paste at the beginning of the bar
#'
#' @return None
show_progress <- function(i, tot_length, n_hyph_tot = 80, prefix = "") {

    rel_progress <- floor(i/tot_length * 100)
    n_hyph <- round(rel_progress/100 * n_hyph_tot)
    cat("\r", prefix, " (", rel_progress, "%):[", rep("-", n_hyph), ">", rep(" ", 
        n_hyph_tot - n_hyph), "]", sep = "")
}


#' @title Extract covariate metadata
#' @name extract_covariate_metadata
#' @description Function to extract the metadata from covariates - this could need to be tailored to each covariate
#'
#' @param covar_alias the alias of the covariate to look for in the metadata table
#' @param covar_file the file of the raw covariate
#' @param covar_type the type of covariate (either 'static' or 'temporal')
#'
#' @return a dataframe with the covariates metadata
#' @export
extract_covariate_metadata <- function(covar_alias, covar_file, covar_type) {

    if (covar_type == "temporal") {

        nc_covar <- ncdf4::nc_open(covar_file)

        # Time attribute
        time_att <- ncdf4::ncatt_get(nc_covar, "time")
        time_vec <- ncdf4::ncvar_get(nc_covar, "time")
        # This should corresponds to the left bound of each time period covered by the
        # dataset
        date_vec <- as.Date(time_vec, origin = as.Date(stringr::str_extract(time_att$units, 
            "(?<=days since )[0-9]+-[0-9]+-[0-9]+(?= )")))

        if (length(date_vec) > 1) {
            # Time resolution of the source in days
            res_time <- difftime(date_vec[2], date_vec[1])
        } else {
            res_time <- "unspecified"
        }

        # Space attribute
        space_res <- parse_gdal_res(covar_file)
        res_x <- space_res[1]
        res_y <- space_res[2]

        # Output
        covar_metadata <- data.frame(covariate = covar_alias, src_res_x = res_x, 
            src_res_y = res_y, first_TL = min(date_vec), last_TL = max(date_vec), 
            src_res_time = format(res_time))

    } else if (covar_type == "static") {

        # Space attribute
        space_res <- parse_gdal_res(covar_file)
        res_x <- space_res[1]
        res_y <- space_res[2]

        # Output
        covar_metadata <- data.frame(covariate = covar_alias, src_res_x = res_x, 
            src_res_y = res_y, first_TL = as.Date(NA), last_TL = as.Date(NA), src_res_time = "static")
    } else {
        stop("Unknown covariate")
    }
    return(covar_metadata)
}

#' @title Parse time resolution
#' @name parse_time_res
#' @description Function to parse the string of the time step of the model into
#' the time unit and the spacing for the \code{rts} package
#'
#' @param x string of the time resolution
#'
#' @return a list(units, k) with the units and the number of single units
#' the temporal resolution corresponds to.
#'
#' @import stringr
#'
#' @examples
#' parseTimeRes('1 month')
#' # $units
#' # [1] 'months'
#' # $k
#' # [1] 1
#' @export
parse_time_res <- function(x) {
    res_time <- strsplit(x, " ")[[1]]

    # add an 's' to the end
    if (!stringr::str_detect(res_time[2], "s$")) {
        res_time[2] <- stringr::str_c(res_time[2], "s")
    }
    return(list(units = res_time[2], k = as.integer(res_time[1])))
}

#' @title Get NetCDF metadata
#' @name get_ncdf_metadata
#' @description Gets the metadata on the temporal resolution and coverage of a ncdf file.
#'
#' @details The function assumes that the temporal dimension is called 'time'
#'
#' @param src_file netcdf file to process
#'
#' @return a list(time_info, var_name, var_att, dates, date_index) with the output
#' of \code{ncdf4::nc_info} on the time dimension, the variable name, its attributes, the dates
#' covered by the file and the index they correspond to based on the time dimension
#' attributes
#' @export
get_ncdf_metadata <- function(src_file) {

    # Extract time metadata
    r_nc <- ncdf4::nc_open(src_file)
    r_time_info <- try(ncdf4::ncatt_get(r_nc, "time"), silent = T)

    if (!inherits(r_time_info, "try-error")) {
        if (is.null(r_time_info$units)) {
            r_time_info$units <- "static"
        }
        start_date <- as.Date(stringr::str_extract(r_time_info$units, "[0-9]{4}-[0-9]+-[0-9]+"))
        r_dates <- as.Date(ncdf4::ncvar_get(r_nc, "time"), origin = start_date)
    } else {
        r_time_info <- list(units = "static")
        r_dates <- NULL
        start_date <- NULL
    }

    var_name <- names(r_nc$var)
    var_name <- var_name[var_name != "crs"]
    var_att <- ncdf4::ncatt_get(r_nc, var_name)

    if (is.null(var_att$long_name)) {
        var_att$long_name <- var_name
    }

    ncdf4::nc_close(r_nc)

    return(list(time_info = r_time_info, var_name = var_name, var_att = var_att, 
        dates = r_dates, start_date = start_date, date_index = r_dates - start_date))
}

#' @title Parse GDAL resolution
#' @name parse_gdal_res
#' @description Parses the output of gdalinfo to extract the pixel resolution
#' of a raster
#'
#' @param src_file raster file to process
#'
#' @return a two-element vector with the longitude and latitude resolutions (in the raster's coordinate system)
#' @export
parse_gdal_res <- function(src_file) {

    info <- gdalinfo2(src_file)
    res_string <- info[stringr::str_detect(info, "Pixel Size")]
    res_x <- as.numeric(stringr::str_extract(res_string, "(?<=\\()[0-9]+\\.[0-9]+"))
    res_y <- as.numeric(stringr::str_extract(res_string, "[0-9]+\\.[0-9]+(?=\\))"))

    return(c(lon = res_x, lat = res_y))
}

#' @title Transform raster
#' @name transform_raster
#' @description transforms a raster with a user-specified function
#'
#' @param res_file the raster to transform
#' @param transform the transform to apply
#'
#' @return none (writes resulting rater to file)
#' @export
transform_raster <- function(res_file, transform) {

    cat("Transforming ", res_file, "with", transform)

    r <- raster::stack(res_file)
    eval(parse(text = stringr::str_c("trans_fun <- function(x) {return(", transform, 
        "(x))}")))
    # transform data
    raster::values(r) <- trans_fun(raster::values(r))
    raster::writeRaster(r, res_file, NAflag = -9999, overwrite = T)
}

#' @title Sum non-NA cells
#' @name sum_nonNA
#' @description Sums the number of non-NA cells in a resampled raster
#' @param conn the postgres connection to the database
#' @param r_file the processed raster file
#' @param ref_grid_db the reference grid in the database
#' @param dbuser the database user name
#'
#' @return A raster with the number of non-NA cells
#' @export
sum_nonNA <- function(conn, r_file, ref_grid_db, dbuser) {

    # temporary file to which to write the result
    tmp_file1 <- stringr::str_c(raster::tmpDir(), "resampled_zeros_1.tif")
    tmp_file <- stringr::str_c(raster::tmpDir(), "resampled_zeros.tif")
    src_file <- glue::glue("PG:\"dbname=cholera_covariates schema=grids table=master_grid user={dbuser} mode=2\"")

    gdalwarp2(src_file, tmp_file1, srcnodata = "None", overwrite = T)

    src_res <- parse_gdal_res(tmp_file1)[1]
    dst_res <- parse_gdal_res(ref_grid_db)[1]

    n_pixpercell <- (dst_res/src_res)^2

    # Resample the raster to the reference grid
    align_rasters2(unaligned = tmp_file1, reference = ref_grid_db, dstfile = tmp_file, 
        dstnodata = -9999, projres_only = F, r = "average", nThreads = 8, overwrite = T, 
        verbose = F, srcnodata = "None")

    r <- raster::raster(tmp_file)
    raster::values(r) <- round(raster::values(r) * n_pixpercell)
    return(r)
}

#' @title Write NCDF
#' @name write_ncdf
#' @description writes a netcdf file from a raster object
#'
#' @param data the raster data to write
#' @param res_file the file to which to write the result
#' @param mv missing value
#' @param var_name the name of the variable
#' @param long_var_name the long name of the variable
#' @param var_unit the unit of the variable
#' @param time_units the units of the time dimension
#' @param time_vals the values of the time dimension
#'
#' @details From https://gis.stackexchange.com/questions/58550/r-raster-package-write-netcdf-with-time-dimension
#' @return null
#' @export
write_ncdf <- function(data, res_file, mv = -9999, var_name, long_var_name, var_unit,
  time_units, time_vals, chunk_size = 1000) {
  order <- c("lon", "lat", "time")
  switch_lat <- TRUE
  switch_lon <- FALSE
  switch_time <- FALSE
                                        # Longitude and Latitude data
  xvals <- raster::unique(raster::init(data, "x"))
  yvals <- raster::unique(raster::init(data, "y"))
  nx <- length(xvals)
  ny <- length(yvals)
  # if (switch_lon) { xvals <- rev(xvals) }
  lon <- ncdf4::ncdim_def("longitude", "degrees_east", xvals)
  # if (switch_lat) { yvals <- rev(yvals) }
  lat <- ncdf4::ncdim_def("latitude", "degrees_north", yvals)
  dim <- list(lat = lat, lon = lon)
                                        # Time component
  if (!is.null(time_vals)) {
    time <- ncdf4::ncdim_def(name = "time", units = time_units, vals = time_vals,
      unlim = TRUE, longname = "time")
    dim <- list(lat = dim$lat, lon = dim$lon, time = time)
    dim = dim[order]
  }
  provided_dimensions <- sapply(dim,function(x){x$len})[c("lat", "lon", "time")]
  data_dimensions <- dim(data)
  if (any(provided_dimensions != data_dimensions)) {
    stop(glue::glue("The data's dimensions did not match the provided dimensions:\n  data_dimensions: {paste(data_dimensions, collapse = ', ')}\n  provided_dimensions {paste(provided_dimensions, collapse = ', ')}"))
  }
                                        # Define the temperature variables
  var_data <- ncdf4::ncvar_def(name = var_name, units = var_unit, dim = dim, longname = long_var_name,
    missval = mv, compression = 9)
                                        # Add the variables to the file
  ncout <- ncdf4::nc_create(res_file, list(var_data), force_v4 = TRUE)
                                        # add some global attributes
  ncdf4::ncatt_put(ncout, 0, "Title", long_var_name)
  ncdf4::ncatt_put(ncout, 0, "Created on", date())
                                        # Place the precip and tmax values in the file need to loop through the layers to
                                        # get them to match to correct time index Make me configurable
  nchunk_lon <- ceiling(ncol(data)/chunk_size)
  nchunk_lat <- ceiling(nrow(data)/chunk_size)
  chunk_starts <- list()
  chunk_ends <- list()
  mat_starts <- list()
  chunk_sizes <- list(time = 1)
  for (layer_idx in seq_len(raster::nlayers(data))) {
    chunk_starts[["time"]] <- layer_idx
    chunk_ends[["time"]] <- layer_idx
    mat_starts[["time"]] <- ifelse(switch_time, raster::nlayers(data) - layer_idx + 1, layer_idx)
    chunk_sizes[["time"]] <- 1
    for (lon_idx in seq_len(nchunk_lon)) {
      chunk_starts[["lon"]] <- (lon_idx - 1) * chunk_size + 1
      chunk_ends[["lon"]] <- min((lon_idx) * chunk_size, ncol(data))
      mat_starts[["lon"]] <- ifelse(!switch_lon, chunk_starts[["lon"]], ncol(data) - chunk_ends[["lon"]] + 1)
      chunk_sizes[["lon"]] <- chunk_ends[["lon"]] - chunk_starts[["lon"]] + 1
      for (lat_idx in seq_len(nchunk_lat)) {
        chunk_starts[["lat"]] <- (lat_idx - 1) * chunk_size + 1
        chunk_ends[["lat"]] <- min((lat_idx) * chunk_size, nrow(data))
        mat_starts[["lat"]] <- ifelse(!switch_lat, chunk_starts[["lat"]], nrow(data) - chunk_ends[["lat"]] + 1)
        chunk_sizes[["lat"]] <- chunk_ends[["lat"]] - chunk_starts[["lat"]] + 1
        cat("Processing layer ", layer_idx, " of ", raster::nlayers(data),
          "\n")
        cat("Processing lon", lon_idx, " of ", nchunk_lon, "\n")
        cat("Processing lat", lat_idx, " of ", nchunk_lat, "\n")
        dataslice <- raster::getValues(
          data[chunk_starts[["lat"]]:chunk_ends[["lat"]], chunk_starts[["lon"]]:chunk_ends[["lon"]], drop = FALSE][[layer_idx]],
          format = "matrix"
        )
        dimnames(dataslice) <- list("lat" = yvals, "lon" = xvals)
        if( switch_lon ) { dataslice <- dataslice[, rev(seq_len(ncol(dataslice))) ] }
        if( switch_lat ) { dataslice <- dataslice[rev(seq_len(nrow(dataslice))), ] }
        dataslice <- aperm(dataslice, na.omit(match(order, names(dimnames(dataslice)))))
        ncdf4::ncvar_put(
          nc = ncout,
          varid = var_data,
          vals = dataslice,
          start = unlist(mat_starts[order]),
          count = unlist(chunk_sizes[order]),
          verbose = TRUE
        )
      }
    }
  }
  ncdf4::nc_close(ncout)
  tryCatch({
    check_file <- raster::brick(res_file)
    names(check_file) <- names(data)
    if (! isTRUE(all.equal(check_file, data, check.attributes = FALSE))) {
      stop("This should not happen")
    }
    if ((nchunk_lat * nchunk_lon) == 1) {
      if (! isTRUE(all.equal(raster::values(check_file), raster::values(data), check.attributes = FALSE))) {
        stop("This should not happen")
      }
      if (! isTRUE(all.equal(
        raster::values(raster::init(check_file, "x")),
        raster::values(raster::init(data, "x")),
        check.attributes = FALSE
      ))) {
        stop("This should not happen")
      }
      if (! isTRUE(all.equal(
        raster::values(raster::init(check_file, "y")),
        raster::values(raster::init(data, "y")),
        check.attributes = FALSE
      ))) {
        stop("This should not happen")
      }

    }
  }, error = function(e){
    file.remove(check_file)
    stop(e$message)
  })
  invisible()
}

time_resolution_to_period <- function(time_resolution) {
  return(function(x){lubridate::period(time_resolution) * x})
}

#' @title Get time resolution
#' @name get_time_res
#' @description Get the time resolution of covariate in required time units
#'
#' @param dates Covariate dates
#' @param covar_res_time the time resolution of the covariate
#' @param units Time resolution units required for the model, one of days, months, years
#'
#' @return Time resolution as numeric rounded to the first digit
#' @export
get_time_res <- function(dates, covar_res_time, units, include_last_date = FALSE) {

  if (length(dates) == 2) {
    dt <- lubridate::interval(dates[1], dates[2] + include_last_date)
  } else if (length(dates) < 2) {
    dt <- lubridate::interval(dates[1], dates[1] + time_resolution_to_period(covar_res_time)(1))
  } else {
    stop(glue::glue("This function only supports a single date and a covar_res_time, or two dates, but we see ({dates})"))
  }

  return(list(
    dt_units = dt / time_resolution_to_period(units)(1),
    dt_days = as.numeric(dt, units = "days"),
    source_units = covar_res_time,
    target_units = units
  ))
}


#' @title Generate time sequence
#' @name generate_time_sequence
#' @description Generates the sequence of times for the modeling covariate in case
#' the time resolution of the covariate  is coarser than the one required by the model
#'
#' @param dates Covariate dates
#' @param res_time_source Time resolution in units of required time resolution
#' @param res_time Time resolution string
#'
#' @return A vector with the sequence of dates
#' @export
generate_time_sequence <- function(dates, res_time_source, res_time) {
  source_start_dates <- dates
  source_end_dates <- dates + time_resolution_to_period(res_time_source$source_units)(1)

    # Generate the sequence of dates These should correspond to the left bounds of
    # each time period
  target_start_dates <- seq.Date(min(source_start_dates), max(source_end_dates) - lubridate::days(1), by = res_time)
  target_end_dates <- target_start_dates + time_resolution_to_period(res_time)(1)

  source_interval <- lubridate::interval(source_start_dates, source_end_dates)
  target_interval <- lubridate::interval(target_start_dates, target_end_dates)

  overlapping_intervals <- tidyr::crossing(
    tibble::rowid_to_column(tibble::tibble(source_interval), "source_index"),
    tibble::rowid_to_column(tibble::tibble(target_interval), "target_index")
  ) %>%
    dplyr::filter(lubridate::int_overlaps(target_interval, source_interval))
  if (any(duplicated(overlapping_intervals$target_index))) {
    stop(glue::glue("There was a problem creating overlapping intervals, where some target intervals overlapped multiple source intervals. {dplyr::filter(overlapping_intervals, target_index %in% duplicated(target_index))}"))
  }
  if (nrow(overlapping_intervals) != length(target_interval)) {
    stop(glue::glue("There was a problem creating overlapping intervals, where some target intervals did not overlap any source intervals. {target_interval} are the target intervals and {source_interval} are the source intervals"))
  }

  return(list(dates = as.Date(lubridate::int_start(overlapping_intervals$target_interval)), date_mapping = overlapping_intervals$source_index))
}

#' @title Aggregate time
#' @name time_aggregate
#' @description Aggregates a multi-band temporal raster at a desired temporal
#' resolution
#'
#' @param src_file raster file to process
#' @param covar_name name of the covariate
#' @param covar_unit units of the covariate
#' @param covar_type type of covariate, one of 'temporal' or 'static'
#' @param covar_res_time string indicating time resolution of covariate
#' @param res_file file to which to write the aggregated temporal raster
#' @param res_time time resolution string
#' @param aggregator string defining the aggregator to use
#' @param aoi_extent extent of the area of interest to process
#'
#' @details For now the aggregator string needs to be an R function that exists in
#' the global enviroment.
#'
#' @return a string with the path to the result
#' @export
time_aggregate <- function(src_file, covar_name, covar_unit, covar_type, covar_res_time,
    res_file, res_time, aggregator, aoi_extent = NULL) {

    # Extract time resolution
    res_time_list <- parse_time_res(res_time)
    f_format <- stringr::str_extract(src_file, "(?<=\\.)[a-z]+$")

    if (f_format == "nc") {
        # Extract time metadata
        r_metadata <- get_ncdf_metadata(src_file)
    } else {
        if (covar_type == "temporal") {
            print(src_file)
            print(covar_name)
            print(covar_unit)
            print(covar_type)
            print(covar_res_time)
            print(res_file)
            print(res_time)
            print(aggregator)
            stop("temporal covariates must be ncdf4 format")
        }
    }

    if (!is.null(aoi_extent)) {

        cat("Croping ", src_file, " to ", stringr::str_c(c("[xmin:", ", xmax:", ", ymin:",
            ", ymax:"), as.vector(aoi_extent)), "]\n", sep = "")

        r <- raster::crop(raster::stack(src_file), aoi_extent)
    } else {
        r <- raster::stack(src_file)
    }

    if (covar_type == "temporal") {

        if (length(r_metadata$dates) < 2)
            # stop('Covariate', covar_name, 'has only one temporal layer, do not know how to
        # process it')

        # Get the source's temporal resolution
        res_time_source <- get_time_res(dates = r_metadata$dates, covar_res_time = covar_res_time,
            units = res_time_list$units)

        # Check whether the source's temporal resolution is coarser than the required
        # resolution
        if (res_time_source$dt_units > res_time_list$k) {
            cat("Temporal resolution of", covar_name, " (", res_time_source$dt_units,
                " ", res_time_list$units, ")", " is coarser than required resolutionn (",
                res_time, "). Replicating data to required resolution.\n", sep = "")

            # Generate the time sequence to fill
            r_covar_dates <- generate_time_sequence(dates = r_metadata$dates, res_time_source = res_time_source,
                res_time = res_time)
            # Unpack result
            r_dates <- r_covar_dates$dates - r_metadata$start_date
            date_mapping <- r_covar_dates$date_mapping

            # Replicate raster layers
            r_out <- raster::stack()
            for (l in seq_along(r_dates)) {
              r_out <- raster::stack(r_out, r[[date_mapping[[l]] ]])
            }

        } else {

            # Create rts object for temporal aggregation
            r_ts <- rts::rts(r, r_metadata$dates)
            # Endpoints for the temporal aggregation
            ep <- rts::endpoints(r_ts, on = res_time_list$units, k = res_time_list$k)
            eval(parse(text = stringr::str_c("agg_fun <- function(x) {return(", aggregator,
                "(x))}")))

            # Aggregate
            r_ts_agg <- rts::period.apply(r_ts, ep, agg_fun)
            r_out <- r_ts_agg@raster
            r_dates <- as.Date(rts::index.RasterStackBrickTS(r_ts_agg)) - r_metadata$start_date
        }

        # Arguments for writeRaster
        var_unit <- stringr::str_c(r_metadata$var_att$units, " aggregated at [time resolution:",
            res_time, "] with [aggregator:", aggregator, "]")
        long_var_name <- stringr::str_c(r_metadata$var_att$long_name, "-", res_time,
            aggregator, sep = " ")

      ## raster::writeRaster(r_out, res_file)
      write_ncdf(data = r_out, res_file = res_file, mv = -9999, var_name = r_metadata$var_name,
        var_unit = var_unit, long_var_name = long_var_name, time_units = r_metadata$time_info$units,
        time_vals = as.numeric(r_dates))

    } else {
        # For static covariates process the raw file
        res_file <- stringr::str_replace_all(res_file, stringr::str_replace(res_time,
            " ", "-"), "")
        if (!file.exists(res_file)) {

            write_ncdf(data = r, res_file = res_file, mv = -9999, var_name = covar_name,
                var_unit = covar_unit, long_var_name = covar_name, time_units = "static",
                time_vals = NULL)
        }
    }
    return(res_file)
}

#' @title Spatial aggregation
#' @name space_aggregate
#' @description Aggregate raster in space based on a reference raster
#'
#' @param src_file raster file to process
#' @param res_file file to which to write the aggregated raster should be written to
#' @param ref_grid_db reference grid
#' @param covar_type type of covariate, one of 'temporal' or 'static'
#' @param aggregator string defining the aggregator to use
#' @param dbuser the database user name
#'
#' @return None
#' @export
space_aggregate <- function(src_file, res_file, ref_grid_db, covar_type, aggregator, dbuser) {

    # Spatial aggregators available to GDAL
    spat_aggregators <- c("average", "max", "min", "sum")

    if (!(aggregator %in% spat_aggregators)) {
        stop(stringr::str_c("Spatial aggregator '", aggregator, "' not in {", stringr::str_c(spat_aggregators,
            collapse = ", "), "}"))
    }

    if (aggregator == "sum") {
        do_sum <- T
        aggregator <- "average"
    } else {
        do_sum <- F
    }

    # temporary file to which to write the result
    tmp_file <- stringr::str_c(raster::tmpDir(), "resampled.tif")

    # Resample the raster to the reference grid
  cat("Aligning rasters\n")
  align_rasters2(unaligned = src_file, reference = ref_grid_db, dstfile = tmp_file,
      projres_only = F, r = aggregator, nThreads = 8, overwrite = T, verbose = F)
  cat("Aligning rasters complete\n")

    if (do_sum) {
      cat("Doing sum\n")
        # Aggregation with sums is not implemented in gdal: instead compute the average
        # and then multiply by the number of population-lvel cells covered by each
        # modeling grid cell
        r <- raster::stack(tmp_file)
        # Compute the number of non-na cells covered by each modeling cell
        r_nval <- sum_nonNA(conn, tmp_file, ref_grid_db, dbuser)
        # multiply to obtain sums
      r <- r * r_nval
        # write to tmp_file which is read again below
        raster::writeRaster(r, filename = tmp_file, overwrite = T)
      cat("Doing sum complete\n")
    }

    r <- raster::stack(tmp_file)
    r_metadata <- get_ncdf_metadata(src_file)

  cat("Writing raster\n")

  r_extent <- raster::extent(raster::brick(src_file))
  r <- raster::crop(r,r_extent, snap = "out")
    raster::writeRaster(r, res_file, overwrite = TRUE, NAflag = -9999, format = "CDF",
        varname = r_metadata$var_name, varunit = r_metadata$var_att$units, longname = r_metadata$var_att$long_name,
        xname = "longitude", yname = "latitude", zname = "time", zunit = r_metadata$time_info$units)
    rm(r)
    gc()
  cat("Writing raster complete\n")

    if (covar_type == "temporal") {
      cat("Adding time\n")
        # Add time dimension to the result
        r_nc_res <- ncdf4::nc_open(res_file, write = T)
        ncdf4::ncvar_put(r_nc_res, varid = "time", vals = r_metadata$date_index)
        ncdf4::nc_close(r_nc_res)
      cat("Adding time complete\n")
    }
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



#' @title get_all_raster_files
#' @name get_all_raster_files
#' @description Get the files associated with a particular covariate
#' @param covar_dir Directory containing covariate files
#' @param covar_type Either 'temporal' for covariates with time dependence, or 'static' for covariates which don't have time dependence
get_all_raster_files <- function(covar_dir, covar_type) {
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
    return(raster_files)
}

#' @param table_name name of postgres table to write to
write_raster_to_postgres <- function(table_name, res_file_space) {
  r2psql_cmd <- glue::glue(stringr::str_c("raster2pgsql -s 4326:4326 -I -t auto -d ", res_file_space, "{table_name} | psql -d cholera_covariates", sep = " "))
  err <- system(r2psql_cmd)
  if (err != 0) {
    stop(paste("System command", r2psql_cmd, "failed"))
  }
}

#' @title get_processed_files_dir
#' @description Get (and create if missing) a directory for processed files for covariates
#' @param path_to_cholera_covariates A path to the github repo cholera-covariates root directory
#' @param covar_name The name of the covariate in question
#' @param aoi_name The area of interest for the covariate
get_processed_files_dir <- function(path_to_cholera_covariates = "Layers", covar_name, aoi_name) {
  ## Directory to which to write files
  proc_dir <- stringr::str_c(path_to_cholera_covariates, "/processed_covariates/", covar_name, "/", aoi_name, "/")

  if (!dir.exists(proc_dir)) {
    cat("Couldn't find", proc_dir, ", creating it \n")
    ## Create directory for processed data
    dir.create(proc_dir, recursive = T)
  }
  return(proc_dir)
}

ingest_single_covariate_file <- function(
  processed_files_directory,
  covariate_file,
  aoi_extent,
  covar_name,
  covar_unit,
  covar_type,
  covar_res_time,
  ref_grid_db,
  res_x,
  res_y,
  res_time,
  time_aggregator,
  space_aggregator,
  dbuser,
  covar_table,
  transform,
  is_first_covariate,
  conn,
  write_to_db = FALSE
) {

  t_start <- Sys.time()  # timing file

  f_name <- basename(covariate_file) # File name without path
  f_format <- stringr::str_sub(raster::extension(f_name),2) ## File extension without "."

                                        # Aggregate covariate in time
  ## File of the temporal aggregation step
  res_file_time <- stringr::str_c(
    processed_files_directory,
    stringr::str_replace(
      f_name,
      stringr::str_c(".", f_format),
      stringr::str_c("__", time_aggregator, "_", stringr::str_replace(res_time, " ", "-"), ".nc")
    )
  )

  if (!file.exists(res_file_time)) {
    cat("Aggregating", f_name, "in time at", res_time, "resolution \n")
    res_file_time <- time_aggregate(
      src_file = covariate_file,
      covar_name = covar_name,
      covar_unit = covar_unit,
      covar_type = covar_type,
      covar_res_time = covar_res_time,
      res_file = res_file_time,
      res_time = res_time,
      aggregator = time_aggregator,
      aoi_extent = aoi_extent
    )
  }

                                        # Aggregate covariate in space
  ## File of the spatial aggregation step
  transform_string <- ifelse(is.null(transform), stringr::str_c("_", transform, "_trans"), "")
  res_file_space <- stringr::str_replace(
    res_file_time,
    raster::extension(res_file_time),
    stringr::str_c("__resampled_",res_x, "x", res_y, "km", transform_string, ".nc")
  )

  if (!file.exists(res_file_space)) {

    cat("Aggregating", f_name, "in space at", res_x, "x", res_y, "km resolution \n")

    space_aggregate(
      src_file = res_file_time,
      res_file = res_file_space,
      ref_grid_db = ref_grid_db,
      covar_type = covar_type,
      aggregator = space_aggregator,
      dbuser = dbuser
    )

    if (!is.null(transform)) {
                                        # if specified, apply transform
      transform_raster(res_file_space, transform)
    }
  }

                                        # Progress
  t_end <- Sys.time()

  if (write_to_db) {
                                        # Write to database
    if (is_first_covariate) {
      write_raster_to_postgres(covar_table, res_file_space)
    } else {
      write_raster_to_postgres("tmprast", res_file_space)
      n_bands <- DBI::dbGetQuery(conn, "SELECT ST_NumBands(rast) FROM tmprast LIMIT 1;") %>%
        unlist()

      for (nb in 1:n_bands) {
        DBI::dbClearResult(DBI::dbSendStatement(conn, glue::glue_sql("UPDATE {`{DBI::SQL(covar_table)}`} a
                              SET rast = ST_AddBand(a.rast, b.rast, {nb})
                              FROM tmprast b
                              WHERE a.rid = b.rid;",
          .con = conn)))

        show_progress(nb, n_bands, prefix = f_name)
      }

      DBI::dbClearResult(DBI::dbSendStatement(conn, "DROP TABLE IF EXISTS tmprast;"))
    }
    cat("\n-- Done file ", covariate_file, " (took ", format(difftime(t_end,
      t_start, units = "hours"), digits = 2), ")\n", sep = "")
  }
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
ingest_covariate <- function(
  conn,
  covar_name,
  covar_alias,
  covar_dir,
  covar_unit,
  covar_type,
  covar_res_time = NULL,
  covar_schema,
  ref_grid,
  aoi_extent = NULL,
  aoi_name = "raw",
  res_time,
  time_aggregator = "sum",
  res_x,
  res_y,
  space_aggregator = "mean",
  transform = NULL,
  path_to_cholera_covariates,
  write_to_db = F,
  do_parallel = F,
  n_cpus = 0,
  dbuser
) {

    if (covar_type == "static" & do_parallel) {
        do_parallel <- FALSE
        warning("Found parallel computation with static covariate, changing to do_parallel = FALSE")
    }

    # Checks
    if (do_parallel & write_to_db)
        stop("Cannot process in parallel and write to database at the same time, set
         either to FALSE")

    t_start_covar <- Sys.time()  # timing covariate

    # What are we doing?
    action <- ifelse(write_to_db, "ingesting", "pre-computing")

    cat("---- ", paste(toupper(substr(action, 1, 1)), substr(action, 2, nchar(action)),
        sep = ""), " ", stringr::str_to_upper(covar_name), " at resolution [", res_time,
        ", ", res_x, "x", res_y, "km] at ", format(t_start_covar), "\n", sep = "")

    raster_files <- get_all_raster_files(covar_dir, covar_type)

    ref_schema <- strsplit(ref_grid, "\\.")[[1]][1]
    ref_table <- strsplit(ref_grid, "\\.")[[1]][2]
    ref_grid_db <- glue::glue("PG:\"dbname=cholera_covariates schema={ref_schema} table={ref_table} user={dbuser} mode=2\"")

    covar_table <- stringr::str_c(covar_schema, covar_alias, sep = ".")

    proc_dir <- get_processed_files_dir(path_to_cholera_covariates, covar_name, aoi_name)

    # Parallel setup
    if (do_parallel) {
        if (n_cpus == 0) {
          stop("Specify the number of CPUS to use")
        }

        if (n_cpus == 1) {
          do_parallel = FALSE
        }
    }

  if (do_parallel) {
    ## Parallel setup
    cl <- parallel::makeCluster(n_cpus)
    doParallel::registerDoParallel(cl)

    parallel::clusterExport(cl = cl, list("connectToDB", "dbuser", "getTimeRes",
      "generateTimeSequence", "writeNCDF"), envir = environment())

    parallel::clusterEvalQ(cl, {
      conn <- connect_to_db(dbuser)
      NULL
    })
    print(paste("Running in parallel over", n_cpus, "cores"))

    no_export = "conn"
    export_funs <- c("extractCovariateMetadata", "parse_gdal_res", "parseTimeRes",
      "dbExistsTableMulti", "buildGeomsQuery", "showProgress", "getNCDFMetadata",
      "timeAggregate", "spaceAggregate", "gdalinfo2", "gdal_cmd_builder2", "gdalwarp2",
      "align_rasters2", "ingest_single_covariate_file")
    foreach::`%dopar%`(
      foreach::foreach(
        j = seq_along(raster_files),
        .combine = rbind,
        .inorder = T,
        .export = export_funs,
        .noexport = no_export,
        .packages = c("dplyr", "stringr", "raster", "gdalUtils")
      ),
      {
        ingest_single_covariate_file(
          processed_files_directory = proc_dir,
          covariate_file = raster_files[[j]],
          aoi_extent = aoi_extent,
          covar_name = covar_name,
          covar_unit = covar_unit,
          covar_type = covar_type,
          covar_res_time = covar_res_time,
          ref_grid_db = ref_grid_db,
          res_x = res_x,
          res_y = res_y,
          res_time = res_time,
          time_aggregator = time_aggregator,
          space_aggregator = space_aggregator,
          dbuser = dbuser,
          covar_table = covar_table,
          transform = transform,
          is_first_covariate = (j == 1),
          write_to_db = write_to_db,
          conn = conn
        )
      })

    parallel::clusterEvalQ(cl, {
      DBI::dbDisconnect(conn)
    })
    parallel::stopCluster(cl)
  } else {
    print("Running in Serial")
    is_first_covariate <- TRUE
    for (raster_file in raster_files) {
      ingest_single_covariate_file(
        processed_files_directory = proc_dir,
        covariate_file = raster_file,
        aoi_extent = aoi_extent,
        covar_name = covar_name,
        covar_unit = covar_unit,
        covar_type = covar_type,
        covar_res_time = covar_res_time,
        ref_grid_db = ref_grid_db,
        res_x = res_x,
        res_y = res_y,
        res_time = res_time,
        time_aggregator = time_aggregator,
        space_aggregator = space_aggregator,
        dbuser = dbuser,
        covar_table = covar_table,
        transform = transform,
        is_first_covariate = is_first_covariate,
        write_to_db = write_to_db,
        conn = conn
      )
      is_first_covariate <- FALSE
    }
  }

  DBI::dbClearResult(DBI::dbSendStatement(
    conn,
    glue::glue_sql("SELECT AddRasterConstraints({covar_schema}::name, {covar_alias}::name, 'rast'::name);",
      .con = conn
    )
  ))
  DBI::dbClearResult(DBI::dbSendStatement(
    conn,
    glue::glue_sql("VACUUM ANALYZE {`DBI::SQL(covar_table)`};", .con = conn)
  ))

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


#' @title Write postgis raster
#' @name write_pg_raster
#' @description Write raster from postgis table
#'
#' @param dbuser user name
#' @param schema schema read the raster from
#' @param table raster table name
#' @param outfile file to which to write the raster
#'
#' @details currently only works for 1 band rasters
#' from https://gis.stackexchange.com/questions/118138/how-to-load-postgis-raster-layers-into-r
#'
#' @export
write_pg_raster <- function(dbuser, schema, table, outfile, band = 1) {
    dsn <- glue::glue("PG:dbname='cholera_covariates'", " user='{dbuser}' port=5432",
        " schema='{schema}' table='{table}' mode=2")

    ras <- rgdal::readGDAL(dsn)
    ras2 <- raster::raster(ras, band)
    raster::writeRaster(ras2, outfile)
}

# Modifications of the gdalUtils package ---------------------------------------

# align_raster, gdalinfo and gdal_cmd_builder were modified so as to allow for
# unquoted dataset names, which enables the use of postgis layers, the rest of
# the codes were untouched

#' @title gdalinfo2
#' @name gdalinfo2
#' @description Modified version of gdalUtils::gdalinfo to allow for unquoted dataset names, which enables the use of postgis layers, the rest of the codes were untouched
#' @export
gdalinfo2 <- function(datasetname, json, mm, stats, approx_stats, hist, nogcp, nomd,
    nrat, noct, nofl, checksum, proj4, oo, mdd, sd, version, formats, format, optfile,
    config, debug, raw_output = TRUE, ignore.full_scan = TRUE, verbose = FALSE) {
    parameter_values <- as.list(environment())
    if (verbose)
        message("Checking gdal_installation...")
    gdalUtils::gdal_setInstallation(ignore.full_scan = ignore.full_scan, verbose = verbose)
    if (is.null(getOption("gdalUtils_gdalPath")))
        return()
    parameter_variables <- list(logical = list(varnames <- c("json", "mm", "stats",
        "approx_stats", "hist", "nogcp", "nomd", "nrat", "noct", "checksum", "nofl",
        "proj4", "version", "formats")), vector = list(varnames <- c()), scalar = list(varnames <- c("sd")),
        character = list(varnames <- c("datasetname", "mdd", "format", "optfile",
            "config", "debug", "oo")), repeatable = list(varnames <- NULL))
    parameter_order <- c("json", "mm", "stats", "approx_stats", "hist", "nogcp",
        "nomd", "nrat", "noct", "nofl", "checksum", "proj4", "mdd", "sd", "version",
        "formats", "format", "optfile", "config", "debug", "oo", "datasetname")
    parameter_noflags <- c("datasetname")
    parameter_doubledash <- c("version", "formats", "format", "optfile", "config",
        "debug")
    executable <- "gdalinfo"
    cmd <- gdal_cmd_builder2(executable = executable, parameter_noquotes = "datasetname",
        parameter_variables = parameter_variables, parameter_values = parameter_values,
        parameter_order = parameter_order, parameter_noflags = parameter_noflags,
        parameter_doubledash = parameter_doubledash, verbose = verbose)
    if (verbose)
        message(paste("GDAL command being used:", cmd))
    cmd_output <- system(cmd, intern = TRUE)
    if (verbose) {
        message(cmd_output)
    }
    if (raw_output) {
        return(cmd_output)
    } else {
        result <- list()
        dims <- strsplit(gsub(grep(cmd_output, pattern = "Size is ", value = TRUE),
            pattern = "Size is ", replacement = ""), ",")[[1]]
        result$rows <- as.numeric(dims[2])
        result$columns <- as.numeric(dims[1])
        bands <- grep(cmd_output, pattern = "Band ", value = TRUE)
        if (length(bands) == 0)
            result$bands = 1 else {
            result$bands <- length(bands)
        }
        orig <- as.numeric(strsplit(gsub(strsplit(grep(cmd_output, pattern = "Lower Left  \\(",
            value = TRUE), "\\) \\(")[[1]][1], pattern = "Lower Left  \\(", replacement = ""),
            ",")[[1]])
        result$ll.x <- orig[1]
        result$ll.y <- orig[2]
        res <- as.numeric(strsplit(gsub(gsub(grep(cmd_output, pattern = "Pixel Size = \\(",
            value = TRUE), pattern = "Pixel Size = \\(", replacement = ""), pattern = "\\)",
            replacement = ""), ",")[[1]])
        result$res.x <- res[1]
        result$res.y <- res[2]
        result$file <- gsub(grep(cmd_output, pattern = "Files: ", value = TRUE),
            pattern = "Files: ", replacement = "")
        if (!missing(proj4)) {
            if (proj4)
                result$proj4 <- sub("\\s+$", "", gsub("'", "", cmd_output[grep(pattern = "PROJ.4 string is:",
                  cmd_output) + 1]))
        }
        result$driver <- strsplit(gsub(grep(cmd_output, pattern = "Driver: ", value = TRUE),
            pattern = "Driver: ", replacement = ""), "/")[[1]][1]
        ul <- as.numeric(strsplit(strsplit(strsplit(cmd_output[grep(pattern = utils::glob2rx("Upper Left*"),
            cmd_output)], "\\(")[[1]][2], "\\)")[[1]][1], ",")[[1]])
        ll <- as.numeric(strsplit(strsplit(strsplit(cmd_output[grep(pattern = utils::glob2rx("Lower Left*"),
            cmd_output)], "\\(")[[1]][2], "\\)")[[1]][1], ",")[[1]])
        ur <- as.numeric(strsplit(strsplit(strsplit(cmd_output[grep(pattern = utils::glob2rx("Upper Right*"),
            cmd_output)], "\\(")[[1]][2], "\\)")[[1]][1], ",")[[1]])
        lr <- as.numeric(strsplit(strsplit(strsplit(cmd_output[grep(pattern = utils::glob2rx("Lower Right*"),
            cmd_output)], "\\(")[[1]][2], "\\)")[[1]][1], ",")[[1]])
        corners_rbind <- rbind(ul, ll, ur, lr)
        result$bbox <- matrix(c(min(corners_rbind[, 1]), max(corners_rbind[, 1]),
            min(corners_rbind[, 2]), max(corners_rbind[, 2])), nrow = 2, ncol = 2,
            byrow = TRUE)
        colnames(result$bbox) <- c("min", "max")
        rownames(result$bbox) <- c("s1", "s2")
        return(result)
    }
}


#' @title gdal_cmd_builder2
#' @name gdal_cmd_builder2
#' @description Modified version of gdalUtils::gdal_cmd)builder to allow for unquoted dataset names, which enables the use of postgis layers, the rest of the codes were untouched
gdal_cmd_builder2 <- function(executable, parameter_variables = c(), parameter_values = c(),
    parameter_order = c(), parameter_noflags = c(), parameter_doubledash = c(), parameter_noquotes = c(),
    gdal_installation_id = 1, python_util = FALSE, verbose = FALSE) {
    if (verbose)
        message("Checking installation...")
    gdalUtils::gdal_setInstallation()
    if (is.null(getOption("gdalUtils_gdalPath")))
        return()
    executable <- normalizePath(list.files(getOption("gdalUtils_gdalPath")[[gdal_installation_id]]$path,
        executable, full.names = TRUE))
    if (!file.exists(executable) && !file.exists(paste0(executable, ".exe"))) {
        stop(paste0(executable, " does not exist on your system.  Please check your installation."))
    }
    parameter_variables_types <- names(parameter_variables)
    defined_variables <- names(parameter_values)[sapply(parameter_values, function(X) class(X)[1] !=
        "name")]
    if (verbose)
        message("Setting up logical variables...")
    if (any("logical" %in% parameter_variables_types)) {
        parameter_variables_logical <- parameter_variables$logical[[1]]
        parameter_variables_logical_defined <- defined_variables[defined_variables %in%
            parameter_variables_logical]
        if (length(parameter_variables_logical_defined) > 0) {
            parameter_variables_logical_defined_true <- sapply(parameter_variables_logical_defined,
                function(X, parameter_values) {
                  return(parameter_values[[which(names(parameter_values) == X)]])
                }, parameter_values = parameter_values)
            parameter_variables_logical_strings <- sapply(parameter_variables_logical_defined,
                function(X, parameter_doubledash) {
                  if (X %in% parameter_noflags) {
                    flag = NULL
                  } else {
                    if (X %in% parameter_doubledash) {
                      flag = paste("--", X, " ", sep = "")
                    } else {
                      flag = paste("-", X, " ", sep = "")
                    }
                  }
                  return(flag)
                }, parameter_doubledash = parameter_doubledash)
            names(parameter_variables_logical_strings) <- names(parameter_variables_logical_defined_true)
            parameter_variables_logical_strings <- parameter_variables_logical_strings[parameter_variables_logical_defined_true ==
                T]
        } else {
            parameter_variables_logical_strings <- NULL
        }
    }
    if (verbose)
        message("Setting up vector variables...")
    if (any("vector" %in% parameter_variables_types)) {
        parameter_variables_vector <- parameter_variables$vector[[1]]
        parameter_variables_vector_defined <- defined_variables[defined_variables %in%
            parameter_variables_vector]
        if (length(parameter_variables_vector_defined) > 0) {
            parameter_variables_vector_strings <- sapply(parameter_variables_vector_defined,
                function(X, parameter_values, parameter_doubledash) {
                  if (X %in% parameter_noflags) {
                    flag = NULL
                  } else {
                    if (X %in% parameter_doubledash) {
                      flag = paste("--", X, " ", sep = "")
                    } else {
                      flag = paste("-", X, " ", sep = "")
                    }
                  }
                  if (X %in% parameter_noquotes) {
                    parameter_variables_vector_string <- paste(flag, paste(parameter_values[[which(names(parameter_values) ==
                      X)]], collapse = " "), sep = "")
                  } else {
                    parameter_variables_vector_string <- paste(flag, gdalUtils::qm(paste(parameter_values[[which(names(parameter_values) ==
                      X)]], collapse = " ")), sep = "")
                  }
                  return(parameter_variables_vector_string)
                }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
        } else {
            parameter_variables_vector_strings <- NULL
        }
    } else {
        parameter_variables_vector_strings <- NULL
    }
    if (verbose)
        message("Setting up scalar variables...")
    if (any("scalar" %in% parameter_variables_types)) {
        parameter_variables_scalar <- parameter_variables$scalar[[1]]
        parameter_variables_scalar_defined <- defined_variables[defined_variables %in%
            parameter_variables_scalar]
        if (length(parameter_variables_scalar_defined) > 0) {
            parameter_variables_scalar_strings <- sapply(parameter_variables_scalar_defined,
                function(X, parameter_values, parameter_doubledash) {
                  if (X %in% parameter_noflags) {
                    flag = NULL
                  } else {
                    if (X %in% parameter_doubledash) {
                      flag = paste("--", X, " ", sep = "")
                    } else {
                      flag = paste("-", X, " ", sep = "")
                    }
                  }
                  parameter_variables_scalar_string <- paste(flag, gdalUtils::qm(parameter_values[[which(names(parameter_values) ==
                    X)]]), sep = "")
                  return(parameter_variables_scalar_string)
                }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
        } else {
            parameter_variables_scalar_strings <- NULL
        }
    } else {
        parameter_variables_scalar_strings <- NULL
    }
    if (verbose)
        message("Setting up character variables...")
    if (any("character" %in% parameter_variables_types)) {
        parameter_variables_character <- parameter_variables$character[[1]]
        parameter_variables_character_defined <- defined_variables[defined_variables %in%
            parameter_variables_character]
        if (length(parameter_variables_character_defined) > 0) {
            parameter_variables_character_strings <- sapply(parameter_variables_character_defined,
                function(X, parameter_values, parameter_noflags, parameter_doubledash) {
                  if (X %in% parameter_noflags) {
                    flag = NULL
                  } else {
                    if (X %in% parameter_doubledash) {
                      flag = paste("--", X, " ", sep = "")
                    } else {
                      flag = paste("-", X, " ", sep = "")
                    }
                  }

                  if (X %in% parameter_noquotes) {
                    parameter_variables_character_string <- paste(flag, parameter_values[[which(names(parameter_values) ==
                      X)]], sep = "")
                  } else {
                    parameter_variables_character_string <- paste(flag, gdalUtils::qm(parameter_values[[which(names(parameter_values) ==
                      X)]]), sep = "")
                  }

                  return(parameter_variables_character_string)
                }, parameter_values = parameter_values, parameter_noflags = parameter_noflags,
                parameter_doubledash = parameter_doubledash)
        } else {
            parameter_variables_character_strings <- NULL
        }
    } else {
        parameter_variables_character_strings <- NULL
    }
    if (verbose)
        message("Setting up repeatable variables...")
    if (any("repeatable" %in% parameter_variables_types)) {
        parameter_variables_repeatable <- parameter_variables$repeatable[[1]]
        parameter_variables_repeatable_defined <- defined_variables[defined_variables %in%
            parameter_variables_repeatable]
        if (length(parameter_variables_repeatable_defined) > 0) {
            parameter_variables_repeatable_strings <- sapply(parameter_variables_repeatable_defined,
                function(X, parameter_values, parameter_doubledash) {
                  if (X %in% parameter_noflags) {
                    flag = NULL
                  } else {
                    if (X %in% parameter_doubledash) {
                      flag = paste("--", X, " ", sep = "")
                    } else {
                      flag = paste("-", X, " ", sep = "")
                    }
                  }
                  if (X %in% parameter_noquotes) {
                    parameter_variables_repeatable_string <- paste(paste(flag, (parameter_values[[which(names(parameter_values) ==
                      X)]]), sep = ""), collapse = " ")
                  } else {
                    parameter_variables_repeatable_string <- paste(paste(flag, gdalUtils::qm(parameter_values[[which(names(parameter_values) ==
                      X)]]), sep = ""), collapse = " ")
                  }
                  return(parameter_variables_repeatable_string)
                }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
        } else {
            parameter_variables_repeatable_strings <- NULL
        }
    } else {
        parameter_variables_repeatable_strings <- NULL
    }
    if (verbose)
        message("Setting up noflag variables...")
    if (!is.null(parameter_noflags)) {
        parameter_variables_noflag_strings <- sapply(parameter_noflags, function(X,
            parameter_values) {
            parameter_variables_noflag_string <- paste(parameter_values[[which(names(parameter_values) ==
                X)]], sep = "")
            return(parameter_variables_noflag_string)
        }, parameter_values = parameter_values)
    } else {
        parameter_variables_noflag_strings <- NULL
    }
    if (verbose)
        message("Putting them all together...")
    parameter_vector <- c(parameter_variables_logical_strings, parameter_variables_vector_strings,
        parameter_variables_scalar_strings, parameter_variables_character_strings,
        parameter_variables_repeatable_strings, parameter_variables_noflag_strings)
    if (!missing(parameter_order)) {
        parameter_order_defined <- parameter_order[which(parameter_order %in% names(parameter_vector))]
        parameter_vector <- parameter_vector[parameter_order_defined]
    }
    parameter_vector <- sapply(parameter_vector, function(x) paste(x, collapse = " "))
    cmd <- paste(c(gdalUtils::qm(executable), parameter_vector), collapse = " ")
    return(cmd)
}


#' @title align_rasters2
#' @name align_rasters2
#' @description Modified version of gdalUtils::align_rasters to allow for unquoted dataset names, which enables the use of postgis layers, the rest of the codes were untouched
align_rasters2 <- function(unaligned, reference, dstfile, output_raster = FALSE,
    nThreads = 1, projres_only = FALSE, verbose = FALSE, ...) {
    reference_info <- gdalinfo2(reference, proj4 = TRUE, raw_output = FALSE, verbose = verbose)
    proj4_string <- reference_info$proj4
    bbox <- reference_info$bbox
    te <- c(reference_info$bbox[1, 1], reference_info$bbox[2, 1], reference_info$bbox[1,
        2], reference_info$bbox[2, 2])
    ts <- c(reference_info$columns, reference_info$rows)
    if (missing(dstfile))
        dstfile <- tempfile()
    if (is.character(nThreads)) {
        if (nThreads == "ALL_CPUS") {
            multi = TRUE
            wo = "NUM_THREADS=ALL_CPUS"
        }
    } else {
        if (nThreads == 1) {
            multi = FALSE
            wo = NULL
        } else {
            multi = TRUE
            wo = paste("NUM_THREADS=", nThreads, sep = "")
        }
    }
    if (projres_only) {
        synced <- gdalwarp2(srcfile = unaligned, dstfile = dstfile, t_srs = proj4_string,
            output_raster = output_raster, multi = multi, wo = wo, verbose = verbose,
            ...)
    } else {
        synced <- gdalwarp2(srcfile = unaligned, dstfile = dstfile, te = te, t_srs = proj4_string,
            ts = ts, output_raster = output_raster, multi = multi, wo = wo, verbose = verbose,
            ...)
    }
    return(synced)
}


#' @title gdalwarp2
#' @name gdalwarp2
#' @description Modified version of gdalUtils::gdalwarp to allow for unquoted dataset names, which enables the use of postgis layers, the rest of the codes were untouched
#' @export
gdalwarp2 <- function(srcfile, dstfile, s_srs, t_srs, to, order, tps, rpc, geoloc,
    et, refine_gcps, te, te_srs, tr, tap, ts, ovr, wo, ot, wt, r, srcnodata, dstnodata,
    dstalpha, wm, multi, q, of = "GTiff", co, cutline, cl, cwhere, csql, cblend,
    crop_to_cutline, overwrite, nomd, cvmd, setci, oo, doo, output_raster = FALSE,
    ignore.full_scan = TRUE, verbose = FALSE, ...) {
    if (output_raster && (!requireNamespace("raster") || !requireNamespace("rgdal"))) {
        warning("rgdal and/or raster not installed. Please install.packages(c('rgdal','raster')) or set output_raster=FALSE")
        return(NULL)
    }
    parameter_values <- as.list(environment())
    if (verbose)
        message("Checking gdal_installation...")
    gdalUtils::gdal_setInstallation(ignore.full_scan = ignore.full_scan, verbose = verbose)
    if (is.null(getOption("gdalUtils_gdalPath")))
        return()
    parameter_variables <- list(logical = list(varnames <- c("tps", "rpc", "geoloc",
        "tap", "dstalpha", "multi", "q", "crop_to_cutline", "overwrite", "nomd",
        "setci")), vector = list(varnames <- c("te", "tr", "ts")), scalar = list(varnames <- c("order",
        "et", "refine_gcps", "wm", "cblend")), character = list(varnames <- c("s_srs",
        "t_srs", "to", "te_srs", "ovr", "ot", "wt", "r", "srcnodata", "dstnodata",
        "of", "cutline", "cl", "cwhere", "csql", "cvmd", "oo", "doo", "dstfile")),
        repeatable = list(varnames <- c("wo", "co", "srcfile")))
    parameter_order <- c("tps", "rpc", "geoloc", "tap", "dstalpha", "multi", "q",
        "crop_to_cutline", "overwrite", "nomd", "setci", "te", "te_srs", "tr", "ts",
        "ovr", "order", "et", "refine_gcps", "wm", "cblend", "s_srs", "t_srs", "to",
        "ot", "wt", "r", "srcnodata", "dstnodata", "of", "cutline", "cl", "cwhere",
        "csql", "cvmd", "wo", "co", "oo", "doo", "srcfile", "dstfile")

    parameter_noflags <- c("srcfile", "dstfile")
    parameter_noquotes <- c("srcfile", "dstfile", unlist(parameter_variables$vector))
    executable <- "gdalwarp"
    cmd <- gdal_cmd_builder2(executable = executable, parameter_variables = parameter_variables,
        parameter_values = parameter_values, parameter_order = parameter_order, parameter_noflags = parameter_noflags,
        parameter_noquotes = parameter_noquotes, gdal_installation_id = gdalUtils::gdal_chooseInstallation(hasDrivers = of))
    if (verbose)
        message(paste("GDAL command being used:", cmd))
    cmd_output <- system(cmd, intern = TRUE)
    if (output_raster) {
        return(raster::brick(dstfile))
    } else {
        return(NULL)
    }
}


#' @title Time overlap
#' @name time_overlap
#' @description Computes the time overlap between a date range and a vector of ranges
#'
#' @param tl time left, starting time
#' @param tr time right, ending time
#' @param tl_vec vector of time lefts
#' @param tr_vec vector of time rights
#'
#' @return a list with the time overlaps
#'
time_overlap <- function(tl, tr, res_time, tl_vec, tr_vec) {
    # TODO
    warning("This function is not written")
}
