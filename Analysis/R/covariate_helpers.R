#' @title Connect to databse
#' @description Helper to connect to cholera_covariates database
#'
#' @param dbuser
#'
#' @return return
connectToDB <- function(dbuser) {
  #' @title Connect to database
  #' @description Connects to the postgres/postgis cholera_covariates database
  #' @return db connection object
  RPostgres::dbConnect(RPostgres::Postgres(), 
                       dbname = "cholera_covariates",
                       user = dbuser)
}

#' @title Make covariate alias
#' @description Function makes the alias of for a given covariate
#'
#' @param alias covariate alias as specified in the dictionary
#' @param type covariate type, either 'temporal' or 'static'
#' @param res_time temporal resolution
#' @param res_space spatial resolution, vector with two elements for latitude and longitude
#'
#' @return a string with the alias
makeCovarAlias <- function(alias, 
                           type,
                           res_time,
                           res_space) {
  
  if (!(type %in% c("temporal", "static"))) 
    stop("Covariate type needs to be either 'temporal' or 'static'")
  
  covar_alias <- str_c(alias, 
                       ifelse(type == "temporal", str_replace(res_time, " ", "_"),  ""),
                       res_space[1], res_space[2],
                       sep = "_")
  return(covar_alias)
}


#' @title Table exists in multiple schemas
#'
#' @description Checks whether a given table exists in multiple schemas
#'
#' @param conn connection to the databas
#' @param schemas schemas in which to look for the table
#' @param table_name name of the table to look for
#'
#' @return logical
dbExistsTableMulti <- function(conn, schemas, table_name) {
  check <- lapply(schemas, 
                  function(x, conn, table) {
                    dbExistsTable(conn, Id(schema = x, table = table))
                  }, conn = conn, table = table_name
  ) %>% 
    unlist(.)
  
  names(check) <- schemas
  return(check)
}

#' @title make location periods table name
#' @description makes the table name for the requested location periods
#'
#' @param dbuser
#'
#' @return the table name
makeLocationPeriodsTableName <- function(dbuser, map_name) {
  md5hash <- digest::digest(stringr::str_c(dbuser, "_", map_name, algo = "md5"))
  cat("-- MD5 hash for location periods table is:", md5hash, "\n")
  glue::glue("location_periods_{md5hash}")
}

#' @title make grid centroids table name
#' @description makes the table name for the centroids of the computation grid 
#' corresponding to the requested location periods
#'
#' @param dbuser
#'
#' @return the table name
makeGridCentroidsTableName <- function(dbuser, map_name) {
  md5hash <- digest::digest(stringr::str_c(dbuser, "_", map_name, algo = "md5"))
  glue::glue("grid_cntrds_{md5hash}")
}

cleanAllTmp <- function(dbuser, map_name) {
  print("-- Cleaning temporary tables")
  conn <- connectToDB(dbuser)
  lp_name <- makeLocationPeriodsTableName(dbuser, map_name)
  DBI::dbSendStatement(conn, glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(lp_name)}`};", .con = conn)) 
  cntrd_table <- makeGridCentroidsTableName(dbuser, map_name)
  DBI::dbSendStatement(conn, glue::glue_sql("DROP TABLE IF EXISTS {`{DBI::SQL(cntrd_table)}`};", .con = conn)) 
  DBI::dbDisconnect(conn)
}

makeGridFile <- function(dbuser) {
  glue::glue("grid_name_{dbuser}.txt")
}

#' Build geometry
#'
#' Builds the centroids or the polygons corresponding to a given raster and 
#' writes the result to a new table in the database
#'
#' @param conn connection to the database, object of type DBI::dbConnect
#' @param schema schema of the database where the raster is stored
#' @param table_name name of the raster to process
#' @param type type of geometry to compute, needs to be either 'centroids' or 
#' 'polygons'
#'
#' @return None
buildGeomsQuery <- function(conn, schema = "public", table_name, type = "centroids") {
  
  
  if (!(type %in% c("centroids", "polygons"))) {
    stop("Type not in {centroids, polygons}")
  }
  
  grid_full_name <- paste(schema, table_name, sep = ".")
  grid <-DBI::SQL(grid_full_name)
  suffix <- c(centroids = "centroids", polygons = "polys")
  src_file <- c(centroids = "ST_PixelAsCentroids", polygons = "ST_PixelAsPolygons")
  table_full_name <- str_c(grid_full_name, suffix[type], sep = "_")
  table <- DBI::SQL(table_full_name)
  f_sql <-  DBI::SQL(src_file[type])
  
  # Drop table
  suppressMessages(
    dbSendStatement(conn, 
                    glue::glue_sql("DROP TABLE IF EXISTS {`table`};", 
                                   .con = conn)))
  # Create polygons or centrdois table
  query2 <- glue::glue_sql("
    CREATE TABLE {`table`} AS 
    SELECT rid, x, y, geom
    FROM (
      SELECT rid, dp.* 
      FROM {`grid`}, LATERAL {`f_sql`}(rast, 1) AS dp
    ) foo;", .con = conn)
  suppressMessages(dbSendStatement(conn, query2))
  
  # Create indices
  suppressMessages(
    dbSendStatement(conn, 
                    glue::glue_sql("
                    CREATE INDEX {`{str_c(table_full_name, '_gidx')}`}
    ON {`table`} USING GIST(geom);;", 
                                   .con = conn)))
  suppressMessages(
    dbSendStatement(conn, 
                    glue::glue_sql("
                    CREATE INDEX {`{str_c(table_full_name, '_idx')}`}
    ON {`table`} (rid, x, y);;", .con = conn)))
  
  dbSendStatement(conn, glue::glue_sql("VACUUM ANALYZE {`table`};", .con = conn))
}

#' Show progress
#'
#' Shows a bar of the progess of a process based on a scalar iterator
#'
#' @param i current step
#' @param tot_length total number of steps to do
#' @param n_hyph_tot number of hyphens corresponding to the full completion
#' @param prefix string to paste at the begging of the bar
#'
#' @return None
showProgress <- function(i, tot_length, n_hyph_tot = 80, prefix = "") {
  
  rel_progress <- floor(i / tot_length * 100) 
  n_hyph <- round(rel_progress / 100 * n_hyph_tot)
  cat("\r", prefix, " (", rel_progress, "%):[", rep("-", n_hyph), ">", 
      rep(" ", n_hyph_tot - n_hyph), "]", sep = "")
}

# Function to extract the metadata from covariates 
# this could need to be tailored to each covariate
extractCovariateMetadata <- function(covar_alias, 
                                     covar_file, 
                                     covar_type) {
  
  if (covar_type == "temporal") {
    
    nc_covar <- ncdf4::nc_open(covar_file)
    
    # Time attribute
    time_att <- ncdf4::ncatt_get(nc_covar, "time")
    time_vec <- ncdf4::ncvar_get(nc_covar, "time")
    # This should corresponds to the left bound of each time period covered 
    # by the dataset
    date_vec <- as.Date(time_vec, 
                        origin = as.Date(
                          str_extract(time_att$units, 
                                      "(?<=days since )[0-9]+-[0-9]+-[0-9]+(?= )")))
    
    if (length(date_vec) > 1) {
      # Time resolution of the source in days
      res_time <- difftime(date_vec[2], date_vec[1])
    } else {
      res_time <- "unspecified"
    }
    
    # Space attribute
    space_res <- parseGdalRes(covar_file)
    res_x <- space_res[1]
    res_y <- space_res[2]
    
    # Output
    covar_metadata <- data.frame(
      covariate = covar_alias, 
      src_res_x = res_x,
      src_res_y = res_y,
      first_TL = min(date_vec),    # the first left bound of time periods
      last_TL = max(date_vec),     # the last left bound of time periods
      src_res_time = format(res_time)
    )
    
  } else if (covar_type == "static") {
    
    # Space attribute
    space_res <- parseGdalRes(covar_file)
    res_x <- space_res[1]
    res_y <- space_res[2]
    
    # Output
    covar_metadata <- data.frame(
      covariate = covar_alias, 
      src_res_x = res_x,
      src_res_y = res_y,
      first_TL = as.Date(NA),
      last_TL = as.Date(NA),
      src_res_time = "static"
    )
  } else {
    stop("Uknown covariate")
  }
  return(covar_metadata)
}

#' @title Parse time resolution
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
#' parseTimeRes("1 month")
#' # $units
#' # [1] "months"
#' # $k
#' # [1] 1
#' 
parseTimeRes <- function(x) {
  res_time <- strsplit(x, " ")[[1]]
  
  # add an 's' to the end
  if (!str_detect(res_time[2], "s$")) {
    res_time[2] <- str_c(res_time[2], "s")
  }
  return(
    list(
      units = res_time[2],
      k = as.integer(res_time[1])
    )
  )
}

#' @title Get NetCDF metadata
#'
#' @description Gets the metadata on the temporal resolution and coverage of a
#' ncdf file.
#' 
#' @details The function assumes that the temporal dimension is called 'time'
#'
#' @param src_file netcdf file to process
#'
#' @return a list(time_info, var_name, var_att, dates, date_index) with the output
#' of \code{ncdf4::nc_info} on the time dimension, the variable name, its attributes, the dates
#' covered by the file and the index they correspond to based on the time dimension
#' attributes
#' 
getNCDFMetadata <- function(src_file) {
  
  # Extract time metadata
  r_nc <- ncdf4::nc_open(src_file)
  r_time_info <- try(ncdf4::ncatt_get(r_nc, "time"), silent = T)
  
  if (!inherits(r_time_info, "try-error")) {
    if (is.null(r_time_info$units)) {
      r_time_info$units <- "static"
    }
    start_date <- as.Date(str_extract(r_time_info$units, "[0-9]{4}-[0-9]+-[0-9]+"))
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
  
  return(
    list(
      time_info = r_time_info,
      var_name = var_name,
      var_att = var_att,
      dates = r_dates,
      start_date = start_date,
      date_index = r_dates - start_date
    )
  )
}

#' @title Parse GDAL resolution
#'
#' @description Parses the output of gdalinfo to extract the pixel resolution 
#' of a raster
#'
#' @param src_file raster file to process
#'
#' @return a two-element vector with the longitude and latitude resolutions (in 
#' the raster's coordinate system)
#' 
parseGdalRes <- function(src_file) {
  
  info <- gdalinfo2(src_file)
  res_string <- info[str_detect(info, "Pixel Size")]
  res_x <- as.numeric(str_extract(res_string, "(?<=\\()[0-9]+\\.[0-9]+"))
  res_y <- as.numeric(str_extract(res_string, "[0-9]+\\.[0-9]+(?=\\))"))
  
  return(
    c(lon = res_x, lat = res_y)
  )
}

#' @title Transform raster
#'
#' @description transforms a raster with a user-specified function
#'
#' @param res_file the raster to transform
#' @param transform the transform to apply
#'
#' @return none
#' 
transformRaster <- function(res_file, 
                            transform) {
  
  cat("Transforming ", res_file, "with", transform)
  
  r <- stack(res_file)
  eval(
    parse(text = str_c("trans_fun <- function(x) {return(", transform, "(x))}"))
  )
  # transform data
  raster::values(r) <- trans_fun(raster::values(r))
  raster::writeRaster(r, 
                      res_file,
                      NAflag = -9999,
                      overwrite = T
  )
}

#' @title Sum non-NA cells
#'
#' @description Sums the number of non-NA cells in a resampled raster
#'
#' @param r_file the processed raster file 
#' @param ref_grid_db the reference grid in the database
#' @param dbuser
#' 
#' @return A raster with the number of non-NA cells
#' 
sumNonNA <- function(conn,
                     r_file, 
                     ref_grid_db,
                     dbuser) {
  
  # temporary file to which to write the result
  tmp_file1 <- str_c(tmpDir(), "resampled_zeros_1.tif")
  tmp_file <- str_c(tmpDir(), "resampled_zeros.tif") 
  src_file <- glue::glue("PG:\"host=localhost dbname=cholera_covariates schema=grids table=master_grid user={dbuser} mode=2\"")
  
  gdalwarp2(src_file, tmp_file1, srcnodata = "None", overwrite = T)
  
  src_res <- parseGdalRes(tmp_file1)[1]
  dst_res <- parseGdalRes(ref_grid_db)[1]
  
  n_pixpercell <- (dst_res/src_res)^2
  
  # Resample the raster to the reference grid
  align_rasters2(unaligned = tmp_file1,
                 reference = ref_grid_db,
                 dstfile = tmp_file,
                 dstnodata = -9999,
                 projres_only = F,
                 r = "average",
                 nThreads = 8,
                 overwrite = T,
                 verbose = F,
                 srcnodata = "None"
  )
  
  r <- raster(tmp_file)
  raster::values(r) <- round(raster::values(r) * n_pixpercell)
  return(r)
}

#' @title Write NCDF
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
#' 
writeNCDF <- function(data, 
                      res_file,
                      mv = -9999,
                      var_name, 
                      long_var_name,
                      var_unit,
                      time_units,
                      time_vals) {
  # Longitude and Latitude data
  xvals <- unique(values(init(data, "x")))
  yvals <- unique(values(init(data, "y")))
  nx <- length(xvals)
  ny <- length(yvals)
  lon <- ncdim_def("longitude", "degrees_east", xvals)
  lat <- ncdim_def("latitude", "degrees_north", yvals)
  
  # Time component
  if (!is.null(time_vals)) {
    time <- ncdim_def(name = "time", 
                      units = time_units, 
                      vals = time_vals, 
                      unlim = TRUE,
                      longname = "time")
    dim <- list(lon, lat, time) 
  } else {
    dim <- list(lon, lat) 
  }
  
  # Define the temperature variables
  var_data <- ncvar_def(name = var_name,
                        units = var_unit,
                        dim = dim,
                        longname = long_var_name,
                        missval = mv,
                        compression = 9)
  
  # Add the variables to the file
  ncout <- nc_create(res_file, list(var_data), force_v4 = TRUE)
  
  # add some global attributes
  ncatt_put(ncout, 0, "Title", long_var_name)
  ncatt_put(ncout, 0, "Created on", date())
  
  # Place the precip and tmax values in the file
  # need to loop through the layers to get them 
  # to match to correct time index
  for (i in 1:nlayers(data)) { 
    cat("Processing layer ", i, " of ", nlayers(data), "\n")
    
    if (is.null(time_vals)) {
      ncvar_put(nc = ncout, 
                varid = var_data, 
                vals = values(data), 
                start = c(1, 1), 
                count = c(-1, -1))
    } else {
      ncvar_put(nc = ncout, 
                varid = var_data, 
                vals = values(data[[i]]), 
                start = c(1, 1, i), 
                count = c(-1, -1, 1))
    }
  }
  # Close the netcdf file when finished adding variables
  nc_close(ncout)
}

#' @title Get time resolution
#' @description Get the time resolution of covariate in required time units
#'
#' @param dates Covariate dates
#' @param units Time resolution units required for the model, one of days, months, years
#'
#' @return Time resolution as numeric rounded to the first digit
getTimeRes <- function(dates, covar_res_time, units) {
  
  if (length(dates) < 2) {
    dt_days <- ifelse(str_detect(covar_res_time, "day"), 1, 
                      ifelse(str_detect(covar_res_time, "month"), 30, 365)) 
  } else {
    dt_days <- as.numeric(difftime(dates[2], dates[1], units = "days"))
  }
  
  if (str_detect(units, "month")) {
    dt <- dt_days/30
  } else if (str_detect(units, "year")) {
    dt <- dt_days/365
  } 
  return(list(dt_units = round(dt*10)/10,
              dt_days = dt_days))
}

#' @title Generate time sequence
#' @description Generates the sequence of times for the modeling covariate in case
#' the time resolution of the covariate  is coarser than the one required by the model
#'
#' @param dates Covariate dates
#' @param res_time_source Time resolution in units of required time resolution
#' @param res_time Time resolution string
#'
#' @return A vector with the sequence of dates
generateTimeSequence <- function(dates, res_time_source, res_time) {
  start_date <- dates[1]
  end_date <- dplyr::last(dates)
  
  # If years get last day of the year
  if (str_detect(res_time, "year") | (str_detect(res_time, "month") & res_time_source$dt_units > 11)) {
    lubridate::year(end_date) <- lubridate::year(end_date) + 1
    end_date <- lubridate::floor_date(end_date, unit = "years") - 1
  } 
  
  # Generate the sequence of dates
  # These should correspond to the left bounds of each time period
  seq_dates <- seq.Date(start_date, end_date, by = res_time)
  
  # Generate the mapping between source dates and generated dates
  date_mapping <- purrr::map_dbl(seq_dates, function(x) {
    if (res_time_source$dt_days > 360) {
      which(lubridate::year(dates) == lubridate::year(x))
    } else if (res_time_source$dt_days > 28){
      which(format(dates, "%Y-%m") == format(x, "%Y-%m"))
    } else if (res_time_source$dt_days > 5){
      which(format(dates, "%Y-%U") == format(x, "%Y-%U"))
    }
  })
  
  return(list(dates = seq_dates,
              date_mapping = date_mapping))
}

#' @title Aggregate time
#'
#' @description Aggregates a multi-band temporal raster at a desired temporal 
#' resolution
#'
#' @param src_file raster file to process
#' @param covar_name name of the covariate
#' @param covar_unit units of the covariate
#' @param covar_type type of covarite, one of 'temporal' or 'static'
#' @param res_file file to which to write the aggregated temporal raster
#' @param aggregator string defining the aggregator to use
#' @param aoi_extent extent of the area of interest to process
#' 
#' @details For now the aggregator string needs to be an R function that exists in 
#' the global enviroment.  
#'
#' @return a string with the path to the result
#' 
timeAggregate <- function(src_file,
                          covar_name, 
                          covar_unit, 
                          covar_type, 
                          covar_res_time,
                          res_file, 
                          res_time, 
                          aggregator, 
                          aoi_extent = NULL) {
  
  # Extract time resolution
  res_time_list <- parseTimeRes(res_time)
  f_format <- str_extract(src_file, "(?<=\\.)[a-z]+$")
  
  if (f_format == "nc") {
    # Extract time metadata
    r_metadata <- getNCDFMetadata(src_file)
  }
  
  # This should be removed at some point after testing
  if (!is.null(aoi_extent)) {
    
    cat("Croping ", src_file, " to ", 
        str_c(c("[xmin:", ", xmax:", ", ymin:", ", ymax:"), as.vector(aoi_extent)),
        "]\n", sep = "")
    
    r <- raster::crop(raster::stack(src_file), aoi_extent)
  } else {
    r <- raster::stack(src_file)
  }
  
  if (covar_type == "temporal") {
    
    if (length(r_metadata$dates) < 2)
      # stop("Covariate", covar_name, "has only one temporal layer, do not know how to process it")
      
      # Get the source's temporal resolution
      res_time_source <- getTimeRes(dates = r_metadata$dates, 
                                    covar_res_time = covar_res_time,
                                    units = res_time_list$units)
    
    # Check whether the source's temporal resolution is coarser than the required resolution
    if (res_time_source$dt_units >= res_time_list$k) {
      cat("Temporal resolution of", 
          covar_name, " (", res_time_source$dt_units , " ",  res_time_list$units, ")",
          " is coarser than required resolutionn (", res_time, 
          "). Replicating data to required resolution.\n", sep = "")
      
      # Generate the time sequence to fill
      r_covar_dates <- generateTimeSequence(dates = r_metadata$dates,
                                            res_time_source = res_time_source,
                                            res_time = res_time)
      # Unpack result
      r_dates <- r_covar_dates$dates - r_metadata$start_date
      date_mapping <- r_covar_dates$date_mapping
      
      # Replicate raster layers
      for (l in seq_along(r_dates)) {
        if (l == 1) {
          r_out <- raster::raster(src_file, band = 1)
        } else {
          r_out <- raster::stack(r_out, raster::raster(src_file, band = date_mapping[l]))
        }
      }
      
    } else {
      
      # Create rts object for temporal aggregation
      r_ts <- rts::rts(r, r_metadata$dates)
      # Endpoints for the temporal aggregation
      ep <- rts::endpoints(r_ts, on = res_time_list$units, k = res_time_list$k)
      eval(
        parse(text = str_c("agg_fun <- function(x) {return(", aggregator, "(x))}"))
      )
      
      # Aggregate
      r_ts_agg <- rts::period.apply(r_ts, ep, agg_fun)
      r_out <- r_ts_agg@raster
      r_dates <- as.Date(rts::index.RasterStackBrickTS(r_ts_agg)) - r_metadata$start_date
    }
    
    # Arguments for writeRaster
    var_unit <- str_c(r_metadata$var_att$units,
                      " aggregated at [time resolution:", 
                      res_time, 
                      "] with [aggregator:", 
                      aggregator, "]")
    long_var_name <- str_c(r_metadata$var_att$long_name, "-",
                           res_time, aggregator, sep = " ")
    
    writeNCDF(data = r_out, 
              res_file = res_file,
              mv = -9999,
              var_name = r_metadata$var_name,
              var_unit = var_unit, 
              long_var_name = long_var_name,
              time_units = r_metadata$time_info$units,
              time_vals = as.numeric(r_dates))
    
  } else {
    # For static covariates process the raw file
    res_file <- str_replace_all(res_file, str_replace(res_time, " ", "-"), "")
    if (!file.exists(res_file)) {
      
      writeNCDF(data = r, 
                res_file = res_file,
                mv = -9999,
                var_name = covar_name,
                var_unit = covar_unit, 
                long_var_name = covar_name,
                time_units = "static",
                time_vals = NULL)
    }
  }
  return(res_file)
}

#' @title Spatial aggregation
#'
#' @description Aggregate raster in space based on a reference raster
#'
#' @param src_file raster file to process
#' @param res_file file to which to write the aggregated raster should be written to
#' @param ref_grid reference grid
#' @param aggregator string defining the aggregator to use 
#' @param dbuser
#' 
#' @return None
#' 
spaceAggregate <- function(src_file, 
                           res_file, 
                           ref_grid_db, 
                           covar_type, 
                           aggregator,
                           dbuser) {
  
  # Spatial aggregators available to GDAL
  spat_aggregators <- c("average", "max", "min", "sum")
  
  if (!(aggregator %in% spat_aggregators)) {
    stop(
      str_c("Spatial aggregator '", aggregator,"' not in {",
            str_c(spat_aggregators, collapse = ", "), "}")
    )
  }
  
  if (aggregator == "sum") {
    do_sum <- T
    aggregator <- "average"
  } else {
    do_sum <- F
  }
  
  # temporary file to which to write the result
  tmp_file <- str_c(tmpDir(), "resampled.tif") 
  
  # Resample the raster to the reference grid
  align_rasters2(unaligned = src_file,
                 reference = ref_grid_db,
                 dstfile = tmp_file,
                 projres_only = F,
                 r = aggregator,
                 nThreads = 8,
                 overwrite = T,
                 verbose = F)
  
  if (do_sum) {
    # Aggregation with sums is not implemented in gdal: 
    # instead compute the average and then multiply by the number of 
    # population-lvel cells covered by each modeling grid cell
    r <- raster::stack(tmp_file)
    # Compute the number of non-na cells covered by each modeling cell
    r_nval <- sumNonNA(conn, tmp_file, ref_grid_db, dbuser)
    # multiply to obtain sums
    raster::values(r) <- raster::values(r) * raster::values(r_nval)
    # write to tmp_file which is read again below
    raster::writeRaster(r, filename = tmp_file, overwrite = T)
  }
  
  r <- raster::stack(tmp_file)
  r_metadata <- getNCDFMetadata(src_file)
  
  raster::writeRaster(r, 
                      res_file,
                      overwrite = TRUE,
                      NAflag = -9999,
                      format = "CDF",
                      varname = r_metadata$var_name,
                      varunit = r_metadata$var_att$units,  
                      longname = r_metadata$var_att$long_name,
                      xname = "longitude",
                      yname = "latitude",
                      zname = "time",
                      zunit = r_metadata$time_info$units)
  rm(r)
  gc()
  
  if (covar_type == "temporal") {
    # Add time dimension to the result
    r_nc_res <- ncdf4::nc_open(res_file, write = T)
    ncdf4::ncvar_put(r_nc_res, varid = "time", vals = r_metadata$date_index)
    ncdf4::nc_close(r_nc_res)
  }
}


#' @title Get temporal bands
#'
#' @description Get the band numbers corresponding of a temporal covariate
#' corresponding to the modelling time window
#'
#' @param model_time_slices dataframe of model time slices (TL and TR)
#' @param covar_TL_seq sequence of left time bounds of covariate
#' @param covar_TR_seq sequence of right time bounds of covariate
#'
#' @return a list with the band indices, and the left and right time bounds
#' 
getTemporalBands <- function(model_time_slices, 
                             covar_TL_seq,
                             covar_TR_seq) {
  
  # Get the covariate stack band indices that correspond to the model time slices
  ind_vec <- map(1:nrow(model_time_slices),
                 ~ which(covar_TL_seq == model_time_slices$TL[.] &
                           covar_TR_seq == model_time_slices$TR[.]))
  
  for (i in 1:nrow(model_time_slices)) {
    if (length(ind_vec[[i]]) == 0)  
      stop("Covariate not found for modeling time slice ", i)
  }
  
  ind_vec <- unlist(ind_vec)
  
  return(list(ind = ind_vec, 
              tl = covar_TL_seq[ind_vec], 
              tr = covar_TR_seq[ind_vec]))
}

#' @title Ingest covariate
#'
#' @description Processes a raster covariate at the desired temporal and spatial
#' resolution and adds it to the database.
#'
#' @param conn connection to the database, object of type DBI::dbConnect
#' @param covar_name name of the covariate
#' @param covar_alias alias of the covariate in the database
#' @param covar_dir directory where to find the covariate
#' @param covar_type type of covariate, either 'temporal' or 'static'
#' @param covar_schema schema where to save the covariate
#' @param ref_grid name of the reference grid in the database
#' @param aoi_extent extent of the area of interest to process
#' @param res_time string with the input temporal resolution
#' @param time_aggregator string with the aggregator to use along the temporal
#' dimention
#' @param res_x longitudinal spatial resolution in km
#' @param res_y latitudinal spatial resolution in km
#' @param space_aggregator string with the spatial aggregator to use
#' @param path_to_trunk path to the trunk of cholera-taxonomy
#' @param write_to_db flag to write tables to database or not
#' @param do_parellel flag to perform map computations in parallel 
#' @param n_cpus number of CPUS to run
#' @param dbuser User for database connections
#' 
#' @details The covariates are assumed to be stored in separate folders, each with 
#' potentially multiple multi-band rasters. Covariates of type 'static' will 
#' not be processed temporally
#' 
#' @return None
ingestCovariate <- function(conn, 
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
                            path_to_trunk,
                            write_to_db = F,
                            do_parallel = F,
                            n_cpus = 0,
                            dbuser) {
  
  # Checks
  if(do_parallel & write_to_db)
    stop("Cannot process in parallel and write to database at the same time, set
         either to FALSE")
  
  if(covar_type == "static" & do_parallel) {
    do_parallel <- F
    warning("Found parallel computation with static covariate, changing to do_parallel = FALSE")
  }
  
  t_start_covar <- Sys.time()    # timing covariate
  
  # What are we doing?
  action <- ifelse(write_to_db, "ingesting", "pre-computing")
  
  cat("---- ", paste(toupper(substr(action, 1, 1)), substr(action, 2, nchar(action)), sep=""),
      " ",  str_to_upper(covar_name), " at resolution [", 
      res_time, ", ", res_x, "x", res_y, "km] at ", format(t_start_covar), "\n", sep = "")
  
  if (covar_type == "temporal") {
    raster_files <- dir(covar_dir, pattern = "\\.", full.names = T)
  } else {
    if (!file.exists(covar_dir)) {
      stop("Couldn't find the file", covar_dir)
    }
    raster_files <- covar_dir
  }
  
  ref_schema <- strsplit(ref_grid, "\\.")[[1]][1]
  ref_table <- strsplit(ref_grid, "\\.")[[1]][2]
  ref_grid_db <- glue::glue("PG:\"host=localhost dbname=cholera_covariates schema={ref_schema} table={ref_table} user={dbuser} mode=2\"")
  
  covar_table <- str_c(covar_schema, covar_alias, sep = ".")
  
  # Directory to which to write files
  proc_dir <- str_c(path_to_trunk, "/Layers/processed_covariates/",
                    covar_name, "/", aoi_name, "/")
  
  if (!dir.exists(proc_dir)) {
    cat("Couldn't find", proc_dir, ", creating it \n")
    # Create directory for processed data
    dir.create(proc_dir, recursive = T)
  }
  
  # Parallel setup
  if(do_parallel) {
    if(n_cpus == 0)
      stop("Specify the number of CPUS to use")
    
    # Parallel setup
    cl <- parallel::makeCluster(n_cpus)
    doParallel::registerDoParallel(cl)
    
    parallel::clusterExport(
      cl = cl, 
      list("connectToDB", "dbuser", "getTimeRes","generateTimeSequence", "writeNCDF"),
      envir = environment())
    
    parallel::clusterEvalQ(cl, {
      library(DBI)
      library(RPostgres)
      library(ncdf4)
      conn <- connectToDB(dbuser)
      NULL
    })
  }
  
  doFun <- ifelse(do_parallel, `%dopar%`, `%do%`)
  no_export<- ifelse(do_parallel, "conn", "")
  export_funs <- c("extractCovariateMetadata", "parseGdalRes", "parseTimeRes",
                   "dbExistsTableMulti", "buildGeomsQuery", "showProgress",
                   "getNCDFMetadata", "timeAggregate", "spaceAggregate",
                   "gdalinfo2", "gdal_cmd_builder2", "gdalwarp2", "align_rasters2")
  doFun(
    foreach(j = seq_along(raster_files),
            .combine = rbind,
            .inorder = T,
            .export = export_funs,
            .noexport = no_export,
            .packages = c("dplyr", "stringr", "raster", "gdalUtils")),
    {
      t_start <- Sys.time()    # timing file
      
      full_path <- raster_files[j]    # full path to raster to process  
      f_name <- strsplit(full_path, "/")[[1]] %>% .[length(.)] # name of raster
      f_format <- str_extract(f_name, "(?<=\\.)[a-z]+$")
      
      # File of the temporal aggregation step
      res_file_time <- str_c(
        proc_dir,
        str_replace(f_name, str_c(".", f_format),
                    str_c("__", time_aggregator, "_",
                          str_replace(res_time, " ", "-"), ".nc")))
      
      # Aggregate covariate in time 
      if (!file.exists(res_file_time)) {
        
        cat("Aggregating", f_name, "in time at", res_time, "resolution \n")
        
        res_file_time <- timeAggregate(src_file = full_path,
                                       covar_name = covar_name,
                                       covar_unit = covar_unit,
                                       covar_type = covar_type,
                                       covar_res_time = covar_res_time,
                                       res_file = res_file_time,
                                       res_time = res_time,
                                       aggregator = time_aggregator,
                                       aoi_extent = aoi_extent)
      }  
      
      # File of the spatial aggregation step
      res_file_space <- str_replace(
        res_file_time, "\\.nc",
        str_c("__resampled_", res_x, "x", res_y, "km.nc")) %>% 
        { str <- .
        if (!is.null(transform)) {
          str_replace(str, "\\.nc", 
                      str_c("_", transform, "_trans.nc"))
        } else {
          str
        }
        }
      
      # Aggregate covariate in space
      if (!file.exists(res_file_space)) {
        
        cat("Aggregating", f_name, "in space at", res_x, "x", res_y, "km resolution \n")
        
        spaceAggregate(res_file_time,
                       res_file = res_file_space,
                       ref_grid_db = ref_grid_db,
                       covar_type = covar_type,
                       aggregator = space_aggregator,
                       dbuser = dbuser)
        
        if (!is.null(transform)) {
          # if specified, apply transform
          transformRaster(res_file_space, transform)
        } 
      }
      
      # Progress
      t_end <- Sys.time()
      
      if (write_to_db) {
        if (j == 1) {
          # Write to database
          r2psql_cmd <- str_c("raster2pgsql -s 4326:4326 -I -t auto -d ",
                              res_file_space, covar_table,
                              "| psql -w -h localhost -d cholera_covariates",
                              sep = " ")
          system(r2psql_cmd)
        } else {
          # Write to database
          r2psql_cmd <- str_c("raster2pgsql -s 4326:4326 -I -t auto -d ",
                              res_file_space, 
                              " tmprast | psql -w -h localhost -d cholera_covariates",
                              sep = " ")
          system(r2psql_cmd)
          n_bands <- dbGetQuery(conn, "SELECT ST_NumBands(rast) 
                            FROM tmprast LIMIT 1;") %>% unlist()
          
          for (nb in 1:n_bands) {
            dbSendStatement(conn, 
                            glue::glue_sql(
                              "UPDATE {`{DBI::SQL(covar_table)}`} a
                              SET rast = ST_AddBand(a.rast, b.rast, {nb})
                              FROM tmprast b
                              WHERE a.rid = b.rid;",
                              .con = conn))
            
            showProgress(nb, n_bands, prefix = f_name)
          }
          
          dbSendStatement(conn, "DROP TABLE IF EXISTS tmprast;")
        }
        cat("\n-- Done file ", j, "/",  length(raster_files), " (took ",
            format(difftime(t_end, t_start, units = "hours"), digits = 2),
            ")\n", sep = "")
      }
      
    })
  
  if (do_parallel) {
    parallel::clusterEvalQ(cl, {
      dbDisconnect(conn)
    })
    parallel::stopCluster(cl)
  }
  
  dbGetQuery(conn, glue::glue_sql(
    "SELECT AddRasterConstraints({covar_schema}::name, {covar_alias}::name,
      'rast'::name);", .con = conn))
  dbSendStatement(conn, glue::glue_sql("VACUUM ANALYZE {`DBI::SQL(covar_table)`};", .con = conn))
  
  t_end_covar <- Sys.time()
  
  cat("---- Done", action, str_to_upper(covar_name), "(took",
      format(difftime(t_end_covar, t_start_covar, units = "hours"), digits = 2),
      ")\n")
}

#' @title Write metadata
#'
#' @description writes the covariate metadata to the metadata table
#'
#' @param conn database connection
#'
#' @return return
writeMetadata <- function(conn, 
                          covar_dir,
                          covar_type,
                          covar_alias,
                          res_x, 
                          res_y,
                          res_time,
                          space_aggregator,
                          time_aggregator,
                          dbuser) {
  
  if (covar_type == "temporal") {
    raster_files <- dir(covar_dir, pattern = "\\.", full.names = T)
  } else {
    if (!file.exists(covar_dir)) {
      stop("Couldn't find the file", covar_dir)
    }
    raster_files <- covar_dir
  }
  
  if (length(raster_files) == 0)
    stop("Didn't find any raster files for ", covar_alias, " in ", covar_dir)
  
  # Get covariate metadata for each layer
  covar_metadata <- foreach(full_path = raster_files,
                            .combine = rbind,
                            .inorder = T) %do% 
    {
      # Extract metatadat
      extractCovariateMetadata(covar_alias, full_path, covar_type)
    } %>% 
    mutate_at(vars(contains("date")), as.Date)
  
  # Extract metadata
  covar_metadata <-  covar_metadata %>% 
    group_by(covariate) %>% 
    summarise(src_res_x = src_res_x[1], 
              src_res_y = src_res_y[1], 
              src_res_time = src_res_time[1],
              first_TL = min(first_TL),
              last_TL = max(last_TL)) %>% 
    ungroup() %>% 
    mutate(src_dir = covar_dir,
           res_x = res_x, 
           res_y = res_y,
           res_time = ifelse(covar_type == "temporal", res_time, as.character(NA)),
           space_agg = space_aggregator,
           time_agg = time_aggregator) 
  
  # Temporary table to write metadata
  tmp_name <- str_c("tmp_", dbuser)
  
  RPostgres::dbWriteTable(conn = conn, 
                          name = tmp_name, 
                          covar_metadata,
                          row.names = F,
                          apppend = F,
                          overwrite = T)
  
  try(dbSendStatement(conn, 
                      glue::glue_sql("INSERT INTO covariates.metadata 
                                     SELECT * FROM {`DBI::SQL(tmp_name)`};", 
                                     .con = conn)), 
      silent = T)
  
  dbSendStatement(conn, glue::glue_sql("DROP TABLE IF EXISTS {`DBI::SQL(tmp_name)`};", 
                                       .con = conn))
}


#' @title Time overlap
#'
#' @description Computes the time overlap between a date range and a vector of ranges
#'
#' @param tl
#' @param tr
#' @param tl_vec
#' @param tr_vec
#'
#' @return a list with the time overlaps
#' 
timeOverlap <- function(tl, tr, res_time, tl_vec, tr_vec){
  # TODO 
}

#' @title Write postgis raster
#'
#' @description Write raster from postgis table
#'
#' @param dbuser
#' @param schema
#' @param table
#' @param outfile
#' 
#' @details currently only works for 1 band rasters
#' from https://gis.stackexchange.com/questions/118138/how-to-load-postgis-raster-layers-into-r
#'
writePGRaster <- function(dbuser, schema, table, outfile, band = 1) {
  dsn <- glue::glue("PG:dbname='cholera_covariates' host=localhost",
                    " user='{dbuser}' port=5432", 
                    " schema='{schema}' table='{table}' mode=2")
  
  ras <- rgdal::readGDAL(dsn) 
  ras2 <- raster::raster(ras, band) 
  raster::writeRaster(ras2, outfile)
}

# Modifications of the gdalUtils package ---------------------------------------

# align_raster, gdalinfo and gdal_cmd_builder were modified so as to allow for unquoted dataset
# names, which enables the use of postgis layers, the rest of the codes were untouched

gdalinfo2 <- function (datasetname, json, mm, stats, approx_stats, hist, nogcp, 
                       nomd, nrat, noct, nofl, checksum, proj4, oo, mdd, sd, version, 
                       formats, format, optfile, config, debug, raw_output = TRUE, 
                       ignore.full_scan = TRUE, verbose = FALSE) 
{
  parameter_values <- as.list(environment())
  if (verbose) 
    message("Checking gdal_installation...")
  gdal_setInstallation(ignore.full_scan = ignore.full_scan, 
                       verbose = verbose)
  if (is.null(getOption("gdalUtils_gdalPath"))) 
    return()
  parameter_variables <- list(logical = list(varnames <- c("json", 
                                                           "mm", "stats", "approx_stats", "hist", "nogcp", "nomd", 
                                                           "nrat", "noct", "checksum", "nofl", "proj4", "version", 
                                                           "formats")),
                              vector = list(varnames <- c()), 
                              scalar = list(varnames <- c("sd")), 
                              character = list(varnames <- c("datasetname","mdd", 
                                                             "format", "optfile", "config", "debug", "oo")), repeatable = list(varnames <- NULL))
  parameter_order <- c("json", "mm", "stats", "approx_stats", 
                       "hist", "nogcp", "nomd", "nrat", "noct", "nofl", "checksum", 
                       "proj4", "mdd", "sd", "version", "formats", "format", 
                       "optfile", "config", "debug", "oo", "datasetname")
  parameter_noflags <- c("datasetname")
  parameter_doubledash <- c("version", "formats", "format", 
                            "optfile", "config", "debug")
  executable <- "gdalinfo"
  cmd <- gdal_cmd_builder2(executable = executable, parameter_noquotes = "datasetname", parameter_variables = parameter_variables, 
                           parameter_values = parameter_values, parameter_order = parameter_order, 
                           parameter_noflags = parameter_noflags, parameter_doubledash = parameter_doubledash,
                           verbose = verbose)
  if (verbose) 
    message(paste("GDAL command being used:", cmd))
  cmd_output <- system(cmd, intern = TRUE)
  if (verbose) {
    message(cmd_output)
  }
  if (raw_output) {
    return(cmd_output)
  }
  else {
    result <- list()
    dims <- strsplit(gsub(grep(cmd_output, pattern = "Size is ", 
                               value = TRUE), pattern = "Size is ", replacement = ""), 
                     ",")[[1]]
    result$rows <- as.numeric(dims[2])
    result$columns <- as.numeric(dims[1])
    bands <- grep(cmd_output, pattern = "Band ", value = TRUE)
    if (length(bands) == 0) 
      result$bands = 1
    else {
      result$bands <- length(bands)
    }
    orig <- as.numeric(strsplit(gsub(strsplit(grep(cmd_output, 
                                                   pattern = "Lower Left  \\(", value = TRUE), "\\) \\(")[[1]][1], 
                                     pattern = "Lower Left  \\(", replacement = ""), ",")[[1]])
    result$ll.x <- orig[1]
    result$ll.y <- orig[2]
    res <- as.numeric(strsplit(gsub(gsub(grep(cmd_output, 
                                              pattern = "Pixel Size = \\(", value = TRUE), pattern = "Pixel Size = \\(", 
                                         replacement = ""), pattern = "\\)", replacement = ""), 
                               ",")[[1]])
    result$res.x <- res[1]
    result$res.y <- res[2]
    result$file <- gsub(grep(cmd_output, pattern = "Files: ", 
                             value = TRUE), pattern = "Files: ", replacement = "")
    if (!missing(proj4)) {
      if (proj4) 
        result$proj4 <- sub("\\s+$", "", gsub("'", "", 
                                              cmd_output[grep(pattern = "PROJ.4 string is:", 
                                                              cmd_output) + 1]))
    }
    result$driver <- strsplit(gsub(grep(cmd_output, pattern = "Driver: ", 
                                        value = TRUE), pattern = "Driver: ", replacement = ""), 
                              "/")[[1]][1]
    ul <- as.numeric(strsplit(strsplit(strsplit(cmd_output[grep(pattern = glob2rx("Upper Left*"), 
                                                                cmd_output)], "\\(")[[1]][2], "\\)")[[1]][1], ",")[[1]])
    ll <- as.numeric(strsplit(strsplit(strsplit(cmd_output[grep(pattern = glob2rx("Lower Left*"), 
                                                                cmd_output)], "\\(")[[1]][2], "\\)")[[1]][1], ",")[[1]])
    ur <- as.numeric(strsplit(strsplit(strsplit(cmd_output[grep(pattern = glob2rx("Upper Right*"), 
                                                                cmd_output)], "\\(")[[1]][2], "\\)")[[1]][1], ",")[[1]])
    lr <- as.numeric(strsplit(strsplit(strsplit(cmd_output[grep(pattern = glob2rx("Lower Right*"), 
                                                                cmd_output)], "\\(")[[1]][2], "\\)")[[1]][1], ",")[[1]])
    corners_rbind <- rbind(ul, ll, ur, lr)
    result$bbox <- matrix(c(min(corners_rbind[, 1]), max(corners_rbind[, 
                                                                       1]), min(corners_rbind[, 2]), max(corners_rbind[, 
                                                                                                                       2])), nrow = 2, ncol = 2, byrow = TRUE)
    colnames(result$bbox) <- c("min", "max")
    rownames(result$bbox) <- c("s1", "s2")
    return(result)
  }
}


gdal_cmd_builder2 <- function (executable, parameter_variables = c(), parameter_values = c(), 
                               parameter_order = c(), parameter_noflags = c(), parameter_doubledash = c(), 
                               parameter_noquotes = c(), gdal_installation_id = 1, python_util = FALSE, 
                               verbose = FALSE) 
{
  if (verbose) 
    message("Checking installation...")
  gdal_setInstallation()
  if (is.null(getOption("gdalUtils_gdalPath"))) 
    return()
  executable <- normalizePath(list.files(getOption("gdalUtils_gdalPath")[[gdal_installation_id]]$path, 
                                         executable, full.names = TRUE))
  if (!file.exists(executable) && !file.exists(paste0(executable, 
                                                      ".exe"))) {
    stop(paste0(executable, " does not exist on your system.  Please check your installation."))
  }
  parameter_variables_types <- names(parameter_variables)
  defined_variables <- names(parameter_values)[sapply(parameter_values, 
                                                      function(X) class(X)[1] != "name")]
  if (verbose) 
    message("Setting up logical variables...")
  if (any("logical" %in% parameter_variables_types)) {
    parameter_variables_logical <- parameter_variables$logical[[1]]
    parameter_variables_logical_defined <- defined_variables[defined_variables %in% 
                                                               parameter_variables_logical]
    if (length(parameter_variables_logical_defined) > 0) {
      parameter_variables_logical_defined_true <- sapply(parameter_variables_logical_defined, 
                                                         function(X, parameter_values) {
                                                           return(parameter_values[[which(names(parameter_values) == 
                                                                                            X)]])
                                                         }, parameter_values = parameter_values)
      parameter_variables_logical_strings <- sapply(parameter_variables_logical_defined, 
                                                    function(X, parameter_doubledash) {
                                                      if (X %in% parameter_noflags) {
                                                        flag = NULL
                                                      }
                                                      else {
                                                        if (X %in% parameter_doubledash) {
                                                          flag = paste("--", X, " ", sep = "")
                                                        }
                                                        else {
                                                          flag = paste("-", X, " ", sep = "")
                                                        }
                                                      }
                                                      return(flag)
                                                    }, parameter_doubledash = parameter_doubledash)
      names(parameter_variables_logical_strings) <- names(parameter_variables_logical_defined_true)
      parameter_variables_logical_strings <- parameter_variables_logical_strings[parameter_variables_logical_defined_true == 
                                                                                   T]
    }
    else {
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
                                                     }
                                                     else {
                                                       if (X %in% parameter_doubledash) {
                                                         flag = paste("--", X, " ", sep = "")
                                                       }
                                                       else {
                                                         flag = paste("-", X, " ", sep = "")
                                                       }
                                                     }
                                                     if (X %in% parameter_noquotes) {
                                                       parameter_variables_vector_string <- paste(flag, 
                                                                                                  paste(parameter_values[[which(names(parameter_values) == 
                                                                                                                                  X)]], collapse = " "), sep = "")
                                                     }
                                                     else {
                                                       parameter_variables_vector_string <- paste(flag, 
                                                                                                  qm(paste(parameter_values[[which(names(parameter_values) == 
                                                                                                                                     X)]], collapse = " ")), sep = "")
                                                     }
                                                     return(parameter_variables_vector_string)
                                                   }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
    }
    else {
      parameter_variables_vector_strings <- NULL
    }
  }
  else {
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
                                                     }
                                                     else {
                                                       if (X %in% parameter_doubledash) {
                                                         flag = paste("--", X, " ", sep = "")
                                                       }
                                                       else {
                                                         flag = paste("-", X, " ", sep = "")
                                                       }
                                                     }
                                                     parameter_variables_scalar_string <- paste(flag, 
                                                                                                qm(parameter_values[[which(names(parameter_values) == 
                                                                                                                             X)]]), sep = "")
                                                     return(parameter_variables_scalar_string)
                                                   }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
    }
    else {
      parameter_variables_scalar_strings <- NULL
    }
  }
  else {
    parameter_variables_scalar_strings <- NULL
  }
  if (verbose) 
    message("Setting up character variables...")
  if (any("character" %in% parameter_variables_types)) {
    parameter_variables_character <- parameter_variables$character[[1]]
    parameter_variables_character_defined <- defined_variables[defined_variables %in% 
                                                                 parameter_variables_character]
    if (length(parameter_variables_character_defined) > 
        0) {
      parameter_variables_character_strings <- sapply(parameter_variables_character_defined, 
                                                      function(X, parameter_values, parameter_noflags, 
                                                               parameter_doubledash) {
                                                        if (X %in% parameter_noflags) {
                                                          flag = NULL
                                                        }
                                                        else {
                                                          if (X %in% parameter_doubledash) {
                                                            flag = paste("--", X, " ", sep = "")
                                                          }
                                                          else {
                                                            flag = paste("-", X, " ", sep = "")
                                                          }
                                                        }
                                                        
                                                        if (X %in% parameter_noquotes) {
                                                          parameter_variables_character_string <- paste(flag, 
                                                                                                        parameter_values[[which(names(parameter_values) == 
                                                                                                                                  X)]], sep = "")
                                                        } else {
                                                          parameter_variables_character_string <- paste(flag, 
                                                                                                        qm(parameter_values[[which(names(parameter_values) == 
                                                                                                                                     X)]]), sep = "")
                                                        }
                                                        
                                                        return(parameter_variables_character_string)
                                                      }, parameter_values = parameter_values, parameter_noflags = parameter_noflags, 
                                                      parameter_doubledash = parameter_doubledash)
    }
    else {
      parameter_variables_character_strings <- NULL
    }
  }
  else {
    parameter_variables_character_strings <- NULL
  }
  if (verbose) 
    message("Setting up repeatable variables...")
  if (any("repeatable" %in% parameter_variables_types)) {
    parameter_variables_repeatable <- parameter_variables$repeatable[[1]]
    parameter_variables_repeatable_defined <- defined_variables[defined_variables %in% 
                                                                  parameter_variables_repeatable]
    if (length(parameter_variables_repeatable_defined) > 
        0) {
      parameter_variables_repeatable_strings <- sapply(parameter_variables_repeatable_defined, 
                                                       function(X, parameter_values, parameter_doubledash) {
                                                         if (X %in% parameter_noflags) {
                                                           flag = NULL
                                                         }
                                                         else {
                                                           if (X %in% parameter_doubledash) {
                                                             flag = paste("--", X, " ", sep = "")
                                                           }
                                                           else {
                                                             flag = paste("-", X, " ", sep = "")
                                                           }
                                                         }
                                                         if (X %in% parameter_noquotes) {
                                                           parameter_variables_repeatable_string <- paste(paste(flag, 
                                                                                                                (parameter_values[[which(names(parameter_values) == 
                                                                                                                                           X)]]), sep = ""), collapse = " ")
                                                         }
                                                         else {
                                                           parameter_variables_repeatable_string <- paste(paste(flag, 
                                                                                                                qm(parameter_values[[which(names(parameter_values) == 
                                                                                                                                             X)]]), sep = ""), collapse = " ")
                                                         }
                                                         return(parameter_variables_repeatable_string)
                                                       }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
    }
    else {
      parameter_variables_repeatable_strings <- NULL
    }
  }
  else {
    parameter_variables_repeatable_strings <- NULL
  }
  if (verbose) 
    message("Setting up noflag variables...")
  if (!is.null(parameter_noflags)) {
    parameter_variables_noflag_strings <- sapply(parameter_noflags, 
                                                 function(X, parameter_values) {
                                                   parameter_variables_noflag_string <- paste(parameter_values[[which(names(parameter_values) == 
                                                                                                                        X)]], sep = "")
                                                   return(parameter_variables_noflag_string)
                                                 }, parameter_values = parameter_values)
  }
  else {
    parameter_variables_noflag_strings <- NULL
  }
  if (verbose) 
    message("Putting them all together...")
  parameter_vector <- c(parameter_variables_logical_strings, 
                        parameter_variables_vector_strings, parameter_variables_scalar_strings, 
                        parameter_variables_character_strings, parameter_variables_repeatable_strings, 
                        parameter_variables_noflag_strings)
  if (!missing(parameter_order)) {
    parameter_order_defined <- parameter_order[which(parameter_order %in% 
                                                       names(parameter_vector))]
    parameter_vector <- parameter_vector[parameter_order_defined]
  }
  parameter_vector <- sapply(parameter_vector, function(x) paste(x, 
                                                                 collapse = " "))
  cmd <- paste(c(qm(executable), parameter_vector), collapse = " ")
  return(cmd)
}

align_rasters2 <- function (unaligned, reference, dstfile, output_Raster = FALSE, 
                            nThreads = 1, projres_only = FALSE, verbose = FALSE, ...) 
{
  reference_info <- gdalinfo2(reference, proj4 = TRUE, raw_output = FALSE, 
                              verbose = verbose)
  proj4_string <- reference_info$proj4
  bbox <- reference_info$bbox
  te <- c(reference_info$bbox[1, 1], reference_info$bbox[2, 
                                                         1], reference_info$bbox[1, 2], reference_info$bbox[2, 
                                                                                                            2])
  ts <- c(reference_info$columns, reference_info$rows)
  if (missing(dstfile)) 
    dstfile <- tempfile()
  if (is.character(nThreads)) {
    if (nThreads == "ALL_CPUS") {
      multi = TRUE
      wo = "NUM_THREADS=ALL_CPUS"
    }
  }
  else {
    if (nThreads == 1) {
      multi = FALSE
      wo = NULL
    }
    else {
      multi = TRUE
      wo = paste("NUM_THREADS=", nThreads, sep = "")
    }
  }
  if (projres_only) {
    synced <- gdalwarp2(srcfile = unaligned, dstfile = dstfile, 
                        t_srs = proj4_string, output_Raster = output_Raster, 
                        multi = multi, wo = wo, verbose = verbose, ...)
  }
  else {
    synced <- gdalwarp2(srcfile = unaligned, dstfile = dstfile, 
                        te = te, t_srs = proj4_string, ts = ts, output_Raster = output_Raster, 
                        multi = multi, wo = wo, verbose = verbose, ...)
  }
  return(synced)
}

gdalwarp2 <- function (srcfile, dstfile, s_srs, t_srs, to, order, tps, rpc, 
                       geoloc, et, refine_gcps, te, te_srs, tr, tap, ts, ovr, wo, 
                       ot, wt, r, srcnodata, dstnodata, dstalpha, wm, multi, q, 
                       of = "GTiff", co, cutline, cl, cwhere, csql, cblend, crop_to_cutline, 
                       overwrite, nomd, cvmd, setci, oo, doo, output_Raster = FALSE, 
                       ignore.full_scan = TRUE, verbose = FALSE, ...) 
{
  if (output_Raster && (!requireNamespace("raster") || !requireNamespace("rgdal"))) {
    warning("rgdal and/or raster not installed. Please install.packages(c('rgdal','raster')) or set output_Raster=FALSE")
    return(NULL)
  }
  parameter_values <- as.list(environment())
  if (verbose) 
    message("Checking gdal_installation...")
  gdal_setInstallation(ignore.full_scan = ignore.full_scan, 
                       verbose = verbose)
  if (is.null(getOption("gdalUtils_gdalPath"))) 
    return()
  parameter_variables <- list(
    logical = list(varnames <- c("tps", 
                                 "rpc", "geoloc", "tap", "dstalpha", "multi", "q", "crop_to_cutline", 
                                 "overwrite", "nomd", "setci")),
    vector = list(varnames <- c("te", 
                                "tr", "ts")), 
    scalar = list(varnames <- c("order", "et", 
                                "refine_gcps", "wm", "cblend")),
    character = list(varnames <- c("s_srs", 
                                   "t_srs", "to", "te_srs", "ovr", "ot", "wt", "r", "srcnodata", 
                                   "dstnodata", "of", "cutline", "cl", "cwhere", "csql", 
                                   "cvmd", "oo", "doo", "dstfile")),
    repeatable = list(varnames <- c("wo", 
                                    "co", "srcfile")))
  parameter_order <- c("tps", "rpc", "geoloc", "tap", "dstalpha", 
                       "multi", "q", "crop_to_cutline", "overwrite", "nomd", 
                       "setci", "te", "te_srs", "tr", "ts", "ovr", "order", 
                       "et", "refine_gcps", "wm", "cblend", "s_srs", "t_srs", 
                       "to", "ot", "wt", "r", "srcnodata", "dstnodata", "of", 
                       "cutline", "cl", "cwhere", "csql", "cvmd", "wo", "co", 
                       "oo", "doo", "srcfile", "dstfile")
  
  parameter_noflags <- c("srcfile", "dstfile")
  parameter_noquotes <- c("srcfile", "dstfile", unlist(parameter_variables$vector))
  executable <- "gdalwarp"
  cmd <- gdal_cmd_builder2(executable = executable, parameter_variables = parameter_variables, 
                           parameter_values = parameter_values, parameter_order = parameter_order, 
                           parameter_noflags = parameter_noflags, parameter_noquotes = parameter_noquotes, 
                           gdal_installation_id = gdal_chooseInstallation(hasDrivers = of))
  if (verbose) 
    message(paste("GDAL command being used:", cmd))
  cmd_output <- system(cmd, intern = TRUE)
  if (output_Raster) {
    return(brick(dstfile))
  }
  else {
    return(NULL)
  }
}
