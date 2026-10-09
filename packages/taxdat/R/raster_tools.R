# Raster processing for covariate ingestion, on terra and the GDAL command-line
# tools. Replaces the former raster / rts / gdalUtils / rgdal code.
#
# File conventions (unchanged from the previous pipeline):
#   * covariate NetCDF files have dimensions longitude / latitude / time, a time
#     axis in "days since <origin>", missing value -9999, and one data variable
#     (terra also adds a `crs` variable, which get_ncdf_metadata ignores);
#   * static covariates have no time dimension;
#   * band i of a temporal file covers the period whose LEFT bound is time[i].

# GDAL wrappers ---------------------------------------------------------------

#' @title gdalinfo as a list
#' @name gdal_info
#' @param dsn file path or GDAL data source name
#' @return parsed `gdalinfo -json` output
#' @export
gdal_info <- function(dsn) {
  if (startsWith(dsn, "PG:")) export_pg_env()
  out <- run_cmd(c("gdalinfo", "-json", dsn), label = "gdalinfo")
  jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = TRUE)
}

#' @title Grid specification of a raster
#' @name gdal_grid_spec
#' @description Extent, size and resolution of a raster, in the form gdalwarp
#' needs to reproduce its grid exactly (`-te`, `-ts`).
#'
#' @param dsn file path or GDAL data source name
#' @return list(ncol, nrow, xmin, ymin, xmax, ymax, res_x, res_y, wkt, te, ts)
#' @export
gdal_grid_spec <- function(dsn) {
  info <- gdal_info(dsn)
  gt <- info$geoTransform
  ncol <- info$size[1]
  nrow <- info$size[2]
  xmin <- gt[1]
  ymax <- gt[4]
  xmax <- xmin + ncol * gt[2]
  ymin <- ymax + nrow * gt[6]
  list(ncol = ncol, nrow = nrow,
       xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
       res_x = gt[2], res_y = abs(gt[6]),
       wkt = info$coordinateSystem$wkt %||% "",
       te = c(xmin, ymin, xmax, ymax), ts = c(ncol, nrow))
}

#' @title Parse GDAL resolution
#' @name parse_gdal_res
#' @param src_file raster file or data source name
#' @return two-element vector (lon, lat) of pixel sizes
#' @export
parse_gdal_res <- function(src_file) {
  spec <- gdal_grid_spec(src_file)
  c(lon = spec$res_x, lat = spec$res_y)
}

#' @title Run gdalwarp
#' @name gdal_warp
#' @description Runs gdalwarp with an argument vector (no shell string) and
#' stops if it fails. The destination is always overwritten.
#'
#' @param src source file or data source name
#' @param dst destination file
#' @param te target extent c(xmin, ymin, xmax, ymax)
#' @param ts target size c(ncol, nrow)
#' @param tr target resolution c(res_x, res_y)
#' @param t_srs target SRS
#' @param s_srs source SRS (only needed when the source has none)
#' @param r resampling method
#' @param srcnodata,dstnodata NoData values ("None" to ignore)
#' @param ot output type
#' @param threads number of warp threads
#' @param extra extra arguments
#' @return dst, invisibly
#' @export
gdal_warp <- function(src, dst, te = NULL, ts = NULL, tr = NULL,
                      t_srs = "EPSG:4326", s_srs = NULL, r = "near",
                      srcnodata = NULL, dstnodata = NULL, ot = NULL,
                      threads = 1, extra = character()) {
  if (startsWith(src, "PG:")) export_pg_env()
  if (file.exists(dst)) file.remove(dst)
  args <- c("-overwrite", "-q",
            if (!is.null(s_srs)) c("-s_srs", s_srs),
            if (!is.null(t_srs)) c("-t_srs", t_srs),
            if (!is.null(te)) c("-te", sprintf("%.15g", te)),
            if (!is.null(ts)) c("-ts", ts),
            if (!is.null(tr)) c("-tr", sprintf("%.15g", tr)),
            "-r", r,
            if (!is.null(srcnodata)) c("-srcnodata", srcnodata),
            if (!is.null(dstnodata)) c("-dstnodata", dstnodata),
            if (!is.null(ot)) c("-ot", ot),
            if (threads > 1) c("-multi", "-wo", paste0("NUM_THREADS=", threads)),
            if (grepl("\\.tif+$", dst, ignore.case = TRUE)) c("-co", "COMPRESS=DEFLATE", "-co", "TILED=YES"),
            extra, src, dst)
  run_cmd(c("gdalwarp", args), label = paste("gdalwarp", basename(src)))
  if (!file.exists(dst)) {
    stop("gdalwarp reported success but did not write ", dst)
  }
  invisible(dst)
}

#' @title Warp a raster onto a reference grid
#' @name warp_to_reference
#' @param src source raster
#' @param dst destination file
#' @param ref_spec output of `gdal_grid_spec()` for the reference grid
#' @param r resampling method
#' @param ... passed to `gdal_warp`
#' @return dst, invisibly
#' @export
warp_to_reference <- function(src, dst, ref_spec, r, ...) {
  gdal_warp(src, dst, te = ref_spec$te, ts = ref_spec$ts, r = r, ...)
}

# NetCDF ----------------------------------------------------------------------

#' @title Write a covariate NetCDF file
#' @name write_covariate_ncdf
#' @param r SpatRaster
#' @param file output path
#' @param var_name variable name
#' @param long_name long variable name
#' @param unit unit string
#' @param dates Date vector (one per layer) for temporal covariates, NULL for static
#' @return file, invisibly
#' @export
write_covariate_ncdf <- function(r, file, var_name, long_name = var_name, unit = "",
                                 dates = NULL) {
  if (!is.null(dates)) {
    if (length(dates) != terra::nlyr(r)) {
      stop("write_covariate_ncdf: ", length(dates), " dates for ", terra::nlyr(r), " layers")
    }
    terra::time(r) <- as.Date(dates)
  } else if (terra::nlyr(r) > 1) {
    stop("write_covariate_ncdf: static covariates must have one layer")
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  terra::writeCDF(r, file, varname = var_name, longname = long_name,
                  unit = unit %||% "", zname = "time", missval = -9999,
                  compression = 4, overwrite = TRUE)
  invisible(file)
}

#' @title Get NetCDF metadata
#' @name get_ncdf_metadata
#' @description Time axis and variable attributes of a NetCDF file. The time
#' dimension must be called `time`; files without one are static.
#'
#' @param src_file NetCDF file
#' @return list(time_info, var_name, var_att, dates, start_date, date_index)
#' @export
get_ncdf_metadata <- function(src_file) {
  r_nc <- ncdf4::nc_open(src_file)
  on.exit(ncdf4::nc_close(r_nc))

  if ("time" %in% names(r_nc$dim)) {
    r_time_info <- ncdf4::ncatt_get(r_nc, "time")
    start_date <- as.Date(stringr::str_extract(r_time_info$units, "[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}"))
    r_dates <- as.Date(as.vector(ncdf4::ncvar_get(r_nc, "time")), origin = start_date)
  } else {
    r_time_info <- list(units = "static")
    r_dates <- NULL
    start_date <- NULL
  }

  var_name <- setdiff(names(r_nc$var), "crs")
  if (length(var_name) != 1) {
    stop("Expected one data variable in ", src_file, ", found: ", paste(var_name, collapse = ", "))
  }
  var_att <- ncdf4::ncatt_get(r_nc, var_name)
  if (is.null(var_att$long_name)) {
    var_att$long_name <- var_name
  }

  list(time_info = r_time_info, var_name = var_name, var_att = var_att,
       dates = r_dates, start_date = start_date,
       date_index = if (is.null(r_dates)) NULL else r_dates - start_date)
}

#' @title Dates of a covariate file
#' @name covariate_file_dates
#' @param file raster file
#' @return Date vector, or NULL for static (no time axis) files
#' @export
covariate_file_dates <- function(file) {
  if (tolower(tools::file_ext(file)) == "nc") {
    get_ncdf_metadata(file)$dates
  } else {
    NULL
  }
}

# Time --------------------------------------------------------------------------

#' @title Parse time resolution
#' @name parse_time_res
#' @param x string such as "1 years" or "1 month"
#' @return list(units, k)
#' @export
parse_time_res <- function(x) {
  res_time <- strsplit(x, " ")[[1]]
  if (!stringr::str_detect(res_time[2], "s$")) {
    res_time[2] <- stringr::str_c(res_time[2], "s")
  }
  list(units = res_time[2], k = as.integer(res_time[1]))
}

#' @title Get time resolution
#' @name get_time_res
#' @description Time resolution of a covariate, in units of the model's
#' temporal resolution.
#'
#' @param dates covariate dates
#' @param covar_res_time the covariate's declared time resolution (used when
#'   there is a single date)
#' @param units model time units ("months" or "years")
#' @return list(dt_units, dt_days)
#' @export
get_time_res <- function(dates, covar_res_time, units) {
  if (length(dates) < 2) {
    if (is.null(covar_res_time)) {
      stop("A single-date covariate needs `res_time` in the covariate dictionary")
    }
    dt_days <- ifelse(stringr::str_detect(covar_res_time, "day"), 1,
                      ifelse(stringr::str_detect(covar_res_time, "month"), 30, 365))
  } else {
    dt_days <- as.numeric(difftime(dates[2], dates[1], units = "days"))
  }
  dt <- if (stringr::str_detect(units, "month")) dt_days / 30 else dt_days / 365
  list(dt_units = round(dt * 10) / 10, dt_days = dt_days)
}

#' @title Generate time sequence
#' @name generate_time_sequence
#' @description Model time steps covered by a covariate whose temporal
#' resolution is coarser than the model's, and the covariate layer that
#' provides each step.
#'
#' @param dates covariate dates
#' @param res_time_source output of `get_time_res`
#' @param res_time model time resolution string
#' @return list(dates, date_mapping)
#' @export
generate_time_sequence <- function(dates, res_time_source, res_time) {
  start_date <- dates[1]
  end_date <- dplyr::last(dates)

  if (stringr::str_detect(res_time, "year") ||
      (stringr::str_detect(res_time, "month") && res_time_source$dt_units > 11)) {
    lubridate::year(end_date) <- lubridate::year(end_date) + 1
    end_date <- lubridate::floor_date(end_date, unit = "years") - 1
  }

  seq_dates <- seq.Date(start_date, end_date, by = res_time)

  key <- if (res_time_source$dt_days > 360) {
    function(d) format(d, "%Y")
  } else if (res_time_source$dt_days > 28) {
    function(d) format(d, "%Y-%m")
  } else {
    function(d) format(d, "%Y-%U")
  }
  date_mapping <- match(key(seq_dates), key(dates))
  if (anyNA(date_mapping)) {
    stop("Could not map model time steps ",
         paste(seq_dates[is.na(date_mapping)], collapse = ", "),
         " to covariate layers")
  }
  list(dates = seq_dates, date_mapping = date_mapping)
}

#' @title Aggregate time
#' @name time_aggregate
#' @description Crops a covariate file to the area of interest and aggregates
#' it to the model's temporal resolution. Static covariates are only cropped.
#'
#' @param src_file raster file to process (NetCDF for temporal covariates)
#' @param covar_name name of the covariate
#' @param covar_unit units of the covariate
#' @param covar_type "temporal" or "static"
#' @param covar_res_time declared time resolution of the covariate
#' @param res_file output NetCDF file
#' @param res_time model time resolution string
#' @param aggregator one of "sum", "mean", "min", "max", "median"
#' @param aoi area of interest from `get_aoi()`, or NULL for no crop
#' @return list(file, dates); dates is NULL for static covariates
#' @export
time_aggregate <- function(src_file, covar_name, covar_unit, covar_type,
                           covar_res_time = NULL, res_file, res_time,
                           aggregator = "mean", aoi = NULL) {
  r <- terra::rast(src_file)
  if (!is.null(aoi)) {
    r <- terra::crop(r, aoi$extent, snap = "out")
  }

  if (covar_type == "static") {
    if (terra::nlyr(r) > 1) {
      stop("Static covariate ", covar_name, " has ", terra::nlyr(r), " layers in ", src_file)
    }
    write_covariate_ncdf(r, res_file, var_name = covar_name, long_name = covar_name,
                         unit = covar_unit)
    return(list(file = res_file, dates = NULL))
  }

  if (tolower(tools::file_ext(src_file)) != "nc") {
    stop("Temporal covariates must be NetCDF files, found ", src_file)
  }
  meta <- get_ncdf_metadata(src_file)
  dates <- meta$dates
  if (length(dates) != terra::nlyr(r)) {
    stop(src_file, ": ", length(dates), " time steps but ", terra::nlyr(r), " layers")
  }

  res_time_list <- parse_time_res(res_time)
  res_src <- get_time_res(dates = dates, covar_res_time = covar_res_time,
                          units = res_time_list$units)

  if (res_src$dt_units > res_time_list$k) {
    message("-- ", covar_name, ": source coarser than ", res_time, ", replicating layers")
    seqd <- generate_time_sequence(dates, res_src, res_time)
    r_out <- r[[seqd$date_mapping]]
    out_dates <- seqd$dates
  } else {
    allowed <- c("sum", "mean", "min", "max", "median")
    if (!(aggregator %in% allowed)) {
      stop("Time aggregator '", aggregator, "' not in {", paste(allowed, collapse = ", "), "}")
    }
    period <- lubridate::floor_date(dates, unit = paste(res_time_list$k, res_time_list$units))
    out_dates <- unique(period)
    idx <- match(period, out_dates)
    r_out <- terra::tapp(r, index = idx, fun = aggregator, na.rm = TRUE)
  }

  long_name <- paste(meta$var_att$long_name, "-", res_time, aggregator)
  unit <- paste0(meta$var_att$units %||% covar_unit %||% "",
                 " aggregated at [time resolution:", res_time, "] with [aggregator:", aggregator, "]")
  write_covariate_ncdf(r_out, res_file, var_name = meta$var_name, long_name = long_name,
                       unit = unit, dates = out_dates)
  list(file = res_file, dates = as.Date(out_dates))
}

# Space -------------------------------------------------------------------------

#' @title Transform a raster
#' @name transform_spatraster
#' @description Applies a named function (e.g. "log1p") cell-wise. Non-finite
#' results (e.g. log of 0) become NA, with a warning.
#'
#' @param r SpatRaster
#' @param transform function name
#' @return transformed SpatRaster
#' @export
transform_spatraster <- function(r, transform) {
  fun <- match.fun(transform)
  out <- terra::app(r, fun)
  bad <- terra::global(!is.finite(out) & !is.na(out), "sum", na.rm = TRUE)$sum
  if (sum(bad) > 0) {
    warning("Transform '", transform, "' produced ", sum(bad), " non-finite values, set to NA")
    out <- terra::classify(out, cbind(c(-Inf, Inf), NA))
  }
  out
}

#' @title Spatial aggregation
#' @name space_aggregate
#' @description Aggregates a covariate file onto the modelling grid. Uses
#' gdalwarp with the grid's exact extent and size, so the output pixels are
#' the grid cells. "sum" uses GDAL's area-weighted sum resampling.
#'
#' @param src_file time-aggregated NetCDF file
#' @param res_file output NetCDF file
#' @param ref_spec output of `gdal_grid_spec()` for the modelling grid
#' @param covar_type "temporal" or "static"
#' @param aggregator "average" (or "mean"), "sum", "min", "max", "med", "mode"
#' @param transform optional function name applied after aggregation
#' @param threads gdalwarp threads
#' @return list(file, dates)
#' @export
space_aggregate <- function(src_file, res_file, ref_spec, covar_type,
                            aggregator = "average", transform = NULL, threads = 1) {
  if (aggregator == "mean") aggregator <- "average"
  allowed <- c("average", "sum", "min", "max", "med", "mode")
  if (!(aggregator %in% allowed)) {
    stop("Spatial aggregator '", aggregator, "' not in {", paste(allowed, collapse = ", "), "}")
  }

  meta <- get_ncdf_metadata(src_file)
  src <- terra::rast(src_file)
  s_srs <- if (terra::crs(src) == "") "EPSG:4326" else NULL

  tmp <- tempfile(fileext = ".tif")
  on.exit(unlink(tmp), add = TRUE)
  warp_to_reference(src_file, tmp, ref_spec, r = aggregator, s_srs = s_srs,
                    dstnodata = -9999, ot = "Float32", threads = threads)

  r <- terra::rast(tmp)
  if (!is.null(transform)) {
    r <- transform_spatraster(r, transform)
  }
  dates <- if (covar_type == "temporal") meta$dates else NULL
  write_covariate_ncdf(r, res_file, var_name = meta$var_name,
                       long_name = meta$var_att$long_name,
                       unit = meta$var_att$units %||% "", dates = dates)
  list(file = res_file, dates = dates)
}

#' @title Read a raster from the database
#' @name read_pg_raster
#' @description Reads a PostGIS raster table through GDAL's PostGISRaster
#' driver (needs a GDAL build with that driver).
#'
#' @param schema schema
#' @param table raster table
#' @param bands optional band indices
#' @return SpatRaster
#' @export
read_pg_raster <- function(schema, table, bands = NULL) {
  export_pg_env()
  r <- terra::rast(as.character(get_pg_gdal_dsn(schema, table)))
  if (!is.null(bands)) r <- r[[bands]]
  r
}
