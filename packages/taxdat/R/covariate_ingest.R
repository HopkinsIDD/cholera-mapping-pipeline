# Covariate ingestion: raw rasters -> processed NetCDF cache -> PostGIS table.
#
# Two phases, which can run as separate jobs:
#   1. pre-compute (no database): crop to the AOI, aggregate in time, warp onto
#      the modelling grid file; cached under
#      <layers>/processed_covariates/<covariate>/<aoi>/ with one
#      grid_<res>km.json sidecar per resolution recording the grid it used;
#   2. load (database): stack every processed band in date order into one VRT,
#      load it into covariates.<alias>__staging, check the band count, then swap
#      it in and write covariates.metadata and covariates.bands in one
#      transaction. A failed load never leaves a partial table behind.

#' @title Make covariate alias
#' @name make_covar_alias
#' @param alias covariate alias from the dictionary
#' @param type "temporal" or "static"
#' @param res_time temporal resolution
#' @param res_space spatial resolution, c(x, y) in km
#' @return the table name of the covariate at this resolution
#' @export
make_covar_alias <- function(alias, type, res_time, res_space) {
  if (!(type %in% c("temporal", "static"))) {
    stop("Covariate type needs to be either 'temporal' or 'static'")
  }
  stringr::str_c(alias,
                 ifelse(type == "temporal", stringr::str_replace(res_time, " ", "_"), ""),
                 res_space[1], res_space[2], sep = "_")
}

#' @title Table exists in multiple schemas
#' @name db_exists_table_multi
#' @param conn DBI connection
#' @param schemas schemas to look in
#' @param table_name table name
#' @return named logical vector
#' @export
db_exists_table_multi <- function(conn, schemas, table_name) {
  check <- vapply(schemas, function(x) {
    DBI::dbExistsTable(conn, DBI::Id(schema = x, table = table_name))
  }, logical(1))
  names(check) <- schemas
  check
}

#' @title List covariate source files
#' @name list_covariate_files
#' @description Raw files of a covariate, in chronological order. Temporal
#' covariates are directories of NetCDF files; static covariates are one file.
#'
#' @param covar_path directory (temporal) or file (static)
#' @param covar_type "temporal" or "static"
#' @return list(files, first_dates)
#' @export
list_covariate_files <- function(covar_path, covar_type) {
  if (covar_type == "static") {
    if (!file.exists(covar_path) || dir.exists(covar_path)) {
      stop("Static covariate file not found: ", covar_path)
    }
    return(list(files = covar_path, first_dates = as.Date(NA)))
  }
  files <- list.files(covar_path, pattern = "\\.nc$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop("No NetCDF files found in ", covar_path)
  }
  first <- do.call(c, lapply(files, function(f) {
    d <- get_ncdf_metadata(f)$dates
    if (length(d) == 0) stop("Temporal covariate file without a time axis: ", f)
    min(d)
  }))
  o <- order(first)
  list(files = files[o], first_dates = first[o])
}

covariate_proc_dir <- function(layers_dir, covar_name, aoi) {
  file.path(layers_dir, "processed_covariates", covar_name, aoi_metadata(aoi)$aoi_name)
}

check_proc_sidecar <- function(proc_dir, aoi, grid_spec, res_space) {
  # One sidecar per resolution: the directory holds every resolution's files.
  side <- file.path(proc_dir, sprintf("grid_%skm.json", res_space))
  want <- list(aoi = aoi_metadata(aoi), te = grid_spec$te, ts = grid_spec$ts)
  if (file.exists(side)) {
    have <- jsonlite::read_json(side, simplifyVector = TRUE)
    same <- isTRUE(all.equal(have$te, want$te, tolerance = 1e-9)) &&
      isTRUE(all.equal(have$ts, want$ts)) &&
      identical(have$aoi$aoi_name, want$aoi$aoi_name)
    if (!same) {
      stop("Processed files in ", proc_dir, " were made for another grid or area of ",
           "interest. Delete that directory to recompute them.")
    }
  } else {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(want, side, auto_unbox = TRUE, digits = NA, pretty = TRUE)
  }
  invisible(side)
}

processed_file_names <- function(proc_dir, src_file, covar, res_time, res_space) {
  stem <- tools::file_path_sans_ext(basename(src_file))
  time_part <- if (covar$type == "temporal") {
    paste0("__", covar$time_aggregator, "_", gsub(" ", "-", res_time))
  } else {
    "__static"
  }
  trans <- if (is.null(covar$transform)) "" else paste0("_", covar$transform, "_trans")
  time_file <- file.path(proc_dir, paste0(stem, time_part, ".nc"))
  space_file <- file.path(proc_dir, paste0(stem, time_part, "__resampled_",
                                           res_space, "x", res_space, "km", trans, ".nc"))
  list(time = time_file, space = space_file)
}

process_covariate_file <- function(src_file, covar, res_time, res_space, grid_spec,
                                   aoi, proc_dir, threads = 1) {
  fn <- processed_file_names(proc_dir, src_file, covar, res_time, res_space)
  if (!file.exists(fn$space)) {
    if (!file.exists(fn$time)) {
      message("-- ", covar$name, ": aggregating ", basename(src_file), " in time (", res_time, ")")
      time_aggregate(src_file = src_file, covar_name = covar$name, covar_unit = covar$unit,
                     covar_type = covar$type, covar_res_time = covar$res_time,
                     res_file = fn$time, res_time = res_time,
                     aggregator = covar$time_aggregator %||% "mean", aoi = aoi)
    }
    message("-- ", covar$name, ": aggregating ", basename(src_file), " in space (",
            res_space, " km)")
    space_aggregate(src_file = fn$time, res_file = fn$space, ref_spec = grid_spec,
                    covar_type = covar$type, aggregator = covar$space_aggregator %||% "average",
                    transform = covar$transform, threads = threads)
  }
  list(file = fn$space, dates = covariate_file_dates(fn$space))
}

#' @title Write a VRT stacking bands of several files
#' @name write_band_stack_vrt
#' @description All files must share one grid (the modelling grid). Band order
#' follows `files`, then the bands inside each file.
#'
#' @param files raster files
#' @param vrt_file output VRT path
#' @return vrt_file, invisibly
#' @export
write_band_stack_vrt <- function(files, vrt_file) {
  info <- gdal_info(files[1])
  gt <- paste(sprintf("%.17g", info$geoTransform), collapse = ", ")
  srs <- info$coordinateSystem$wkt %||% ""
  band_xml <- character()
  k <- 0
  for (f in files) {
    fi <- gdal_info(f)
    if (!identical(fi$size, info$size) || !isTRUE(all.equal(fi$geoTransform, info$geoTransform))) {
      stop("write_band_stack_vrt: ", f, " is not on the same grid as ", files[1])
    }
    for (b in seq_len(nrow(fi$bands))) {
      k <- k + 1
      band_xml <- c(band_xml, sprintf(
        paste0('  <VRTRasterBand dataType="Float32" band="%d">\n',
               '    <NoDataValue>-9999</NoDataValue>\n',
               '    <SimpleSource>\n',
               '      <SourceFilename relativeToVRT="0">%s</SourceFilename>\n',
               '      <SourceBand>%d</SourceBand>\n',
               '    </SimpleSource>\n',
               '  </VRTRasterBand>'),
        k, normalizePath(f), b))
    }
  }
  xml <- c(sprintf('<VRTDataset rasterXSize="%d" rasterYSize="%d">', info$size[1], info$size[2]),
           sprintf("  <SRS>%s</SRS>", gsub("<", "&lt;", gsub("&", "&amp;", srs))),
           sprintf("  <GeoTransform>%s</GeoTransform>", gt),
           band_xml, "</VRTDataset>")
  writeLines(xml, vrt_file)
  invisible(vrt_file)
}

band_bounds <- function(dates, res_time) {
  if (is.null(dates) || all(is.na(dates))) {
    return(data.frame(band = 1L, tl = as.Date(NA), tr = as.Date(NA)))
  }
  to_unit <- time_unit_to_aggregate_function(res_time)
  to_end <- time_unit_to_end_function(res_time)
  data.frame(band = seq_along(dates), tl = as.Date(dates), tr = to_end(to_unit(dates)))
}

#' @title Load processed covariate files into the database
#' @name load_covariate_table
#' @description Stacks the processed files into one VRT, loads it into a
#' staging table, checks the band count, and swaps it in together with the
#' metadata and band-to-date rows in one transaction.
#'
#' @param conn DBI connection
#' @param processed list of list(file, dates) in chronological order
#' @param covar dictionary entry
#' @param covar_alias table name
#' @param src_files raw files (for source resolution metadata)
#' @param res_time,res_space model resolutions
#' @param aoi area of interest
#' @param ref_grid modelling grid name
#' @return NULL, invisibly
#' @export
load_covariate_table <- function(conn, processed, covar, covar_alias, src_files,
                                 res_time, res_space, aoi, ref_grid) {
  files <- vapply(processed, `[[`, "", "file")
  dates <- if (covar$type == "temporal") do.call(c, lapply(processed, `[[`, "dates")) else NULL
  if (!is.null(dates)) {
    if (anyDuplicated(dates)) {
      stop(covar$name, ": duplicated dates across files: ",
           paste(unique(dates[duplicated(dates)]), collapse = ", "))
    }
    if (is.unsorted(dates)) stop(covar$name, ": dates are not in chronological order")
    expected <- seq.Date(min(dates), max(dates), by = res_time)
    if (length(expected) != length(dates)) {
      warning(covar$name, ": dates are not contiguous at ", res_time, "; missing ",
              paste(setdiff(as.character(expected), as.character(dates)), collapse = ", "))
    }
  }
  bands <- band_bounds(dates, res_time)

  vrt <- tempfile(fileext = ".vrt")
  on.exit(unlink(vrt), add = TRUE)
  write_band_stack_vrt(files, vrt)

  staging <- paste0(covar_alias, "__staging")
  raster2pgsql_pipe(vrt, paste0("covariates.", staging), mode = "create", index = TRUE)
  n_bands <- DBI::dbGetQuery(conn, glue::glue_sql(
    "SELECT max(ST_NumBands(rast)) AS n, min(ST_NumBands(rast)) AS m FROM covariates.{`staging`};",
    .con = conn))
  if (n_bands$n != nrow(bands) || n_bands$m != nrow(bands)) {
    stop(covar$name, ": staging table has ", n_bands$m, "-", n_bands$n, " bands, expected ",
         nrow(bands), ". The previous table was left untouched.")
  }

  src_spec <- gdal_grid_spec(src_files[1])
  meta <- cbind(
    data.frame(covariate = covar_alias,
               src_res_x = src_spec$res_x, src_res_y = src_spec$res_y,
               src_res_time = if (covar$type == "static") "static" else (covar$res_time %||% res_time),
               first_tl = if (is.null(dates)) as.Date(NA) else min(dates),
               last_tl = if (is.null(dates)) as.Date(NA) else max(dates),
               src_dir = covar$dir, res_x = res_space, res_y = res_space,
               res_time = if (covar$type == "static") NA_character_ else res_time,
               space_agg = covar$space_aggregator %||% "average",
               time_agg = covar$time_aggregator %||% NA_character_),
    aoi_metadata(aoi),
    data.frame(ref_grid = ref_grid, n_bands = nrow(bands)))

  DBI::dbWithTransaction(conn, {
    db_exec(conn, glue::glue_sql("DROP TABLE IF EXISTS covariates.{`covar_alias`};", .con = conn))
    db_exec(conn, glue::glue_sql("ALTER TABLE covariates.{`staging`} RENAME TO {`covar_alias`};",
                                 .con = conn))
    idx <- DBI::dbGetQuery(conn, glue::glue_sql(
      "SELECT indexname FROM pg_indexes WHERE schemaname = 'covariates' AND tablename = {covar_alias};",
      .con = conn))$indexname
    for (i in idx[grepl("__staging", idx, fixed = TRUE)]) {
      db_exec(conn, glue::glue_sql("ALTER INDEX covariates.{`i`} RENAME TO {`sub('__staging', '', i, fixed = TRUE)`};",
                                   .con = conn))
    }
    write_metadata(conn, meta, bands)
  })
  db_exec(conn, glue::glue_sql(
    "SELECT AddRasterConstraints('covariates'::name, {covar_alias}::name, 'rast'::name);", .con = conn))
  db_exec(conn, glue::glue_sql("ANALYZE covariates.{`covar_alias`};", .con = conn))
  invisible(NULL)
}

#' @title Write metadata
#' @name write_metadata
#' @description Upserts one covariate's row in covariates.metadata and replaces
#' its rows in covariates.bands.
#'
#' @param conn DBI connection
#' @param meta one-row data frame matching covariates.metadata columns
#' @param bands data frame(band, tl, tr)
#' @return NULL, invisibly
#' @export
write_metadata <- function(conn, meta, bands) {
  cols <- names(meta)
  ident <- DBI::dbQuoteIdentifier(conn, cols)
  vals <- vapply(cols, function(c) as.character(DBI::dbQuoteLiteral(conn, meta[[c]][1])), "")
  updates <- paste0(ident[cols != "covariate"], " = EXCLUDED.", ident[cols != "covariate"])
  db_exec(conn, DBI::SQL(paste0(
    "INSERT INTO covariates.metadata (", paste(ident, collapse = ", "), ", ingested_at) VALUES (",
    paste(vals, collapse = ", "), ", now()) ON CONFLICT (covariate) DO UPDATE SET ",
    paste(updates, collapse = ", "), ", ingested_at = now();")))
  db_exec(conn, glue::glue_sql("DELETE FROM covariates.bands WHERE covariate = {meta$covariate};",
                               .con = conn))
  rows <- data.frame(covariate = meta$covariate, band = bands$band, tl = bands$tl, tr = bands$tr)
  DBI::dbAppendTable(conn, DBI::Id(schema = "covariates", table = "bands"), rows)
  invisible(NULL)
}

#' @title Ingest covariate
#' @name ingest_covariate
#' @description Processes one covariate at the model's resolution and, unless
#' `write_to_db` is FALSE, loads it into `covariates.<alias>`.
#'
#' @param conn DBI connection (only used when write_to_db is TRUE)
#' @param covar dictionary entry (name, alias, dir, type, res_time,
#'   time_aggregator, space_aggregator, transform, unit)
#' @param covar_alias table name
#' @param layers_dir Layers directory (dictionary paths are relative to it)
#' @param res_time,res_space model resolutions
#' @param grid list(full_grid_name, grid_file) from `prepare_grid`
#' @param aoi area of interest, or NULL
#' @param write_to_db load into the database after pre-computing
#' @param n_cpus processes for pre-computing files in parallel
#' @return covariates.<alias>, invisibly
#' @export
ingest_covariate <- function(conn, covar, covar_alias, layers_dir, res_time, res_space,
                             grid, aoi = NULL, write_to_db = TRUE, n_cpus = 1) {
  t0 <- Sys.time()
  covar_path <- file.path(layers_dir, covar$dir)
  src <- list_covariate_files(covar_path, covar$type)
  grid_spec <- gdal_grid_spec(grid$grid_file)
  proc_dir <- covariate_proc_dir(layers_dir, covar$name, aoi)
  check_proc_sidecar(proc_dir, aoi, grid_spec, res_space)

  cat("---- ", if (write_to_db) "Ingesting " else "Pre-computing ", toupper(covar$name),
      " at [", res_time, ", ", res_space, "x", res_space, " km] from ",
      length(src$files), " file(s)\n", sep = "")

  run_one <- function(f) {
    process_covariate_file(f, covar, res_time, res_space, grid_spec, aoi, proc_dir,
                           threads = if (n_cpus > 1) 1 else 4)
  }
  processed <- if (n_cpus > 1 && length(src$files) > 1) {
    parallel::mclapply(src$files, run_one, mc.cores = n_cpus, mc.preschedule = FALSE)
  } else {
    lapply(src$files, run_one)
  }
  failed <- vapply(processed, inherits, logical(1), "try-error")
  if (any(failed)) {
    stop(covar$name, ": pre-computing failed for ", paste(basename(src$files[failed]), collapse = ", "),
         ":\n", paste(unlist(processed[failed]), collapse = "\n"))
  }

  if (write_to_db) {
    load_covariate_table(conn, processed, covar, covar_alias, src$files,
                         res_time, res_space, aoi, ref_grid = grid$full_grid_name)
  }
  cat("---- Done ", covar$name, " (", format(difftime(Sys.time(), t0, units = "mins"), digits = 2),
      ")\n", sep = "")
  invisible(paste0("covariates.", covar_alias))
}

#' @title Covariate ingestion modes
#' @name covariate_mode
#' @description Maps the legacy config flags to one mode:
#'   `use_existing` (never ingest; stop if missing), `ingest_missing` (ingest
#'   only covariates absent from the database), `reingest` (rebuild the
#'   requested covariates). Metadata of other covariates is never dropped.
#' @param ingest_covariates config flag
#' @param ingest_new_covariates config flag
#' @return mode string
#' @export
covariate_mode <- function(ingest_covariates, ingest_new_covariates) {
  if (!isTRUE(ingest_covariates)) {
    "use_existing"
  } else if (isTRUE(ingest_new_covariates)) {
    "reingest"
  } else {
    "ingest_missing"
  }
}

#' @title Prepare covariates
#' @name prepare_covariates
#' @description Makes sure every requested covariate exists in the database at
#' the model's resolution and returns their table names, population first.
#'
#' @param covar_abbr covariate abbreviations; population ("p") is added first
#' @param covar_dict covariate dictionary (list read from covariate_dictionary.yml)
#' @param layers_dir Layers directory
#' @param res_space,res_time model resolutions
#' @param grid list(full_grid_name, grid_file) from `prepare_grid`
#' @param aoi area of interest, or NULL
#' @param mode see `covariate_mode`
#' @param n_cpus processes for pre-computing
#' @param precompute_only only fill the processed-file cache (no database)
#' @param conn optional DBI connection
#' @return character vector "covariates.<alias>", population first
#' @export
prepare_covariates <- function(covar_abbr, covar_dict, layers_dir, res_space, res_time,
                               grid, aoi = NULL,
                               mode = c("ingest_missing", "use_existing", "reingest"),
                               n_cpus = 1, precompute_only = FALSE, conn = NULL) {
  mode <- match.arg(mode)
  covar_abbr <- unique(c("p", setdiff(covar_abbr, "p")))
  abbrs <- vapply(covar_dict, `[[`, "", "abbr")
  missing <- setdiff(covar_abbr, abbrs)
  if (length(missing) > 0) {
    stop("Covariate(s) ", paste(missing, collapse = ", "), " not specified in dictionary")
  }
  # Order follows the request, population first (downstream code assumes it)
  covars <- covar_dict[match(covar_abbr, abbrs)]

  if (!precompute_only && is.null(conn)) {
    conn <- connect_to_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  cat("**** PREPARING COVARIATES (mode: ", mode, if (precompute_only) ", pre-compute only", ") ****\n", sep = "")
  out <- character()
  for (covar in covars) {
    alias <- make_covar_alias(covar$alias, covar$type, res_time, c(res_space, res_space))
    if (precompute_only) {
      ingest_covariate(NULL, covar, alias, layers_dir, res_time, res_space, grid, aoi,
                       write_to_db = FALSE, n_cpus = n_cpus)
      out <- c(out, paste0("covariates.", alias))
      next
    }
    in_db <- DBI::dbExistsTable(conn, DBI::Id(schema = "covariates", table = alias))
    has_meta <- in_db && nrow(DBI::dbGetQuery(conn, glue::glue_sql(
      "SELECT 1 FROM covariates.metadata WHERE covariate = {alias};", .con = conn))) == 1
    if (in_db && has_meta && mode != "reingest") {
      cat("---- Found covariates.", alias, "\n", sep = "")
    } else if (mode == "use_existing") {
      stop("Couldn't find covariate covariates.", alias, if (in_db) " metadata",
           ". It needs to be ingested by authorized users.")
    } else {
      ingest_covariate(conn, covar, alias, layers_dir, res_time, res_space, grid, aoi,
                       write_to_db = TRUE, n_cpus = n_cpus)
    }
    out <- c(out, paste0("covariates.", alias))
  }
  if (!startsWith(sub("^covariates\\.", "", out[1]), covars[[1]]$alias) || covars[[1]]$abbr != "p") {
    stop("The first covariate must be population")
  }
  cat("**** DONE COVARIATES:", paste(out, collapse = ", "), "\n")
  out
}
