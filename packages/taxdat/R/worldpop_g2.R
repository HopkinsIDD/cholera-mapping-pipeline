# WorldPop Global 2 (2015-2030) population as input of the covariates database.
#
# Global 2 differs from the Global 1 files the pipeline used before:
#   - years 2015 to 2030 only, dated 1 January;
#   - constrained: unsettled land is NoData (not 0), so the file cannot serve
#     as a land mask;
#   - a different 1 km lattice (origin -180, 84; extent down to 60 S).
# The yearly GeoTIFFs are therefore rewritten as the yearly NetCDF files the
# ingestion reads (`<layers>/pop/population_<year>_yearly.nc`), with 0 on
# unsettled land. Land comes from a separate raster on the same lattice (the
# unconstrained 2020 mosaic of release R2024B), which is also the source of
# the master grid (`CHOLERA_MASTER_GRID_FILE`).

#' @title WorldPop Global 2 download URL
#' @name worldpop_g2_url
#' @description URL of a 1 km population file of WorldPop Global 2.
#'
#' @param year 2015 to 2030
#' @param release "R2025A" (constrained only) or "R2024B"
#' @param constrained constrained (TRUE) or unconstrained (FALSE, R2024B only)
#' @param iso3 NULL for the global mosaic, or a country code
#' @return character vector of URLs, one per year
#' @export
worldpop_g2_url <- function(year, release = "R2025A", constrained = TRUE, iso3 = NULL) {
  if (any(year < 2015 | year > 2030)) {
    stop("WorldPop Global 2 covers 2015 to 2030")
  }
  if (!constrained && release != "R2024B") {
    stop("Unconstrained 1 km files exist in release R2024B only")
  }
  kind <- if (constrained) c("constrained", "CN") else c("unconstrained", "UC")
  dir <- if (is.null(iso3)) "0_Mosaicked" else toupper(iso3)
  prefix <- if (is.null(iso3)) "global" else tolower(iso3)
  sprintf("https://data.worldpop.org/GIS/Population/Global_2015_2030/%s/%d/%s/v1/1km_ua/%s/%s_pop_%d_%s_1km_%s_UA_v1.tif",
          release, year, dir, kind[1], prefix, year, kind[2], release)
}

#' @title Find a WorldPop Global 2 file
#' @name find_worldpop_g2_file
#' @param raw_dir directory holding the downloaded GeoTIFFs (searched recursively)
#' @param year year
#' @param kind "CN" (constrained) or "UC" (unconstrained)
#' @return file path; stops unless exactly one file matches
#' @export
find_worldpop_g2_file <- function(raw_dir, year, kind = "CN") {
  f <- list.files(raw_dir, pattern = sprintf("_pop_%d_%s_1km_.*\\.tif$", year, kind),
                  recursive = TRUE, full.names = TRUE)
  if (length(f) != 1) {
    stop("Expected one ", kind, " 1 km file for ", year, " under ", raw_dir, ", found ", length(f),
         if (length(f) > 0) paste0(": ", paste(basename(f), collapse = ", ")))
  }
  f
}

#' @title WorldPop Global 2 year to pipeline NetCDF
#' @name worldpop_g2_to_ncdf
#' @description Rewrites one yearly 1 km population GeoTIFF as
#' `population_<year>_yearly.nc`: population where the source has a value, 0 on
#' land the source leaves empty (unsettled), NoData off land. Pixels are never
#' resampled: the source must be on the lattice of `land_file`; it is only
#' cropped or padded to that extent.
#'
#' People on pixels that `land_file` does not count as land are kept (totals
#' are conserved), and reported, because the master grid built from
#' `land_file` does not contain those pixels.
#'
#' @param src_file constrained population GeoTIFF of one year
#' @param land_file raster on the same lattice with a value >= 0 on land
#' @param out_file NetCDF to write
#' @param year year of `src_file` (band date: 1 January)
#' @param overwrite rewrite an existing `out_file`
#' @return one-row data frame: year, file, total population, settled pixels,
#'   and the pixels and people outside the land mask
#' @export
worldpop_g2_to_ncdf <- function(src_file, land_file, out_file, year, overwrite = FALSE) {
  pop <- terra::rast(src_file)
  land <- terra::rast(land_file)
  if (terra::nlyr(pop) != 1 || terra::nlyr(land) < 1) {
    stop("Expected single-layer rasters: ", src_file, ", ", land_file)
  }
  land <- land[[1]]
  same_lattice <- terra::same.crs(pop, land) &&
    isTRUE(all.equal(terra::res(pop), terra::res(land), tolerance = 1e-9)) &&
    max(abs(terra::origin(pop) - terra::origin(land))) < 1e-6 * min(terra::res(land))
  if (!same_lattice) {
    stop(basename(src_file), " is not on the lattice of ", basename(land_file),
         " (resolution ", paste(signif(terra::res(pop), 10), collapse = " x "), " vs ",
         paste(signif(terra::res(land), 10), collapse = " x "), ", origin ",
         paste(signif(terra::origin(pop), 6), collapse = ", "), " vs ",
         paste(signif(terra::origin(land), 6), collapse = ", "), "). Population is not resampled.")
  }
  total_src <- terra::global(pop, "sum", na.rm = TRUE)$sum

  # Same extent as the land raster, without moving pixels
  if (terra::ext(pop) != terra::ext(land)) {
    pop <- terra::extend(terra::crop(pop, land, snap = "near"), land, snap = "near")
    cropped <- total_src - terra::global(pop, "sum", na.rm = TRUE)$sum
    if (cropped > 0.5) {
      stop(basename(src_file), ": ", round(cropped), " people lie outside the extent of ",
           basename(land_file))
    }
  }
  if (isTRUE(terra::global(pop, "min", na.rm = TRUE)$min < 0)) {
    stop(basename(src_file), " has negative population values")
  }

  is_land <- !is.na(land) & land >= 0
  off_land <- terra::mask(pop, is_land, maskvalues = TRUE)   # population where land is FALSE
  n_off <- terra::global(!is.na(off_land) & off_land > 0, "sum")$sum
  pop_off <- terra::global(off_land, "sum", na.rm = TRUE)$sum
  if (is.na(pop_off)) pop_off <- 0

  res <- data.frame(year = year, file = out_file, total = total_src,
                    settled_pixels = terra::global(!is.na(pop) & pop > 0, "sum")$sum,
                    pixels_off_land = n_off, people_off_land = pop_off)
  if (file.exists(out_file) && !overwrite) {
    message("-- Found ", out_file)
    return(res)
  }

  zero_on_land <- terra::ifel(is_land, 0, NA)
  out <- terra::cover(pop, zero_on_land)
  names(out) <- "pop"
  write_covariate_ncdf(out, out_file, var_name = "pop", long_name = "population", unit = "number",
                       dates = as.Date(sprintf("%d-01-01", year)))

  total_out <- terra::global(terra::rast(out_file), "sum", na.rm = TRUE)$sum
  if (abs(total_out - total_src) > 1e-6 * max(total_src, 1)) {
    unlink(out_file)
    stop(basename(out_file), ": total population ", total_out, " differs from the source ", total_src)
  }
  res
}

#' @title Prepare WorldPop Global 2 population
#' @name prepare_worldpop_g2
#' @description Converts the downloaded yearly files to
#' `<out_dir>/population_<year>_yearly.nc` and writes `PROVENANCE.txt` and
#' `worldpop_g2_summary.csv` next to them.
#'
#' @param raw_dir directory with the constrained GeoTIFFs
#' @param land_file land raster (see `worldpop_g2_to_ncdf`)
#' @param out_dir population directory of the Layers directory
#' @param years years to convert
#' @param overwrite rewrite existing files
#' @return data frame, one row per year
#' @export
prepare_worldpop_g2 <- function(raw_dir, land_file, out_dir, years = 2015:2030, overwrite = FALSE) {
  if (!file.exists(land_file)) {
    stop("Land raster not found: ", land_file)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  other <- setdiff(list.files(out_dir, pattern = "\\.nc$"),
                   sprintf("population_%d_yearly.nc", 2015:2030))
  if (length(other) > 0) {
    stop(out_dir, " holds other NetCDF files (", paste(utils::head(other, 3), collapse = ", "),
         "): every .nc file there is ingested as population. Use an empty directory.")
  }
  # Global 1 files of 2015-2020 carry the same names: never mix the two
  provenance <- file.path(out_dir, "PROVENANCE.txt")
  existing <- list.files(out_dir, pattern = "\\.nc$")
  if (length(existing) > 0 && !(file.exists(provenance) &&
                                any(grepl("WorldPop Global 2", readLines(provenance))))) {
    stop(out_dir, " already holds population files that were not written by this function ",
         "(no Global 2 PROVENANCE.txt). Use an empty directory.")
  }
  res <- do.call(rbind, lapply(years, function(y) {
    src <- find_worldpop_g2_file(raw_dir, y, "CN")
    cat("---- ", y, ": ", basename(src), "\n", sep = "")
    r <- worldpop_g2_to_ncdf(src, land_file, file.path(out_dir, sprintf("population_%d_yearly.nc", y)),
                             year = y, overwrite = overwrite)
    r$source <- basename(src)
    r
  }))
  # A later call for other years keeps the rows already there
  summary_file <- file.path(out_dir, "worldpop_g2_summary.csv")
  if (file.exists(summary_file)) {
    old <- utils::read.csv(summary_file)
    res_all <- rbind(old[!(old$year %in% res$year), names(res), drop = FALSE], res)
  } else {
    res_all <- res
  }
  utils::write.csv(res_all[order(res_all$year), ], summary_file, row.names = FALSE)
  writeLines(c(
    "WorldPop Global 2 (2015-2030), constrained, 1 km (30 arc-seconds), people per pixel, 1 January.",
    "Source files: see worldpop_g2_summary.csv (column source); https://data.worldpop.org/GIS/Population/Global_2015_2030/",
    "Citation: Bondarenko M. et al. (2025), WorldPop, University of Southampton. doi:10.5258/SOTON/WP00845. CC BY 4.0.",
    paste0("Land (0 where unsettled) from: ", basename(land_file)),
    paste0("Converted with taxdat::prepare_worldpop_g2 on ", format(Sys.Date()), ".")),
    provenance)
  res
}
