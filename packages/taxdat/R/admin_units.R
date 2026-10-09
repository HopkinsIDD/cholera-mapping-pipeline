# Administrative boundaries (rgeoboundaries) used for output summaries, ADM0
# clipping and the area of interest.
#
# Compute nodes on the cluster have no internet access, so boundaries are read
# from a GeoPackage cache by default (<cache_dir>/<ISO>_adm<level>.gpkg). The
# cache is filled by `cache_admin_units()` on a machine with internet access;
# reading falls back to the API only when the cache misses and internet works.

#' @title Admin-units cache directory
#' @name admin_units_cache_dir
#' @return path from `CHOLERA_AOI_CACHE_DIR`, else `Layers/admin_units` under
#'   the working directory
#' @export
admin_units_cache_dir <- function() {
  Sys.getenv("CHOLERA_AOI_CACHE_DIR", file.path(getwd(), "Layers", "admin_units"))
}

admin_units_cache_file <- function(iso_code, admin_level, cache_dir) {
  file.path(cache_dir, sprintf("%s_adm%d.gpkg", toupper(iso_code), as.integer(admin_level)))
}

#' Get country admin units
#'
#' Pulls the admin units of one level from the geoBoundaries API with
#' rgeoboundaries.
#'
#' @param iso_code ISO3 country code
#' @param admin_level 0 to 3
#' @return an sf object with shapeName, country, location_period_id,
#'   shapeType, source and a `geom` column
#' @export
get_country_admin_units <- function(iso_code, admin_level = 1) {
  if (admin_level > 3) {
    stop("Admin level ", admin_level, " is invalid; use 0 to 3.")
  }
  message("Using the rgeoboundaries shapefiles for all countries, for this country a admin level: ", admin_level)

  boundary_sf <- rgeoboundaries::geoboundaries(country = iso_code,
                                                adm_lvl = paste0("adm", admin_level)) %>%
    dplyr::mutate(shapeID = paste0(shapeGroup, "-", shapeType, "-", shapeID)) %>%
    dplyr::select(shapeName, shapeID, shapeType, geometry) %>%
    dplyr::mutate(source = "rgeoboundaries", country = iso_code) %>%
    dplyr::rename(location_period_id = shapeID)

  sf::st_crs(boundary_sf) <- sf::st_crs(4326)
  sf::st_geometry(boundary_sf) <- "geom"
  boundary_sf <- fix_geomcollections(boundary_sf)
  boundary_sf <- sf::st_cast(boundary_sf, "MULTIPOLYGON")
  boundary_sf %>%
    dplyr::group_by(shapeName, country) %>%
    dplyr::summarise(location_period_id = stringr::str_c(location_period_id, collapse = "_"),
                     shapeType = shapeType[1],
                     source = source[1],
                     .groups = "drop")
}

#' @title Cache admin units
#' @name cache_admin_units
#' @description Downloads admin units with rgeoboundaries and stores one
#' GeoPackage per level. Run on a machine with internet access.
#'
#' @param iso_code ISO3 country code
#' @param admin_levels levels to cache
#' @param cache_dir cache directory
#' @param overwrite re-download levels already cached
#' @return paths of the cached files, invisibly
#' @export
cache_admin_units <- function(iso_code, admin_levels = 0:2,
                              cache_dir = admin_units_cache_dir(), overwrite = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  files <- vapply(admin_levels, function(l) {
    f <- admin_units_cache_file(iso_code, l, cache_dir)
    if (overwrite || !file.exists(f)) {
      adm <- get_country_admin_units(iso_code = iso_code, admin_level = l)
      sf::st_write(adm, f, delete_dsn = TRUE, quiet = TRUE)
    }
    f
  }, character(1))
  invisible(files)
}

read_admin_units_cache <- function(iso_code, admin_level, cache_dir) {
  f <- admin_units_cache_file(iso_code, admin_level, cache_dir)
  if (!file.exists(f)) {
    return(NULL)
  }
  adm <- sf::st_read(f, quiet = TRUE)
  if (attr(adm, "sf_column") != "geom") {
    sf::st_geometry(adm) <- "geom"
  }
  adm
}

#' get_country_admin_units_db
#'
#' Reads output shapefiles from the `output_shapefiles` table (schema `data`,
#' found through the search path).
#'
#' @param iso_code ISO3 country code
#' @param admin_levels levels to read
#' @return an sf object
#' @export
get_country_admin_units_db <- function(iso_code, admin_levels = 0:2) {
  conn <- connect_to_db()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  adm_sf <- sf::st_read(
    dsn = conn,
    query = glue::glue_sql(.con = conn,
                           "SELECT * FROM output_shapefiles
                            WHERE country = {iso_code}
                            AND admin_level IN ({paste0('ADM', admin_levels)*});"))
  if (nrow(adm_sf) == 0) {
    stop("-- No output shapefiles for ", iso_code, " in the database.")
  }
  if ("get_country_admin_units_hash" %in% names(adm_sf)) {
    ref_hash <- digest::digest(deparse(get_country_admin_units), algo = "md5")
    if (any(adm_sf$get_country_admin_units_hash != ref_hash)) {
      warning("output_shapefiles was built with another version of get_country_admin_units.")
    }
  }
  adm_sf
}

#' Get multi-level country admin units
#'
#' Admin units of several levels for one country, optionally clipped to the
#' national boundary.
#'
#' @param iso_code ISO3 code, or "testing" for test runs
#' @param admin_levels levels to return
#' @param lps location periods, returned as is for test runs
#' @param clip_to_adm0 clip subnational units to the ADM0 geometry
#' @param source "cache" (GeoPackage cache, then API on a miss), "database"
#'   (output_shapefiles table) or "api"
#' @param cache_dir cache directory
#' @return an sf object with an `admin_level` column ("ADM0", "ADM1", ...)
#' @export
get_multi_country_admin_units <- function(iso_code,
                                          admin_levels = 0:2,
                                          lps = NULL,
                                          clip_to_adm0 = TRUE,
                                          source = c("cache", "database", "api"),
                                          cache_dir = admin_units_cache_dir()) {
  source <- match.arg(source)
  if (identical(iso_code, "testing")) {
    return(lps)
  }
  if (source == "database") {
    return(get_country_admin_units_db(iso_code = iso_code, admin_levels = admin_levels))
  }
  if (clip_to_adm0 && !(0 %in% admin_levels)) {
    stop("Admin level 0 needs to be included if clip_to_adm0 = TRUE.")
  }

  adm_sf <- purrr::map_df(admin_levels, function(l) {
    adm <- if (source == "cache") read_admin_units_cache(iso_code, l, cache_dir) else NULL
    if (is.null(adm)) {
      if (source == "cache") {
        message("-- Admin units ", iso_code, " ADM", l, " not cached in ", cache_dir,
                "; trying the geoBoundaries API")
      }
      adm <- get_country_admin_units(iso_code = iso_code, admin_level = l)
      if (source == "cache") {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        sf::st_write(adm, admin_units_cache_file(iso_code, l, cache_dir),
                     delete_dsn = TRUE, quiet = TRUE)
      }
    }
    dplyr::rename(adm, admin_level = shapeType)
  })

  if (clip_to_adm0) {
    adm0_geom <- adm_sf %>%
      dplyr::filter(admin_level == "ADM0") %>%
      sf::st_make_valid() %>%
      sf::st_union()
    adm_sf <- adm_sf %>%
      sf::st_filter(sf::st_sf(geom = adm0_geom)) %>%
      dplyr::mutate(geom = sf::st_make_valid(sf::st_intersection(geom, adm0_geom)))
  }

  adm_sf <- fix_geomcollections(adm_sf)
  dplyr::arrange(adm_sf, location_period_id)
}

#' Get country isocode
#'
#' @param config run config
#' @return ISO3 code(s), or "testing"
#' @export
get_country_isocode <- function(config) {
  if (all(grepl("testing", config$countries))) {
    return("testing")
  }
  if (!all(nchar(config$countries_name) == 3)) {
    warning("Not all countries_names in the config are valid country iso code.")
  }
  as.character(config$countries_name)
}

#' clip_shapefiles_to_adm0
#'
#' Drops location periods that do not intersect the national boundary and
#' clips the others to it.
#'
#' @param iso_code ISO3 code
#' @param shapefiles sf object with location_period_id and geom
#' @param source,cache_dir passed to `get_multi_country_admin_units`
#' @return clipped sf object
#' @export
clip_shapefiles_to_adm0 <- function(iso_code, shapefiles, source = "cache",
                                    cache_dir = admin_units_cache_dir()) {
  adm0 <- get_multi_country_admin_units(iso_code = iso_code, admin_levels = 0,
                                        lps = shapefiles, source = source,
                                        cache_dir = cache_dir)
  sf::st_crs(adm0) <- sf::st_crs(shapefiles)
  adm0_geom <- sf::st_union(sf::st_make_valid(adm0))

  keep <- lengths(sf::st_intersects(shapefiles, adm0_geom)) > 0
  if (any(!keep)) {
    message("-- Dropping ", sum(!keep), " location periods that do not intersect the national ",
            "boundary: ", paste(shapefiles$location_period_id[!keep], collapse = ", "))
  }
  shapefiles <- shapefiles[keep, ]
  sf::st_geometry(shapefiles) <- sf::st_intersection(sf::st_geometry(shapefiles), adm0_geom)
  fix_geomcollections(shapefiles)
}
