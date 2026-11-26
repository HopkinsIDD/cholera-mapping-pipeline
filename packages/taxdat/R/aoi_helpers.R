# Area of interest (AOI): the country boundary plus a buffer, used to crop and
# mask every raster before it enters the covariates database.

#' @title AOI tag
#' @name aoi_tag
#' @param iso_code ISO3 code or "raw"
#' @param buffer_km buffer in km
#' @return short tag such as "bdi_b50km", or "raw"
#' @export
aoi_tag <- function(iso_code, buffer_km) {
  if (is.null(iso_code) || identical(tolower(iso_code), "raw")) {
    return("raw")
  }
  sprintf("%s_b%skm", tolower(iso_code), format(buffer_km, trim = TRUE))
}

#' @title Get area of interest
#' @name get_aoi
#' @description National boundary of one country, buffered, with its bounding
#' box. The buffer is computed in a local Lambert azimuthal equal-area
#' projection so it is in true kilometres.
#'
#' @param iso_code ISO3 code, or "raw" for no area of interest
#' @param buffer_km buffer around the national boundary, in km
#' @param cache_dir admin-units cache (see `admin_units_cache_dir`)
#' @param snap_to optional raster file whose pixel lattice the extent is snapped
#'   to (outwards), so every cropped product shares the same pixel edges
#' @param source admin-units source, see `get_multi_country_admin_units`
#' @return NULL for "raw"; otherwise a list with name, iso_code, buffer_km,
#'   extent (SpatExtent), bbox (named numeric), polygon (sf), vect (SpatVector)
#'   and hash
#' @export
get_aoi <- function(iso_code, buffer_km = 50, cache_dir = admin_units_cache_dir(),
                    snap_to = NULL, source = "cache") {
  if (is.null(iso_code) || identical(tolower(iso_code), "raw")) {
    return(NULL)
  }
  if (!is.numeric(buffer_km) || length(buffer_km) != 1 || buffer_km < 0) {
    stop("buffer_km must be a single non-negative number")
  }
  adm0 <- get_multi_country_admin_units(iso_code = iso_code, admin_levels = 0,
                                        clip_to_adm0 = FALSE, source = source,
                                        cache_dir = cache_dir)
  geom <- sf::st_union(sf::st_make_valid(sf::st_geometry(adm0)))

  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  ctr <- suppressWarnings(sf::st_coordinates(sf::st_centroid(geom)))
  laea <- sprintf("+proj=laea +lat_0=%f +lon_0=%f +units=m +datum=WGS84", ctr[2], ctr[1])
  buffered <- geom %>%
    sf::st_transform(laea) %>%
    sf::st_buffer(buffer_km * 1000) %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid()

  bb <- sf::st_bbox(buffered)
  ext <- terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]])
  if (!is.null(snap_to)) {
    ext <- terra::align(ext, terra::rast(snap_to), snap = "out")
  }
  polygon <- sf::st_sf(iso_code = toupper(iso_code), geom = buffered)

  list(name = aoi_tag(iso_code, buffer_km),
       iso_code = toupper(iso_code),
       buffer_km = buffer_km,
       extent = ext,
       bbox = c(xmin = terra::xmin(ext), xmax = terra::xmax(ext),
                ymin = terra::ymin(ext), ymax = terra::ymax(ext)),
       polygon = polygon,
       vect = terra::vect(polygon),
       hash = digest::digest(list(sf::st_as_text(buffered), as.vector(ext)), algo = "md5"))
}

#' @title Crop and mask a raster to an area of interest
#' @name crop_mask_to_aoi
#' @param r SpatRaster
#' @param aoi output of `get_aoi`, or NULL (returned unchanged)
#' @param mask also set cells outside the buffered polygon to NA
#' @return SpatRaster
#' @export
crop_mask_to_aoi <- function(r, aoi, mask = TRUE) {
  if (is.null(aoi)) {
    return(r)
  }
  r <- terra::crop(r, aoi$extent, snap = "out")
  if (mask) {
    r <- terra::mask(r, aoi$vect, touches = TRUE)
  }
  r
}

#' @title AOI description for metadata
#' @name aoi_metadata
#' @param aoi output of `get_aoi`, or NULL
#' @return one-row data frame with aoi_name, aoi_buffer_km and bbox columns
#' @export
aoi_metadata <- function(aoi) {
  if (is.null(aoi)) {
    return(data.frame(aoi_name = "raw", aoi_buffer_km = NA_real_,
                      aoi_xmin = NA_real_, aoi_xmax = NA_real_,
                      aoi_ymin = NA_real_, aoi_ymax = NA_real_))
  }
  data.frame(aoi_name = aoi$name, aoi_buffer_km = aoi$buffer_km,
             aoi_xmin = aoi$bbox[["xmin"]], aoi_xmax = aoi$bbox[["xmax"]],
             aoi_ymin = aoi$bbox[["ymin"]], aoi_ymax = aoi$bbox[["ymax"]])
}
