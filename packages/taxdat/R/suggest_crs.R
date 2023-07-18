# This is a re-purpousing of the functions in crsuggest
# https://github.com/walkerke/crsuggest
# This is basically a copy of the code except for line
# st_crs(crs_type) <- st_crs("EPSG:32663")


#' Title
#'
#' @param input 
#' @param type 
#' @param limit 
#' @param gcs 
#' @param units 
#' @param drop_na 
#'
#' @return
#' @export
#'
#' @examples
suggest_crs_v2 <- function (input, 
                            type = "projected", 
                            limit = 10, 
                            gcs = NULL, 
                            units = NULL, 
                            drop_na = TRUE) {
  
  if (is.na(st_crs(input))) {
    stop("Your dataset is missing an existing CRS definition.\nEither assign an appropriate CRS to your dataset or find one with\nthe crsuggest::guess_crs() function.", 
         call. = FALSE)
  }
  if (inherits(input, "RasterLayer") || inherits(input, "SpatRaster") || 
      inherits(input, "SpatVector")) {
    input <- input %>% st_bbox() %>% st_as_sfc()
  }
  if (any(grepl("Spatial", class(input)))) {
    input <- st_as_sf(input)
  }
  if (inherits(input, "sfc")) {
    input <- st_sf(input)
  }
  crs_type <- dplyr::filter(crsuggest::crs_sf, crs_type == 
                              type)
  st_crs(crs_type) <- st_crs("EPSG:32663")
  
  if (drop_na) {
    crs_type <- dplyr::filter(crs_type, !is.na(crs_proj4))
  }
  if (!is.null(gcs)) {
    gcs <- as.character(gcs)
    crs_type <- dplyr::filter(crs_type, crs_gcs == gcs)
  }
  if (!is.null(units)) {
    if (!units %in% c("ft", "m", "us-ft")) {
      stop("Units must be one of 'm', 'ft', or 'us-ft'")
    }
    crs_type <- dplyr::filter(crs_type, crs_units == units)
  }
  sf_proj <- st_transform(input, st_crs(crs_type))
  geom_type <- unique(st_geometry_type(sf_proj))
  if (length(geom_type) > 1) {
    geom_buf <- st_buffer(st_union(sf_proj), 100)
    geom_type <- unique(st_geometry_type(geom_buf))
    sf_proj <- st_sf(geom_buf)
  }
  if (geom_type %in% c("POINT", "MULTIPOINT")) {
    if (nrow(sf_proj) %in% 1:2) {
      sf_proj <- st_buffer(sf_proj, 1000)
    }
    sf_poly <- sf_proj %>% st_union() %>% st_convex_hull()
  }
  else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
    sf_poly <- sf_proj %>% st_cast("MULTIPOINT") %>% st_union() %>% 
      st_convex_hull()
  }
  else if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
    sf_poly <- sf_proj %>% st_union()
  }
  reverse_buf <- st_buffer(sf_poly, -500)
  crs_sub <- crs_type[reverse_buf, ]
  if (nrow(crs_sub) == 0) {
    rows <- nrow(crs_sub)
    bufdist <- -250
    while (rows == 0) {
      new_buf <- st_buffer(sf_poly, bufdist)
      crs_sub <- crs_type[new_buf, ]
      rows <- nrow(crs_sub)
      bufdist <- bufdist/2
    }
  }
  vertex_count <- mapview::npts(sf_poly)
  sf_poly2 <- sf_poly
  if (vertex_count > 500) {
    tol <- 5000
    vc <- vertex_count
    previous_vc <- vc
    while (vc > 500) {
      sf_poly <- st_simplify(sf_poly, dTolerance = tol)
      if (st_is_empty(sf_poly)) {
        vc <- 499
        sf_poly <- sf_poly2
      }
      else {
        vc <- mapview::npts(sf_poly)
        tol <- tol * 2
      }
      if (vc == previous_vc) 
        break
      previous_vc <- vc
    }
  }
  crs_output <- crs_sub %>% dplyr::mutate(hausdist = as.numeric(st_distance(sf_poly, 
                                                                            ., which = "Hausdorff"))) %>% st_drop_geometry() %>% 
    dplyr::arrange(hausdist, desc(crs_code)) %>% dplyr::filter(dplyr::row_number() <= 
                                                                 limit) %>% dplyr::select(-hausdist)
  return(crs_output)
}


#' Title
#'
#' @param input 
#' @param target_location 
#' @param units 
#' @param n_return 
#' @param input_sf 
#'
#' @return
#' @export
#'
#' @examples
guess_crs_v2 <- function (input, 
                          target_location, 
                          units = NULL, 
                          n_return = 10,
                          input_sf = NULL) 
{
  if (!is.null(input_sf)) {
    stop("The `input_sf` argument has been renamed to `input`, please adjust your code accordingly and re-run.", 
         call. = FALSE)
  }
  if (!is.na(sf::st_crs(input)$epsg)) {
    stop("Your data already has a CRS set; perhaps you want `crsuggest::suggest_crs()` instead.", 
         call. = FALSE)
  }
  original_input <- input
  if (is.character(target_location)) {
    check_mapbox <- try(find.package("mapboxapi"), silent = TRUE)
    if ("try-error" %in% class(check_mapbox)) {
      stop("The mapboxapi package is used for geocoding functionality; please install and set up mapboxapi or supply a coordinate pair instead.", 
           call. = FALSE)
    }
    else {
      target_coords <- mapboxapi::mb_geocode(target_location)
    }
  }
  else if (is.numeric(target_location)) {
    if (length(target_location) != 2) {
      stop("Please supply a length-2 vector representing the coordinates of format `c(lon, lat)`.", 
           call. = FALSE)
    }
    else {
      target_coords <- target_location
    }
  }
  target_sf <- target_coords %>% st_point() %>% st_sfc(crs = 4326) %>% 
    st_sf()
  crs_options <- suggest_crs_v2(target_sf, limit = 50, 
                                units = units) %>% dplyr::filter(!is.na(crs_units))
  if (inherits(input, "RasterLayer") || inherits(input, "SpatRaster") || 
      inherits(input, "SpatVector")) {
    input <- input %>% st_bbox() %>% st_as_sfc()
  }
  if (any(grepl("Spatial", class(input)))) {
    input <- st_as_sf(input)
  }
  if (inherits(input, "sfc")) {
    input <- st_sf(input)
  }
  no_crs_centroid <- suppressMessages(suppressWarnings(st_centroid(st_union(input))))
  codes <- crs_options$crs_code
  message("Evaluating CRS options...")
  dist_df <- purrr::map_df(codes, ~{
    target_sf_transformed <- sf::st_transform(target_sf, 
                                              as.integer(.x))
    centroid_with_crs <- sf::st_set_crs(no_crs_centroid, 
                                        as.integer(.x))
    dist <- sf::st_distance(target_sf_transformed, centroid_with_crs) %>% 
      units::set_units("km") %>% as.numeric()
    dplyr::tibble(crs_code = .x, dist_km = dist)
  }) %>% dplyr::arrange(dist_km, dplyr::desc(crs_code)) %>% 
    dplyr::slice_min(dist_km, n = n_return)
  top_crs <- dist_df$crs_code[1]
  if (inherits(original_input, "sf") || inherits(original_input, 
                                                 "sfc")) {
    message(sprintf("The 'best guess' for the CRS of your data is EPSG code %s.\nUse `sf::st_crs(your_data) <- %s` to use this CRS for your data.\nView the returned dataset for other possible options.", 
                    top_crs, top_crs))
  }
  else if (inherits(original_input, "SpatRaster") || inherits(original_input, 
                                                              "SpatVector")) {
    message(sprintf("The 'best guess' for the CRS of your data is EPSG code %s.\nUse `terra::crs(your_data) <- 'EPSG:%s'` to use this CRS for your data.\nView the returned dataset for other possible options.", 
                    top_crs, top_crs))
  }
  else if (any(grepl("Spatial", class(original_input)))) {
    message(sprintf("The 'best guess' for the CRS of your data is EPSG code %s.\nUse `proj4string(your_data) <- 'CRS('+init=epsg:%s')` to use this CRS for your data.\nView the returned dataset for other possible options.", 
                    top_crs, top_crs))
  }
  else if (inherits(original_input, "RasterLayer")) {
    message(sprintf("The 'best guess' for the CRS of your data is EPSG code %s.\nUse `raster::crs(your_data) <- %s` to use this CRS for your data.\nView the returned dataset for other possible options.", 
                    top_crs, top_crs))
  }
  return(dist_df)
}


