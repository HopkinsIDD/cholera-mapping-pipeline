# This is a re-purpousing of the functions in crsuggest
# https://github.com/walkerke/crsuggest
# This is basically a copy of the code except for line
# sf::st_crs(crs_type) <- sf::st_crs("EPSG:32663")


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
  
  if (is.na(sf::st_crs(input))) {
    stop("Your dataset is missing an existing CRS definition.\nEither assign an appropriate CRS to your dataset or find one with\nthe crsuggest::guess_crs() function.", 
         call. = FALSE)
  }
  if (inherits(input, "RasterLayer") || inherits(input, "SpatRaster") || 
      inherits(input, "SpatVector")) {
    input <- input %>% sf::st_bbox() %>% sf::st_as_sfc()
  }
  if (any(grepl("Spatial", class(input)))) {
    input <- sf::st_as_sf(input)
  }
  if (inherits(input, "sfc")) {
    input <- sf::st_sf(input)
  }
  
  crs_type <- dplyr::filter(taxdat:::crs_sf, crs_type == type)
  sf::st_crs(crs_type) <- sf::st_crs("EPSG:32663")
  
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
  
  sf_proj <- sf::st_transform(input, sf::st_crs(crs_type))
  geom_type <- unique(sf::st_geometry_type(sf_proj))
  
  if (length(geom_type) > 1) {
    geom_buf <- sf::st_buffer(sf::st_union(sf_proj), 100)
    geom_type <- unique(sf::st_geometry_type(geom_buf))
    sf_proj <- sf::st_sf(geom_buf)
  }
  
  if (geom_type %in% c("POINT", "MULTIPOINT")) {
    if (nrow(sf_proj) %in% 1:2) {
      sf_proj <- sf::st_buffer(sf_proj, 1000)
    }
    sf_poly <- sf_proj %>% sf::st_union() %>% sf::st_convex_hull()
  } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
    sf_poly <- sf_proj %>% sf::st_cast("MULTIPOINT") %>% sf::st_union() %>% 
      sf::st_convex_hull()
  } else if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
    sf_poly <- sf_proj %>% sf::st_union()
  }
  
  reverse_buf <- sf::st_buffer(sf_poly, -500)
  crs_sub <- crs_type[reverse_buf, ]
  
  if (nrow(crs_sub) == 0) {
    rows <- nrow(crs_sub)
    bufdist <- -250
    while (rows == 0) {
      new_buf <- sf::st_buffer(sf_poly, bufdist)
      crs_sub <- crs_type[new_buf, ]
      rows <- nrow(crs_sub)
      bufdist <- bufdist/2
    }
  }
  vertex_count <- npts(sf_poly)
  sf_poly2 <- sf_poly
  if (vertex_count > 500) {
    tol <- 5000
    vc <- vertex_count
    previous_vc <- vc
    while (vc > 500) {
      sf_poly <- sf::st_simplify(sf_poly, dTolerance = tol)
      if (sf::st_is_empty(sf_poly)) {
        vc <- 499
        sf_poly <- sf_poly2
      }
      else {
        vc <- npts(sf_poly)
        tol <- tol * 2
      }
      if (vc == previous_vc) 
        break
      previous_vc <- vc
    }
  }
  crs_output <- crs_sub %>% 
    dplyr::mutate(hausdist = as.numeric(sf::st_distance(sf_poly, 
                                                        ., which = "Hausdorff"))) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::arrange(hausdist, desc(crs_code)) %>%
    dplyr::filter(dplyr::row_number() <= 
                    limit) %>% 
    dplyr::select(-hausdist)
  
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
guess_crs_v2 <- function(input, 
                         target_location, 
                         units = NULL, 
                         n_return = 10,
                         input_sf = NULL) {
  
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
  } else if (is.numeric(target_location)) {
    if (length(target_location) != 2) {
      stop("Please supply a length-2 vector representing the coordinates of format `c(lon, lat)`.", 
           call. = FALSE)
    }
    else {
      target_coords <- target_location
    }
  }
  
  target_sf <- target_coords %>% 
    sf::st_point() %>% 
    sf::st_sfc(crs = 4326) %>% 
    sf::st_sf()
  
  crs_options <- suggest_crs_v2(target_sf, 
                                limit = 50, 
                                units = units) %>% 
    dplyr::filter(!is.na(crs_units))
  
  if (inherits(input, "RasterLayer") || inherits(input, "SpatRaster") || 
      inherits(input, "SpatVector")) {
    input <- input %>% sf::st_bbox() %>% sf::st_as_sfc()
  }
  if (any(grepl("Spatial", class(input)))) {
    input <- sf::st_as_sf(input)
  }
  if (inherits(input, "sfc")) {
    input <- sf::st_sf(input)
  }
  no_crs_centroid <- suppressMessages(suppressWarnings(sf::st_centroid(sf::st_union(input))))
  codes <- crs_options$crs_code
  message("Evaluating CRS options...")
  
  dist_df <- purrr::map_df(codes, function(code) {
    target_sf_transformed <- try(sf::st_transform(target_sf, 
                                                  as.integer(code)))
    
    if (!inherits(target_sf_transformed, "try-error")) {
      centroid_with_crs <- sf::st_set_crs(no_crs_centroid, 
                                          as.integer(code))
      dist <- sf::st_distance(target_sf_transformed, centroid_with_crs) %>% 
        units::set_units("km") %>% as.numeric()
      
      dplyr::tibble(crs_code = code, 
                    dist_km = dist)
    }
  }) %>% 
    dplyr::arrange(dist_km, dplyr::desc(crs_code)) %>% 
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



# From mapview
npts <- function(x) {
  sum(nVerts(sf::st_geometry(x)))
}

nVerts <- function (x) {
  out = if (is.list(x)) 
    sapply(sapply(x, nVerts), sum)
  else {
    if (is.matrix(x)) 
      nrow(x)
    else {
      if (sf::st_is_empty(x)) 
        0
      else 1
    }
  }
  unname(out)
}