
#' clip_shapefiles_to_adm0
#'
#' @param iso_code 
#' @param shapefiles 
#'
#' @return
#' @export
#'
#' @examples
clip_shapefiles_to_adm0 <- function(iso_code, 
                                    shapefiles) {
  # Clip shapefiles to the national level output shapefile
  adm0_geom <- get_multi_country_admin_units(
    iso_code = iso_code,
    admin_levels = c(0),
    lps = shapefiles
  ) 
  
  sf::st_crs(adm0_geom) <- sf::st_crs(shapefiles) ## same crs needed for st_intersection
  
  # Drop data with no intersections
  shapefiles <- shapefiles %>% 
    dplyr::mutate(adm0_intersect = sf::st_intersects(shapefiles$geom, adm0_geom$geom, sparse = FALSE) %>% 
                    as.vector())
  
  drop_shapefiles <- shapefiles %>% 
    dplyr::filter(!adm0_intersect)
  
  if (nrow(drop_shapefiles) > 0) {
    cat("-- Dropping", nrow(drop_shapefiles), "that do not intersect the national level output shapefile\n")
    shapefiles <- shapefiles %>% 
      dplyr::filter(!(location_period_id %in% drop_shapefiles$location_period_id))
  }
  
  shapefiles <- shapefiles %>% 
    dplyr::mutate(geom = sf::st_intersection(geom, adm0_geom$geom)) %>% 
    fix_geomcollections()
  
  shapefiles
}

#' drop_missing_shapefiles
#'
#' @param cases 
#' @param cases_column 
#'
#' @return
#' @export
#'
#' @examples
#' 
drop_missing_shapefiles <- function(cases,
                                    cases_column) {
  
  # Flag missing shapefiles
  cases <- cases %>%
    dplyr::mutate(shapefile.exists = !is.na(sf::st_dimension(geojson)) & 
                    (sf::st_dimension(geojson) > 0))
  
  if (sum(!cases$shapefile.exists) > 0) {
    warning("There was a problem with at least one shapefile. See output for details.")
    print(paste(sum(!cases$shapefile.exists), "of", length(cases$shapefile.exists), "observations have shapefile problems."))
    
    problem_cases <- dplyr::filter(cases, !shapefile.exists)
    problem_indices <- which(!cases$shapefile.exists)
    problem_OCs <- unique(problem_cases$OC_UID) ## relationships.observation_collection.data.id)
    
    # print(paste("The following indexes are affected:", paste(problem_indices, collapse = ", ")))
    print(paste("The following OC UIDs are affected:", paste(problem_OCs, collapse = ", ")))
    print(paste(sum(problem_cases[[cases_column]]), "/", sum(cases[[cases_column]]), cases_column, "are missing due to shapefile problems."))
    
    print("Observations attached to problematic location-periods will be ignored. Here are the problematic location-periods *****************")
    print(dplyr::select(problem_cases, location_name) %>%
            as.data.frame() %>%
            dplyr::distinct(location_name) %>%
            dplyr::arrange(location_name))
    
    cases <- cases %>%
      dplyr::filter(shapefile.exists)
    
    cases
  } else {
    cases
  }
}


#' make_shapefiles
#'
#' @param cases 
#'
#' @return
#' @export
#'
#' @examples
#' 
get_valid_shapefiles <- function(cases) {
  
  shapefiles <- cases %>%
    dplyr::group_by(attributes.location_period_id, location_name) %>%
    dplyr::slice(1) %>% 
    dplyr::rename(location_period_id = attributes.location_period_id) %>% 
    dplyr::ungroup()
  
  shapefiles$valid <- sf::st_is_valid(shapefiles)
  if (!all(shapefiles$valid)) {
    warning("At least one location period is invalid.  See output for details")
    print(paste(sum(!shapefiles$valid), "shapefiles were invalid."))
    print(
      paste("The following location periods were affected:", 
            paste(shapefiles$location_period_id[!shapefiles$valid], collapse = ", "))
    )
    shapefiles$geojson[!shapefiles$valid] <- sf::st_make_valid(shapefiles$geojson[!shapefiles$valid])
    shapefiles$valid <- sf::st_is_valid(shapefiles)
    print("An attempt was made to fix the invalid shapefiles")
  }
  
  ## Fix geometry collections
  shapefiles <- fix_geomcollections(shapefiles, geom_col = "geojson")
  
  # Make sf object to multipolygons to be consistent
  shapefiles <- sf::st_cast(shapefiles, "MULTIPOLYGON") %>%
    dplyr::rename(geom = geojson)
  
  # Enforce WGS84 to geoms
  sf::st_crs(shapefiles) <- 4326
  
  shapefiles <- shapefiles %>% 
    dplyr::ungroup() %>% 
    dplyr::select(location_period_id, location_name)
  
  
  # Check if there are any shapefiles that have problematic projections
  shapefiles <- fix_projection(shapefiles)
  
  shapefiles
}


#' Fix geometry collections
#'
#' @param shapefiles shapefiles to modify (sfc object)
#'
#' @return
#' @export
#'
fix_geomcollections <- function(shapefiles,
                                geom_col = "geom") {
  
  for (i in 1:nrow(shapefiles)) {
    
    tmp <- shapefiles[i,]
    
    if (stringr::str_detect(sf::st_geometry_type(tmp), "COLL")) {
      
      cat("---- Found GEOMETRYCOLLECTION, converting to MULTIPOLYGON. \n")
      
      new_geom <- tmp  %>% 
        sf::st_collection_extract(type = "POLYGON") %>% 
        dplyr::summarise(geom = sf::st_union(!!rlang::sym(geom_col)))
      
      # Overwrite the geometry
      sf::st_geometry(shapefiles[i, ]) <- sf::st_geometry(new_geom)
      sf::st_geometry(shapefiles) <- geom_col
    }
  }
  
  shapefiles
}


#' fix_projection
#'
#' @param shapefiles 
#'
#' @return
#' @export
#'
#' @examples
fix_projection <- function(shapefiles) {
  
  # Get all bounding boxes
  bboxes <- purrr::map_df(1:nrow(shapefiles), 
                          function(x) {
                            res <- sf::st_bbox(shapefiles[x, ]) 
                            res_vec <- as.vector(res)
                            names(res_vec) <- names(res)
                            as.data.frame(t(res_vec)) %>% 
                              dplyr::as_tibble()
                          }) %>% 
    dplyr::mutate(shapefile = dplyr::row_number())
  
  # Problematic shapefiles have unreasonable numbers in coordinates
  issues <- bboxes %>% 
    dplyr::filter(xmin < -180 | xmax > 180 | ymin < -90 | ymax > 90)
  
  # If no issues return
  if (nrow(issues) == 0) {
    return(shapefiles %>% 
             dplyr::mutate(reprojection_epsg = NA))
  }
  
  cat("---- Found", nrow(issues), "shapefiles with coordinate projection issues. Trying to fix. \n")
  
  # Get reference point to which to attempt to project to
  ref_shapefiles <- shapefiles[-issues$shapefile, ]
  
  
  fixed_shapefiles <- purrr::map_df(
    issues$shapefile,
    function(x) {
      
      this_issue <- shapefiles[x, ]
      
      # To be smarter try to use location nesting
      # This will eventually return the national level shapefile if 
      # no containing subnational units are in the data
      
      # Initialize
      new_ref <- ref_shapefiles
      for (i in 1:get_admin_level(this_issue$location_name)) {
        # Get the name of the nesting location
        upper_level <- move_up_location_hierarchy(this_issue$location_name, n_steps = i)
        # If present in data use this new reference
        new_ref <- ref_shapefiles %>% 
          dplyr::filter(location_name %in% upper_level)
        if (nrow(new_ref) > 0) {
          break()
        }
      }

      # Extract the centroid coordinates to use as target
      ref_coords <- sf::st_union(new_ref) %>% 
        sf::st_centroid() %>% 
        sf::st_coordinates()
      
      sf::st_crs(this_issue) <- NA
      
      # Try to guess
      crs_fix <- try(guess_crs_v2(input = this_issue,
                                  target_location = ref_coords,
                                  n_return = 1))
      
      if (inherits(crs_fix, "try-error")) {
        cat("-- Failed to fix crs for following shapefile\n")
        print(this_issue)
        
        sf::st_crs(this_issue) <- 4326
        this_issue <- this_issue %>% 
          dplyr::mutate(reprojection_epsg = "failed")
        
      } else {
        sf::st_crs(this_issue) <- crs_fix$crs_code %>% as.numeric()
        
        this_issue <- this_issue %>% 
          dplyr::mutate(reprojection_epsg = crs_fix$crs_code)
      }
      
      # Project back to WGS84
      this_issue <- sf::st_transform(this_issue, 4326)
      
      this_issue
    })
  
  failed_fixes <- fixed_shapefiles %>% 
    dplyr::filter(reprojection_epsg == "failed")
  
  if (nrow(failed_fixes) > 0) {
    cat("---- Failed re-projecting for", nrow(failed_fixes), "shapefiles. These will be dropped. \n")
    print(failed_fixes)
  } 
  
  # Indicate if any shapefile was reprojected
  dplyr::bind_rows(
    shapefiles[-issues$shapefile, ],
    fixed_shapefiles %>% 
      dplyr::filter(reprojection_epsg != "failed")
  ) 
}



#' move_up_location_hierarchy
#'
#' @param location_name 
#' @param n_steps 
#'
#' @return
#' @export
#'
#' @examples

move_up_location_hierarchy <- function(location_name, 
                                       n_steps) {
  # Get all levels
  levels <- stringr::str_split(location_name, "::")[[1]]
  n_levels <- length(levels)
  
  if (n_steps < n_levels){
    res <- stringr::str_c(levels[1:(n_levels - n_steps)], collapse = "::")
  } else {
    stop("Number of steps to move up is larger than available location depth.",
         "Please provied a number n_steps < ", n_levels)
  }
  
  res
}
