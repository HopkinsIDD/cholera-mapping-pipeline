#' @export
#' @name load_orig_map_mean_as_sf
#' @title load_orig_map_mean_as_sf
#' @description load original cholera data estimates (tif or grd), take mean, and return equivalent sf object
#' @param filename object representing the imported YAML config file for the model
#' @param column string for renaming the values column in sf object (typically "cases" or "rate")
#' @return sf object version of original mean cholera data estimates
load_orig_map_mean_as_sf <- function(filename, column){
  
  if(grepl(".tif$", filename)){ ## tif files on website have no crs
    orig <- raster::stack(filename)
  
  } else if(grepl(".grd$", filename)){
    orig <- raster::stack(filename)
     
  } else{
    stop(paste("Loading a file type of", filename, "is not supported"))
  }

  ## add missing CRS as needed
  if(is.na(raster::crs(orig))){
    ## this is the CRS for all of the old raster products
    raster::crs(orig) <- "+proj=longlat +datum=WGS84 +no_defs"
  }

  ## take mean across raster layers as needed
  if(raster::nlayers(orig)>1){
    orig <- raster::calc(orig, fun = mean, na.rm = TRUE)
  }

  new_spdf <- raster::rasterToPolygons(orig)
  new_sf <- sf::st_as_sf(new_spdf)
  
  ## rename default column name if appropriate
  if (!(column %in% names(new_sf))){
      tryCatch(
        {
          new_sf <- dplyr::rename(new_sf, !!column := layer)
          return(new_sf)
        },
        error = function(e) {
          print(paste("trycatch", e))
          return(new_sf)
        }
      )
  }

  return(new_sf)
}

