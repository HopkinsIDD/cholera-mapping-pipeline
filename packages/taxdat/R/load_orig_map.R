#' @export
#' @name load_orig_map
#' @title load_orig_map
#' @description load original cholera data estimates (tif or grd) as raster
#' @param filename object representing the imported YAML config file for the model
#' @return raster of mean or samples of cholera data estimates
load_orig_map <- function(filename){
  
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

  return(orig)
}

