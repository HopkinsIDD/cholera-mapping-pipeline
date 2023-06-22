# This script contains functions to plot output figures


# Colors ------------------------------------------------------------------
color_lake_fill <- function(){"#aad3df"}
color_lake_broder <- function(){"#7fb4c4"}


# Figure functions --------------------------------------------------------

#' output_plot_map
#'
#' @param sf_obj 
#' @param country_borders 
#' @param lakes_sf 
#' @param fill_var 
#' @param fill_color_scale_type 
#'
#' @return
#' @export
#'
#' @examples
output_plot_map <- function(sf_obj,
                            country_borders,
                            lakes_sf = get_lakes(),
                            fill_var,
                            fill_color_scale_type) {
  
  sf_obj %>% 
    ggplot(aes(fill = !!sym(fill_var))) +
    geom_sf() + 
    geom_sf(data = lakes_sf, fill = color_lake_fill(),
            color = color_lake_broder(), 
            size = .06) +
    geom_sf(data = country_borders,
            inherit.aes = FALSE,
            linewidth = .5,
            alpha = 0) +
    taxdat::color_scale(type = fill_color_scale_type, 
                        use_case = "ggplot map", 
                        use_log = TRUE) + 
    taxdat::map_theme() +
    # Zoom to bounding box
    coord_sf(xlim = st_bbox(sf_obj)[c(1, 3)],
             ylim = st_bbox(sf_obj)[c(2, 4)])
  
}

# Auxiliary functions ----------------------------------------------------

#' @title Get Lakes
#'
#' @description Gets large waterbodies in SSA from
#' https://datacatalog.worldbank.org/dataset/africa-water-bodies-2015
#'
#' @param path to data file
#'
#' @return an sf_object
#' 
get_lakes <- function(path = "Layers/geodata/Africa_waterbody.shp") {
  
  if (!file.exists(path)) {
    stop("Coudn't find shapefile for lakes. ",
         "Please download from https://datacatalog.worldbank.org/dataset/africa-water-bodies-2015 and save in folder Layers/geodata/",
         "or provied path to folder")
  }
  
  lakes_sf <- sf::st_read(path) %>% 
    dplyr::filter(Shape_area>.5) %>% 
    rmapshaper::ms_simplify(keep = 0.1,
                            keep_shapes = FALSE)
  
  lakes_sf
}


