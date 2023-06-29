# This script contains functions to plot output figures


# Colors ------------------------------------------------------------------
color_lake_fill <- function(){"#aad3df"}
color_lake_border <- function(){"#7fb4c4"}
color_run_intended <- function(){c("#A1A1A1")}
color_no_run_intended <- function(){c("#E2FFDE")}


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
                            all_countries_sf,
                            lakes_sf = get_lakes(),
                            fill_var,
                            fill_color_scale_type) {
  
  sf_obj %>% 
    ggplot2::ggplot(aes(fill = !!sym(fill_var))) +
    ggplot2::geom_sf(data = all_countries_sf %>% 
                       dplyr::filter(!intended_run),
                     inherit.aes = FALSE,
                     linewidth = 0,
                     alpha = 1,
                     fill = color_no_run_intended()) +
    ggplot2::geom_sf(data = all_countries_sf %>% 
                       dplyr::filter(intended_run),
                     inherit.aes = FALSE,
                     linewidth = 0,
                     alpha = 1,
                     fill = color_run_intended()) +
    ggplot2::geom_sf(linewidth = .02, color = "white") + 
    ggplot2::geom_sf(data = lakes_sf, fill = color_lake_fill(),
                     color = color_lake_border(), 
                     linewidth = .06,
                     alpha = .6) +
    ggplot2::geom_sf(data = all_countries_sf,
                     inherit.aes = FALSE,
                     linewidth = .3,
                     alpha = 0) +
    {  
      if(fill_color_scale_type == "rates") {
        scale_fill_viridis_c(breaks = seq(-1, 2), 
                             labels = formatC(10^(seq(-1, 2)),
                                              digits = 1,
                                              format = "fg", 
                                              big.mark = ",") %>% 
                               {
                                 x <- .
                                 x[1] <- str_c("<= ", x[1])
                                 x[length(x)] <- str_c(">= ", x[length(x)])
                                 x
                               },
                             limits = c(-1, 2.5),
                             option = "plasma",
                             oob = scales::squish)
      } else if(fill_color_scale_type == "ratio") {
        scale_fill_gradient2(breaks = seq(-3, 2), 
                             labels = formatC(10^(seq(-3, 2)),
                                              digits = 1,
                                              format = "fg", 
                                              big.mark = ","),
                             limits = c(-3.1, 2.1),
                             midpoint = 0,
                             oob = scales::squish, 
                             na.value = c("#D4BE77"), 
                             low = "blue",
                             high = "red")
      } else if(fill_color_scale_type == "cases") {
        scale_fill_gradientn(colours = c("#FFFFFF", "#FED98E", "#FE9929", "#D95F0E", "#993404"),
                             oob = scales::censor, 
                             limits = c(0, NA), 
                             breaks = seq(0, 3), 
                             labels = formatC(10^(seq(0, 3)),
                                              digits = 1,
                                              format = "fg", 
                                              big.mark = ","),
                             na.value = "lightgray")
        
      } else if(fill_color_scale_type == "risk category") {
        scale_fill_viridis_d()
      }
    } +
    taxdat::map_theme() +
    # Zoom to bounding box
    ggplot2::coord_sf(xlim = st_bbox(sf_obj)[c(1, 3)],
                      ylim = st_bbox(sf_obj)[c(2, 4)]) +
    theme(panel.border = element_blank())
  
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
#' @export
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


#' Title
#'
#' @return
#' @export
#'
#' @examples
get_risk_cat_dict <- function() {
  risk_cat_dict <- c("<1", "1-10", "10-20", "20-50", "50-100", ">100")
}

