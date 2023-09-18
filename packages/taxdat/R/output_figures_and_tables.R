# This script contains functions to plot output figures


# Colors ------------------------------------------------------------------
color_lake_fill <- function(){"#aad3df"}
color_lake_border <- function(){"#7fb4c4"}
color_run_intended <- function(){c("#A1A1A1")}
color_no_run_intended <- function(){c("#E2FFDE")}
color_afr_continent_fill <- function(){c("#FFFFFF")}

colors_lisa_clusters <- function(){c("lightgray", "#D40A07", "#4543C4", "#F26F6D", "#7C7AC2", "#424141", "#A059F7")}
coloramp_cases <- function(){c("#FFFFFF", "#FED98E", "#FE9929", "#D95F0E", "#993404")}

# colors_admin_levels <- function(){c( "#4F802A", "#5449C7", "#BF0B07", "#DBB50B")}
colors_admin_levels <- function(){c("#CAE0C9", "#99CCFF", "#FF9999", "#CC9900", "#FF9933",'black')}

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
                            fill_color_scale_type,
                            border_width = 0.02,
                            border_color = "white",
                            lake_alpha = 0.6,
                            country_border_width = .3,
                            country_border_color = "black",
                            cholera_dir = 'cholera-mapping-pipeline') {
  
  afr_sf <- sf::st_read(paste(cholera_dir,"packages/taxdat/data/afr_sf_cleaned.shp",sep="/"))
  
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
    ggplot2::geom_sf(linewidth = border_width, color = border_color) + 
    ggplot2::geom_sf(data = lakes_sf, fill = color_lake_fill(),
                     color = color_lake_border(), 
                     linewidth = .06,
                     alpha = lake_alpha) +
    ggplot2::geom_sf(data = afr_sf,
                     fill = color_afr_continent_fill(),
                     color = "black",
                     linewidth = country_border_width,
                     alpha = 0) +
    ggplot2::geom_sf(data = all_countries_sf,
                     inherit.aes = FALSE,
                     linewidth = country_border_width,
                     color = country_border_color,
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
        scale_fill_gradientn(colours = coloramp_cases(),
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
      } else if(fill_color_scale_type == "lisa cluster") {
        scale_fill_manual(values = colors_lisa_clusters())
      } else if(fill_color_scale_type == "admin levels") {
        scale_fill_manual(values = colors_admin_levels())
      }
    } +
    taxdat::map_theme() +
    # Zoom to bounding box
    ggplot2::coord_sf(xlim = st_bbox(sf_obj)[c(1, 3)],
                      ylim = st_bbox(sf_obj)[c(5, 6)]) +
    theme(panel.border = element_blank())
  
}


#' plot_posterior_coverage
#'
#' @param gen_obs 
#'
#' @return
#' @export
#'
#' @examples
plot_posterior_coverage <- function(gen_obs) {
  gen_obs %>% 
    dplyr::filter(censoring == "full") %>% 
    get_coverage() %>% 
    dplyr::mutate(admin_level = factor(admin_level, levels = 0:10)) %>% 
    ggplot2::ggplot(aes(x = cri, y = frac_covered, color = admin_level)) +
    ggplot2::geom_line(aes(lty = admin_level), linewidth = 1) +
    ggplot2::facet_wrap(~ country) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "CrI width", y = "Fraction of observations covered",
                  color = "Admin level", lty = "Admin level") +
    ggplot2::scale_color_manual(values = colors_admin_levels())
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

