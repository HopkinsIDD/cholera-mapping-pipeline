# This script contains functions to plot output figures


# Colors ------------------------------------------------------------------
color_lake_fill <- function(){"#aad3df"}
color_lake_border <- function(){"#7fb4c4"}
color_rivers <- function(){"#7fb4c4"}

color_run_intended <- function(){c("#A1A1A1")}
color_no_run_intended <- function(){c("#E2FFDE")}
color_afr_continent_fill <- function(){c("#FFFFFF")}

colors_lisa_clusters <- function(){c("lightgray", "#D40A07", "#4543C4", "#F26F6D", "#7C7AC2", "#424141", "#A059F7")}
coloramp_cases <- function(){c("#FFFFFF", "#FED98E", "#FE9929", "#D95F0E", "#993404")}

# colors_admin_levels <- function(){c( "#4F802A", "#5449C7", "#BF0B07", "#DBB50B")}
#colors_admin_levels <- function(){c("#FFFF00","#CAE0C9", "#99CCFF", "#FF9999", "#CC9900", "#FF9933",'black')}
colors_admin_levels <- function(){RColorBrewer::brewer.pal(n=4,name = 'Blues')}
colors_endemicity_high <- function(){c("red", "gray")}
colors_endemicity_low <- function(){c("blue", "gray")}
# colors_endemicity <- function(){c("#FF0000", "#E65F5F", "#F2A8A7", "#837EE6")} #"#0C14ED"
colors_endemicity <- function(){
  # Based on cases
  c("#993404", "#D95F0E", "#FED98E", "gray")
}


colors_periods <- function(){c("purple", "orange")}

colors_risk_categories <- function() {
  c( paletteer::paletteer_d("fishualize::Epinephelus_striatus", direction = 1),"gray")
}

#' @export
colors_afro_regions <- function(){
  # colors <- RColorBrewer::brewer.pal("Set2", n = 4)
  colors <- c("#FFA378", "#A8B545", "#8C796D", "#024554")
  names(colors) <- c("Western Africa", "Central Africa",
                     # "Eastern Mediterranean",
                     "Eastern Africa", "Southern Africa")
  colors
}

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
                            lakes_sf = NULL,
                            rivers_sf = NULL,
                            fill_var,
                            fill_color_scale_type,
                            border_width = 0.005,
                            border_color = "white",
                            lake_alpha = 1,
                            country_border_width = .3,
                            country_border_color = "black",
                            cholera_dir = 'cholera-mapping-pipeline') {
  
  sf_obj %>% 
    ggplot2::ggplot(aes(fill = !!sym(fill_var))) +
    ggplot2::geom_sf(data = all_countries_sf %>%
                       dplyr::filter(!intended_run),
                     inherit.aes = FALSE,
                     lwd = 0,
                     alpha = 1,
                     fill = color_no_run_intended()) +
    ggplot2::geom_sf(data = all_countries_sf %>% 
                       dplyr::filter(intended_run),
                     inherit.aes = FALSE,
                     lwd = 0,
                     alpha = 1,
                     fill = color_run_intended()) +
    ggplot2::geom_sf(lwd = border_width, color = border_color) + 
    ggplot2::geom_sf(data = all_countries_sf,
                     fill = color_afr_continent_fill(),
                     color = "black",
                     lwd = country_border_width,
                     alpha = 0) +
    {
      if (!is.null(rivers_sf)) {
        ggplot2::geom_sf(inherit.aes = FALSE,
                         data = rivers_sf,
                         color = color_rivers(),
                         lwd = .1,
                         alpha = 1)
      }
    } +
    {
      if (!is.null(lakes_sf)) {
        ggplot2::geom_sf(inherit.aes = FALSE,
                         data = lakes_sf, 
                         fill = color_lake_fill(),
                         color = color_lake_border(),
                         lwd = .06,
                         alpha = lake_alpha)
      }
    } +
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
        scale_fill_manual(values = colors_risk_categories())
      } else if(fill_color_scale_type == "lisa cluster") {
        scale_fill_manual(values = colors_lisa_clusters())
      } else if(fill_color_scale_type == "endemicity_high") {
        scale_fill_manual(values = colors_endemicity_high())
      } else if(fill_color_scale_type == "endemicity_low") {
        scale_fill_manual(values = colors_endemicity_low())
      } else if(fill_color_scale_type == "endemicity") {
        scale_fill_manual(values = colors_endemicity(),drop=FALSE)
      } else if(fill_color_scale_type == "admin levels") {
        scale_fill_manual(values = colors_admin_levels(),drop=FALSE)
      }
    } + 
    ggplot2::geom_sf(data = all_countries_sf,
                     inherit.aes = FALSE,
                     lwd = country_border_width,
                     color = country_border_color,
                     alpha = 0) +
    taxdat::map_theme() +
    # Zoom to bounding box
    # ggplot2::coord_sf(xlim = st_bbox(sf_obj)[c(1, 3)],
    #                   ylim = st_bbox(sf_obj)[c(5, 6)]) +
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
plot_posterior_coverage <- function(gen_obs,
                                    with_period = FALSE) {
  gen_obs %>% 
    dplyr::filter(censoring == "full") %>% 
    get_coverage(with_period = with_period) %>% 
    dplyr::mutate(admin_level = factor(admin_level, levels = 0:10)) %>% 
    ggplot2::ggplot(aes(x = cri, y = frac_covered, color = admin_level)) +
    ggplot2::geom_line(aes(lty = admin_level), lwd = 1) +
    geom_abline(intercept = 0, slope = 1, colour = "darkgray",linetype=2) +
    {
      if (!with_period) {
        ggplot2::facet_wrap(~ country)
      } else {
        ggplot2::facet_grid(country ~ period)
      }
    } +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "CrI width", y = "Fraction of full observations covered",
                  color = "Admin level", lty = "Admin level") +
    ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(n=7,name = 'Blues')[-1])
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
    dplyr::filter(Shape_area>.15) %>% 
    rmapshaper::ms_simplify(keep = 0.1,
                            keep_shapes = FALSE) %>% 
    sf::st_make_valid()
  
  lakes_sf
}


#' @title Get rivers
#'
#' @description Gets rivers in SSA from
#' https://data.apps.fao.org/catalog/iso/b891ca64-4cd4-4efd-a7ca-b386e98d52e8
#'
#' @param path to data file
#' @param stream_order maximum A_Strahler order to keep (larger keeps smaller rivers) 
#'
#' @return an sf_object
#' @export
#' 
get_rivers <- function(path = "Layers/geodata/rivers_africa_37333.shp",
                       stream_order = 4) {
  
  if (!file.exists(path)) {
    stop("Coudn't find shapefile for rivers. ",
         "Please download from https://storage.googleapis.com/fao-maps-catalog-data/geonetwork/aquamaps/rivers_africa_37333.zip and save in folder Layers/geodata/",
         "or provied path to folder")
  }
  
  rivers_sf <- sf::st_read(path) %>% 
    dplyr::filter(A_Strahler <= stream_order) %>% 
    rmapshaper::ms_simplify(keep = 0.1,
                            keep_shapes = FALSE)
  
  rivers_sf
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
get_risk_cat_dict <- function() {
  risk_cat_dict <- c("<1", "1-10", "10-20", "20-50", "50-100", "\u2265100")
  risk_cat_dict
}
