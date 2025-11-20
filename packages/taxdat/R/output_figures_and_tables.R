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

coloramp_num_years_exceeding_threshold <- function(){
  colors <- c(
    "#D3D3D3",  
    "#E0F2E9",  
    "#A8D5BA",  
    "#6FBF73",  
    "#388E3C",  
    "#1B5E20"   
  )
  names(colors) <- c("0", "1", "2", "3", "4", "5")
  colors
}

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

#' @export
colors_global_regions <- function(){
  # colors <- RColorBrewer::brewer.pal("Set2", n = 4)
  colors <- c("#FFA378", "#A8B545", "#8C796D", "#024554","#C93E3E","#E1AF00","#6A5ACA","#009999","#7B7B7B")
  names(colors) <- c("Western Africa", "Central Africa",
                     "Eastern Africa", "Southern Africa",
                     "Americas","South-East Asia","Eastern Mediterranean","Western Pacific","Europe")
  colors
}

#' @export
colors_ranking <- function() {
  c("2011-2015" = "purple", 
    "2016-2020" = "orange", 
    "2011-2020" = "darkgreen", 
    "2022-2023"= "darkgray",
    "optimal"= "gray")
}

# Crop background and expand margins functions --------------------------------------------------------
#' expand_bbox_ratio
#'
#' @param bb 
#' @param ratio
#' @return
#' @export
expand_bbox_ratio <- function(bb, ratio = 0.06) {
  w <- bb["xmax"] - bb["xmin"] 
  h <- bb["ymax"] - bb["ymin"]
  dx <- as.numeric(w) * ratio 
  dy <- as.numeric(h) * ratio
  bb["xmin"] <- bb["xmin"] - dx 
  bb["xmax"] <- bb["xmax"] + dx
  bb["ymin"] <- bb["ymin"] - dy 
  bb["ymax"] <- bb["ymax"] + dy
  bb
}

#' crop_polygon
#'
#' @param sf_obj 
#' @param margin_km
#' @param margin_ratio 
#' @param crs_out
#' @return
#' @export
crop_polygon <- function(sf_obj, margin_km = NULL, margin_ratio = 0.06, crs_out = sf::st_crs(sf_obj)) {
  
  if (length(sf_obj) == 0) {
    bb <- sf::st_bbox(sf_obj)
    bb_exp <- expand_bbox_ratio(bb, margin_ratio)
    return(sf::st_as_sfc(bb_exp, crs = sf::st_crs(sf_obj)))
  }
  
  g <- suppressWarnings(sf::st_make_valid(sf::st_union(sf_obj)))
  
  if (!is.null(margin_km) && is.finite(margin_km) && margin_km > 0) {
    g_merc <- sf::st_transform(g, 3857)
    g_buf  <- sf::st_buffer(g_merc, margin_km * 1000)
    g_crop <- sf::st_transform(g_buf, crs_out)
  } else {
    bb     <- sf::st_bbox(g)
    bb_exp <- expand_bbox_ratio(bb, margin_ratio)
    g_crop <- sf::st_as_sfc(bb_exp, crs = sf::st_crs(sf_obj))
  }
  g_crop
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
                            cholera_dir = 'cholera-mapping-pipeline',
                            crop_background = F,
                            crop_margin_km = NULL,
                            crop_margin_ratio = 0.06) {
  
  # for regions that are intended to run but haven't run
  fv <- rlang::as_string(rlang::ensym(fill_var))
  if (nrow(sf_obj) == 0) {
    sf_obj <- all_countries_sf %>%
      dplyr::mutate(!!fv := NA_real_,
                    intended_run = TRUE)
    sf::st_crs(sf_obj) <- sf::st_crs(all_countries_sf)
  }
  
  if (crop_background) {
    # whether any of the countries in that region has cases >0
    has_cases <- sum(is.finite(sf_obj[[fv]]), na.rm = TRUE) > 0
    
    if (has_cases) {
      crop_poly <- crop_polygon(
        sf_obj      = sf_obj,
        margin_km   = crop_margin_km,
        margin_ratio= crop_margin_ratio,
        crs_out     = sf::st_crs(all_countries_sf)
      )
    } else {
      # for regions that are intended to run but haven't run: 
      base_bb <- sf::st_bbox(all_countries_sf)
      bb_exp <- expand_bbox_ratio(base_bb, margin_ratio)
      crop_poly <- sf::st_as_sfc(bb_exp, crs = sf::st_crs(all_countries_sf))
    }
    
    bg_trim  <- suppressWarnings(sf::st_crop(all_countries_sf, sf::st_bbox(crop_poly)))
    
    all_countries_sf_clipped <- suppressWarnings(sf::st_intersection(sf::st_make_valid(bg_trim),sf::st_make_valid(crop_poly)))
    bb <- sf::st_bbox(crop_poly)
    
  } else {
    all_countries_sf_clipped <- all_countries_sf
    bb <- sf::st_bbox(sf_obj)
  }
  
  sf_obj %>% 
    ggplot2::ggplot(aes(fill = !!sym(fill_var))) +
    ggplot2::geom_sf(data = all_countries_sf_clipped %>%
                       dplyr::filter(!intended_run),
                     inherit.aes = FALSE,
                     lwd = 0,
                     alpha = 1,
                     fill = color_no_run_intended()) +
    ggplot2::geom_sf(data = all_countries_sf_clipped %>% 
                       dplyr::filter(intended_run),
                     inherit.aes = FALSE,
                     lwd = 0,
                     alpha = 1,
                     fill = color_run_intended()) +
    ggplot2::geom_sf(lwd = border_width, color = border_color) + 
    ggplot2::geom_sf(data = all_countries_sf_clipped,
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
        
      } else if(fill_color_scale_type == "adm0_cases") {
        scale_fill_gradientn(colours = coloramp_cases(),
                             oob = scales::censor, 
                             limits = c(0, NA), 
                             breaks = seq(0, 3), 
                             labels = formatC(10^(seq(0, 3)),
                                              digits = 1,
                                              format = "fg", 
                                              big.mark = ","),
                             na.value = "lightgray")
        
      } else if(fill_color_scale_type == "mai_exceeding_thresh") {
        scale_fill_manual(values = coloramp_num_years_exceeding_threshold())
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
    {if(crop_background){
      ggplot2::coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
                        ylim = c(bb["ymin"], bb["ymax"]),
                        expand = FALSE)       
    }
  } +
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
