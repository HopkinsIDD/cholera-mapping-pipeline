#' @export
#' @name color_scale
#' @title color_scale
#' @description default color scale for cholera maps
#' @param type string indicating 'cases' or 'rates' (default: 'cases')
#' @param use_case string indicating type of map 'leaflet' or 'ggplot map' (default: 'leaflet')
#' @param use_log logical for whether outcome should be log10 transformed (default: FALSE)
#' @return leaflet or ggplot color palette for mapping
color_scale <- function(type = "cases", use_case = "leaflet", use_log = FALSE) {
  rate_palette <- RColorBrewer::brewer.pal(9, name = "RdBu")
  discrete_rate_palette <- colorRampPalette(rate_palette, space = "Lab")

  transform <- c()
  colors <- c()
  limits <- c()
  if (type %in% c("case", "cases")) {
    colors <- c("#FFFFFF", "#FED98E", "#FE9929", "#D95F0E", "#993404")
    if (use_log) {
      transform <- scales::log10_trans()
    } else {
      transform <- scales::identity_trans()
    }

    if (use_log) {
      limits <- c(exp(-5), NA)
    } else {
      limits <- c(0, NA)
    }
  } else if (type %in% c("rate", "rates")) {
    colors <- c("blue", "white", "red")
    if (use_log) {
      limits <- c(1e-7, 1e-1) # 1e-2 to 1e4 on cases per 1e5
    } else {
      limits <- c(0, 1e-1)
    }
    breaks <- function(x) {
      logscale_x <- log(x * 1e5) / log(10)
      return(10^seq(floor(logscale_x[1]), ceiling(logscale_x[2]), by = 1) / 1e5)
    }
    if (use_log) {
      transform <- scales::trans_new(
        name = "log10per1e5",
        transform = function(x) {
          log10(x * 1e5)
        },
        inverse = function(x) {
          (10^x) / 1e5
        },
        domain = c(1e-100, Inf),
        breaks = breaks,
        format = function(x) {
          new_x <- scales::label_number(scale = 1e5, big.mark = ",")(x)
          new_x[which.min(x)] <- paste("<=", new_x[which.min(x)])
          new_x[which.max(x)] <- paste(">=", new_x[which.max(x)])
          return(new_x)
        }
      )
    } else {
      transform <- scales::trans_new(name = "per1e5", transform = function(x) {
        x * 1e5
      }, inverse = function(x) {
        x / 1e5
      })
    }
  } else if (type %in% c("test case rates")) {
    colors <- c("blue", "white", "red")
    limits <- c(1e-2, 1e-1) # 1e3 to 1e4 on cases per 1e5
    breaks <- function(x) {
      logscale_x <- log(x * 1e5) / log(10)
      return(10^seq(floor(logscale_x[1]), ceiling(logscale_x[2]), by = 1) / 1e5)
    }
    if (use_log) {
      transform <- scales::trans_new(
        name = "log10per1e5",
        transform = function(x) {
          log10(x * 1e5)
        },
        inverse = function(x) {
          (10^x) / 1e5
        },
        domain = c(1e-100, Inf),
        breaks = breaks,
        format = function(x) {
          new_x <- scales::label_number(scale = 1e5, big.mark = ",")(x)
          new_x[which.min(x)] <- paste("<=", new_x[which.min(x)])
          new_x[which.max(x)] <- paste(">=", new_x[which.max(x)])
          return(new_x)
        }
      )
    } else {
      transform <- scales::trans_new(name = "per1e5", transform = function(x) {
        x * 1e5
      }, inverse = function(x) {
        x / 1e5
      })
    }
  } else if (type %in% c("observation_counts")) {
    colors <- scales::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1)(5)
    if (use_log) {
      transform <- scales::log10_trans()
    } else {
      transform <- scales::identity_trans()
    }

    limits <- c(0, NA)
  } else if (type %in% c("covariate")) {
    colors <- scales::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1)(5)
    if (use_log) {
      warning("Using a log transform of the covariates is strange")
      transform <- scales::log10_trans()
    } else {
      transform <- scales::identity_trans()
    }

    limits <- c(-3, 3)
  } else if (type %in% c("population")) {
    colors <- scales::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1)(5)
    if (use_log) {
      transform <- scales::log10_trans()
    } else {
      transform <- scales::identity_trans()
    }

    limits <- c(1, 1e+5)
  } else {
    stop(paste("The type", type, "is not recognized"))
  }

  if (use_case == "leaflet") {
    return(colorRampPalette(colors, space = "Lab"))
  } else if (use_case == "ggplot map") {
    return(
      ggplot2::scale_fill_gradientn(colours = colors, oob = scales::squish, limits = limits, trans = transform, guide = ggplot2::guide_colorbar(label.theme = ggplot2::element_text(angle = 45)))
    )
  } else {
    stop(paste("The use case", use_case, "is not recognized"))
  }
  stop(paste("The type", type, "is not recognized"))
}
