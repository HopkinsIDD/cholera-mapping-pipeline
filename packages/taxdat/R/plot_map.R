#' @export
#' @name plot_map
#' @title plot_map
#' @description default function to plot cholera maps with ggplot
#' @param sf_rates sf grid object containing data to plot
#' @param column string with column name to plot, typically `cases` or `rate` (default: `cases`)
#' @param facet_column string with column name to facet
#' @param colorscale_type string with type of default color scale to use (default: `cases`) (See [`color_scale()`] for more details)
#' @param render logical for whether to render figure (default: FALSE)
#' @param plot_file if NULL, save to file (default: NULL)
#' @param width width of saved figure in inches
#' @param height height of saved figure in inches
#' @param plot_border logical to include grid borders
#' @param ... additional parameters passed to [`color_scale()`]
#' @include color_scale.R map_theme.R
#' @return ggplot object of cholera case or incidence maps on standard color scale
plot_map <- function(
  sf_rates,
  column = 'cases', 
  facet_column = "type", 
  colorscale_type = 'cases', 
  render = FALSE, 
  plot_file = NULL, 
  width = NULL, 
  height = NULL, 
  plot_border = TRUE, 
  ...){

  plt <- ggplot2::ggplot()
  if(isTRUE(plot_border)){
    plt <- plt +
      ggplot2::geom_sf(
        data = sf_rates,
        ggplot2::aes_string(fill = column)
      )
  } else {
    plt <- plt +
      ggplot2::geom_sf(
        data = sf_rates,
        ggplot2::aes_string(fill = column),
        color=NA
      )
  }
  plt <- plt + 
    color_scale(type = colorscale_type, use_case = 'ggplot map', ...) +
    map_theme() + 
    ggplot2::facet_wrap(formula(paste("~", facet_column)))
  
  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  if("sf" %in% class(plot_border)){
    plt <- plt + ggplot2::geom_sf(data = plot_border)
  }
  
  if(render){
    plt
  }
}