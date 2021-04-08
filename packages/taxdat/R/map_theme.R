#' @export
#' @name map_theme
#' @title map_theme
#' @description default ggplot map theme settings for cholera mapping
#' @return ggplot map theme for cholera mapping
map_theme <- function(){
  return(
    ggplot2::theme_bw() + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = 'bottom'
      )
  )
}