#' @export
#' @name plot_theme
#' @title plot_theme
#' @description default ggplot plot theme settings for cholera plotting
#' @return ggplot plot theme for cholera plotting
plot_theme <- function(theme_type="bw") {
  if(theme_type == "gray"){
    return(
      ggplot2::theme(panel.background = element_rect(fill = 'gray'))
    )
  }else{
    return(
      ggplot2::theme_bw()
    )    
  }
}

#' @export
#' @name map_theme
#' @title map_theme
#' @description default ggplot map theme settings for cholera mapping
#' @return ggplot map theme for cholera mapping
map_theme <- function(theme_type="bw") {
  return(
    taxdat::plot_theme(theme_type = theme_type) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom"
      )
  )
}
