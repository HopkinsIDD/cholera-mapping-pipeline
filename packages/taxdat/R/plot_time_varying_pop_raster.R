#' @include plot_cache_function.R

#' @name plot_time_varying_pop_raster
#' @description plot the time varying population raster
#' @param cache the cache environment
#' @param config config file that contains the parameter information
#' @param pars a list of parameters which we want to display
plot_time_varying_pop_raster <- function(cache,config, cholera_directory){
  config<-yaml::read_yaml(paste0(cholera_directory,config))

  covar_cube <- cache[["covar_cube"]]
  sf_grid <- cache[["sf_grid"]]
  pop_layer <- covar_cube[,,1, drop = F] ## population is always the first layer
  covar <- data.frame(covar = unlist(lapply(1:ncol(pop_layer), function(x){
    pop_layer[, x, 1]
  })))
  pltdata <- dplyr::bind_cols(sf_grid, covar)
  
  analysis_years <- lubridate::year(config$start_time):lubridate::year(config$end_time)
  pltdata$t<- factor(pltdata$t, labels = analysis_years )
  
  plot<-ggplot2::ggplot()+ggplot2::geom_sf(data = pltdata , 
                                           ggplot2::aes(fill = covar,color = covar)) + 
    ggplot2::scale_fill_viridis_c("Population",trans = "log", breaks = c(100, 1000, 10000,1e+05), 
                                  aesthetics = c("colour", "fill"),
                                  guide = ggplot2::guide_colorbar(title = "Population density [per grid cell]"),
                                  option = "E", na.value = "white") + 
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = ggplot2::element_text(angle = 45,hjust = 1, vjust = 1))+
    facet_wrap(~ t)
  
  return(plot)
}
