#' @export
#' @name plot_case_raster_admin_shp
#' @param config
#' @param cholera_directory
#' @param cache
plot_case_raster_admin_shp <- function(cache, cholera_directory, config) {
  #load data and admin1 shapefiles
  taxdat::merge_modeled_cases_mean_into_raster(config=config, cache=cache, cholera_directory=cholera_directory,name = "modeled_cases_mean_raster")
  taxdat::get_output_shapefiles(config=config, cache=cache, cholera_directory=cholera_directory,name='output_shapefiles')
  
  #load config for country name and year
  config_file <- yaml::read_yaml(paste0(cholera_directory,'/',config))
  
  plot <- ggplot2::ggplot() + 
    ggplot2::geom_sf(data = cache[['modeled_cases_mean_raster']],
                     ggplot2::aes(fill = modeled_cases_mean,col=modeled_cases_mean),lwd = .1) + 
    ggplot2::scale_fill_gradientn(colours=c("#FFFFFF", "#FED98E", "#FE9929", "#D95F0E", "#993404"),
                                  oob=scales::censor, 
                                  limits = c(1,NA),
                                  trans=scales::log10_trans(), 
                                  guide = ggplot2::guide_colorbar(label.theme=ggplot2::element_text(angle=45)),
                                  na.value = "lightgray")+
    ggplot2::scale_color_gradientn(colours=c("#FFFFFF", "#FED98E", "#FE9929", "#D95F0E", "#993404"),
                                   oob=scales::censor, 
                                   limits = c(1,NA),
                                   trans=scales::log10_trans(), 
                                   guide = NULL,
                                   na.value = "lightgray")+
    taxdat::map_theme() + 
    ggplot2::labs(fill="Incidence\n [cases/year]")   +
    ggplot2::geom_sf(data=cache[['output_shapefiles']] %>% 
                       subset(admin_level=="ADM1"),
                     alpha=0) +
    geom_sf_label(data=cache[['output_shapefiles']] %>% 
                    subset(admin_level=="ADM1"),
                  ggplot2::aes(label = shapeName)) + 
    ggplot2::ggtitle(paste(config_file$countries_name,config_file$start_time,config_file$end_time,sep="-"))
  
  return(plot)
  
}