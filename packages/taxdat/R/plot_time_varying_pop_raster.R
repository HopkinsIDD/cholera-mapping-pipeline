#' @include plot_cache_function.R

#' @name get_covar_no_cache
#' @description load covariate output data
#' @param config 
#' @param cache 
#' @return covar cube
get_covar_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  covar_cube_output <- taxdat::read_file_of_type(file_names["covar"], "covar_cube_output")
  require(bit64)
  require(sf)
  return(covar_cube_output)
}
# cache the results
get_covar <- cache_fun_results(name = "covar_cube_output", fun = get_covar_no_cache,
                                    overwrite = T, config = config)

#' @name get_covar_cube_no_cache
#' @title get_covar_cube_no_cache
#' @description extrac covar_cube from covar_cube_output
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  covar_cube
get_covar_cube_no_cache <- function(config, cache, cholera_directory, ...) {
  get_covar(config, cache, cholera_directory, ...)
  covar_cube <- cache[["covar_cube_output"]]$covar_cube
  return(covar_cube)
}
# cache the results
get_covar_cube <- cache_fun_results("covar_cube", get_covar_cube_no_cache,
                                       overwrite = T, config = config)

#' @name get_sf_grid_no_cache
#' @title get_sf_grid_no_cache
#' @description extrac sf_grid from covar_cube_output
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  sf_grid
get_sf_grid_no_cache <- function(config, cache, cholera_directory, ...) {
  get_covar(config, cache, cholera_directory, ...)
  sf_grid <- cache[["covar_cube_output"]]$sf_grid
  return(sf_grid)
}
# cache the results
get_sf_grid <- cache_fun_results("sf_grid", get_sf_grid_no_cache,
                                    overwrite = T, config = config)

#' @name plot_time_varying_pop_raster
#' @description plot the time varying population raster
#' @param cache the cache environment
#' @param config config file that contains the parameter information
#' @param pars a list of parameters which we want to display
plot_time_varying_pop_raster <- function(cache,config ){
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