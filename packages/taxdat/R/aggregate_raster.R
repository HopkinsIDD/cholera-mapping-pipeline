#' @include plot_cache_function.R
#' @include get_stan_output.R

#' @export
#' @name disaggregate_rate_raster
#' @description disaggregate_rate_raster
#' @param cache
#' @param config
#' @param cholera_directory
#' @return 
disaggregate_rate_raster <- function(cache,config,cholera_directory){
  case_raster <- NULL
  case_raster <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
  
  #disaggregate incidence raster to 1*1KM 
  rate_raster <- case_raster %>%
    dplyr::select(dplyr::contains("modeled_rates_mean"),id,t,x,y,geom) %>%
    tidyr::gather(dplyr::contains("modeled_rates_mean"), key = "chain", value = "value")
  
  empty_raster <- raster::raster(rate_raster, res = max(0.1666666, 0.1666666))#20*20km raster
  disaggregated_rate_raster <- fasterize::fasterize(rate_raster, empty_raster, field = c("value"),by="t")
  return(disaggregated_rate_raster)
}

#' @export
#' @name disaggregate_case_raster
#' @description disaggregate_case_raster
#' @param cache
#' @param config
#' @param cholera_directory
#' @return 
disaggregate_case_raster <- function(cache,config,cholera_directory){
  case_raster <- NULL
  case_raster <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
  
  #disaggregate incidence raster to 1*1KM 
  rate_raster <- case_raster %>%
    dplyr::select(dplyr::contains("modeled_rates_mean"),id,t,x,y,geom) %>%
    tidyr::gather(dplyr::contains("modeled_rates_mean"), key = "chain", value = "value")
  #load 1*1 pop raster
  covar_cube <- cache[["covar_cube"]]
  sf_grid <- cache[["sf_grid"]]
  pop_layer <- covar_cube[,,1, drop = F] ## population is always the first layer
  covar <- data.frame(covar = unlist(lapply(1:ncol(pop_layer), function(x){
    pop_layer[, x, 1]
  })))
  pltdata <- dplyr::bind_cols(sf_grid, covar)
  colnames(pltdata)[colnames(pltdata)=="covar"]<-"pop"
  
  empty_raster <- raster::raster(pltdata, res = max(0.1666666, 0.1666666))#20*20km raster
  disaggregate_pop <- fasterize::fasterize(pltdata, empty_raster, field = c("pop"),by="t") 
  
  empty_raster <- raster::raster(rate_raster, res = max(0.1666666, 0.1666666))#20*20km raster
  disaggregated_rate_raster <- fasterize::fasterize(rate_raster, empty_raster, field = c("value"),by="t")
  
  # Make sure extents match
  disaggregate_pop2 <- raster::resample(disaggregate_pop, disaggregated_rate_raster)
  
  #get 1*1 pop-weighted case raster
  disaggregated_case_raster<-disaggregate_pop2*disaggregated_rate_raster
  
  return(disaggregated_case_raster)
}
