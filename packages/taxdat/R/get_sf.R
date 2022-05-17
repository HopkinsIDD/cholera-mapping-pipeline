#' @include plot_cache_function.R

#' @export
#' @name get_disaggregated_cases_sf
#' @title get_disaggregated_cases_sf
#' @description add
#' @param cache
#' @param config
#' @param cholera_directory
#' @return
get_disaggregated_cases_sf <- function(cache,cholera_directory,config
) {
  case_raster <- NULL
  case_raster <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
  case_raster <- case_raster %>%
    dplyr::select(dplyr::contains("modeled_cases_mean"),id,t,x,y,geom) %>%
    tidyr::gather(dplyr::contains("modeled_cases_mean"), key = "chain", value = "value")
  
  cache[[name="disaggregated_case_raster"]]<-disaggregate_case_raster(cache=cache,config=params$config,cholera_directory=params$cholera_directory)
  
  disaggregated_case_sf=cache[["disaggregated_case_raster"]]%>%
    as(.,'SpatialPolygonsDataFrame')%>% 
    sf::st_as_sf(.)%>%
    rename_with(stringr::str_replace, 
                pattern = "X", replacement = "")%>%
    tidyr::pivot_longer(.,cols=colnames(.)[stringr::str_detect(colnames(.),"[0-9]")],names_to="t")%>%
    as.data.frame(.)%>%sf::st_as_sf(.)
  
  colnames(disaggregated_case_sf)[stringr::str_detect( colnames(disaggregated_case_sf),"value")]=colnames(case_raster)[stringr::str_detect(colnames(case_raster),"value")]
  disaggregated_case_sf$id<-rep(1:(nrow(disaggregated_case_sf)/length(unique(disaggregated_case_sf$t))),each=length(unique(disaggregated_case_sf$t)))
  
  return(disaggregated_case_sf)
  
}

#' @export
#' @name get_disaggregated_rates_sf
#' @title get_disaggregated_rates_sf
#' @description add
#' @param cache
#' @param config
#' @param cholera_directory
#' @return
get_disaggregated_rates_sf <- function(cache,cholera_directory,config
) {
  case_raster <- NULL
  case_raster <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
  rate_raster <- case_raster %>%
    dplyr::select(dplyr::contains("modeled_rates_mean"),id,t,x,y,geom) %>%
    tidyr::gather(dplyr::contains("modeled_rates_mean"), key = "chain", value = "value")
  
  cache[[name="disaggregated_rate_raster"]]<-disaggregate_rate_raster(cache=cache,config=params$config,cholera_directory=params$cholera_directory)
  
  disaggregated_rate_sf=cache[["disaggregated_rate_raster"]]%>%
    as(.,'SpatialPolygonsDataFrame')%>% 
    sf::st_as_sf(.)%>%
    rename_with(stringr::str_replace, 
                pattern = "X", replacement = "")%>%
    tidyr::pivot_longer(.,cols=colnames(.)[stringr::str_detect(colnames(.),"[0-9]")],names_to="t")%>%
    as.data.frame(.)%>%sf::st_as_sf(.)
  
  colnames(disaggregated_rate_sf)[stringr::str_detect( colnames(disaggregated_rate_sf),"value")]=colnames(rate_raster)[stringr::str_detect(colnames(rate_raster),"value")]
  disaggregated_rate_sf$id<-rep(1:(nrow(disaggregated_rate_sf)/length(unique(disaggregated_rate_sf$t))),each=length(unique(disaggregated_rate_sf$t)))
  
  return(disaggregated_rate_sf)
  
}