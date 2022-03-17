#' @include file_name_functions.R

#' @name get_rate_raster
#' @title get_rate_raster
#' @description get rate raster for each layer 
#' @param preprocessed_data_filename
#' @param covar_data_filename
#' @param model_output_filenames
#' @return rate_raster
get_rate_raster <- function(covar_data_filename,
                            model_output_filenames,
                            stan_input_filenames,
                            if_single_year_run,
                            res=c(0.1666667,0.1666667),
                            starting_year=2015){
  #load model output data
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  rate_raster <- covar_cube_output$sf_grid
  non_na_gridcells <- taxdat::get_non_na_gridcells(covar_data_filename)
  rate_raster <- rate_raster[non_na_gridcells,]
  model.rand <- read_file_of_type(model_output_filenames, "model.rand")
  niter_per_chain <- dim(MCMCvis::MCMCchains(model.rand, params='lp__', chain_num=1))[1]
  
  # average modeled cases across chains
  modeled_cases <- as.array(model.rand)[, , grepl("grid_case", names(model.rand)),drop=FALSE]
  modeled_cases_mean_by_grid_layer_tmp <- as.data.frame(t(apply(modeled_cases, c(1,3), mean)))%>%
    mutate(id=seq_len(dim(modeled_cases)[3]))
  
  #subset years with obervations and remove years without obsevations (drop some years)
  stan_input <- read_file_of_type(stan_input_filenames, "stan_input")
  if(params$single_year==FALSE){
    
    obs_years <- (nrow(modeled_cases_mean_by_grid_layer_tmp)/length(unique(lubridate::year(stan_input$sf_cases_resized$TL),lubridate::year(stan_input$sf_cases_resized$TR))))*((unique(c(lubridate::year(stan_input$sf_cases_resized$TL),lubridate::year(stan_input$sf_cases_resized$TR))))-starting_year)
    
    modeled_cases_mean_by_grid_layer_tmp1<-data.frame()
    for (row_idx in unique(obs_years)) {
      
      tmp <- modeled_cases_mean_by_grid_layer_tmp[(row_idx+1):(row_idx+nrow(modeled_cases_mean_by_grid_layer_tmp)/length(unique(lubridate::year(stan_input$sf_cases_resized$TL),lubridate::year(stan_input$sf_cases_resized$TR)))),1:niter_per_chain]
      
      if(length(modeled_cases_mean_by_grid_layer_tmp1)==0){
        modeled_cases_mean_by_grid_layer_tmp1<-tmp
      }else{
        modeled_cases_mean_by_grid_layer_tmp1<-cbind(modeled_cases_mean_by_grid_layer_tmp1,tmp)}
    }
    modeled_cases_mean_by_grid_layer<-  modeled_cases_mean_by_grid_layer_tmp1
  }else{
    modeled_cases_mean_by_grid_layer_tmp1 <-  modeled_cases_mean_by_grid_layer_tmp
  }
  
  #estimate 2017 population data from the covariate database
  stan_input <- read_file_of_type(stan_input_filenames, "stan_input")
  pop_sf_grid<-stan_input$sf_grid
  pop_sf_grid_2017<-get_pop2017(pop_sf_grid)
  
  #combine the population estimates into case raster
  modeled_cases_mean_by_grid_layer$pop2017<-pop_sf_grid_2017[1:nrow(modeled_cases_mean_by_grid_layer),]$pop2017
  
  modeled_rates_mean_by_grid_layer<-modeled_cases_mean_by_grid_layer
  modeled_rates_mean_by_grid_layer[str_detect(colnames(modeled_rates_mean_by_grid_layer),".*[0-9].*")] <- modeled_cases_mean_by_grid_layer[str_detect(colnames(modeled_cases_mean_by_grid_layer),".*[0-9].*")]/modeled_cases_mean_by_grid_layer$pop2017
  modeled_rates_mean_by_grid_layer$id=1:nrow(modeled_rates_mean_by_grid_layer)
  
  rate_raster <- merge(rate_raster[1:length(unique(rate_raster$geom)),],modeled_rates_mean_by_grid_layer,by="id")
  
  colnames(rate_raster)[str_detect(colnames(rate_raster),".*[0-9].*")&!colnames(rate_raster)%in%"pop2017"] <-   paste0("layer",seq_len(length(unique(lubridate::year(stan_input$sf_cases_resized$TL),lubridate::year(stan_input$sf_cases_resized$TR)))*dim(modeled_cases)[1]))
  
  rate_raster <- rate_raster[,str_detect(colnames(rate_raster),".*[0-9].*")&!colnames(rate_raster)%in%"pop2017"]
  
  raster_2020<-raster::raster(rate_raster, res =res) 
  
  for (layer_idx in seq_len(ncol(rate_raster)-1)){
    
    layer_value <- rate_raster[,layer_idx]
    
    #empty raster
    single_layer <- raster::raster(rate_raster, res =res)
    #assign rate values into the raster
    single_rate_raster_2020 <- fasterize::fasterize(layer_value, single_layer, field = paste0("layer",layer_idx))
    
    raster_2020 <- stack(raster_2020,single_rate_raster_2020)
    rm(single_rate_raster_2020)
    gc()
  }
  names(raster_2020)<-colnames(rate_raster)[-ncol(rate_raster)]
  
  return(raster_2020)
}

#' @name aggregate_affected_pop_across_cells
#' @title aggregate_affected_pop_across_cells: aggregate proportion of population living in each incidence group across cells for each layer
#' @param pop_raster_cropped: 1*1 KM population raster cropped by crop_to_shapefile function
#' @param rate_raster_cropped: 1*1 KM rate_raster (disaggregated from 20*20 KM rater raster) cropped by crop_to_shapefile function
#' @param shapefile: country shapefile
#' @param threshold_list: the threshold to determine mild/moderate/high incidence areas
#' @return results_by_layer
aggregate_affected_pop_across_cells <- function(pop_raster_cropped, rate_raster_cropped,shapefile,threshold_list){
  threshold_list <- threshold_list[order(threshold_list,decreasing = T)]
  pop_prop<-as.data.frame(matrix(NA,nrow=1,ncol=length(unique(threshold_list))))
  names(pop_prop) <- paste0(">=",threshold_list)
  for (threshold_idx in seq_len(length(threshold_list))) {
    pop_prop[,threshold_idx] <-
      100*sum(values(pop_raster_cropped)[values(rate_raster_cropped)>=threshold_list[threshold_idx]],na.rm=T)/
      sum(values(pop_raster_cropped),na.rm = T)
  }
  pop_prop<-t(t(cbind(0,pop_prop))-dplyr::lag(t(cbind(0,pop_prop))))[,-1]
  return(pop_prop)
}

#' @name aggregate_affected_pop_across_layers
#' @title aggregate_affected_pop_across_layers: estimate the CI of the proportion of population living in each incidence group across layers
#' @param pop_prop: aggregated population by incidence category for all layers
#' @param probability_cutoffs: the probability cutoffs for distributions
#' @param include_mean: whether to include mean value
#' @param calculate_disjoint_values: whether to return disjoint statistics across layers
#' @return s
aggregate_affected_pop_across_layers <- function(pop_prop,probability_cutoffs, include_mean = TRUE, calculate_disjoint_values = TRUE) {
  probability_distribution <- apply(pop_prop, 2, quantile,probability_cutoffs)
  if (include_mean) {
    mean_values <- apply(pop_prop,2,mean)
  }
  return(rbind(mean_values,probability_distribution))
}

#' @name get_pop_at_risk
#' @title get_pop_at_risk
#' @param pop_raster: 1*1 KM population raster
#' @param rate_raster: 1*1 KM rate_raster (disaggregated from 20*20 KM rater raster)
#' @param country_shp: country shapefile
#' @param threshold_list: the threshold to determine mild/moderate/high incidence areas
#' @param include_mean: parameter in across_layer_aggregator function (whether to include mean values)
#' @param calculate_disjoint_values: parameter in across_layer_aggregator function (whether to return disjoint statistics across layers)
#' @param within_layer_aggregator: the function to aggregate cells within one layer
#' @param across_layer_aggregator: the function to aggregate across layers
#' @return 
get_pop_at_risk <- function(pop_raster_cropped,
                            rate_raster_cropped,
                            shapefile,
                            threshold_list=c(0.001,0.0001,0.00001),
                            include_mean=TRUE,
                            calculate_disjoint_values = TRUE,
                            within_layer_aggregator=aggregate_affected_pop_across_cells,
                            across_layer_aggregator=aggregate_affected_pop_across_layers,
                            probability_cutoffs=c(0.025,0.5,0.975)){
  results_by_layer <- list()
  for (layer_idx in seq_len(nlayers(rate_raster_cropped))) {
    results_by_layer[[layer_idx]] <- within_layer_aggregator(pop_raster_cropped=pop_raster_cropped,
                                                             rate_raster_cropped=rate_raster_cropped[[layer_idx]],
                                                             shapefile=shapefile,
                                                             threshold_list=threshold_list)
  }
  pop_prop=do.call('rbind',results_by_layer)
  return(across_layer_aggregator(pop_prop=pop_prop,
                                 probability_cutoffs = probability_cutoffs,
                                 include_mean = include_mean,
                                 calculate_disjoint_values = calculate_disjoint_values))
}