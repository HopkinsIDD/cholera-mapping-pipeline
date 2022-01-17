
library(stringr)
library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(ggplot2)
library(taxdat)
library(sf)
library(raster)
library(stars)

### other new packages (mainly for "rgeoboundaries")
chooseCRANmirror(ind = 77)

package_list <- c(
  "fasterize", 
  "remotes",
  "rgeoboundaries",
  "taxdat",
  "stars"
)

### Run options

option_list <- list(
  optparse::make_option(
    c("-c", "--config"),
    action = "store",
    type = "character",
    help = "Model run configuration file"
  ),
  optparse::make_option(c("-d", "--cholera_directory"), action = "store", default = NULL, type="character", help = "Cholera directory"),
  optparse::make_option(c("-o", "--old_runs"), action = "store", default = NULL, type="character", help = "old runs or not")
  optparse::make_option(c("-s", "--single_year"), action = "store", default = NULL, type="character", help = "single year runs or not")
)

params <- optparse::OptionParser(option_list = option_list) %>% optparse::parse_args()

for (package in package_list) {
  if (!require(package = package, character.only = T)) {
    if (package == "rgeoboundaries"){
      try({
        remotes::install_gitlab("dickoa/rgeoboundaries")
        remotes::install_github("wmgeolab/rgeoboundaries")
      })
    } else if (package == "taxdat"){
      install.packages(paste0(params$cholera_directory,  "/packages/taxdat"), type="source", repos=NULL)
    }else{
      install.packages(pkgs = package)
      library(package = package, character.only = T)
    }
  }
  # detach(pos = which(grepl(package, search())))
}

##Method1: create summary values for each layer across cells, then summarize across layers to get CIs.
#for each layer, group the cells by incidence rates (mild, moderate, and high incidence areas)

#' @name crop_to_shapefile
#' @title crop_to_shapefile: crop the raster with country shapefile
#' @param raster: raster file
#' @param shapefile: the country or location shapefile
#' @param snap: in which direct the extext should be aligned to
#' @return cropped and maksed raster 
crop_to_shapefile <- function(raster, shapefile, snap) {
  raster_cropped <- raster::crop(raster,extent(shapefile),snap=snap)
  raster_cropped<-raster::mask(raster_cropped,shapefile)
  return(raster_cropped)
}

#' @name get_disaggregated_rate_raster
#' @title get_disaggregated_rate_raster
#' @description get rate raster for each layer 
#' @param preprocessed_data_filename
#' @param covar_data_filename
#' @param model_output_filenames
#' @return rate_raster
get_disaggregated_rate_raster <- function(covar_data_filename,
                                          model_output_filenames,
                                          stan_input_filenames,
                                          if_single_year_run){
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  rate_raster <- covar_cube_output$sf_grid
  non_na_gridcells <- taxdat::get_non_na_gridcells(covar_data_filename)
  rate_raster <- rate_raster[non_na_gridcells,]

  model.rand <- read_file_of_type(model_output_filenames, "model.rand")
  modeled_rates <- exp(as.array(model.rand)[, , grepl("log_lambda",
                                                      names(model.rand)), drop = FALSE])
  modeled_rates_mean_by_grid_layer_tmp <- as.data.frame(t(apply(modeled_rates, c(1,3), mean)))%>%
  mutate(id=seq_len(dim(modeled_rates)[3]))

  #subset years with obervations
  stan_input <- read_file_of_type(stan_input_filenames, "stan_input")
  if(params$single_year==FALSE){
  obs_years <- (nrow(modeled_rates_mean_by_grid_layer_tmp)/5)*((min(lubridate::year(stan_input$sf_cases_resized$TL)):max(lubridate::year(stan_input$sf_cases_resized$TR)))-2015)    
  modeled_rates_mean_by_grid_layer<-data.frame()
  for (row_idx in unique(obs_years)) {
   tmp <- modeled_rates_mean_by_grid_layer_tmp[(row_idx+1):(row_idx+nrow(modeled_rates_mean_by_grid_layer_tmp)/5),]
   modeled_rates_mean_by_grid_layer<-rbind(modeled_rates_mean_by_grid_layer,tmp)}
  }else{
  modeled_rates_mean_by_grid_layer <- modeled_rates_mean_by_grid_layer_tmp
  }
  
  rate_raster <- merge(rate_raster[rate_raster$long_id%in%unique(modeled_rates_mean_by_grid_layer$id),],modeled_rates_mean_by_grid_layer,by.x="long_id",by.y="id")

  colnames(rate_raster)[str_detect(colnames(rate_raster),".*[0-9].*")] <- paste0("layer",seq_len(dim(modeled_rates)[1]))
  rate_raster <- rate_raster[,str_detect(colnames(rate_raster),".*[0-9].*")]
  
  disaggregated_rate_raster <- raster::raster(rate_raster, res = max(0.00833333, 0.00833333))
  for (layer_idx in seq_len(ncol(rate_raster)-1)){
    layer_value <- rate_raster[,layer_idx]
    single_layer <- raster::raster(rate_raster, res = max(0.00833333, 0.00833333))
    single_disaggregated_rate_raster <- fasterize::fasterize(layer_value, single_layer, field = paste0("layer",layer_idx))
    disaggregated_rate_raster <- stack(disaggregated_rate_raster,single_disaggregated_rate_raster)
  }
  names(disaggregated_rate_raster)<-colnames(rate_raster)[-ncol(rate_raster)]

  if(if_single_year_run){
#import all single-year data and stack the raster data
## replace config file name with year 2016-2019
year_with_config <- str_sub(params$config,-8,-5)

if(iso_code=="AGO"){
  year_list=c("2016","2017","2018")
}else if(iso_code=="SDN"){
  year_list=c("2016","2017","2018","2019")
}else if(iso_code=="COG"){
  year_list=c("2015","2016","2017","2019")#2018 stan output is incorrect
}else if(iso_code=="TZA"){
  year_list=c("2015","2019")#2019 shapefiles are different
}else if(iso_code=="NER"){
  year_list=c("2015")#try in small rasters
}else if(iso_code=="COD"){
  year_list=c("2015","2016")#try in small rasters
}else{
  year_list=c("2015","2016","2017","2018","2019")
}
additional_years <- year_list[!year_list%in%year_with_config]

for(year_idx in additional_years){
if(params$old_runs){
new_config_name=str_replace_all(params$config,year_with_config,year_idx)
config_tmp <- yaml::read_yaml(paste0(params$cholera_directory, new_config_name))
file_names_tmp <- taxdat::get_filenames(config_tmp, params$cholera_directory)
file_names_tmp<-update_filename_oldruns(filename=file_names_tmp,old_runs=params$old_runs)
file_names_tmp[["stan_output"]] <- stringr::str_remove(file_names_tmp[["stan_output"]],"iv-wN-cwN-csF-teT-teaF-weF.")
file_names_tmp[["stan_output"]] <- stringr::str_remove( file_names_tmp[["stan_output"]],"-F")
}else{
new_config_name=paste(paste0(c(strsplit(params$config,"/")[[1]][1:(length(strsplit(params$config,"/")[[1]])-1)]),collapse="/"),c(str_replace_all(strsplit(params$config,"/")[[1]][length(strsplit(params$config,"/")[[1]])],year_with_config,year_idx)),sep="/")
config_tmp <- yaml::read_yaml(paste0(params$cholera_directory, new_config_name))
file_names_tmp <- taxdat::get_filenames(config_tmp, params$cholera_directory)
file_names_tmp<-update_filename_oldruns(filename=file_names_tmp,old_runs=params$old_runs)
}

covar_data_filename_tmp=file_names_tmp[["covar"]]
model_output_filenames_tmp=file_names_tmp[["stan_output"]]
covar_cube_output_tmp <- read_file_of_type(covar_data_filename_tmp, "covar_cube_output")
rate_raster_tmp <- covar_cube_output_tmp$sf_grid
non_na_gridcells_tmp <- taxdat::get_non_na_gridcells(covar_data_filename_tmp)
rate_raster_tmp <- rate_raster_tmp[non_na_gridcells_tmp,]

model.rand_tmp <- read_file_of_type(model_output_filenames_tmp, "model.rand")
modeled_rates_tmp <- exp(as.array(model.rand_tmp)[, , grepl("log_lambda",
                                                      names(model.rand_tmp)), drop = FALSE])
modeled_rates_mean_by_grid_layer_tmp <- as.data.frame(t(apply(modeled_rates_tmp, c(1,3), mean)))%>%
    mutate(id=seq_len(dim(modeled_rates_tmp)[3]))
  
rate_raster_tmp <- merge(rate_raster_tmp[rate_raster_tmp$long_id%in%unique(modeled_rates_mean_by_grid_layer_tmp$id),],modeled_rates_mean_by_grid_layer_tmp,by.x="long_id",by.y="id")

colnames(rate_raster_tmp)[str_detect(colnames(rate_raster_tmp),".*[0-9].*")] <- paste0("layer",seq_len(dim(modeled_rates_tmp)[1]))
rate_raster_tmp <- rate_raster_tmp[,str_detect(colnames(rate_raster_tmp),".*[0-9].*")]
 disaggregated_rate_raster_tmp <- raster::raster(rate_raster_tmp, res = max(0.00833333, 0.00833333))

  for (layer_idx in seq_len(ncol(rate_raster_tmp)-1)){
    layer_value <- rate_raster_tmp[,layer_idx]
    single_layer <- raster::raster(rate_raster_tmp, res = max(0.00833333, 0.00833333))
    single_disaggregated_rate_raster_tmp <- fasterize::fasterize(layer_value, single_layer, field = paste0("layer",layer_idx))
    if(!extent(single_disaggregated_rate_raster_tmp)==extent(disaggregated_rate_raster_tmp)){
      single_disaggregated_rate_raster_tmp <- extend(single_disaggregated_rate_raster_tmp,disaggregated_rate_raster_tmp,value=NA)
      extent(single_disaggregated_rate_raster_tmp)=extent(disaggregated_rate_raster_tmp)
   }
    disaggregated_rate_raster_tmp <- stack(disaggregated_rate_raster_tmp,single_disaggregated_rate_raster_tmp)
  }

names(disaggregated_rate_raster_tmp)<-colnames(rate_raster_tmp)[-ncol(rate_raster_tmp)]
disaggregated_rate_raster_tmp <- extend(disaggregated_rate_raster_tmp,disaggregated_rate_raster,value=NA)
extent(disaggregated_rate_raster_tmp)=extent(disaggregated_rate_raster)
disaggregated_rate_raster <- stack(disaggregated_rate_raster,disaggregated_rate_raster_tmp)

}
  }
  return(disaggregated_rate_raster)
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

#get_affected_pop
#' @name get_affected_pop
#' @title get_affected_pop
#' @param pop_raster: 1*1 KM population raster
#' @param rate_raster: 1*1 KM rate_raster (disaggregated from 20*20 KM rater raster)
#' @param country_shp: country shapefile
#' @param threshold_list: the threshold to determine mild/moderate/high incidence areas
#' @param include_mean: parameter in across_layer_aggregator function (whether to include mean values)
#' @param calculate_disjoint_values: parameter in across_layer_aggregator function (whether to return disjoint statistics across layers)
#' @param within_layer_aggregator: the function to aggregate cells within one layer
#' @param across_layer_aggregator: the function to aggregate across layers
#' @return 
get_affected_pop <- function(pop_raster_cropped,
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

config <- yaml::read_yaml(paste0(params$cholera_directory, params$config))
file_names <- taxdat::get_filenames(config, params$cholera_directory)
file_names<-update_filename_oldruns(filename=file_names,old_runs=params$old_runs)
if(params$old_runs){
 file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],"iv-wN-cwN-csF-teT-teaF-weF.")
 file_names[["stan_output"]] <- stringr::str_remove( file_names[["stan_output"]],"-F")
}

iso_code <- as.character(stringr::str_extract(params$config, "[A-Z]{3}"))
shapefile <- rgeoboundaries::gb_adm0(iso_code)
pop_raster <- raster(paste0(params$cholera_directory,"/Layers/pop/2000_1km_Aggregated.tif"))
pop_raster_cropped <- crop_to_shapefile(raster = pop_raster,shapefile = shapefile,snap="out")

disaggregated_rate_raster <- get_disaggregated_rate_raster(covar_data_filename=file_names[["covar"]],
                                                           model_output_filenames=file_names[["stan_output"]],
                                                           stan_input_filenames=file_names[['stan_input']],
                                                           if_single_year_run=params$single_year
                                                           )
#rate_raster_cropped <- crop_to_shapefile(raster = disaggregated_rate_raster,shapefile = shapefile,snap="out")
rate_raster_cropped <- crop_to_shapefile(raster = disaggregated_rate_raster[[1]],shapefile = shapefile,snap="out")

for(i in seq_len(nlayers(disaggregated_rate_raster))[-1]){
  tmp <- crop_to_shapefile(raster = disaggregated_rate_raster[[i]],shapefile = shapefile,snap="out")
  rate_raster_cropped <- stack(rate_raster_cropped,tmp)
}

rm(disaggregated_rate_raster)
gc()

table<-get_affected_pop(pop_raster_cropped=pop_raster_cropped,
                             rate_raster_cropped=rate_raster_cropped,
                             shapefile=shapefile,
                             threshold_list=c(0.001,0.0001,0.00001),
                             include_mean=TRUE,
                             calculate_disjoint_values = TRUE,
                             within_layer_aggregator=aggregate_affected_pop_across_cells,
                             across_layer_aggregator=aggregate_affected_pop_across_layers,
                             probability_cutoffs=c(0.025,0.5,0.975))

if(params$single_year){
year_list=str_sub(params$config,-8,-5)
}else{
year_list="2015_2019"
}

filename<-paste0("final_table",iso_code,year_list,".csv")
write.csv(table,filename)

