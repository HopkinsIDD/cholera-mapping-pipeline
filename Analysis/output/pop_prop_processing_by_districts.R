####### TODO
# Check if the dropped years are really dropped
# Check if ZNZ shapefile can be used
# Rerun the multi-year countries that have dropped years and watch out for SDN
# ***Change how the sum table is summarized and rerun the completed countries 

####### Get the country code
option_list <- list(
  optparse::make_option(
    c("-c", "--country_code"),
    action = "store",
    #default = Sys.getenv("CHOLERA_CONFIG", "config.yml"),
    type = "character",
    help = "Model run country code"
  ) #,
  # optparse::make_option(c("-d", "--cholera_directory"), action = "store", default = NULL, type="character", help = "Cholera directory"),
  # optparse::make_option(c("-l", "--layers_directory"), action = "store", default = NULL, type="character", help = "Layers directory")
)

library(dplyr)
country_code <- optparse::OptionParser(option_list = option_list) %>% optparse::parse_args()
if(tolower(country_code) == "all"){
  run_individual_country <- FALSE
}else{
  country_code <- toupper(country_code)
  run_individual_country <- TRUE
  Dec_2021_list <- c(country_code)
}
rm(country_code)


####### Set up 
library(stringr)
library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(ggplot2)
library(kableExtra)
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
  "gsheet", 
  "stringdist", 
  "reproducible"
)

for (package in package_list) {
  if (!require(package = package, character.only = T)) {
    if (package == "rgeoboundaries"){
      try({
        remotes::install_gitlab("dickoa/rgeoboundaries")
        remotes::install_github("wmgeolab/rgeoboundaries")
      })
    }else{
      install.packages(pkgs = package)
      library(package = package, character.only = T)
    }
  }
}





####### Create functions 
fix_stan_output_name <- function(fn){
  print("Trying to fix stan output name now. ")
  
  tmp_vector <- stringr::str_split(fn, "cholera-mapping-output/")
  dir_for_search <- paste0(tmp_vector[[1]][1], "cholera-mapping-output/")
  fn_for_search <- tmp_vector[[1]][2]
  fn_idx <- stringdist::amatch(fn_for_search, list.files(dir_for_search), maxDist=100)
  new_fn <- list.files(dir_for_search)[fn_idx]
  new_fn <- paste0(dir_for_search, new_fn)
  return(new_fn)
}

read_country_status <- function(URL = "https://docs.google.com/spreadsheets/d/1zsjEJk1H1d-ALoICmJO1juQJZc4snPOuAc3c0rDwkLA"){
  new_URL <- gsheet::construct_download_url(URL, format = "csv", sheetid = NULL)
  table <- gsheet::gsheet2tbl(new_URL, sheetid = NULL) 
  return(table)
}

find_config_fn <- function(cholera_directory, country_code, country_status, new_config_dir, old_config_dir){
  
  if(country_status[country_status$approved_country==country_code, ]$report_old_new_output == "old"){
    if(country_status[country_status$approved_country==country_code, ]$single_year_multiple_year == "multiple"){
      config_fn <- paste(cholera_directory, old_config_dir, paste0(country_code, ".yml"), sep = "/")
      if(file.exists(config_fn)){return(config_fn)}else{
        stop(paste0("The config file for ", country_code, " cannot be found using ", config_fn, ", please check. "))
      }
    }else if(country_status[country_status$approved_country==country_code, ]$single_year_multiple_year == "single"){
      old_config_dir_single <- stringr::str_remove(old_config_dir, "/2015_2019_full_base")
      config_dir_tmp <- paste(cholera_directory, old_config_dir_single, "single_year_configs/mcov", sep = "/")
      all_folders <- list.dirs(config_dir_tmp, full.names = TRUE, recursive=FALSE)
      config_fn <- c()
      config_fn <- unlist(lapply(all_folders, function(folder){
        paste(folder, list.files(folder)[grepl(country_code, list.files(folder))], sep = "/")
      }))
    }
  
  }else if(country_status[country_status$approved_country==country_code, ]$report_old_new_output == "new"){
    # a little fix for TZA
    if(country_code == "TZA"){country_code <- "TZA_mainland"}
    # get all the .yml files in each country folder and choose the only correct one/ones 
    all_config_folders <- list.dirs(paste(cholera_directory, new_config_dir, sep = "/"), full.names = TRUE, recursive=FALSE)
    config_folder_index <- match(TRUE, grepl(country_code, all_config_folders, fixed = FALSE))
    all_files <- list.files(all_config_folders[config_folder_index], full.names = TRUE, recursive=FALSE)
    potential_config_fn <- all_files[grepl('^.*?/config_[A-Z]{3}_.*?.yml$', all_files)]
    correct_config_fn <- potential_config_fn[nchar(potential_config_fn) == max(nchar(potential_config_fn))]
    # a little fix for TZA
    if(country_code == "TZA_mainland"){country_code <- "TZA"}
    # return the filename
    return(correct_config_fn)
  
  }else{
    stop(paste0("The country ", country_code, " has unclear or unmatched info in the country status table, please check. "))
  }
}

find_output_dir <- function(cholera_directory, 
                            country_code, 
                            country_status, 
                            new_output_dir, 
                            old_output_dir_single, 
                            old_output_dir_multiple){
  
  if(country_status[country_status$approved_country==country_code, ]$report_old_new_output == "old"){
    if(country_status[country_status$approved_country==country_code, ]$single_year_multiple_year == "single"){old_output_dir <- old_output_dir_single}else{
      old_output_dir <- old_output_dir_multiple
    }
    output_dir <- paste(cholera_directory, old_output_dir, sep = "/")
    if(dir.exists(output_dir)){return(output_dir)}else{
      stop(paste0("The output directory for ", country_code, " cannot be found using ", output_dir, ", please check. "))
    }
  
  }else if(country_status[country_status$approved_country==country_code, ]$report_old_new_output == "new"){
    output_dir <- paste(cholera_directory, new_output_dir, sep = "/")
    if(dir.exists(output_dir)){return(output_dir)}else{
      stop(paste0("The output directory for ", country_code, " cannot be found using ", output_dir, ", please check. "))
    }
  
  }else{
    stop(paste0("The country ", country_code, " has unclear or unmatched info in the country status table, please check. "))
  }
}

get_non_na_gridcells <- function(covar_data_filename){
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  non_na_gridcells <- covar_cube_output$non_na_gridcells
  non_na_gridcells
}

#' @name crop_to_shapefile
#' @title crop_to_shapefile: crop the raster with country shapefile
#' @param raster: raster file
#' @param shapefile: the country or location shapefile
#' @param snap: in which direct the extext should be aligned to
#' @return cropped and maksed raster 
crop_to_shapefile <- function(raster, shapefile, snap) {
  raster_cropped <- raster::crop(raster, shapefile, snap=snap)
  # raster_cropped <- raster::alignExtent(extent(shapefile), raster, snap='near')
  raster_cropped <- raster::mask(raster_cropped,shapefile)

  # ### An alternative
  # if( nlayers(raster_cropped) > 1 ){
  #   raster_cropped_layers <- raster::raster(raster_cropped, layer = 1:nlayers(raster_cropped))
  # }else{
  #   raster_cropped_layers <- raster::raster(raster_cropped)
  # }
  
  # raster_cropped_layers <- reproducible::fastMask(raster_cropped_layers,shapefile) 
  # # or try (https://rdrr.io/github/Pakillo/rgis/man/fast_mask.html)
  return(raster_cropped)
}

#Generalize this function to different year?
#' Get 2017 population
#'
#' @param sf_grid the sf_grid object from the stan_input file
#'
#' @return a new sf_grid object with a column for the 2017 population
#'
get_pop2017 <- function(sf_grid) {
  # Connect to database
  conn <- taxdat::connect_to_db(Sys.getenv("USER"))
  
  # Pul the 2017 population data
  pop2017 <- rpostgis::pgGetRast(conn, 
                                 name = c("covariates", "pop_1_years_20_20"), 
                                 band =  which(2000:2020 == 2017))
  
  # Extract the values at the centroids of sf_grid
  sf_grid$pop2017 <- raster::extract(pop2017, sf::st_centroid(sf_grid))
  
  DBI::dbDisconnect(conn)
  
  return(sf_grid)
}

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
                            assume_0_year_index=NULL, 
                            dropped_year_index=NULL){
  #load model output data
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  rate_raster <- covar_cube_output$sf_grid
  non_na_gridcells <- get_non_na_gridcells(covar_data_filename)
  rate_raster <- rate_raster[non_na_gridcells,]
  model.rand <- read_file_of_type(model_output_filenames, "model.rand")
  niter_per_chain <- dim(MCMCvis::MCMCchains(model.rand, params='lp__', chain_num=1))[1]

  # average modeled cases across chains
  modeled_cases <- as.array(model.rand)[, , grepl("grid_case", names(model.rand)),drop=FALSE]
  modeled_cases_mean_by_grid_layer_tmp <- as.data.frame(t(apply(modeled_cases, c(1,3), mean)))%>%
  mutate(id=seq_len(dim(modeled_cases)[3]))

  #subset years with obervations and remove years without obsevations (drop some years)
  stan_input <- read_file_of_type(stan_input_filenames, "stan_input")
  
  if(!if_single_year_run){  
    obs_years <- ( nrow(modeled_cases_mean_by_grid_layer_tmp) / 5 ) * ( (2015:2019) - 2015 ) #change this 2015 for later runs
      
    # stan_input_years <- unique(c(lubridate::year(stan_input$sf_cases_resized$TL),lubridate::year(stan_input$sf_cases_resized$TR)))  
    # years_approved <- (2015:2019)[!(1:5) %in% c(assume_0_year_index, dropped_year_index)]
    obs_years <- sort(obs_years)
    ### sometimes the stan input already has the approved years 
    # if(length(obs_years) < 5){

    #   if(!is.null(assume_0_year_index) | !is.null(dropped_year_index)){
    #     if( all(sort(stan_input_years) == sort(years_approved)) ){
    #       assume_0_year_index <- NULL
    #       dropped_year_index <- NULL 
    #       print(paste0("The stan input already only has years that are: ", stan_input_years))
    #     }else{
    #       stop(paste0("The stan input is incomplete, the years it includes are: ", stan_input_years))
    #     }
    #   }

    #   else{ stop(paste0("The stan input is incomplete while all years are approved, the years it includes are: ", stan_input_years)) }
    # }
    
    

    modeled_cases_mean_by_grid_layer_tmp1<-data.frame()
    for (row_idx in unique(obs_years)) {
      # To totally drop a year
      if( !is.null(dropped_year_index) & match(row_idx, unique(obs_years))%in%dropped_year_index ){next}
      
      # To assume 0 for a year 
      tmp <- modeled_cases_mean_by_grid_layer_tmp[(row_idx+1):( row_idx + (nrow(modeled_cases_mean_by_grid_layer_tmp)/
                                                  length(unique(c(lubridate::year(stan_input$sf_cases_resized$TL), lubridate::year(stan_input$sf_cases_resized$TR))))) ), 
                                                  1:niter_per_chain]
      if( !is.null(assume_0_year_index) & match(row_idx, unique(obs_years))%in%assume_0_year_index ){
        tmp[tmp!= 0] <- 0
      }

      if(length(modeled_cases_mean_by_grid_layer_tmp1)==0){
        modeled_cases_mean_by_grid_layer_tmp1<-tmp
      }else{
        modeled_cases_mean_by_grid_layer_tmp1<-cbind(modeled_cases_mean_by_grid_layer_tmp1,tmp)}
    }
    modeled_cases_mean_by_grid_layer <- modeled_cases_mean_by_grid_layer_tmp1
  
  }else{
    modeled_cases_mean_by_grid_layer <- modeled_cases_mean_by_grid_layer_tmp
    if( !is.null(assume_0_year_index) ){
        modeled_cases_mean_by_grid_layer[modeled_cases_mean_by_grid_layer != 0] <- 0
    }
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

  colnames(rate_raster)[str_detect(colnames(rate_raster),".*[0-9].*")&!colnames(rate_raster)%in%"pop2017"] <- paste0("layer",seq_len(length( 2015:2019 )*dim(modeled_cases)[1]))

  rate_raster <- rate_raster[,str_detect(colnames(rate_raster),".*[0-9].*")&!colnames(rate_raster)%in%"pop2017"]

  raster_2020<-raster::raster(rate_raster, res =res) #this one is empty, which is gonna be used to keep stacking layers 

  ###==========================================### For the BEN test Kaiyue added on Mar 9th 2022 ###==========================================###
  single_layer_original <- raster::raster(rate_raster, res =res) #this empty raster will be used as the empty mode to fasterize sf data for each layer
  raster_list <- list() #create a list to stash the 100-layer objects 

  for (layer_idx in seq_len(ncol(rate_raster)-1)){

    layer_value <- rate_raster[,layer_idx]
     
    #assign rate values into the raster
    single_rate_raster_2020 <- fasterize::fasterize(layer_value, single_layer_original, field = paste0("layer",layer_idx))
    
    #stack layers 
    if(nlayers(raster_2020) < 100){
      raster_2020 <- stack(raster_2020,single_rate_raster_2020)
    }else{
      raster_list[[length(raster_list)+1]] <- raster_2020 #stash
      print(paste0(length(raster_list), "00 rate layers out of ", (ncol(rate_raster)-1), " have been completed. "))
      raster_2020 <- raster::raster(rate_raster, res =res) #a new empty raster
      raster_2020 <- stack(raster_2020,single_rate_raster_2020)
    }
    rm(single_rate_raster_2020)
    gc()

    #by the end of the outer for loop 
    if( layer_idx == ncol(rate_raster)-1 ){
      ### Don't forget the last 100-layer object
      raster_list[[length(raster_list)+1]] <- raster_2020 
      while(length(raster_list) > 1){ #folding process will keep going until there is only one element left
        for(i in 1:length(raster_list)){

          if(i >= length(raster_list)){ #if there is nothing next to it to fold, check if folding is done
            break
          }else{
            raster_list[[i]] <- stack(raster_list[[i]], raster_list[[i+1]]) #fold
            raster_list <- raster_list[-(i+1)] #delete the element already folded 
          }

        }
      }
      raster_2020 <- raster_list[[1]] #there is supposed to be only one element left in the list 
    }
    
  }
  names(raster_2020)<-colnames(rate_raster)[-ncol(rate_raster)]
###==========================================###                End of the edit                ###==========================================###

  return(raster_2020)
}

#' @name aggregate_affected_pop_across_cells
#' @title aggregate_affected_pop_across_cells: aggregate proportion of population living in each incidence group across cells for each layer
#' @param pop_raster_cropped: 1*1 KM population raster cropped by crop_to_shapefile function
#' @param rate_raster_cropped: 1*1 KM rate_raster (disaggregated from 20*20 KM rater raster) cropped by crop_to_shapefile function
#' @param threshold_list: the threshold to determine mild/moderate/high incidence areas
#' @return results_by_layer
aggregate_affected_pop_across_cells <- function(pop_raster_cropped, rate_raster_cropped,threshold_list){
  threshold_list <- threshold_list[order(threshold_list,decreasing = T)]
  pop_prop<-as.data.frame(matrix(NA,nrow=1,ncol=length(unique(threshold_list))))
  names(pop_prop) <- paste0(">=",threshold_list)
  for (threshold_idx in seq_len(length(threshold_list))) {
    pop_prop[,threshold_idx] <-
      100*sum(values(pop_raster_cropped)[values(rate_raster_cropped)>=threshold_list[threshold_idx]],na.rm=T)/
      sum(values(pop_raster_cropped),na.rm = T)
  }
  pop_prop<-t(  t(cbind(0,pop_prop)) - dplyr::lag(t(cbind(0,pop_prop)))  )[,-1]
  return(pop_prop)
}

#' @name aggregate_affected_pop_across_cells_by_districts 
#' @title aggregate_affected_pop_across_cells_by_districts: aggregate proportion of population living in each incidence group across districts for each layer
#' @param pop_raster_cropped: 1*1 KM population raster cropped by crop_to_shapefile function
#' @param rate_raster_cropped: 1*1 KM rate_raster (disaggregated from 20*20 KM rater raster) cropped by crop_to_shapefile function
#' @param threshold_list: the threshold to determine mild/moderate/high incidence areas
#' @param iso_code: country code 
#' @param district_level: 1-3 
#' @return results_by_layer
aggregate_affected_pop_across_cells_by_districts <- function( pop_raster_cropped, 
                                                              rate_raster_cropped, 
                                                              threshold_list, 
                                                              iso_code, 
                                                              district_level = 2){ 
  ### Loop through each admin-2 district 
  library(rgeoboundaries)
  if(iso_code == "ZNZ"){
    shapefiles <- gb_adm1("TZA")[gb_adm1("TZA")$shapeName %in% 
                                c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
  }else{
    shapefiles <- do.call(paste0('gb_adm', district_level), list(iso_code)) 
  }
  intermediate_table <- tibble(initial = NA) #initialize the output table
  threshold_list <- sort(threshold_list, decreasing = TRUE)
  
  for(i in 1:nrow(shapefiles)){ 
    ## Test if the district has any grid cells in it after cropping (using the first layer only)
    district_shp <- shapefiles[i, ] 
    district_pop_tmp <- crop_to_shapefile(pop_raster_cropped[[1]], district_shp, snap = "near") 
    district_rate_tmp <- crop_to_shapefile(rate_raster_cropped[[1]], district_shp, snap = "near") 
    if(all(is.na(raster::values(district_pop_tmp))) | all(is.na(raster::values(district_rate_tmp)))){
      next
    }
    rm(district_pop_tmp)
    rm(district_rate_tmp)

    ## If the district has any grid cells, carry out the rest 
    district_pop <- crop_to_shapefile(pop_raster_cropped, district_shp, snap = "near") 
    district_rate <- crop_to_shapefile(rate_raster_cropped, district_shp, snap = "near") 
    pop_prop <- as.data.frame(  matrix( NA, nrow=nlayers(district_rate), ncol=length(unique(threshold_list))+1 )  ) #the extra col is for pop 
    names(pop_prop) <- c(paste0(threshold_list, "<=", district_shp$shapeName), paste0("pop_", district_shp$shapeName)) #district name should be included
    # if there are duplicates
    duplicate_idx <- 2
    while(i > 1 & any(names(pop_prop) %in% names(intermediate_table))){
      names(pop_prop) <- c(paste0(names(pop_prop), "_", duplicate_idx))
      duplicate_idx <- duplicate_idx + 1
    }

    for (threshold_idx in seq_len(length(threshold_list))) {
      #to calculate the intervals -- based on the fact that the threshold_list order is decreasing
      if(threshold_idx == 1){
        rate_matrix <- matrix( as.numeric(values(district_rate) >= threshold_list[threshold_idx]), ncol = nlayers(district_rate) )
      }else{
        rate_matrix <- matrix(  as.numeric( values(district_rate) >= threshold_list[threshold_idx]
                                            & values(district_rate) <= threshold_list[threshold_idx - 1] ), 
                                ncol = nlayers(district_rate) )
      }
      
      pop_matrix <- matrix(values(district_pop), nrow = nrow(rate_matrix))
      rate_matrix[is.na(rate_matrix)] <- 0
      pop_matrix[is.na(pop_matrix)] <- 0
      target_pop_matrix <- t(rate_matrix) %*% pop_matrix

      pop_prop[, threshold_idx] <- as.numeric(target_pop_matrix / sum( values(district_pop), na.rm = T ) >= 0.1 | 
                                              target_pop_matrix >= 100000 ) 
      
      #add district pop to the last col                                        
      if( threshold_idx == length(threshold_list) ){
        pop_prop[, threshold_idx+1] <- sum( values(district_pop), na.rm = T ) 
      }
    }

    ## Combine the dataframe with the output table
    intermediate_table <- cbind(intermediate_table, pop_prop)
  }

  ### Return the table
  intermediate_table <- intermediate_table %>% dplyr::select(-initial)
  return(intermediate_table)

}

#' @name generate_final_table  
#' @title generate_final_table
#' @param intermediate_table
#' @param iso_code country code 
#' @param probability_cutoffs
#' @return 
generate_final_table <- function( intermediate_table, 
                                  iso_code, 
                                  probability_cutoffs = c(0.025,0.5,0.975)){ 
  district_vector <- unique(gsub( ".*<=", "", names(intermediate_table)[!grepl("^pop_.*", names(intermediate_table))] ))   
  threshold_vector <- unique(gsub( "<=.*", "", names(intermediate_table)[!grepl("^pop_.*", names(intermediate_table))] ))
  threshold_vector <- sort(as.numeric(threshold_vector), decreasing = TRUE)
  final_table <- as.data.frame(matrix(0, nrow = nrow(intermediate_table), ncol = length(threshold_vector)))
  names(final_table) <- paste0(">= ", threshold_vector, " population")

  # ### fix name -- remove ? and ()
  # if( any(grepl("\\?", district_vector)) | any(grepl("\\(", district_vector)) ){
  #   names(intermediate_table) <- stringr::str_replace_all(names(intermediate_table), "(?![!'#S%&'*+,-./:;<=>@[/]^_{|}~])[[:punct:]]", "")
  #   print(paste0(iso_code, " has had all the punctuation marks removed in its column names in the intermediate table. "))
  # }

  ### a little fix -- only count the highest threshold 
  for(threshold_value in threshold_vector){
    for(district_name in district_vector){

      if(threshold_value != max(threshold_vector)){
        higher_threshold_vector <- threshold_vector[threshold_vector > threshold_value]
        higher_threshold_vector <- sort(higher_threshold_vector, decreasing = FALSE)
        for(higher_threshold_value in higher_threshold_vector ){
          intermediate_table[, c(paste0(threshold_value, "<=", district_name))] <- (
            intermediate_table[, c(paste0(threshold_value, "<=", district_name))] * 
            as.numeric(intermediate_table[, c(paste0(higher_threshold_value, "<=", district_name))] == 0)
          )
        }
      }

      # if(threshold_value != max(threshold_vector)){
      #   higher_threshold_vector <- threshold_vector[threshold_vector > threshold_value]
      #   higher_threshold_vector <- sort(higher_threshold_vector, decreasing = FALSE)
      #   for(higher_threshold_value in higher_threshold_vector ){
      #     intermediate_table[, c(names(intermediate_table)[grepl( paste0("^", threshold_value, "<=", district_name, "$"), names(intermediate_table) )])] <- (
      #       intermediate_table[, c(names(intermediate_table)[grepl( paste0("^", threshold_value, "<=", district_name, "$"), names(intermediate_table) )])] * 
      #       as.numeric(intermediate_table[, c(names(intermediate_table)[grepl( paste0("^", higher_threshold_value, "<=", district_name, "$"), names(intermediate_table) )])] == 0)
      #     )
      #   }
      # }

    }
  }
  ## test -- only applies to three vector threshold for now
  for(idx in 1:(ncol(intermediate_table)/4)){
    # list_of_2 <- list()
    if(3 %in% apply(intermediate_table[, (4*(idx-1)+1):(4*(idx-1)+3)], 1, sum) | 2 %in% apply(intermediate_table[, (4*(idx-1)+1):(4*(idx-1)+3)], 1, sum)){
      col_idx <- (4*(idx - 1) + 1):(4*idx)
      row_idx <- match(2, apply(intermediate_table[, (4*(idx-1)+1):(4*(idx-1)+3)], 1, sum))
      print(c(row_idx, col_idx))
      # for(col_idx_s in col_idx){
        # list_of_2[[ (length(list_of_2) + 1) ]] <- c(row_idx, col_idx_s)
        # print(c(row_idx, col_idx_s))
      # }
      
      warning("The sum table is wrong. Please consider remaking it. ")
    }
  }

  ### Summerize -- new trying to use the exact column names 
  for(threshold_value in threshold_vector){
    for(district_name in district_vector){
      final_table[, c(names(final_table)[grepl(threshold_value, names(final_table))])] <- ( 
        final_table[, c(names(final_table)[grepl(threshold_value, names(final_table))])] + 
        (intermediate_table[, c(paste0(threshold_value, "<=", district_name))]
          * intermediate_table[, c(paste0("pop_", district_name))]) 
        )
    }
  }  

  # for(threshold_value in threshold_vector){
  #   for(district_name in district_vector){
  #     final_table[, c(names(final_table)[grepl(threshold_value, names(final_table))])] <- (final_table[, c(names(final_table)[grepl(threshold_value, names(final_table))])] + 
  #       (intermediate_table[, c(names(intermediate_table)[grepl( paste0(threshold_value, "<=", district_name, "$"), names(intermediate_table) )])]
  #       # (intermediate_table[, c(names(intermediate_table)[grepl(threshold_value, names(intermediate_table)) & grepl(district_name, names(intermediate_table))])]
  #       * intermediate_table[, c(names(intermediate_table)[grepl( paste0("^pop_", district_name, "$"), names(intermediate_table) )])]))
  #       # * intermediate_table[, c(names(intermediate_table)[grepl("^pop_.*", names(intermediate_table)) & grepl(district_name, names(intermediate_table))])]))
  #   }
  # }

  final_summary_table <- apply(final_table, 2, mean)
  for(cutoffs in probability_cutoffs){
    final_summary_table <- rbind(final_summary_table, apply(final_table, 2, quantile, cutoffs))
  }
  final_summary_table <- as.data.frame(final_summary_table) %>% 
    dplyr::mutate(statistics = c("mean", probability_cutoffs)) %>%
    dplyr::select(statistics, names(final_table))

  return(final_summary_table)
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
                                                             threshold_list=threshold_list)
 }
  pop_prop=do.call('rbind',results_by_layer)
  return(across_layer_aggregator(pop_prop=pop_prop,
                                 probability_cutoffs = probability_cutoffs,
                                 include_mean = include_mean,
                                 calculate_disjoint_values = calculate_disjoint_values))
}





####### Run the whole script 
### Define the local pipeline model directory, what model output to summarize, and where the final output should be saved
cholera_directory <- "/home/kaiyuezou/mapping_pipeline/9_23_dev/cholera-mapping-pipeline" 
new_config_dir <- "Analysis/configs/Gavi_reports/Dec_2021_runs"
old_config_dir <- "Analysis/configs/Gavi_reports/Jul_2021_runs/2015_2019_full_base"
new_output_dir <- "Analysis/data/true_2021_Dec_runs_output_for_push/cholera-mapping-output"
old_output_dir_single <- "Analysis/data/June_2021_runs/single/cholera-mapping-output"
old_output_dir_multiple <- "Analysis/data/June_2021_runs/multiple/cholera-mapping-output"
model_output_version <- "both" #new/old/both
final_output_dir <- "/home/kaiyuezou/mapping_pipeline/prop_3_17" 



### List the new countries for model output -- this is from country status
country_status <- read_country_status() #download from google
if(!run_individual_country){
  if(tolower(model_output_version) == "new"){
      Dec_2021_list <- unique(country_status[ country_status$report_old_new_output == 'new'
                                              & country_status$rerun_approved == 'TRUE' 
                                              & grepl("done", country_status$`% of pop`), ]$approved_country)
  }else if(tolower(model_output_version) == "old"){
      Dec_2021_list <- unique(country_status[ country_status$report_old_new_output == 'old'
                                              & grepl("done", country_status$`% of pop`), ]$approved_country)
  }else{
      Dec_2021_list <- unique(country_status[grepl("done", country_status$`% of pop`), ]$approved_country)
  }

  # Dec_2021_list <- unique(country_status[ country_status$report_old_new_output == 'new'
  #                                         # & country_status$rerun_approved == 'TRUE' 
  #                                         & grepl("done", country_status$`% of pop`)
  #                                         # & country_status$single_year_multiple_year == 'single'
  #                                         , ]$approved_country)
  Dec_2021_list <- Dec_2021_list[!paste0(Dec_2021_list, "_sum_table.csv") %in% list.files(final_output_dir)]
  # Dec_2021_list <- Dec_2021_list[!Dec_2021_list %in% c("ZNZ")]
  ######!!!!!!!!!!!!!! JUST FOR NOW !!!!!!!!!!!!!
}


### Loop through the countries to run
for(country_code in Dec_2021_list){
  print(paste0(country_code, " just started running. "))
  params <- new.env()
  params$cholera_directory <- cholera_directory 
  params$config <- find_config_fn(cholera_directory, country_code, country_status, new_config_dir, old_config_dir)
  params$local_output_dir <- find_output_dir(cholera_directory, country_code, country_status, new_output_dir, old_output_dir_single, old_output_dir_multiple)
  
  params$old_runs <- (country_status[country_status$approved_country==country_code, ]$report_old_new_output == "old")
  params$single_year <- (country_status[country_status$approved_country==country_code, ]$single_year_multiple_year == "single")
  if(params$single_year & length(params$config) < 5){
    print(paste0("Country ", country_code, " needs 5 configs but only has ", length(params$config), ". "))
    print("It has not been run due to this issue, please try it again after check. ")
    next
  }else if((!params$single_year) & length(params$config) > 1){
    print(paste0("Country ", country_code, " only needs 1 configs but instead has ", length(params$config), ". "))
    print("It has not been run due to this issue, please try it again after check. ")
    next
  }

  year_range <- country_status[country_status$approved_country == country_code, ]$approved_years
  if(length(grep("^[0-9]{4}-[0-9]{4}$", year_range)) != 0){
    assume_0_year_index <- NULL
  }else{
    year_list <- as.numeric(unlist(stringr::str_split(year_range, ",")))
    year_index<- match(year_list, 2015:2019)
    assume_0_year_index <- (1:5)[!(1:5) %in% year_index]
  }

  if(country_code == "SDN"){ 
    dropped_year_index <- c(3) 
    assume_0_year_index <- c(1) 
  }else{ dropped_year_index <- NULL }
  
  

  ### When everything is ready
  skip_to_next <- FALSE
  tryCatch({
    ## It is different for multi-year run and single year run
    if(!params$single_year){

      config <- yaml::read_yaml(params$config)
      file_names <- taxdat::get_filenames(config, params$cholera_directory)
      file_names <- unlist(lapply(names(file_names), function(file_name){
                                        gsub("^.*?/Analysis/data", params$local_output_dir, file_names[file_name])
                                        }))

      if(params$old_runs){
        file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],"iv-wN-cwN-csF-teT-teaF-weF.")
        file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],"-F")
        # file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],".F")
        for(file_name in names(file_names)){
          file_names[[file_name]] <- stringr::str_remove(file_names[[file_name]],"_allOCs")
        } 
      }
      if( !file.exists(file_names[["covar"]]) | !file.exists(file_names[["stan_output"]]) | !file.exists(file_names[['stan_input']]) ){
        all_files <- c(file_names[["covar"]], file_names[["stan_output"]], file_names[["stan_input"]])
        missing_file <- all_files[!file.exists(all_files)]
        print(paste0(country_code, " does not have the required model outputs ", missing_file, ", please check. "))
        file_names[["stan_output"]] <- fix_stan_output_name(file_names[["stan_output"]]) 
        # next
      }

      iso_code <- country_code
      if(iso_code != "ZNZ"){shapefile <- rgeoboundaries::gb_adm0(iso_code)}else{
        country_shp <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% 
                                                      c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
        shapefile <- sf::st_union(country_shp)
        shapefile <- sf::as_Spatial(shapefile)
      }

      rate_raster_2020 <- get_rate_raster(covar_data_filename=file_names[["covar"]],
                                          model_output_filenames=file_names[["stan_output"]],
                                          stan_input_filenames=file_names[['stan_input']],
                                          if_single_year_run=params$single_year,
                                          res=c(0.1666667,0.1666667), 
                                          assume_0_year_index=assume_0_year_index, 
                                          dropped_year_index=dropped_year_index)
      
      if(iso_code %in% c("COD", "KEN", "SDN", "TGO", "TZA", "SEN")){
        rate_raster_2020_cropped <- rate_raster_2020
      }else{
        rate_raster_2020_cropped <- crop_to_shapefile(raster = rate_raster_2020,shapefile = shapefile,snap="near")
      }
      
      # save the rate raster to save time 
      raster::writeRaster(raster::stack(rate_raster_2020_cropped), filename = paste0(final_output_dir, "/", country_code, "_rate_raster.tif"))
      print("The rate raster have been saved. ")
      rm(rate_raster_2020)
      gc()
    
    }else{

      rate_raster_2020_stacked <- NULL
      for(config_fns in params$config){
        # first the dropped years
        if( !is.null(dropped_year_index) & (match(config_fns, params$config) %in% dropped_year_index) ){next}
        # then the assume-0 years
        if( !is.null(assume_0_year_index) & (match(config_fns, params$config) %in% assume_0_year_index) ){
          dropped_year_index_single_year <- NULL
          assume_0_year_index_single_year <- c(1)
        }else{
          dropped_year_index_single_year <- NULL
          assume_0_year_index_single_year <- NULL
        }
        
        # carry out the rest 
        config <- yaml::read_yaml(config_fns)
        file_names <- taxdat::get_filenames(config, params$cholera_directory)
        file_names <- unlist(lapply(names(file_names), function(file_name){
                                          gsub("^.*?/Analysis/data", params$local_output_dir, file_names[file_name])
                                          }))

        if(params$old_runs){
          file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],"iv-wN-cwN-csF-teT-teaF-weF.")
          file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],"-F")
          # file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],".F")
          file_names_saved <- names(file_names)
          file_names <- unlist(lapply(names(file_names), function(file_name){
                                      stringr::str_remove(file_names[[file_name]], "_allOCs")
                                      })) 
          names(file_names) <- file_names_saved
        }
        if( !file.exists(file_names[["covar"]]) | !file.exists(file_names[["stan_output"]]) | !file.exists(file_names[['stan_input']]) ){
          all_files <- c(file_names[["covar"]], file_names[["stan_output"]], file_names[["stan_input"]])
          missing_file <- all_files[!file.exists(all_files)]
          print(paste0(country_code, " does not have the required model outputs ", missing_file, ", please check. "))
          file_names[["stan_output"]] <- fix_stan_output_name(file_names[["stan_output"]])
          # next
        }

        iso_code <- country_code
        if(iso_code != "ZNZ"){shapefile <- rgeoboundaries::gb_adm0(iso_code)}else{
          country_shp <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% 
                                                        c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
          shapefile <- sf::st_union(country_shp)
          shapefile <- sf::as_Spatial(shapefile)
        }



        ### what if the output is not even created 
        if(!is.null(assume_0_year_index_single_year) & !file.exists(file_names[["covar"]]) & !file.exists(file_names[['stan_input']])){
          ## get a year that has output and fill in 0's 
          years_to_use <- (1:5)[!(1:5) %in% assume_0_year_index][1]
          print(paste0("Now using the year ", years_to_use, " to creat a 0-case year for year ", match(config_fns, params$config)))
          

          config_fns <- params$config[years_to_use]
          config <- yaml::read_yaml(config_fns)
          file_names <- taxdat::get_filenames(config, params$cholera_directory)
          file_names <- unlist(lapply(names(file_names), function(file_name){
                                            gsub("^.*?/Analysis/data", params$local_output_dir, file_names[file_name])
                                            }))

          if(params$old_runs){
            file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],"iv-wN-cwN-csF-teT-teaF-weF.")
            file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],"-F")
            # file_names[["stan_output"]] <- stringr::str_remove(file_names[["stan_output"]],".F")
            file_names_saved <- names(file_names)
            file_names <- unlist(lapply(names(file_names), function(file_name){
                                        stringr::str_remove(file_names[[file_name]], "_allOCs")
                                        })) 
            names(file_names) <- file_names_saved
          }
          if( !file.exists(file_names[["covar"]]) | !file.exists(file_names[["stan_output"]]) | !file.exists(file_names[['stan_input']]) ){
            all_files <- c(file_names[["covar"]], file_names[["stan_output"]], file_names[["stan_input"]])
            missing_file <- all_files[!file.exists(all_files)]
            print(paste0(country_code, " does not have the required model outputs ", missing_file, ", please check. "))
            file_names[["stan_output"]] <- fix_stan_output_name(file_names[["stan_output"]])
            # next
          }

          iso_code <- country_code
          if(iso_code != "ZNZ"){shapefile <- rgeoboundaries::gb_adm0(iso_code)}else{
            country_shp <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% 
                                                          c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
            shapefile <- sf::st_union(country_shp)
            shapefile <- sf::as_Spatial(shapefile)
          }

          rate_raster_2020 <- get_rate_raster(covar_data_filename=file_names[["covar"]],
                                              model_output_filenames=file_names[["stan_output"]],
                                              stan_input_filenames=file_names[['stan_input']],
                                              if_single_year_run=params$single_year,
                                              res=c(0.1666667,0.1666667), 
                                              assume_0_year_index=assume_0_year_index_single_year, 
                                              dropped_year_index=dropped_year_index_single_year)

        }else{
          rate_raster_2020 <- get_rate_raster(covar_data_filename=file_names[["covar"]],
                                              model_output_filenames=file_names[["stan_output"]],
                                              stan_input_filenames=file_names[['stan_input']],
                                              if_single_year_run=params$single_year,
                                              res=c(0.1666667,0.1666667), 
                                              assume_0_year_index=assume_0_year_index_single_year, 
                                              dropped_year_index=dropped_year_index_single_year)
        }





        # if(iso_code %in% c("COD", "KEN", "SDN", "TGO", "TZA", "SEN")){
        #   rate_raster_2020_cropped_tmp <- rate_raster_2020
        # }else{
        #   rate_raster_2020_cropped_tmp <- crop_to_shapefile(raster = rate_raster_2020,shapefile = shapefile,snap="near")
        # }
        # rate_raster_2020_cropped_tmp <- crop_to_shapefile(raster = rate_raster_2020,shapefile = shapefile,snap="near")
        ### stack different years together 
        if(is.null(rate_raster_2020_stacked)){rate_raster_2020_stacked <- rate_raster_2020}else{
          if(extent(rate_raster_2020) != extent(rate_raster_2020_stacked) & country_code == "TZA"){
            rate_raster_2020 <- raster::resample(rate_raster_2020, rate_raster_2020_stacked) 
          }
          rate_raster_2020_stacked <- stack(rate_raster_2020_stacked, rate_raster_2020)
        }
        
        rm(rate_raster_2020)
        # rm(rate_raster_2020_cropped_tmp) 
        gc()
      }

      # save the rate raster to save time 
      if(iso_code %in% c("COD", "KEN", "SDN", "TGO", "TZA", "SEN")){
        rate_raster_2020_cropped <- rate_raster_2020_stacked
      }else{
        rate_raster_2020_cropped <- crop_to_shapefile(raster = rate_raster_2020_stacked,shapefile = shapefile,snap="near")
      }
      raster::writeRaster(raster::stack(rate_raster_2020_cropped), filename = paste0(final_output_dir, "/", country_code, "_rate_raster.tif"))
      print("The rate raster have been saved. ")
    }






    #pop raster -- only use the 2017 population data 
    if(country_code == "TZA"){
      config_fns <- params$config[1]
      config <- yaml::read_yaml(config_fns)
      file_names <- taxdat::get_filenames(config, params$cholera_directory)
      file_names <- unlist(lapply(names(file_names), function(file_name){
                                        gsub("^.*?/Analysis/data", params$local_output_dir, file_names[file_name])
                                        }))
    }
    stan_input <- read_file_of_type(file_names[['stan_input']], "stan_input")
    pop_sf_grid<-stan_input$sf_grid
    pop_sf_grid_2017<-get_pop2017(pop_sf_grid)
    pop_sf_grid_2017<-pop_sf_grid_2017[1:length(unique(pop_sf_grid_2017$geom)),]
    empty_pop_raster<-raster::raster(pop_sf_grid_2017, res =c(0.1666667,0.1666667)) 

    #assign pop values into the raster
    pop_raster_2017 <- fasterize::fasterize(pop_sf_grid_2017, empty_pop_raster, field = "pop2017")

    pop_raster_2017_cropped <- crop_to_shapefile(raster = pop_raster_2017,shapefile = shapefile,snap="near")
    #rate_raster_2020_cropped <- raster::resample(rate_raster_2020_cropped, pop_raster_2017_cropped) # only for certain countries!! 
    
    # if(extent(pop_raster_2017_cropped) != extent(rate_raster_2020_cropped)){
    #   rate_raster_2020_cropped <- raster::resample(rate_raster_2020_cropped, pop_raster_2017_cropped)
    # }

    rm(pop_raster_2017)
    gc()

    ###==========================================### For the BEN test Kaiyue added on Mar 9th 2022 ###==========================================###
    print("The rate and pop rasters have been generated. ")
    # fix for COD KEN SDN TGO TZA SEN
    if(iso_code %in% c("KEN")){
      rate_raster_2020_cropped <- raster::resample(rate_raster_2020_cropped, pop_raster_2017_cropped)
    }else if( iso_code %in% c("COD", "TGO", "SEN") ){
      # pop_raster_2017_cropped <- raster::resample(pop_raster_2017_cropped, rate_raster_2020_cropped)
      # rate_raster_2020_cropped <- raster::resample(rate_raster_2020_cropped, pop_raster_2017_cropped)
      # rate_raster_2020_cropped <- crop_to_shapefile(rate_raster_2020_cropped, pop_raster_2017_cropped, snap="near")

      pop_raster_2017_cropped <- crop_to_shapefile(pop_raster_2017_cropped, rate_raster_2020_cropped[[1]], snap="near") # -- COD
    }else if( iso_code %in% c("SDN") ){
      rate_raster_2020_cropped <- raster::alignExtent(extent(pop_raster_2017_cropped), rate_raster_2020_cropped, snap='near')
      # rate_raster_2020_cropped <- crop_to_shapefile(rate_raster_2020_cropped, pop_raster_2017_cropped, snap="near")
      # rate_raster_2020_cropped <- raster::resample(rate_raster_2020_cropped, pop_raster_2017_cropped)
      # rate_raster_2020_cropped <- raster::stack(rate_raster_2020_cropped)
    }
    

    intermediate_output <- aggregate_affected_pop_across_cells_by_districts(pop_raster_cropped = pop_raster_2017_cropped, 
                                                                            rate_raster_cropped = rate_raster_2020_cropped, 
                                                                            threshold_list = c(0.001,0.0001,0.00001), 
                                                                            iso_code = iso_code, 
                                                                            district_level = 2)
    # If we want to reorganize the column names 
    intermediate_output_by_threshold <- intermediate_output[, sort(colnames(intermediate_output))]
    # Save the table to take a look at
    readr::write_csv(intermediate_output, paste0(final_output_dir, "/", iso_code, "_inter_table.csv"))
    readr::write_csv(intermediate_output_by_threshold, paste0(final_output_dir, "/", iso_code, "_thres_table.csv"))
    # Summarize the table and save
    sum_table <- generate_final_table(intermediate_output, iso_code) 
    readr::write_csv(sum_table, paste0(final_output_dir, "/", iso_code, "_sum_table.csv"))
    print("All the files have been successfully saved. ")

  

  }, error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }    

}





##### aggregate all the sum tables 
country_status <- read_country_status()
Dec_2021_full_list <- unique(country_status[grepl("done", country_status$`% of pop`), ]$approved_country)
Dec_2021_full_list <- sort(Dec_2021_full_list)
Dec_2021_full_list <- Dec_2021_full_list[paste0(Dec_2021_full_list, "_sum_table.csv") %in% list.files(final_output_dir)]

if( all(paste0(Dec_2021_full_list, "_sum_table.csv") %in% list.files(final_output_dir)) ){
  ### convert country code to country name
  country_name_full_list <- countrycode::countrycode(Dec_2021_full_list, "iso3c", "country.name")
  country_name_full_list[match(NA, country_name_full_list)] <- "Zanzibar"
  country_name_full_list[match("Tanzania", country_name_full_list)] <- "Tanzania Mainland"
  country_name_full_list[match("Congo - Kinshasa", country_name_full_list)] <- "DRC"
  country_name_full_list[match("Congo - Brazzaville", country_name_full_list)] <- "Republic of the Congo"

  ### make the table 
  agg_sum_table <- tibble::tibble(country_name = as.character(), 
                                  mean_1e_5 = as.numeric(), 
                                  median_1e_5 = as.numeric(), 
                                  q05_1e_5 = as.numeric(), 
                                  q95_1e_5 = as.numeric(), 
                                  mean_1e_4 = as.numeric(), 
                                  median_1e_4 = as.numeric(), 
                                  q05_1e_4 = as.numeric(), 
                                  q95_1e_4 = as.numeric(), 
                                  mean_1e_3 = as.numeric(), 
                                  median_1e_3 = as.numeric(), 
                                  q05_1e_3 = as.numeric(), 
                                  q95_1e_3 = as.numeric())

  for(country_code in Dec_2021_full_list){
    sum_table <- readr::read_csv(paste0(final_output_dir, "/", country_code, "_sum_table.csv"))

    agg_sum_table <- agg_sum_table %>% 
      add_row(country_name = country_name_full_list[match(country_code, Dec_2021_full_list)], 
              mean_1e_5 = as.numeric(sum_table[1, 4]), 
              median_1e_5 = as.numeric(sum_table[3, 4]), 
              q05_1e_5 = as.numeric(sum_table[2, 4]), 
              q95_1e_5 = as.numeric(sum_table[4, 4]), 
              mean_1e_4 = as.numeric(sum_table[1, 3]), 
              median_1e_4 = as.numeric(sum_table[3, 3]), 
              q05_1e_4 = as.numeric(sum_table[2, 3]), 
              q95_1e_4 = as.numeric(sum_table[4, 3]), 
              mean_1e_3 = as.numeric(sum_table[1, 2]), 
              median_1e_3 = as.numeric(sum_table[3, 2]), 
              q05_1e_3 = as.numeric(sum_table[2, 2]), 
              q95_1e_3 = as.numeric(sum_table[4, 2]) )
  }
  names(agg_sum_table) <- c("Country Name", ">= 1e-05 Mean", ">= 1e-05 Median", ">= 1e-05 Q0.05", ">= 1e-05 Q0.95", 
                                            ">= 1e-04 Mean", ">= 1e-04 Median", ">= 1e-04 Q0.05", ">= 1e-04 Q0.95", 
                                            ">= 1e-03 Mean", ">= 1e-03 Median", ">= 1e-03 Q0.05", ">= 1e-03 Q0.95")
  readr::write_csv(agg_sum_table, paste0(final_output_dir, "/all_country_sum_table.csv"))
}

