#' @include plot_cache_function.R

#' @export
#' @name plot_pop_by_admin
#' @title plot_pop_by_admin
#' @description add
#' @param config
#' @param cache
#' @param cholera_directory
#' @return table
plot_pop_by_admin <- function(cache,cholera_directory,config) {
  config<-yaml::read_yaml(paste0(cholera_directory,config))
  
  #load pop and sf objects
  covar_cube <- cache[["covar_cube"]]
  sf_grid <- cache[["sf_grid"]]
  pop_layer <- covar_cube[,,1, drop = F] ## population is always the first layer
  covar <- data.frame(covar = unlist(lapply(1:ncol(pop_layer), function(x){
    pop_layer[, x, 1]
  })))
  pltdata <- dplyr::bind_cols(sf_grid, covar)
  
  analysis_years <- lubridate::year(config$start_time):lubridate::year(config$end_time)
  pltdata$t<- factor(pltdata$t, labels = analysis_years )
  
if(nrow(cache[["sf_grid"]]) == prod(dim(pop_layer))){
  
  ### Use the geo package
  iso_code <- as.character(stringr::str_extract(params$config, "[A-Z]{3}"))
  admin_level <- as.numeric(params$admin_level_for_summary_table)
  if (iso_code == "ZNZ" & admin_level == 1){
    boundary_sf <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
  } else if (iso_code == "ZNZ"){
    stop('Sorry, currently ZNZ only has the admin 1 level shape files available to use, please try again. ')
  } else{
    if (admin_level == 1){
      boundary_sf <- rgeoboundaries::gb_adm1(iso_code)
    }else if (admin_level == 2){
      boundary_sf <- rgeoboundaries::gb_adm2(iso_code)
      warning('The current admin level is set at 2. ')
    }else if (admin_level == 3){
      boundary_sf <- rgeoboundaries::gb_adm3(iso_code)
      warning('The current admin level is set at 3. ')
    }else{
      stop('Error: the current admin level is unnecessarily high or invalid, 
    please check and change the parameters for the country data report before running again. ')
    }
  }
  
  ### For loops and make the table
  admin_pop_table <- data.frame(matrix(NA, length(boundary_sf$shapeName), length(unique(pltdata$t)) + 1))
  
  start_year <- as.numeric(stringr::str_extract(stringr::str_extract(params$config, "[A-Z]{3}_[0-9]{4}"), "[0-9]{4}"))
  pltdata$t<-as.numeric(pltdata$t)
  pltdata$t<-as.numeric(pltdata$t-min(pltdata$t))+1
  year_list <- unique(pltdata$t) + (start_year-1)
  
  if(is.na(start_year)){
    start_year <- as.numeric(stringr::str_extract(params$config, "[0-9]{1,4}"))
    year_list <- unique(pltdata$t) + (start_year-1)
    
    if(!start_year%in%c(2015,2016,2017,2018,2019)){
      start_year <- as.numeric(stringr::str_extract(params$config, "\\d+(?=.yml)"))
      year_list <- start_year
    }
  }
  
  colnames(admin_pop_table) <- c('adminlevel', year_list)
  admin_pop_table$adminlevel <- boundary_sf$shapeName
  
  pop_raster_data<-raster()
  for(layer in unique(pltdata$t)){
    empty_raster <- raster::raster(pltdata[which(pltdata$t==layer),], res = max(0.1666666, 0.1666666))#20*20km raster
    pop_raster_data_tmp <- fasterize::fasterize(pltdata[which(pltdata$t==layer),], empty_raster, field = c("covar"))
    pop_raster_data<-stack(pop_raster_data,pop_raster_data_tmp)
  }
  
  for(layer in 1:raster::nlayers(pop_raster_data)){
    for (locs in admin_pop_table$adminlevel){
      cropped <- raster::crop(pop_raster_data[[layer]], boundary_sf[boundary_sf$shapeName == locs, layer+1], snap = "out")
      masked <- raster::mask(cropped, boundary_sf[boundary_sf$shapeName == locs, layer+1], updatevalue = NA)
      sum_pop <- sum(raster::getValues(masked), na.rm = TRUE)
      admin_pop_table[match(locs, admin_pop_table$adminlevel), layer+1]<-round(sum_pop,4) 
      rm(cropped, masked)
    }  
  }
  
  ### Add a total row, change colnames, and display the table
  total_row <- c('Total', apply(data.frame(admin_pop_table[, -1]), 2, sum))
  admin_pop_table <- rbind(admin_pop_table, total_row)
  
  admin_pop_table %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
    kableExtra::kable(col.names = c('Admin Level', year_list)) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::row_spec(nrow(admin_pop_table), bold = T)
  
} else{
  warning("sf_grid has a different number of cells or timepoints than covar_cube")
}
}

#' @export
#' @name plot_cases_by_admin
#' @title plot_cases_by_admin
#' @description add
#' @param config
#' @param cache
#' @param cholera_directory
#' @return table
plot_cases_by_admin <- function(cache, config, cholera_directory){
  config<-yaml::read_yaml(paste0(cholera_directory,config))
  ### Clean up the sf dataset
  cache[["disaggregated_rate_sf"]]<-get_disaggregated_rates_sf(
    cache=cache,config=params$config,cholera_directory=params$cholera_directory)

  case_raster_admin<-cache[["disaggregated_rate_sf"]]
  colnames(case_raster_admin)[stringr::str_detect(colnames(case_raster_admin),"modeled_case")] <- "value"
  
  iso_code <- as.character(stringr::str_extract(params$config, "[A-Z]{3}"))
  admin_level <- as.numeric(params$admin_level_for_summary_table)
  if (iso_code == "ZNZ" & admin_level == 1){
    boundary_sf <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
  } else if (iso_code == "ZNZ"){
    stop('Sorry, currently ZNZ only has the admin 1 level shape files available to use, please try again. ')
  } else{
    if (admin_level == 1){
      boundary_sf <- rgeoboundaries::gb_adm1(iso_code)
    }else if (admin_level == 2){
      boundary_sf <- rgeoboundaries::gb_adm2(iso_code)
      warning('The current admin level is set at 2. ')
    }else if (admin_level == 3){
      boundary_sf <- rgeoboundaries::gb_adm3(iso_code)
      warning('The current admin level is set at 3. ')
    }else{
      stop('Error: the current admin level is unnecessarily high or invalid, 
    please check and change the parameters for the country data report before running again. ')
    }
  }
  
  ### For loops and make the table
  admin_case_table <- data.frame(matrix(NA, length(boundary_sf$shapeName), length(unique(case_raster_admin$t)) + 1))
  year_list <- unique(as.numeric(case_raster_admin$t)) + (lubridate::year(as.Date(config_file$start_time,origin="1970-01-01"))-1)
  
  colnames(admin_case_table) <- c('adminlevel', year_list)
  admin_case_table$adminlevel <- boundary_sf$shapeName
  
  disaggregated_case_raster<-disaggregate_case_raster(cache,
                                                      config=params$config,
                                                      cholera_directory=params$cholera_directory)
  
  for (ts in unique(case_raster_admin$t)){
    ## filter and rasterize
    case_raster_admin_ts <- case_raster_admin %>% filter(t == ts)
    empty_raster <- raster::raster(case_raster_admin_ts, res = res(disaggregated_case_raster))
    raster_data <- fasterize::fasterize(case_raster_admin_ts, empty_raster, field = "value")
    
    ## across different locations
    for (locs in admin_case_table$adminlevel){
      cropped <- raster::crop(raster_data, boundary_sf[boundary_sf$shapeName == locs, ], snap = "near")
      masked <- raster::mask(cropped, boundary_sf[boundary_sf$shapeName == locs, ], updatevalue = NA)
      sum_case <- sum(raster::getValues(masked), na.rm = TRUE)
      admin_case_table[match(locs, admin_case_table$adminlevel), (match(ts, unique(case_raster_admin$t)) + 1)] <- sum_case
      rm(cropped, masked)
    } 
    
    ## optimize memory
    rm(case_raster_admin_ts, empty_raster, raster_data)
  }
  
  ### Add a total row, change colnames, and display the table
  total_row <- c('Total', apply(data.frame(admin_case_table[, -1]), 2, sum))
  admin_case_table <- rbind(admin_case_table, total_row)
  admin_case_table[, -1] <- apply(as.matrix(noquote(admin_case_table[, -1])),  # Using apply function
                                  2,
                                  as.numeric)
  admin_case_table$mean_across_years <- apply(admin_case_table[, -1], 1, mean)
  admin_case_table <- admin_case_table %>% 
    mutate_if(is.numeric, round, digits=4)
  
  admin_case_table %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
    kableExtra::kable(col.names = c('Admin Level', year_list, 'Mean across Years')) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::row_spec(nrow(admin_case_table), bold = T)
}

#' @export
#' @name plot_incidence_by_admin
#' @title plot_incidence_by_admin
#' @description add
#' @param config
#' @param cache
#' @param cholera_directory
#' @return table
plot_incidence_by_admin <- function(cache,config,cholera_directory){
  
  config<-yaml::read_yaml(paste0(cholera_directory,config))
  ### Clean up the sf dataset
  cache[["disaggregated_rate_sf"]]<-get_disaggregated_rates_sf(
    cache=cache,config=params$config,cholera_directory=params$cholera_directory)
  
  ### Clean up the sf dataset
  rate_raster_admin<-cache[["disaggregated_rate_sf"]]
  colnames(rate_raster_admin)[stringr::str_detect(colnames(rate_raster_admin),"modeled rate")] <- "value"
  
  iso_code <- as.character(stringr::str_extract(params$config, "[A-Z]{3}"))
  admin_level <- as.numeric(params$admin_level_for_summary_table)
  if (iso_code == "ZNZ" & admin_level == 1){
    boundary_sf <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
  } else if (iso_code == "ZNZ"){
    stop('Sorry, currently ZNZ only has the admin 1 level shape files available to use, please try again. ')
  } else{
    if (admin_level == 1){
      boundary_sf <- rgeoboundaries::gb_adm1(iso_code)
    }else if (admin_level == 2){
      boundary_sf <- rgeoboundaries::gb_adm2(iso_code)
      warning('The current admin level is set at 2. ')
    }else if (admin_level == 3){
      boundary_sf <- rgeoboundaries::gb_adm3(iso_code)
      warning('The current admin level is set at 3. ')
    }else{
      stop('Error: the current admin level is unnecessarily high or invalid, 
    please check and change the parameters for the country data report before running again. ')
    }
  }
  
  ##### Multiply two rasters together
  disaggregated_rate_raster<-disaggregate_rate_raster(cache,
                                                      config=params$config,
                                                      cholera_directory=params$cholera_directory)
  disaggregated_case_raster<-disaggregate_case_raster(cache,
                                                      config=params$config,
                                                      cholera_directory=params$cholera_directory)
  
  #load pop and sf objects
  covar_cube <- cache[["covar_cube"]]
  sf_grid <- cache[["sf_grid"]]
  pop_layer <- covar_cube[,,1, drop = F] ## population is always the first layer
  covar <- data.frame(covar = unlist(lapply(1:ncol(pop_layer), function(x){
    pop_layer[, x, 1]
  })))
  pltdata <- dplyr::bind_cols(sf_grid, covar)
  analysis_years <- lubridate::year(config$start_time):lubridate::year(config$end_time)
  pltdata$t<- factor(pltdata$t, labels = analysis_years )
  pop_raster_data<-raster()
  for(layer in unique(pltdata$t)){
    empty_raster <- raster::raster(pltdata[which(pltdata$t==layer),], res = max(0.1666666, 0.1666666))#20*20km raster
    pop_raster_data_tmp <- fasterize::fasterize(pltdata[which(pltdata$t==layer),], empty_raster, field = c("covar"))
    pop_raster_data<-stack(pop_raster_data,pop_raster_data_tmp)
  }

  ### For loops and make the table
  admin_rate_table <- data.frame(matrix(NA, length(boundary_sf$shapeName), length(unique(rate_raster_admin$t)) + 1))
  year_list <- as.numeric(unique(rate_raster_admin$t)) + (lubridate::year(as.Date(config$start_time,origin="1970-01-01"))-1)
  colnames(admin_rate_table) <- c('adminlevel', year_list)
  admin_rate_table$adminlevel <- boundary_sf$shapeName
  
  total_w_rate<-c()
  for (ts in unique(rate_raster_admin$t)){
    ## the rates raster
    rate_raster_admin_ts <- rate_raster_admin %>% filter(t == ts)
    raster_data_cal_case <- disaggregated_case_raster[[as.numeric(ts)]]
    
    pop_cropped <- raster::crop(pop_raster_data,disaggregated_rate_raster)
    origin(pop_cropped)<-origin(disaggregated_rate_raster)
    raster_data_pop <- pop_cropped
    
    ## across different locations
    for (locs in admin_rate_table$adminlevel){
      ## the calculated case raster
      cropped <- raster::crop(raster_data_cal_case, boundary_sf[boundary_sf$shapeName == locs, ], snap = "near")
      masked <- raster::mask(cropped, boundary_sf[boundary_sf$shapeName == locs, ], updatevalue = NA)
      sum_cal_case <- sum(raster::getValues(masked), na.rm = TRUE)
      ## the pop raster
      cropped <- raster::crop(raster_data_pop, boundary_sf[boundary_sf$shapeName == locs, ], snap = "near")
      masked <- raster::mask(cropped, boundary_sf[boundary_sf$shapeName == locs, ], updatevalue = NA)
      sum_pop <- sum(raster::getValues(masked), na.rm = TRUE)
      ## the table
      w_rate <- (sum_cal_case / sum_pop) * 1e4
      admin_rate_table[match(locs, admin_rate_table$adminlevel), (match(ts, unique(rate_raster_admin$t)) + 1)] <- w_rate
      rm(cropped, masked)
    } 
    
    ## the total weighted rate
    if(exists('total_w_rate')){
      total_w_rate <- c(total_w_rate,
                        sum(raster::getValues(raster_data_cal_case), na.rm = TRUE) / sum(raster::getValues(raster_data_pop), na.rm = TRUE))
    }else{
      total_w_rate <- sum(raster::getValues(raster_data_cal_case), na.rm = TRUE) / sum(raster::getValues(raster_data_pop), na.rm = TRUE)
    }
    
    ## optimize memory
    rm(rate_raster_admin_ts, raster_data_cal_case,raster_data_pop)
    
  }
  
  ### Add a total row, change colnames, and display the table
  total_row <- c('Weighted Total', total_w_rate * 1e4)
  admin_rate_table <- rbind(admin_rate_table, total_row)
  admin_rate_table[, -1] <- apply(as.matrix(noquote(admin_rate_table[, -1])),  # Using apply function
                                  2,
                                  as.numeric)
  admin_rate_table$mean_across_years <- apply(admin_rate_table[, -1], 1, mean)
  admin_rate_table <- admin_rate_table %>% 
    mutate_if(is.numeric, round, digits=4)
  
  admin_rate_table %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
    kableExtra::kable(col.names = c('Admin Level', year_list, 'Mean across Years')) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::row_spec(nrow(admin_rate_table), bold = T)
}