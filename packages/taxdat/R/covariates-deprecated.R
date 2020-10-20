#' @export
#' @name load_population_estimates
#' @title load_population_estimates
#' @description Find population estimates for a year and overlay them on a raster.  This will take much longer on rasters without a calculable resolution
#' @param region The region for which underlying raster to use. one of "AFR" "ASI" or "AMR"
#' @param raster A Raster* object used to determine the extent and resolution
#' @param year Which year to try to estimate
#' @param layers_dir Layers directory of the taxonomy folder
#' @return A RasterLayer object of the same resolution and extent as \code{raster}
load_population_estimates <- function(region,raster,year,layers_dir = "Layers"){

  .Deprecated(msg = "load_population_estimates is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "load_population_estimates")

  allowed_years = c(2000,2005,2010,2015,2020)
  
  if(year > max(allowed_years)){
    warning(paste("Data used to calculate population only extends to ",max(allowed_years)))
  }
  
  if(year < min(allowed_years)){
    warning(paste("Data used to calculate population starts from ",min(allowed_years)))
  }
  ## Get all of the filenames for population rasters
  fnames <- list(
    AFR = paste(
      layers_dir,
      paste("pop/AFR/AFR_PPP",
            allowed_years,
            "adj_v2.tif",
            sep='_'),
      sep="/"
    ),
    ASI = paste(
      layers_dir,
      paste("pop/ASI/Asia_PPP",
            allowed_years,
            "adj_v2.tif",
            sep='_'),
      sep="/"
    ),
    AMR = paste(
      layers_dir,
      paste("pop/AMR/LAC_PPP",
            allowed_years,
            "adj_v2.tif",
            sep='_'),
      sep="/"
    ),
    OCE = paste(
      layers_dir,
      paste("pop/OCE/OCE_PPP",
            allowed_years,
            "adj.tif",
            sep='_'),
      sep="/"
    )
  )
  ## Check to see if the region is one of the rasters available
  if(region == "EMR" | region == "SEAR" | region == "WPR"){region = "ASI"}
  if(region == "SEAR"){region = "ASI"}
  
  ## if it is, set it to pop_file
  fname <- fnames[[region]]
  if(!all(file.exists(fname))){
    warning("Could not load population estimates for the following years:",paste(allowed_years[!file.exists(fname)]))
    allowed_years = allowed_years[file.exists(fname)]
    fname = fname[file.exists(fname)]
    if(length(fname) < 2){stop("Not enough population estimates found")}
  }
  #' @importFrom raster stack
  pop_raster <- stack(fname)
  #' @importFrom raster intersect
  if(is.null(raster::intersect(
    extent(pop_raster),
    extent(raster)
  ))){
    pop_raster <- raster*0
    pop_raster[] <- NA
  }
  #' @importFrom raster crop
  pop_raster <- crop(pop_raster,raster)
  if(all(is.na(pop_raster[]))){
    warning("Not Africa using slower method.")
    return(load_population_estimates_slow(raster,year,layers_dir))
  }
  resol = (dim(pop_raster)/dim(raster))
  if(resol[1] != resol[2]){
    warning("Could not calculate resolution.")
    return(load_population_estimates_slow(raster,year,layers_dir))
  }
  if(resol[1] != floor(resol[1])){
    warning("Non integer resolution.")
    return(load_population_estimates_slow(raster,year,layers_dir))
  }
  
  ##This should probably be replaced with fun=sum
  #' @importFrom raster aggregate
  pop_raster <- aggregate(pop_raster,resol[1])*resol[1]*resol[2]
  if(all(is.na(pop_raster[]))){
    warning("The region does not contain the raster.")
    return(load_population_estimates_slow(raster,year,layers_dir))
  }
  #' @importFrom raster values values<-
  values(pop_raster)[aggregate_raster_xlayers(raster,function(x){all(is.na(x))})[],] <- NA
  
  #' @importFrom reshape2 melt
  #' @importFrom raster values
  tmp = t(as.matrix(values(pop_raster)))
  non_na_indices = apply(tmp,2,function(x){!all(is.na(x))})
  tmp = tmp[,non_na_indices]
  models <- apply(tmp,2,function(x){
    lm(log(1+y)~x,data.frame(y=x,x=allowed_years))
  })
  
  pop_raster <- aggregate_raster_xlayers(pop_raster,function(x){x[[1]]})
  #' @importFrom raster values values<-
  values(pop_raster)[non_na_indices] = exp(sapply(models,predict,newdata=data.frame(x=year)))-1
  return(pop_raster)
}



#' @export
#' @name load_population_estimates_slow
#' @title load_population_estimates_slow
#' @description Find population estimates for a year and overlay them on a raster.  This will work on arbitrary rasters but is memory intensive
#' @param raster A Raster* object used to determine the extent and resolution
#' @param year Which year to try to estimate
#' @param layers_dir Layers directory of the taxonomy folder
#' @return A RasterLayer object of the same resolution and extent as \code{raster_layer}
load_population_estimates_slow <- function(raster_layer,year,layers_dir = "Layers"){

  .Deprecated(msg = "load_population_estimates_slow is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "load_population_estimates_slow")

  ## These are the years that Worldpop has
  allowed_years = c(2000,2005,2010,2015,2020)
  
  ## Warnings for people trying to extrapolate
  if(year > max(allowed_years)){
    warning(paste("Data used to calculate population only extends to ",max(allowed_years)))
  }
  
  ## Warnings for people trying to extrapolate
  if(year < min(allowed_years)){
    warning(paste("Data used to calculate population starts from ",min(allowed_years)))
  }
  
  ## The different files from worldpop.
  ## We want all of the years for each, but separately
  #### What do we do about other populations?
  fnames <- list(
    Africa = paste(
      layers_dir,
      paste("pop/AFR/AFR_PPP",
            allowed_years,
            "adj_v2.tif",
            sep='_'),
      sep="/"
    ),
    Asia = paste(
      layers_dir,
      paste("pop/ASI/Asia_PPP",
            allowed_years,
            "adj_v2.tif",
            sep='_'),
      sep="/"
    ),
    America = paste(
      layers_dir,
      paste("pop/AMR/LAC_PPP",
            allowed_years,
            "adj_v2.tif",
            sep='_'),
      sep="/"
    )
  )
  ## Make a blank raster to start with
  pop_raster <- raster(raster_layer)
  ## For the intersections to work, we need projections for both rasters.  The population stuff already has their own
  if(is.na(projection(raster_layer))){
    stop('Please set the projection of your raster.  We have been using "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"')
  }
  pop_raster[] <- 0
  for(file_idx in 1:length(fnames)){
    this_file <- fnames[[file_idx]]
    this_name <- names(fnames)[file_idx]
    ## We want it to just use the other files if there are broken ones
    if(!all(file.exists(this_file))){
      warning(
        paste(
          "Could not open population estimates from",
          this_name,
          ". They will not be included."
        )
      )
      next
    }
    ## Read each population file.  Its a stack 'cause 5 years of data
    #' @importFrom raster stack
    this_pop_raster <- stack(this_file)
    names(this_pop_raster) <- paste0("layer_",names(this_pop_raster))
    
    ## things break if the extents don't overlap, so we're being careful here.
    #' @importFrom raster intersect
    if(is.null(raster::intersect(
      extent(this_pop_raster),
      extent(raster_layer)
    ))){
      this_pop_raster <- raster_layer*0
      this_pop_raster[] <- 0
    } else {
      ## This is where most of the work happens
      this_pop_raster <- regrid_raster(this_pop_raster,raster_layer)
    }
    ## We're adding everything to a single raster for later use
    pop_raster <- pop_raster + this_pop_raster
  }
  
  ## Preserve NAs (not sure if we want this long term, but...)
  #' @importFrom raster values values<-
  values(pop_raster)[aggregate_raster_xlayers(raster_layer,function(x){all(is.na(x))})[],] <- NA
  #' @importFrom reshape2 melt
  #' @importFrom raster values values<-
  tmp <- t(as.matrix(values(pop_raster)))
  ## Modeling only works without NAs
  non_na_indices <- apply(tmp,2,function(x){!all(is.na(x))})
  tmp <- tmp[,non_na_indices]
  ## Use a log linear model for interpolation.  Input is time only
  models <- apply(tmp,2,function(x){
    lm(log(1+y)~x,data.frame(y=x,x=allowed_years))
  })
  
  ## Make the output
  pop_raster <- aggregate_raster_xlayers(pop_raster,function(x){x[[1]]})
  ## Put the predicted values in based on the year
  #' @importFrom raster values values<-
  values(pop_raster)[non_na_indices] <- exp(sapply(models,predict,newdata=data.frame(x=year)))-1
  return(pop_raster)
}

#' @export
#' @name load_covariate_estimates
#' @title load_covariate_estimates
#' @description Find distance to water estimates for a year and overlay them on a raster.
#' @param raster A Raster* object used to determine the extent and resolution
#' @param region
#' @param layers_dir Layers directory of the taxonomy folder
#' @param tol
#' @return A RasterLayer object of the same resolution and extent as \code{raster_layer}
load_covariate_estimates <- function(
  raster,
  region,
  layers_dir = 'Layers',
  tol = 1e-5
){

  .Deprecated(msg = "load_covariate_estimates is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "load_covariate_estimates")

  if(region != 'AFR'){
    stop("Only written for AFR right now")
  }
  warning("This function only returns the 5km rasters from Africa right now")
  load(paste(layers_dir,'5km_rasters/coast_dist_5km_raster.rda',sep='/'))
  load(paste(layers_dir,'5km_rasters/lake_dist_5km_raster.rda',sep='/'))
  load(paste(layers_dir,'5km_rasters/river_dist_5km_raster.rda',sep='/'))
  water.r <- raster::stack(river.r,lake.r,coast.r)
  water.r <- aggregate_raster_xlayers(water.r,min)
  resol = res(raster)/res(water.r)[1:2]
  if((abs(resol[1] - resol[2]) > tol) | any(abs(round(resol) - resol) > tol)){
    stop("Raster grid does not match")
  }
  #' @importFrom raster aggregate
  coast.r <- aggregate(coast.r,fact=resol[1])
  #' @importFrom raster aggregate
  lake.r <- aggregate(lake.r,fact=resol[1])
  #' @importFrom raster aggregate
  river.r <- aggregate(river.r,fact=resol[1])
  #' @importFrom raster aggregate
  water.r <- aggregate(water.r,fact=resol[1])
  #' @importFrom raster stack
  all_covariates.r <- stack(coast.r)
  names(all_covariates.r) <- 'coast'
  all_covariates.r[['lake']] <- lake.r
  all_covariates.r[['river']] <- river.r
  all_covariates.r[['water.distance']] <- water.r
  ## Watsan
  load(paste(layers_dir,'WSS sharefile/africa_wash_water_raster1km.rda',sep='/'))
  load(paste(layers_dir,'WSS sharefile/africa_wash_san_raster1km.rda',sep='/'))
  load(paste(layers_dir,'WSS sharefile/africa_wash_open_raster1km.rda',sep='/'))
  watsan.r <- raster::stack(water.r,san.r,open.r)
  resol = res(raster)/res(watsan.r)[1:2]
  if((abs(resol[1] - resol[2]) > tol) | any(abs(round(resol) - resol) > tol)){
    stop("Raster grid does not match")
  }
  #' @importFrom raster aggregate
  water.r <- aggregate(water.r,fact=resol[1])
  #' @importFrom raster aggregate
  open.r <- aggregate(open.r,fact=resol[1])
  #' @importFrom raster aggregate
  san.r <- aggregate(san.r,fact=resol[1])
  all_covariates.r[['water.access']] <- water.r
  all_covariates.r[['san']] <- san.r
  all_covariates.r[['open']] <- open.r
  #' @importFrom raster crop
  all_covariates.r <- crop(all_covariates.r,raster)
  #' @importFrom raster resample
  all_covariates.r <- resample(all_covariates.r,raster)
  return(all_covariates.r)
}



#' @export
#' @name create_worldpop_region_shapefiles
#' @title create_worldpop_region_shapefiles
#' @description Create shapefiles for the worldpop regions.  Only intended to be run on a server or similar
#' @return a list of shapefiles one for each worldpop region named.
create_worldpop_region_shapefiles <- function(){

  .Deprecated(msg = "create_worldpop_region_shapefiles is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "create_worldpop_region_shapefiles")

  df = read_csv('packages/taxdat/inst/extdata/all_countries.csv',col_names=FALSE)
  names(df) <- 'country'
  df$who_region = sapply(df$country,lookup_WHO_region)
  df$worldpop_region = sapply(df$country,lookup_WorldPop_region)
  df$shp = lapply(paste(df$who_region,df$country,sep='_'),function(x){try({get_shape_file(x)})})
  
  df %>% group_by(worldpop_region) %>% summarize(shp = list(reduce_sf_vector(shp))) -> worldpop_df
  worldpop_regions <- as.list(worldpop_df$worldpop_region)
  names(worldpop_regions) <-  worldpop_df$worldpop_region
  for(idx in 1:nrow(worldpop_df)){
    worldpop_regions[[worldpop_df$worldpop_region[idx] ]] <- st_union(worldpop_df$shp[[idx]])
  }
  return(worldpop_regions)
}

#' @export
#' @name create_water_shapefiles
#' @title create_water_shapefiles
#' @description Create shapefiles for the water in each worldpop region.  Only intended to be run on a server or similar
#' @param layers_dir
#' @return a list of shapefiles one for each worldpop region named.
create_water_shapefiles <- function(layers_dir = 'Layers'){

  .Deprecated(msg = "create_water_shapefiles is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "create_water_shapefiles")

  #' @importFrom readr read_csv
  fname <- system.file("extdata","all_countries.csv",package = "taxdat")
  df = read_csv(fname,col_names=FALSE)
  names(df) <- 'country'
  df$who_region = sapply(df$country,lookup_WHO_region)
  df$worldpop_region = sapply(df$country,lookup_WorldPop_region)
  df$lake_shp = sf::st_sfc(sf::st_point())
  df$river_shp = sf::st_sfc(sf::st_point())
  
  for(idx in 1:nrow(df)){
    ISO_A1 = df$country[idx]
    destination = paste(layers_dir,'/water/shapefiles/',ISO_A1,'.zip',sep='')
    if(!file.exists(destination)){
      website = paste("http://biogeo.ucdavis.edu/data/diva/wat/",ISO_A1,"_wat.zip",sep='')
      tryCatch({
        download.file(website,destination,mode='wb',quiet = TRUE)
      },
      error = function(e){
        warning(paste("Could not open", website, "and save to", destination,":", e$message))
      }
      )
      unzip(destination,exdir = paste(layers_dir,"water","shapefiles",ISO_A1,sep='/'))
    }
    water_files = list.files(paste(layers_dir,"water","shapefiles",ISO_A1,sep='/'))
    water_files = water_files[grepl(water_files,pattern = 'shp$')]
    lake_files = water_files[grepl(water_files,pattern = 'area')]
    river_files = water_files[grepl(water_files,pattern = 'line')]
    lake_shp <- NULL
    if(length(lake_files) > 0){
      #' @importFrom sf st_read
      lake_shp <- st_read(paste(layers_dir,'water','shapefiles',ISO_A1,lake_files,sep='/'),quiet=T)
      if(nrow(lake_shp) == 0){
        lake_shp = NULL
      }
    }
    if(!is.null(lake_shp)){
      df$lake_shp[[idx]] = lake_shp
    }
    river_shp <- NULL
    if(length(river_files) > 0){
      #' @importFrom sf st_read
      river_shp <- st_read(paste(layers_dir,'water','shapefiles',ISO_A1,river_files,sep='/'),quiet=T)
      if(nrow(river_shp) == 0){
        river_shp = NULL
      }
    }
    if(!is.null(river_shp)){
      df$river_shp[[idx]] = river_shp
    }
  }
  
  lake_shp <- reduce_sf_vector(df$lake_shp)
  st_write(lake_shp,paste(layers_dir,"water","world_lakes.shp",sep='/'))
  river_shp <- reduce_sf_vector(df$river_shp)
  st_write(river_shp,paste(layers_dir,"water","world_rivers.shp",sep='/'))
}


#' @export
#' @name create_water_rasters
#' @title create_water_rasters
#' @description 
#' @param layers_dir
#' @param maximum_shapefiles_in_mem
#' @return 
create_water_rasters <- function(layers_dir = 'Layers',maximum_shapefiles_in_mem = 90000){

  .Deprecated(msg = "create_water_rasters is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "create_water_rasters")

  water_types = c('lake','river','coast')
  
  worldpop_regions <- c('AFR','ASI','LAC','EUR','OCE')
  for(water_type in water_types){
    water_shp_fname <- paste(layers_dir,'/water/world_',water_type,'s.shp',sep='')
    print(water_shp_fname)
    #' @importFrom sf st_read
    if(!file.exists(paste(water_shp_fname,'union.shp',sep=''))){
      water_shp <- st_read(water_shp_fname)
      if(!all(suppressWarnings(st_is_valid(water_shp)))){
        #' @importFrom sf st_make_valid
        water_shp = sf::st_make_valid(water_shp)
      }
      water_shp = st_union(water_shp)
      #' @importFrom sf st_write
      st_write(water_shp,paste(water_shp_fname,'union.shp',sep=''))
    } else {
      water_shp <- st_read(paste(water_shp_fname,'union.shp',sep=''))
    }
    #' @importFrom sf st_is_valid
    for(region in worldpop_regions){
      region_shp_fname <- paste(layers_dir,"/worldpop_shapefiles/",region,".shp",sep='')
      print(region_shp_fname)
      #' @importFrom sf st_read
      region_shp <- st_read(region_shp_fname)
      #' @importFrom sf st_intersection
      this_water_shp <- st_intersection(water_shp,st_as_sfc(st_bbox(region_shp)))
      all_rasters <- list.files(paste(layers_dir,'pop',region,sep='/'))
      all_rasters <- all_rasters[grepl(region,all_rasters)]
      all_rasters <- all_rasters[grepl('adj_v2.tif$',all_rasters)]
      file = all_rasters[1]
      #' @importFrom raster raster
      rl = raster(paste(layers_dir,"pop",region,file,sep='/'))
      #' @importFrom raster extent
      # plot(rl)
      # plot(region_shp$geometry,add=T)
      # plot(this_water_shp,add=T,col='blue')
      rl[] <- 0
      ## tmp will be 0 where there is water and NA elsewhere
      tmp = rl
      tmp[] <- NA
      window_size <- floor(sqrt(maximum_shapefiles_in_mem))
      window_steps_row <- ceiling(nrow(rl)/window_size)
      window_steps_col <- ceiling(ncol(rl)/window_size)
      rl_tmp = raster(nrow=window_size,ncol=window_size)
      ## This is too much to do in memory, so we need to split it up over multiple iterations
      for(window_idx_row in 1:window_steps_row){
        print(paste("Row",window_idx_row,'/',window_steps_row))
        window_row_start <- ((window_idx_row - 1)*window_size + 1)
        window_row_end <- min(window_idx_row*window_size,nrow(rl))
        window_rows <- window_row_start:window_row_end
        for(window_idx_col in 1:window_steps_col){
          print(paste("Col",window_idx_col,'/',window_steps_col))
          window_col_start <- ((window_idx_col - 1)*window_size + 1)
          window_col_end <-min(window_idx_col*window_size,nrow(rl))
          window_cols <- window_col_start:window_col_end
          rl_tmp <- crop(rl,extent(rl,window_row_start,window_row_end,window_col_start,window_col_end))
          rl_tmp[] <- 0
          if(!all(is.na(all(rl_tmp[])))){
            #' @importFrom raster rasterToPoints
            print("Converting to points")
            rl_tmp <- raster(tmp)
            rl_tmp_pf <- as_data_frame(rasterToPoints(rl_tmp))
            rl_tmp_pf$geometry <- st_sfc(mapply(x=rl_tmp_pf$x,y=rl_tmp_pf$y,function(x,y){st_point(c(x,y))},SIMPLIFY=FALSE))
            rl_tmp_sf <- st_sf(rl_tmp_pf)

            print("Finished Converting to points")
            print("Calculating Distance")
            rl_tmp_sf$distance = st_distance(rl_tmp_sf,water_shp)
            rl_tmp[] <- rl_tmp_sf$distance
            print("Finished Calculating Distance")
          }
          tmp[window_row_start:window_row_end,window_col_start:window_col_end] <- rl_tmp[]
          gc()
       }
     }
     #' @importFrom raster distance
      print("Computing Distance")
      tmp = distance(tmp)
      print("Finished Computing Distance")
      #' @importFrom raster writeRaster
      print("Saving")
      writeRaster(tmp,paste(layers_dir,'/water/',region,'_',water_type,'s.tif',sep=''))
      print("Finished Saving")
    }
  }
}
