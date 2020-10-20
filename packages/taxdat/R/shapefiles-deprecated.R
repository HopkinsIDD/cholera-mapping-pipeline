
## Preliminary data
who_region_shortener = c(
  Africa = 'AFR',
  Americas = 'AMR',
  'Eastern Mediterranean' = 'EMR',
  Europe = 'EUR',
  'South-East Asia' = 'SEAR',
  'Western Pacific' = 'WPR'
)

#' @import ISOcodes
my_ISO_3166_1 = apply(
  ISOcodes::ISO_3166_1,
  2,
  function(x){
    toupper(
      gsub('[[:punct:]]',
           '',
           gsub(' ','',
                iconv(x,from='UTF-8',
                      to='ASCII//TRANSLIT'))))
  }
)

#' @import ISOcodes
my_ISO_3166_2 = ISOcodes::ISO_3166_2
my_ISO_3166_2$ISO_3166_1_Alpha_2 =
  sapply(strsplit(my_ISO_3166_2$Code,split = '-'),function(x){x[[1]]})
my_ISO_3166_2 = apply(
  my_ISO_3166_2,
  2,
  function(x){
    toupper(
      gsub('[[:punct:]]',
           '',
           gsub(' ','',
                iconv(x,from='UTF-8',
                      to='ASCII//TRANSLIT'))))
  }
)

my_ISO_3166 = inner_join(
  as.data.frame(my_ISO_3166_1,stringsAsFactors=FALSE),
  as.data.frame(my_ISO_3166_2,stringsAsFactors=FALSE),
  by=c("Alpha_2"="ISO_3166_1_Alpha_2"))

#' @export get_shape_file
#' @name get_shape_file
#' @title get_shape_file
#' @description Look up the GADM shapefile for a particular location
#' @param location character vector containing the names of locations to look up shapefiles for
#' @param taxonomy_dir Name of a taxonomy directory (shapefiles will be stored here)
#' @param layers_dir Name of a layers directory (custom shapefiles will be found here)
#' @param method which method to use when pulling shapefiles.  If \code{method =='center'}, then use the central points listed in the shape files.  If \code{method=='name'} use the names.
#' @return an sf::sf with the results
#' @importFrom lubridate now
get_shape_file <- function(location_name,taxonomy_dir = 'taxonomy-verified', verbose=FALSE,method='name',date=now(),thorough=FALSE){
  .Deprecated(msg = "get_shape_file is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "get_shape_file")
  if(!dir.exists(taxonomy_dir)){
    stop(paste("The taxonomy directory",taxonomy_dir,"does not exist in the current working directory"))
  }
  ## This section is updated.
  if(verbose && ('_' == substr(location_name,nchar(location_name),nchar(location_name)))){
    warning("Location ",location_name," contains an ending underscore.")
  }
  ##
  location_tmp = standardize_locations(location_name)
  
  ## Read location files
  all_location_files = list.files(paste(taxonomy_dir,'Location',sep='/'))
  original_all_location_files = all_location_files
  all_location_files = standardize_locations(all_location_files)
  all_location_files = sapply(
    strsplit(all_location_files,'_'),
    function(x){paste(x[-length(x)],collapse = '_')}
  )
  all_location_files <- data_frame(location_file = original_all_location_files,location_name = all_location_files)
  
  ungrouped_shapefiles <- create_location_sf(location_name,date,thorough)
  ungrouped_shapefiles <- left_join(ungrouped_shapefiles,all_location_files,by='location_name')
  
  max_ISO_A2_level = max(ungrouped_shapefiles$ISO_A2_level)
  if(is.na(max_ISO_A2_level)){
    stop("Something went wrong, please contact package maintainer")
  }
  
  
  ## This section is not working
  ## This section populates the sections of the 
  tmp_shapefiles <- dplyr::filter(ungrouped_shapefiles,ISO_A2_level < 0)
  if(nrow(tmp_shapefiles) > 0){
    unique_who_regions = unique(tmp_shapefiles$who_region)
    unique_who_regions = unique_who_regions[!is.na(unique_who_regions)]
    #' @importFrom sf st_point
    ungrouped_shapefiles$geometry[tmp_shapefiles$idx] = st_sfc(st_point(c(NaN,NaN)))
    ## Get WHO region shapefiles here...
    warning(paste("We do not yet support getting shape files for WHO regions.  The following locations were affected:",paste(unique_who_regions,collapse=',')))
  }
  
  ## If we only have WHO_region level data we are finished.
  if(max_ISO_A2_level <= -1){
    return(group_location_sf(ungrouped_shapefiles))
  }
  
  ## Check the location files:
  location_files = unique(ungrouped_shapefiles$location_file)
  ## Warn if any locations are missing.
  if(method=='center'){
    if(any(is.na(location_files))){
      warning(paste(
        "Could not find location files for the following locations",
        paste(unique(ungrouped_shapefiles$location_name[
          is.na(ungrouped_shapefiles$location_file)
          ]),collapse=', ')
      ))
    }
    location_files = location_files[!is.na(location_files)]
  }
  
  for(location in unique(location_files)){
    if((method =='name') & (is.na(location))){
      idx = which(is.na(ungrouped_shapefiles$location_file))
    } else {
      idx = which(ungrouped_shapefiles$location_file == location)
    }
    if(length(idx) == 0){
      stop("This should never happen.")
    }
    if((method =='name') & is.na(location)){
      loc_file = list()
    } else {
      loc_file = read_location_csv(
        paste(taxonomy_dir,'Location',location,sep='/')
      )
      for(this_idx in idx){
        if('cent_lat' %in% names(loc_file)){
          ungrouped_shapefiles$geometry[[this_idx]][[2]] = loc_file$cent_lat
        }
        if('cent_lat' %in% names(loc_file)){
          ungrouped_shapefiles$geometry[[this_idx]][[1]] = loc_file$cent_lon
        }
      }
      # Update bounding box
      ungrouped_shapefiles = st_update_bounding_box(ungrouped_shapefiles)
    }
    if(
      (!('gis_file' %in% names(loc_file)))
    ){
      if(verbose){
        # warning("Not including a gis_file field is depricated for location ",location)
      }
      ## Using GADM shapefiles
      ungrouped_shapefiles$shapefile_source[idx] = paste0(
        'GADM/',
        fix_country_name(ungrouped_shapefiles$ISO_A1[idx]),
        '_adm',
        (ungrouped_shapefiles$ISO_A2_level[idx]),
        '.rds'
      )
    } else {
      if(is.na(loc_file$gis_file) | (loc_file$gis_file == 'GADM')){
        ## Still using GADM shapefiles
        ungrouped_shapefiles$shapefile_source[idx] = paste0(
          'GADM/',
          fix_country_name(ungrouped_shapefiles$ISO_A1[idx]),
          '_adm',
          (ungrouped_shapefiles$ISO_A2_level[idx]),
          '.rds'
        )
      } else {
        ## Using custom files
        ## Need to check years:
        nshapefile = sum(grepl('gis_file',names(loc_file)))
        if(nshapefile > 1){
          for(shape_idx in 1:nshapefile){
            this_source = loc_file[[paste('gis_file',shape_idx,sep='_')]]
            if(shape_idx == 1){
              this_source = loc_file[['gis_file']]
            }
            #' @importFrom lubridate ymd
            this_start = ymd(loc_file[[paste('gis_start',shape_idx,sep='_')]])
            if(is.na(this_start)){
              #' @importFrom lubridate ymd
              this_start = ymd("0000-01-01")
            }
            #' @importFrom lubridate ymd
            this_end = ymd(loc_file[[paste('gis_end',shape_idx,sep='_')]])
            if(is.na(this_end)){
              this_end = lubridate::now()
            }
            this_idx = idx[(
              (this_start < ungrouped_shapefiles$date[idx]) &
                (this_end > ungrouped_shapefiles$date[idx])
            )]
            if(length(this_idx) > 0){
              ungrouped_shapefiles$shapefile_source[this_idx] = this_source
            }
          }
          if(any(is.na(ungrouped_shapefiles$shapefile_source[idx]))){
            this_idx = idx[which(is.na(
              ungrouped_shapefiles$shapefile_source[idx]
            ))]
            ungrouped_shapefiles$shapefile_source[this_idx] <- paste0(
              'GADM/',
              fix_country_name(ungrouped_shapefiles$ISO_A1[idx]),
              '_adm',
              (ungrouped_shapefiles$ISO_A2_level[idx]),
              '.rds'
            )
          }
        } else {
          if((length(loc_file) > 0) & isTRUE(!(is.na(loc_file$gis_file)))){
            ungrouped_shapefiles$shapefile_source[idx] = loc_file$gis_file
          } else {
            ungrouped_shapefiles$shapefile_source[idx] <- paste0(
              'GADM/',
              fix_country_name(ungrouped_shapefiles$ISO_A1[idx]),
              '_adm',
              (ungrouped_shapefiles$ISO_A2_level[idx]),
              '.rds'
            )
          }
        }
      }
    }
  }
  
  ## Check the shape files
  missing_shapefiles = unique(ungrouped_shapefiles$shapefile_source[
    !file.exists(paste(
      taxonomy_dir,
      'ShapeFiles',
      ungrouped_shapefiles$shapefile_source,
      sep='/'
    ))])
  missing_gadm_links = substr(
    missing_shapefiles,
    start=6,
    stop=nchar(missing_shapefiles)
  )
  for(shapefile_idx in which(startsWith(missing_shapefiles,'GADM'))){
    destination = paste(
      taxonomy_dir,
      'ShapeFiles',
      missing_shapefiles[shapefile_idx],
      sep='/'
    )
    website = paste(
      "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
      missing_gadm_links[shapefile_idx],
      sep=''
    )
    tryCatch({
      download.file(website,destination,mode='wb')
      #' @importFrom sf st_as_sf
      tmp_shapefile = st_as_sf(readRDS(destination))
    },
    error = function(e){
      warning("Could not download shape file",missing_shapefiles[idx])
      if(file.exists(destination)){
        file.remove(destination)
      }
    })
  }
  missing_shapefiles = unique(ungrouped_shapefiles$shapefile_source[
    !file.exists(paste(
      taxonomy_dir,
      'ShapeFiles',
      ungrouped_shapefiles$shapefile_source,
      sep='/'
    ))])
  missing_shapefiles = missing_shapefiles[!is.na(missing_shapefiles)]
  if(length(missing_shapefiles) > 0){
    warning(
      "Missing the following shapefiles ",
      paste(missing_shapefiles,collapse=', ')
    )
  }
  #' @importFrom sf st_as_sf
  ungrouped_shapefiles <- st_as_sf(ungrouped_shapefiles)
  
  invalid_idx = which(!st_is_valid(ungrouped_shapefiles))
  for(shp_src in unique(ungrouped_shapefiles$shapefile_source)){
    if(shp_src %in% missing_shapefiles){next;}
    if(is.na(shp_src)){next;}
    idx = which(ungrouped_shapefiles$shapefile_source == shp_src)
    idx = idx[!(idx %in% invalid_idx)]
    if(length(idx) == 0){next;}
    file = paste(taxonomy_dir,'ShapeFiles',shp_src,sep='/')
    file_ending = substr(file,nchar(file)- 2,nchar(file))
    if(file_ending == 'rds'){
      #' @importFrom sf st_as_sf
      shapefile = st_as_sf(readRDS(file))
    } else {
      #' @importFrom sf st_read
      shapefile = st_read(file,quiet=TRUE)
    }
    if(method == 'center'){
      shapefile = shapefile$geometry
    }
      #' @importFrom sf st_crs
      if(is.na(st_crs(ungrouped_shapefiles))){
        #' @importFrom sf st_crs st_crs<-
        st_crs(ungrouped_shapefiles) = st_crs(shapefile)
      } else if(is.na(st_crs(shapefile))) {
        warning("Shapefile with no crs")
        st_crs(shapefile) = st_crs(ungrouped_shapefiles)
      }else {
        #' @importFrom sf st_crs st_transform
        shapefile = st_transform(shapefile,st_crs(ungrouped_shapefiles))
      }
    if(method == 'center'){
      #' @importFrom sf st_intersects
      intersections = st_intersects(ungrouped_shapefiles[idx,],shapefile)
      tmp_idx = idx[sapply(intersections,length) != 1]
      if(length(tmp_idx) > 0 ){
        warning(paste("This should not happen.  A shapefile was found, but did not intersect the point given in the location file for location(s)", paste(ungrouped_shapefiles$location[tmp_idx], collapse = ', ')))
      }
      ungrouped_shapefiles$geometry[tmp_idx] = st_point()
      tmp_idx = idx[sapply(intersections,length) == 1]
      ungrouped_shapefiles$geometry[tmp_idx] = shapefile[unlist(intersections[1:length(tmp_idx)]) ]
    } else if(method == 'name'){
      ISO_level = unique(ungrouped_shapefiles$ISO_A2_level[idx])
      if(length(ISO_level) > 1){stop("Not sure how to deal with a single shapefile source containing multiple ISO levels")}
      ISO_name = paste0("ISO_A2_L",ISO_level)
      if(ISO_level == 0){
        ISO_name = "ISO_A1"
      }
      if(ISO_level < 0){stop("We do not currently support shapefiles for ISO levels < 0")}
      currently_set_to = 1:length(idx) * NA
      field_names = names(shapefile)
      if(ISO_level > 0){
        field_names = names(shapefile)[endsWith(names(shapefile),suffix = as.character(ISO_level))]
      }
      for(field in field_names){
        std_string = standardize_string(shapefile[[field]])
        for(row_idx in 1:nrow(shapefile)){
	  if(any((!is.na(std_string[[row_idx]])) & (ungrouped_shapefiles[[ISO_name]][idx] == std_string[[row_idx]]))){
            tmp_idx = idx[which(ungrouped_shapefiles[[ISO_name]][idx] == std_string[row_idx])]
	    if(any(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx))){
	      tmp_idx2 = tmp_idx[which(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx))]
	      currently_set_to[tmp_idx2] = row_idx
	      ungrouped_shapefiles$geometry[tmp_idx2] = shapefile$geometry[row_idx]
	    }
	    if(any(!(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx)))){
	      tmp_idx2 = tmp_idx[which(!(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx)))]
	      currently_set_to[tmp_idx2] = 0
	      #' @importFrom sf st_point
	      ungrouped_shapefiles$geometry[tmp_idx2] = st_point(1:2* NA)
	    }
	      which(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx))
	  }
	}
      }
      
    }
  }
  # Update bounding box
  ungrouped_shapefiles = st_update_bounding_box(ungrouped_shapefiles)
  # ungrouped_shapefiles %>% group_by(shapefile_source) %>% do({tmp = .; tmp$shapefile = st_read(tmp$shapefile_source)$geometry})
###   if(method=='center'){
###     
###   } else {
###     warning("This method is not functional")
###     unique_ISO_A1s = unique(ungrouped_shapefiles$ISO_A1)
###     unique_ISO_A1s = unique_ISO_A1s[!is.na(unique_ISO_A1s)]
###     ## Since the shape files are stored separately by country, we look at the countries separately.
###     for(this_ISO_A1 in unique_ISO_A1s){
###       tmp_shapefiles = dplyr::filter(ungrouped_shapefiles,ISO_A1==this_ISO_A1)
###       unique_ISO_levels = max(unique(tmp_shapefiles$ISO_A2_level)):0
###       
###       ## Loop over each ISO level and check to make sure it matches.  If its the right level then put in the shapefile
###       for(ISO_level in unique_ISO_levels){
###         tmp_shapefiles = dplyr::filter(ungrouped_shapefiles,ISO_A1 == this_ISO_A1,ISO_A2_level >= ISO_level)
###         
###         ISO_val = tmp_shapefiles[[paste('ISO_A2_L',ISO_level,sep='')]]
###         if(ISO_level> 3){
###           if(verbose){
###             message("GADM does not normally have ISO levels this deep")
###           }
###         }
###         destination = paste(taxonomy_dir,'/ShapeFiles/',this_ISO_A1,'_adm',ISO_level,'.rds',sep='')
###         if(!file.exists(destination)){
###           if(verbose){
###             message(paste("Could not find shape file at ISO_A2 level",ISO_level))
###           }
###           #' @importFrom sf st_sf
###           #' @importFrom sf st_sfc
###           all_shape_files = st_sf(st_sfc())
###         } else {
###           #' @importFrom sf st_as_sf
###           all_shape_files = st_as_sf(readRDS(destination))
###           #' @importFrom sf st_is_valid
###           if(!all(st_is_valid(all_shape_files))){
###             #' @importFrom sf st_make_valid
###             ## Switch to tryCatch instead of suppressWarnings here in case there are actual warnings (other than the default for fixing things)
###             all_shape_files <- suppressWarnings(st_make_valid(all_shape_files))
###             saveRDS(object=all_shape_files,file=destination)
###           }
###         }
###         #' @importFrom sf st_is_valid
###         if(!all(st_is_valid(all_shape_files))){
###           file.remove(destination)
###           stop("This should be dealt with by now.")
###         }
###         for(field in names(all_shape_files)[endsWith(names(all_shape_files),suffix = as.character(ISO_level))]){
###           tmp_field = paste(field,'tmp',sep='_')
###           all_shape_files[[tmp_field]] = all_shape_files[[field]]
###           if(mode(field)==mode(character(0))){
###             all_shape_files[[tmp_field]] = standardize_string(all_shape_files[[tmp_field]])
###           }
###         }
###         this_shape_file = all_shape_files
###         if(ISO_level > 0){
###           for(index_idx in 1:nrow(tmp_shapefiles)){
###             match_indices = c()
###             for(field in names(all_shape_files)[endsWith(names(all_shape_files),suffix='tmp')]){
###               match_indices = union(match_indices,which(sapply(all_shape_files[[field]],function(x){any(ISO_val[index_idx] %in% x)})))
###             }
###             matching_ISO_regions = all_shape_files[match_indices,]
###             if(nrow(matching_ISO_regions)==0){
###               #' @importFrom sf st_point
###               ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = st_point(c(NaN,NaN))
###             } else {
###               if((tmp_shapefiles$ISO_A2_level[index_idx] == ISO_level) & (nrow(matching_ISO_regions) == 1)){
###                 #' @importFrom sf st_crs st_crs<-
###                 if(is.na(st_crs(ungrouped_shapefiles))){
###                   st_crs(ungrouped_shapefiles) <- st_crs(matching_ISO_regions)
###                 }
###                 ## This is the ISO level to pull the geometry from
###                 ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ] = matching_ISO_regions$geometry
###                 if(st_crs(ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ]) != st_crs(matching_ISO_regions$geometry)){
###                   ## st_transform(ungrouped_shapefiles,st_crs(matching_ISO_regions$geometry))
###                   warning("Lost the crs")
###                 }
###                 ## It matches a real region, just not one containing our point.
###                 #' @importFrom sf st_intersects
###                 if(!st_intersects(
###                   ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ],
###                   matching_ISO_regions,
###                   sparse=FALSE
###                 )){
###                   #' @importFrom sf st_point
###                   ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = st_point(c(NaN,NaN))
###                 }
###               } else {
###                 if(is.na(st_crs(ungrouped_shapefiles))){
###                   st_crs(ungrouped_shapefiles) <- st_crs(matching_ISO_regions)
###                 }
###                 ## Make sure the geometries intersect
###                 #' @importFrom sf st_intersects
###                 if(!all(st_intersects(
###                   ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ],
###                   matching_ISO_regions,
###                   sparse=FALSE
###                 ))){
###                   #' @importFrom sf st_point
###                   ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = st_point(c(NaN,NaN))
###                 }
###               }
###               if(ISO_level == 1){
###                 ungrouped_shapefiles[[paste("ISO_A2_L",ISO_level,sep='')]][tmp_shapefiles$idx[index_idx]] = gsub('.','-',matching_ISO_regions$HASC_1,fixed=T)
###                 ## ungrouped_shapefiles$WHO_region = lookup_WHO_region(ungrouped_shapefiles$ISO_A1)
###               }
###               if(ISO_level > 1){
###                 ungrouped_shapefiles[[paste("ISO_A2_L",ISO_level,sep='')]][tmp_shapefiles$idx[index_idx]] = matching_ISO_regions[[paste('NAME_',ISO_level,sep='')]]
###               }
###             }
###           }
###         } else {
###           this_country_name = fix_country_name(this_shape_file$NAME_ENGLISH,verbose==FALSE)
###           this_WHO_region = lookup_WHO_region(this_country_name)
###           for(index_idx in 1:nrow(tmp_shapefiles)){
###             if(tmp_shapefiles$ISO_A2_level[index_idx] == ISO_level){
###               ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = this_shape_file$geometry
###             } else {
###               #' @importFrom sf st_crs st_crs<-
###               if(is.na(st_crs(ungrouped_shapefiles))){
###                 st_crs(ungrouped_shapefiles) <- st_crs(this_shape_file)
###               }
###               #' @importFrom sf st_intersects
###               if(!all(st_intersects(
###                 ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ],
###                 this_shape_file,
###                 sparse=FALSE
###               ))){
###                 #' @importFrom sf st_point
###                 ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = st_point(c(NaN,NaN))
###               }
###             }
###           }
###           if(!is.na(this_country_name)){
###             ungrouped_shapefiles$ISO_A1[tmp_shapefiles$idx] = this_country_name
###             if(!is.na(this_WHO_region)){
###               ungrouped_shapefiles$who_region[tmp_shapefiles$idx] = this_WHO_region
###             }
###           }
###         }
###         print(paste("ISO level",ISO_level))
###       }
###     }
###   }
  
  return(group_location_sf(ungrouped_shapefiles))
}

#' @export exists_shape_file
#' @name exists_shape_file
#' @title exists_shape_file
#' @description Check to see if a custom shapefile exists in the taxonomy already.
#' @param location Name of a location as a character
#' @param taxonomy_dir Name of a taxonomy directory (shapefiles will be stored here)
#' @param date which date to look for a custom shapefile covering.
#' @return boolean TRUE if a custom shapefile exists, FALSE otherwise
#' @importFrom lubridate now
exists_shape_file = function(location,taxonomy_dir = 'taxonomy-verified',verbose=FALSE,date=now()){
  .Deprecated(msg = "exists_shape_file is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "exists_shape_file")
  file = paste0(taxonomy_dir,'/Location/',location,'_LOC.csv')
  ## Check if there is a shapefile
  if(!file.exists(file)){
    if(verbose){
      warning(paste("Could not find location file",file))
    }
    return(FALSE)
  }
  loc_data <- read_location_csv(file)
  ## Check if there is a shapefile field
  if(!any(grepl('gis',names(loc_data)))){
    return(FALSE)
  }
  
  if(all(is.na(loc_data[,grepl('gis',names(loc_data))]))){
    return(FALSE)
  }
  
  ## Check for time limitations on the shapefile
  if(!(
    any(grepl('gis_start',names(loc_data))) |
    any(grepl('gis_end',names(loc_data)))
  )){
    return(loc_data['gis_file'])
  }
  starts = as.matrix(loc_data)[1,grepl('gis_start',names(loc_data))]
  ends = as.matrix(loc_data)[1,grepl('gis_end',names(loc_data))]
  files = as.matrix(loc_data)[1,grepl('gis_file',names(loc_data))]
  if(length(starts) != length(ends)){
    stop("This should not happen.  Please make sure that",file,"has the same number of gis_start and gis_end lines.")
  }
  if(length(files) != length(starts)){
    stop("This should not happen.  Please make sure that",file,"has the same number of gis_file and gis_end lines.")
  }
  gis_file = FALSE
  for(i in 1:length(files)){
    if(
      ((starts[paste0('gis_start_',i)] < date) | is.na(starts[paste0('gis_start_',i)])) &
      ((starts[paste0('gis_end_',i)] < date) | is.na(starts[paste0('gis_end_',i)]))
    ){
      if(i==1){
        gis_file = files['gis_file']
      } else {
        gis_file = files[paste0('gis_file_',i)]
      }
    }
  }
  return(gis_file)
}

#' @export
#' @name fix_location_name
#' @title fix_location_name
#' @description Attempt to look up a standardized representation of a location.
#' @param location A character vector of names of a locations to fix
#' @param taxonomy_dir Name of a taxonomy directory (shapefiles will be stored here)
#' @return A character containing the corrected filename or NA in case of failure
fix_location_name = function(location,taxonomy_dir = 'taxonomy-verified',verbose=FALSE,method='name'){
  .Deprecated(msg = "fix_location_name is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "fix_location_name")
  location = standardize_locations(location)

  # location should look like `WHO-REGION_ISO-A1_ISO-A2-L1_...`
  # get each component
  ungrouped_shapefiles = create_location_sf(location,date=NA,thorough=TRUE)
  if(nrow(ungrouped_shapefiles) == 0){
    if(verbose){
      warning("Location not provided")
    }
    return(NA)
  }
  #location.array has at least one element
  
  # starting to check the WHO region
  data('WHO_regions',package='taxdat')
  
  ungrouped_shapefiles$old_who_region = ungrouped_shapefiles$who_region
  # check to see if WHO region is in long form (example: "AFRICA" instead of "AFR")
  ungrouped_shapefiles$old_who_region = ifelse(
    ungrouped_shapefiles$who_region %in% who_region_shortener,
    ungrouped_shapefiles$who_region,
    who_region_shortener[ungrouped_shapefiles$who_region]
  )
  if(verbose){
    idx = is.na(ungrouped_shapefiles$who_region)
    if(any(idx)){
      message(paste(paste(ungrouped_shapefiles$old_who_region[idx],collapse = ', '),"is not a valid who region"))
    }
    if(any(ungrouped_shapefiles$who_region[!idx] != ungrouped_shapefiles$old_who_region[!idx])){
      idx = which(ungrouped_shapefiles$who_region[!idx] != ungrouped_shapefiles$old_who_region[!idx])
      message(paste("Changed who region of ",ungrouped_shapefiles$old_who_region,"to",ungrouped_shapefiles$who_region))
    }
  }
  ungrouped_shapefiles$who_region = ifelse(
    ungrouped_shapefiles$who_region %in% who_region_shortener,
    ungrouped_shapefiles$who_region,
    who_region_shortener[ungrouped_shapefiles$old_who_region]
  )
  ungrouped_shapefiles$old_who_region =NULL
  
  ungrouped_shapefiles$ISO_A1 = fix_country_name(ungrouped_shapefiles$ISO_A1,verbose=verbose)
  
  ungrouped_shapefiles$old_location = ungrouped_shapefiles$location
  column_names = c(
    'who_region',
    'ISO_A1',
    sort(names(ungrouped_shapefiles)[grepl('^ISO_A2_L[1234567890]*$',names(ungrouped_shapefiles))])
  )
  ungrouped_shapefiles = mutate_(
    ungrouped_shapefiles,
    .dots = setNames(
      paste(
        "paste(",
        paste(
          column_names,
	  collapse=', '
	),
        ", sep='_')"
      ),'location'
    )
  )
  ungrouped_shapefiles$location = standardize_locations(ungrouped_shapefiles$location)
  ungrouped_shapefiles$geometry = get_shape_file(ungrouped_shapefiles$location,taxonomy_dir = taxonomy_dir,thorough=FALSE,method='name')$geometry
  ungrouped_shapefiles$valid  = st_dimension(ungrouped_shapefiles) > 1
  ungrouped_shapefiles$valid[is.na(ungrouped_shapefiles$valid)] = FALSE
  
  ## Fix invalid shapefiles by looking for their container
  if(any(!ungrouped_shapefiles$valid)){
    invalid_idx = rev(which(!ungrouped_shapefiles$valid))
    for(idx in invalid_idx){
      iso_level = ungrouped_shapefiles$ISO_A2_level[idx]
      possible_shapefiles = get_country_sublevels(ungrouped_shapefiles$ISO_A1,iso_level,taxonomy_dir = taxonomy_dir,method=method)
      if(!is.na(ungrouped_shapefiles$geometry[idx+1])){
        match_idx = which(st_intersects(ungrouped_shapefiles$geometry[idx+1],possible_shapefiles,sparse = FALSE))
        if(length(match_idx) > 1){
            best_match = which.max(st_area(st_intersection(ungrouped_shapefiles[idx+1,],possible_shapefiles[match_idx,])))
            new_match_idx = match_idx[best_match]
            max_area_diff = max(as.numeric(st_area(st_intersection(ungrouped_shapefiles[idx+1,],possible_shapefiles[match_idx,]))/st_area(st_intersection(ungrouped_shapefiles[idx+1,],possible_shapefiles[new_match_idx,])))[-best_match])
            if(max_area_diff < .01){match_idx = new_match_idx}
        }
        if(length(match_idx)==1){
          ungrouped_shapefiles$geometry[idx] = possible_shapefiles$geometry[match_idx]
          ungrouped_shapefiles=ungrouped_shapefiles %>%
            mutate_(.dots = setNames(
              paste(
                "ifelse((arg_idx == ",
                ungrouped_shapefiles$arg_idx[idx],
                ") &(ISO_A2_level >= ",
                iso_level,
                "),\"",
                possible_shapefiles[[paste0('NAME_',iso_level)]][match_idx],
                "\",",
                paste0("ISO_A2_L",iso_level),
                ")"
              ),
              paste0("ISO_A2_L",iso_level)
            ))
        } else {
            warning(
                "Could not find unique match for the parent of",
                ungrouped_shapefiles[idx+1,]$location
            )
        }
      }
    }
  }
  
  ungrouped_shapefiles$ISO_A2_L1 = fix_a2_l1_name(country_name = ungrouped_shapefiles$ISO_A1,location_name=ungrouped_shapefiles$ISO_A2_L1)
  ungrouped_shapefiles$valid  = st_dimension(ungrouped_shapefiles) > 1
  ungrouped_shapefiles$valid[is.na(ungrouped_shapefiles$valid)] = FALSE
  valid_locations = ungrouped_shapefiles %>% group_by(arg_idx) %>% summarize(valid = all(valid)) %>% .$valid
  if(any(!valid_locations)){
    warning(paste("Could not fix the following locations:",paste(ungrouped_shapefiles$old_location,collapse=', ')))
  }
  column_names = c(
    'who_region',
    'ISO_A1',
    sort(names(ungrouped_shapefiles)[grepl('^ISO_A2_L[1234567890]*$',names(ungrouped_shapefiles))])
  )
  ungrouped_shapefiles = mutate_(
    ungrouped_shapefiles,
    .dots = setNames(
      paste(
        "paste(",
        paste(
          column_names,
          collapse=', '
        ),
        ", sep='_')"
      ),'location'
    )
  )
  ungrouped_shapefiles$location = standardize_locations(ungrouped_shapefiles$location)
  grouped_shapefiles = group_location_sf(ungrouped_shapefiles)
  setNames(grouped_shapefiles$location,grouped_shapefiles$old_location)
  ## Things still missing: validate ISO_A2_L1 with that table
  return(setNames(grouped_shapefiles$location,grouped_shapefiles$old_location))

}

#' @export
#' @name create_population_file
#' @title create_population_file
#' @description Create a population file in the taxonomy for the filename
#' @param location Name of a location as a character
#' @param taxonomy_dir Name of a taxonomy directory (shapefiles will be stored here)
create_population_file = function(data,taxonomy_dir = 'taxonomy-verified',replace.existing=FALSE){
  .Deprecated(msg = "create_population_file is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "create_population_file")
  if(!'location' %in% names(data)){
    stop("data should have a column called location with the locations.")
  }
  if(!'pop' %in% names(data)){
    stop("data should have a column called population with the population estimates.")
  }
  if(!'TL' %in% names(data)){
    stop("data should have a column called TL with the first day the population estimate is good for")
  }
  if(!'TR' %in% names(data)){
    stop("data should have a column called TR with the last day the population estimate is good for.")
  }
  if(!'source' %in% names(data)){
    stop("data should have a column called source with the source of the population.")
  }
  data = mutate(
    data,
    loc_file = paste(taxonomy_dir,'/Location/',location,'_LOC.csv',sep=''),
    pop_file = paste(taxonomy_dir,'/Population/',location,'_POP.csv',sep='')
  )
  lapply(data$loc_file,function(this_file){
    if(!file.exists(this_file)){
      ## Not sure what to do here, but for now warn...
      warning("location file does not exist")
    }
  })
  
  for(this_file in unique(data$pop_file)){
    if(!file.exists(this_file)){
      ## Not sure what to do here, but for now warn...
      data %>%
        dplyr::filter(pop_file == this_file) %>%
        select_("TL","TR","pop","source") %>%
        write.csv(file = this_file)
    } else if(replace.existing){
      warning("Replacing existing file",this_file)
      tmp_csv = read.csv(this_file) %>%
        mutate(
          TL = as.Date(TL),
          TR = as.Date(TR),
          pop = as.numeric(pop)
        )
      tmp_data = data %>%
        dplyr::filter(pop_file == this_file) %>%
        select_("TL","TR","pop","source")
      write.csv(
        full_join(tmp_csv,tmp_data,by=c('TL','TR')),
        file = this_file
      )
    } else {
      warning("File exists: Not replacing")
      tmp_csv = read.csv(this_file) %>%
        mutate(
          TL = as.Date(TL),
          TR = as.Date(TR),
          pop = as.numeric(pop)
        )
      tmp_data = data %>%
        dplyr::filter(pop_file == this_file) %>%
        select_("TL","TR","pop","source")
      print(full_join(tmp_csv,tmp_data,,by=c('TL','TR')))
    }
  }
}

#' @export
#' @name create_location_file
#' @title create_location_file
#' @description Create a location description file in the taxonomy for the filename
#' @param location Name of a location as a character
#' @param taxonomy_dir Name of a taxonomy directory (shapefiles will be stored here)
#' @param shapefile shapefile for location
create_location_file = function(location,taxonomy_dir = 'taxonomy-verified',shapefile, start_date, end_date, replace.existing=FALSE,verbose=TRUE){
  .Deprecated(msg = "create_location_file is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "create_location_file")
  missing_shape_file = missing(shapefile)
  if(missing(shapefile)){
    if(location != fix_location_name(location,taxonomy_dir,verbose)){
      warning("The location ",location," is not valid.  Instead consider using ",fix_location_name(location,taxonomy_dir,verbose))
      location = fix_location_name(location,taxonomy_dir,verbose)
    }
  } else {
    location.array = strsplit(location,'_')[[1]]
    tmp_loc = paste(location.array[-length(location.array)],collapse='_')
    tmp_loc = fix_location_name(tmp_loc)
    tmp_array = strsplit(tmp_loc,'_')[[1]]
    location.array[-length(location.array)] = tmp_array
    location = paste(location.array,collapse='_')
    if(verbose){warning("No checking currently being done here")}
    # stop("This code not yet written")
  }
  location_filename = paste(taxonomy_dir,'/Location/',location,'_LOC.csv',sep='')
  location.array = strsplit(location,'_')[[1]]
  who_region = location.array[1]
  ISO_A1 = location.array[2]
  ISO_A2 = location.array[c(-1,-2)]
  ISO_A2_level = length(ISO_A2)
  
  if(missing(shapefile)){
    shapefile = get_shape_file(location,taxonomy_dir)
  }
  
  if(nrow(shapefile) > 1){
    stop('This function is not vectorized.  Only use a single shapefile at a time')
  }
  #' @importFrom sf st_centroid
  centroid = st_centroid(shapefile)
  ##Check to make sure centroid is in polygon
  #' @importFrom sf st_intersects
  if(length(st_intersects(centroid,shapefile)[[1]]) == 0){
    ##Centroid not in shapefile.
    ## Using random point instead of centroid
    #' @importFrom sf st_as_sf
    tmp.centroid = st_as_sf(spsample(as(shapefile,"Spatial"),100,'regular'))
    #' @importFrom sf st_distance
    centroid = tmp.centroid[st_distance(centroid,tmp.centroid) == min(st_distance(centroid,tmp.centroid)),]
    centroid = centroid$geometry
  } else{
    centroid = centroid$geometry
  }
  centroid = setNames(attributes(centroid)$bbox[c('ymin','xmin')],c('cent_lat','cent_long'))
  
  isISO_A1 = c()
  isISO_A2 = c()
  
  if(ISO_A2_level > 0){
    isISO_A2 = setNames(1:ISO_A2_level*0+1,paste('isISO_A2_L',1:ISO_A2_level,sep=''))
  }
  if(length(location.array) > 1){
    isISO_A1 = c(isISO_A1 = 1)
  }
  isISO = c(isISO_A1,isISO_A2)
  
  name = c(name=eval(parse(text=paste('shapefile$NAME_',ISO_A2_level,sep=''))))
  
  if(missing_shape_file){
    shape_filename = paste(taxonomy_dir,'/ShapeFiles/custom_',location,'_LOC.csv',sep='')
    st_write(shapefile,shape_filename)
    output = as.matrix(c(name,centroid,isISO,notes="Automatically generated by taxdat",gis_file=shape_filename,gis_start_1=start_date,gis_end_1=end_date))
  } else {
    output = as.matrix(c(name,centroid,isISO,notes="Automatically generated by taxdat"))
  }
  if(!file.exists(location_filename)){
    write.table(output,location_filename,row.names = TRUE,col.names = FALSE,sep=',')
  } else {
    warning("File ",location_filename," already exists")
  }
}



#' @export
#' @name get_country_shapefile
#' @title get_country_shapefile
#' @param name The name of the country to get a shapefile for
#' @param taxonomy_dir The path to the taxonomy filesystem to store the shapefile in.
get_country_shapefile = function(name,taxonomy_dir = 'taxonomy-verified',verbose=FALSE){
  .Deprecated(msg = "get_country_shapefile is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "get_country_shapefile")
  location_name = paste("UNK",fix_country_name(name),sep='_')
  if(!is.na(location_name)){
    return(get_shape_file(location_name,taxonomy_dir,verbose=FALSE))
  }
  return(NA)
}

#' @export
#' @name get_country_sublevels
#' @title get_country_sublevels
#' @param location_name The name of the country to get a shapefile for
#' @param ISO_level What ISO level to pull shapefiles for.  1 is the level immediately below the country level
#' @param taxonomy_dir The path to the taxonomy filesystem to store the shapefile in.
#' @param land_only boolean If TRUE, try to filter out shapefiles that are not associated with land regions.
#' @param verbose Set to TRUE for more warnings and messages.
#' @importFrom lubridate now
get_country_sublevels = function(location_name,ISO_level,taxonomy_dir = 'taxonomy-verified', land_only=FALSE,method='center',layers_dir = 'Layers',date=now(),verbose=(method=='center')){
  .Deprecated(msg = "get_country_sublevels is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "get_country_sublevels")
  ## just because I always forget 
  if(endsWith(taxonomy_dir,"/")){
    taxonomy_dir <- substr(taxonomy_dir,1,nchar(taxonomy_dir)-1)
  }
  location_name = fix_country_name(location_name)
  location_name = paste(lookup_WHO_region(location_name),location_name,sep='_')
  ISO_A1 = strsplit(x = location_name,split="_")[[1]][2]
  ## Check for custom shapefiles.
  
  if(method == 'center'){
    location_dir <- paste(taxonomy_dir,'Location',sep='/')
    alllocs <- list.files(location_dir,pattern = location_name,ignore.case = TRUE)
    # pattern = paste('^[^_]*',paste(rep('_',ISO_level + 2),collapse='[^_]*'),'LOC.csv',sep='')
    # alllocs = alllocs[grep(pattern,alllocs)]
    allshp <- lapply(
      alllocs,
      function(loc){
        locfile <- read.csv(paste(taxonomy_dir,'Location',loc,sep='/'),stringsAsFactors = FALSE,header = FALSE)
        iso_loc_indices = grep('is_?ISO_A2_L',locfile[,1])
        iso_loc_indices = iso_loc_indices[as.numeric(locfile[iso_loc_indices,2]) %in% c(1,2)]
        iso_level <- max(c(0,as.numeric(substr(locfile[iso_loc_indices,1],nchar(locfile[iso_loc_indices,1]),nchar(locfile[iso_loc_indices,1])))))
        if(iso_level != ISO_level){
          return(NULL)
        }
        loc_name = strsplit(loc,'_')[[1]]
        loc_name = loc_name[1:(length(loc_name)-1)]
        loc_name = paste(loc_name,collapse='_')
        return(get_shape_file(location_name = loc_name,taxonomy_dir = taxonomy_dir,verbose = verbose,method = 'center',date = date))
      }
    )
    if(any(duplicated(allshp) & (!sapply(allshp, is.null)))){
      if(verbose){
        duplicate_idx <- which(duplicated(allshp) & (!sapply(allshp, is.null)))
        for(idx in duplicate_idx){
          matches = sapply(allshp,function(x){all.equal(allshp[[idx]],x) == TRUE})
          paste('Location',alllocs[idx],'is a duplicate of ',alllocs[matches])
        }
      }
    }
    allshp <- reduce_sf_vector(unique(allshp))
    cntry_shp <- get_shape_file(location_name,taxonomy_dir,method=method)
    if(is.na(st_crs(allshp))){
      st_crs(allshp) = st_crs(cntry_shp)
    }
    #' @importFrom sf st_is_valid
    problem_idx = which(!st_is_valid(allshp))
    if(length(problem_idx) > 0){
      #' @importFrom sf st_make_valid
      allshp$geometry[problem_idx] = sf::st_make_valid(allshp$geometry[problem_idx])
    }
    allshp = allshp[!is.na(st_dimension(allshp)),]
    #' @importFrom sf st_equals
    #' @importFrom sf st_union
    if(!st_equals(cntry_shp$geometry,st_union(allshp),sparse = FALSE)){
      warning("Sublevels incomplete")
    }
    return(allshp)
  }
  
  destination = paste(taxonomy_dir,'/ShapeFiles/',ISO_A1,'_adm',ISO_level,'.rds',sep='')
  
  if(!file.exists(paste(taxonomy_dir,"ShapeFiles",sep='/'))){
    stop("The directory ",taxonomy_dir," should exist, and have a subdirectory called ShapeFiles")
  }
  if(!file.exists(destination)){
    website = paste("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",ISO_A1,"_adm",ISO_level,".rds",sep='')
    download.file(website,destination,mode='wb')
  }
  #' @importFrom sf st_as_sf
  all_shape_files = st_as_sf(readRDS(destination))
  #' @importFrom sf st_is_valid
  if(!all(suppressWarnings(st_is_valid(all_shape_files)))){
    #' @importFrom sf st_make_valid
    all_shape_files <- sf::st_make_valid(all_shape_files)
  }
  #' @importFrom sf st_is_valid
  if(!all(suppressWarnings(st_is_valid(all_shape_files)))){
    #' @importFrom sf st_make_valid
    all_shape_files <- sf::st_make_valid(all_shape_files)
  }
  if(land_only){
    all_shape_files <- all_shape_files[all_shape_files[[paste("TYPE",ISO_level,sep='_')]] != "Water body",]
  }
  return(all_shape_files)
}



#' @export
#' @name fix_a2_l1_name
#' @title fix_a2_l1_name
#' @description Change an iso_a2_l1 name to its ISO_3166_1 code
#' @param location_name the name of the location to fix
fix_a2_l1_name <- function(location_name,country_name = NULL,verbose=TRUE){
  .Deprecated(msg = "fix_a2_l1_name is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "fix_a2_l1_name")
  location_name = standardize_string(location_name)
  fname <- system.file("extdata","isoa2l1_aliases.csv",package = "taxdat")
  if(nchar(fname) == 0){
    if(verbose){
      warning("Could not load isoa2l1_aliases.csv from the package directory.  Try reinstalling the package or contacting the maintainer.")
    }
    return(NA)
  }
  a2_l1_aliases  = tryCatch(
    read.csv(
      fname,
      sep=',',
      header=TRUE,
      stringsAsFactors=FALSE,
      na.strings = "",
      colClasses = 'character',
      quote = "\"",
      check.names = FALSE
    ),
    warning = function(w){
      if(length(w$message > 0)){
        if(grepl(pattern="ncomplete final line",x=w$message)){
          return(suppressWarnings(
            read.csv(
              fname,
              sep=',',
              header=TRUE,
              stringsAsFactors=FALSE,
              na.strings = "",
              colClasses = 'character',
              quote = "\"",
              check.names = FALSE
            )
          ))
        }
      }
      if(verbose){
        warning(paste(fname,":",w$message),immediate.=TRUE)
      }
      return(w)
    },
    error = function(e){
      if(verbose){
        warning(paste(fname,":",e),immediate.=TRUE)
      }
      return(e)
    }
  )
  rc <- NA
  # a2_l1_aliases[,1] = sapply(standardize_string(a2_l1_aliases[,1]),function(x){x[[1]]})
  # a2_l1_aliases[,2] = sapply(standardize_string(a2_l1_aliases[,2]),function(x){x[[1]]})
  a2_l1_aliases[,3] = sapply(standardize_string(a2_l1_aliases[,3]),function(x){x[[1]]})
  a2_l1_aliases[,4] = sapply(standardize_string(a2_l1_aliases[,4]),function(x){x[[1]]})
  a2_l1_aliases[,5] = sapply(standardize_string(a2_l1_aliases[,5]),function(x){x[[1]]})
  rc = rep('UNK',length(location_name))
  for(location_idx in 1:length(location_name)){
    if(location_name[location_idx] %in% a2_l1_aliases[,2]){
      rc[location_idx] <- location_name[location_idx]
    } else if(location_name[location_idx] %in% a2_l1_aliases[,3]){
      rc[location_idx] <- unique(a2_l1_aliases[location_name[location_idx] == a2_l1_aliases[,3],2])
    } else if(location_name[location_idx] %in% a2_l1_aliases[,4]){
      rc[location_idx] <- unique(a2_l1_aliases[location_name[location_idx] == a2_l1_aliases[,4],2])
    } else if(location_name[location_idx] %in% a2_l1_aliases[,5]){
      rc[location_idx] <- unique(a2_l1_aliases[location_name[location_idx] == a2_l1_aliases[,5],2])
    }
  }
  if(verbose){
    mapply(
      new=rc,
      old=location_name,
      function(new,old){
        if(is.na(old)){
          message("ISO_A2_L1 ",old," is not a valid region")
        } else if(is.na(new)){
          message("ISO_A2_L1 ",old," could not be located in the lookup table maintained by the package")
        } else if(old != new){
          message("The ISO_A2_L1 was changed from ",old," to ",new," using the lookup table maintained by the package")
        }
      }
    )
  }
  return(rc)
}

#' @export
#' @name extract_shapefile_from_raster
#' @title extract_shapefile_from_raster
#' @description Get only the parts of a shapefile that are nearby the raster in question.
#' @param shapefile A sf or sfc shapefile to get the relevant pieces from.
#' @param raster_layer a Raster* object to use for determining the extent to intersect with the shapefile.
#' @param expansion An optional numeric parameter to use if expanding the extent of \code{raster_layer} to include nearby shapes.
extract_shapefile_from_raster <- function(shapefile,raster_layer,expansion=1){
  .Deprecated(msg = "extract_shapefile_from_raster is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "extract_shapefile_from_raster")
  return(shapefile[
    which(
      sapply(
        #' @importFrom sf st_intersects
        st_intersects(
          shapefile,
          #' @importFrom sf st_set_crs
          st_set_crs(
            #' @importFrom sf st_as_sf
            st_as_sf(
              as(
                #' @importFrom raster extent
                expand_extent(extent(raster_layer),expansion),
                "SpatialPolygons"
              )
            ),
            #' @importFrom sf st_crs
            st_crs(shapefile)
          )
        ),
        length
      ) > 0
    ),])
}

#' @export
#' @name get_shapefile_cover
#' @title get_shapefile_cover
#' @description Get a shapefile covering the union of all of the countries in question.
#' @param country_names A character vector of country names to pull shapefiles for.
#' @param taxonomy_dir The path to the taxonomy filesystem to store the shapefile in.
#' @param verbose Whether to pass along messages about altered location names
get_shapefile_cover <- function(country_names,taxonomy_dir = 'taxonomy-verified',verbose=TRUE){
  .Deprecated(msg = "get_shapefile_cover is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "get_shapefile_cover")
  shapefiles <- lapply(
    country_names,
    get_country_shapefile,
    taxonomy_dir=taxonomy_dir,
    verbose=verbose
  )
  full_shapefile <- shapefiles[[1]]
  if(length(shapefiles) > 1){
    for(idx in 2:length(shapefiles)){
      #' @importFrom sf st_union
      full_shapefile <- st_union(
        full_shapefile,
        shapefiles[[idx]]
      )
    }
  }
  return(full_shapefile)
}



#' @export
#' @name extract_shapefile_from_shapefile
#' @title extract_shapefile_from_shapefile
#' @description Extract the parts of a shapefile that intersect another shapefile.
#' @param shp1 A shapefile to extract things from.
#' @param shp2 The shapefile to use as a bounding box to determine what to extract from shp1
#' @param strict.  boolean  If TRUE, take the intersection of the relevent parts of shp1 with shp2.  If FALSE, keep shapes from shp1 as they are, and include if any of the interior overlaps with the interior of shp2.
extract_shapefile_from_shapefile <- function(shp1,shp2,strict=FALSE){
  .Deprecated(msg = "extract_shapefile_from_shapefile is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "extract_shapefile_from_shapefile")
  #' @importFrom sf st_union
  shp2 <- st_union(shp2)
  #' @importFrom sf st_relate
  #' @importFrom sf st_dimension
  shp1 <- shp1[substr(st_relate(shp2,shp1),1,1) == st_dimension(shp1),]
  if(!strict){
    return(shp1)
  }
  #' @importFrom sf st_intersection
  return(st_intersection(shp1,shp2))
}

#' @export
#' @name coordinate_to_shapefile
#' @title coordinate_to_shapefile
#' @description Convert a coordinate to the shapefile containing it.
coordinate_to_shapefile <- function(lat,long,country,ISO_level){
  .Deprecated(msg = "coordinate_to_shapefile is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "coordinate_to_shapefile")
  #' @importFrom sf st_sfc
  #' @importFrom sf st_point
  point <- st_sfc(st_point(c(long,lat)))
  polys <- get_country_sublevels(country,ISO_level)
  #' @importFrom sf st_crs st_crs<-
  st_crs(point) <- st_crs(polys)
  #' @importFrom sf st_intersection
  return(st_intersection(polys,point))
}


standardize_locations <- function(location_name){
  .Deprecated(msg = "standardize_locations is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "standardize_locations")
  location_tmp = location_name
  location_tmp = gsub('|','VERTCHARACTER',location_tmp,fixed=TRUE)
  location_tmp = gsub('-','DASHCHARACTER',location_tmp,fixed=TRUE)
  location_tmp = gsub('_','UNDERSCORECHARACTER',location_tmp,fixed=TRUE)
  location_tmp = standardize_string(location_tmp)
  location_tmp = gsub('UNDERSCORECHARACTER','_',location_tmp,fixed=TRUE)
  location_tmp = gsub('DASHCHARACTER','-',location_tmp,fixed=TRUE)
  location_tmp = gsub('VERTCHARACTER','|',location_tmp,fixed=TRUE)
  while(any(
    grepl(pattern = '_$',location_tmp) ||
    grepl(pattern = '_NA$',location_tmp) ||
    grepl(pattern = '__',location_tmp)
    )){
    location_tmp = gsub('__','_',location_tmp,fixed=TRUE)
    location_tmp = gsub('_NA$','',location_tmp)
    location_tmp = gsub('_$','',location_tmp)
  }
  return(location_tmp)
}

#' @export
#' @param location_name character vector of location names to turn into a data frame
create_location_sf <- function(location_name,date,thorough=FALSE){
  .Deprecated(msg = "create_location_sf is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "create_location_sf")
  original_location_name = location_name
  location_name = standardize_locations(location_name)
  location_array = strsplit(location_name,split='_')
  if(thorough){
    location_array = lapply(location_array,function(x){lapply(1:length(x),function(i){x[1:i]})})
  } else {
    location_array = lapply(location_array,function(x){list(x)})
  }
  location_array = lapply(location_array,function(y){lapply(y,function(x){strsplit(x,split='|',fixed=TRUE)})})
  ## We currently have the following list levels
  ## 1 Location in args list
  ### 2 If thorough all shapefiles that matter
  #### 3 ISO_level
  ##### 4 location_within that arg
  
  ## Build an array to store things in for easier access
  old_location_array = location_array
  nargs = length(old_location_array)
  nthr = max(sapply(old_location_array,length))
  niso = max(sapply(old_location_array,function(x){max(sapply(x,length))}))
  npipe = max(sapply(old_location_array,function(x){max(sapply(x,function(x){max(sapply(x,length))}))}))
  location_array = array(as.character(NA),c(niso,nargs,npipe,nthr))
  
  ## Populate the array
  for(arg_idx in 1:nargs){
    ## Calculate the number of pipes for each ISO region for this arg.
    this_pipe_lengths = lapply(old_location_array[[arg_idx]],function(y){sapply(y,length)})
    ## We are assuming that the length is the single non-1 value of this_pipe_lengths
    thr_depth = length(old_location_array[[arg_idx]])
    pipe_length = unique(this_pipe_lengths[[thr_depth]])
    pipe_length = pipe_length[pipe_length > 1]
    ## The length might actually be 1...
    if(length(pipe_length) == 0){
      pipe_length = 1
    }
    ## Checking just to be sure
    if(length(pipe_length) > 1){
      ## This function is meant to not fail easily, so the pipe_length is set to 0 to indicate a failed state
      pipe_length = NA
    }
    ISO_depth = sapply(old_location_array[[arg_idx]],length)
    if(!is.na(pipe_length)){
      for(pipe_idx in 1:pipe_length){
        for(thr_idx in 1:thr_depth){
          for(ISO_idx in 1:ISO_depth[thr_idx]){
            if(pipe_idx <= this_pipe_lengths[[thr_idx]][ISO_idx]){
              location_array[ISO_idx, arg_idx, pipe_idx,thr_idx] <- old_location_array[[arg_idx]][[thr_idx]][[ISO_idx]][[pipe_idx]]
            } else {
              location_array[ISO_idx, arg_idx, pipe_idx,thr_idx] <- old_location_array[[arg_idx]][[thr_idx]][[ISO_idx]][[1]]
            }
          }
        }
      }
    }
  }
  ## We will use this for finding the shapefiles.  They will eventually be part of this object
  #' @importFrom dplyr filter
  #' @importFrom reshape2 melt
  ungrouped_shapefiles <- dplyr::filter(melt(location_array),!is.na(value))
  ungrouped_shapefiles$value = as.character(ungrouped_shapefiles$value)
  names(ungrouped_shapefiles)[1:4] <- c('ISO_level','arg_idx','pipe_idx','thr_idx')
  ungrouped_shapefiles <- ungrouped_shapefiles %>%
    group_by(arg_idx,pipe_idx,thr_idx) %>%
    do({
      tmp = .
      tmp$ISO_A2_level = max(tmp$ISO_level) - 2
      tmp
    })
  
  #' @importFrom tidyr spread
  ungrouped_shapefiles <- spread(ungrouped_shapefiles,key=ISO_level,value=value,drop=T)
  names(ungrouped_shapefiles)[5] <- 'who_region'
  if(ncol(ungrouped_shapefiles) > 5){
    names(ungrouped_shapefiles)[6] <- 'ISO_A1'
    if(ncol(ungrouped_shapefiles) > 6){
      names(ungrouped_shapefiles)[7:ncol(ungrouped_shapefiles)] <- paste('ISO_A2_L',as.numeric(names(ungrouped_shapefiles)[7:ncol(ungrouped_shapefiles)])-2,sep='')
    }
  }
  if(thorough){
    ungrouped_shapefiles = ungrouped_shapefiles %>% filter(thr_idx > 1)
  }
  ungrouped_shapefiles$location_name = paste0(apply(ungrouped_shapefiles[,5:ncol(ungrouped_shapefiles)],1,function(x){paste(na.omit(x),collapse='_')}))
  # ungrouped_shapefiles$location_full = location_name[ungrouped_shapefiles$arg_idx]
  if(length(date) > 1){
    ungrouped_shapefiles$date = date[ungrouped_shapefiles$arg_idx]
  } else {
    ungrouped_shapefiles$date = date
  }
  ungrouped_shapefiles = st_sf(
    ungrouped_shapefiles,
    #' @importFrom sf st_point
    geometry = rep(st_sfc(st_point(c(0,0))),nrow(ungrouped_shapefiles))
  )
  ungrouped_shapefiles$idx = 1:nrow(ungrouped_shapefiles)
  ungrouped_shapefiles$shapefile_source = as.character(NA)
  ungrouped_shapefiles$location = original_location_name[ungrouped_shapefiles$arg_idx]
  return(ungrouped_shapefiles)
}

#' @export
group_location_sf <- function(location_sf){
  .Deprecated(msg = "group_location_sf is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "group_location_sf")
  #' @import sf
  library(sf)
  # valid_locations = st_is_valid(location_sf)
  # if(!all(valid_locations)){
  #   location_sf$geometry[!valid_locations] = st_make_valid(location_sf$geometry[!valid_locations])
  # }
  if(any(location_sf$thr_idx > 1)){
    location_list = split(location_sf,as.factor(paste(location_sf$arg_idx,location_sf$pipe_idx)))
    # location_list = head(location_list)
    for(location_idx in 1:length(location_list)){
      if(nrow(location_list[[location_idx]]) > 1){
        orig_tmp_location_sf = location_list[[location_idx]]
        tmp_location_sf = orig_tmp_location_sf
        print(paste(location_idx,"/",length(location_list)))
        tmp_area = min(st_area(tmp_location_sf))
        tmp_shapefile_source = paste("'",paste(arrange(tmp_location_sf,-as.numeric(ISO_A2_level))$shapefile_source,collapse="'+'"),"'",sep='')
        tmp_location_sf = arrange(tmp_location_sf,-as.numeric(ISO_A2_level))
        tmp_location_sf = st_intersection(tmp_location_sf[1,],tmp_location_sf[-1,])
        tmp_location_sf = tmp_location_sf[,!grepl('.1$',names(tmp_location_sf))]
        if(nrow(tmp_location_sf) > 1){
          tmp_location_sf$shapefile_source = tmp_shapefile_source
          if(any(st_area(tmp_location_sf) < .9*tmp_area)){
            tmp_location_sf$geometry = st_sfc(st_point(c(NaN,NaN)))
          }
          tmp_location_sf = tmp_location_sf[1,]
        } else {
          tmp_location_sf = orig_tmp_location_sf
          tmp_location_sf = arrange(tmp_location_sf,-as.numeric(ISO_A2_level))
          tmp_location_sf = tmp_location_sf[1,]
          tmp_location_sf$geometry = st_sfc(st_point(c(NaN,NaN)))
          tmp_location_sf$shapefile_source = tmp_shapefile_source
        }
        if(nrow(tmp_location_sf) > 1){
          stop("This should not happen")
        }
        location_list[[location_idx]] = tmp_location_sf
      }
    }
    for(location_idx in 1:length(location_list)){
      if(nrow(location_list[[location_idx]]) > 1){
        x = location_list[[location_idx]]
        tmp_x = x %>% arrange(-as.numeric(ISO_A2_level)) %>% st_intersection() %>% filter(n.overlaps == nrow(x))
        tmp_x$shapefile_source = paste("'",paste(arrange(x,-as.numeric(ISO_A2_level))$shapefile_source,collapse="'+'"),"'",sep='')
        if(st_area(tmp_x) < .9 * min(st_area(x))){
          tmp_x$geometry = st_sfc(st_point(c(NaN,NaN)))
        }
        location_list[[location_idx]] = tmp_x
      }
    }
    location_sf = reduce_sf_vector(location_list)
  }
  if(any(location_sf$pipe_idx > 1)){
    #' @importFrom dplyr filter
    location_sf_piped = location_sf %>% filter(pipe_idx > 1)
    #' @importFrom dplyr filter
    location_sf_piped = location_sf %>% filter(arg_idx %in% unique(location_sf_piped$arg_idx))
    #' @importFrom dplyr filter
    location_sf_unpiped = location_sf %>% filter(!(arg_idx %in% unique(location_sf_piped$arg_idx)))
    piped_list = location_sf_piped %>%
      split(as.factor(location_sf_piped$arg_idx)) %>%
      lapply(function(x){
        if(nrow(x) > 1){
          if(any(st_dimension(x) < 2)){
            x$geometry = st_sfc(st_point(c(NaN,NaN)))
          }
          tmp_x = x %>%
            arrange(-as.numeric(ISO_A2_level)) %>%
            st_union()
          tmp2_x = as.data.frame(x) %>%
            select(-geometry) %>%
            summarize_all(funs(if(length(unique(.))==1){unique(.)} else {paste(.,collapse='|')})) %>%
            cbind(.,tmp_x) %>%
            st_as_sf()
          x = tmp2_x
        }
        return(x)
      })
    location_sf_piped = reduce_sf_vector(piped_list)
    if(is.na(st_crs(location_sf_piped))){
      st_crs(location_sf_piped) = st_crs(location_sf_unpiped)
    }
    #' @importFrom dplyr arrange
    location_sf = rbind(location_sf_unpiped,location_sf_piped)
  }
  return(location_sf)
}

#' @export
#' @name st_update_bounding_box
#' @title st_update_bounding_box
#' @description Make sure the bounding box of a sf object is correct
#' @param shp an sf object
#' @return a copy of shp with a modified bounding box
st_update_bounding_box <- function(shp){
  .Deprecated(msg = "st_update_bounding_box is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "st_update_bounding_box")
  original_bbox = st_bbox(shp)
  new_bbox = as.vector(st_bbox(shp[1,]$geometry))
  names(new_bbox) = c('xmin','ymin','xmax','ymax')
  for(i in 1:nrow(shp)){
    this_bbox = st_bbox(shp$geometry[[i]])
    new_bbox['xmin'] = min(new_bbox['xmin'],this_bbox['xmin'])
    new_bbox['ymin'] = min(new_bbox['ymin'],this_bbox['ymin'])
    new_bbox['xmax'] = max(new_bbox['xmax'],this_bbox['xmax'])
    new_bbox['ymax'] = max(new_bbox['ymax'],this_bbox['ymax'])
    
  }
  attr(new_bbox,'class') = 'bbox'
  #' @importFrom sf st_geometry st_geometry<-
  attr(st_geometry(shp),'bbox') = new_bbox
  return(shp)
}
