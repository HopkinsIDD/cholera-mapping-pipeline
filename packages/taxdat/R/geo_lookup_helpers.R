#' @export
#' @name lookup_WHO_region
#' @title lookup_WHO_region
#' @param x A vector of country or subcountry codes to look up the WHO region for
#' @return return WHO region string
lookup_WHO_region = function(x){
  data('WHO_regions',package='taxdat')
  rc <- c()
  for (cntry in x) {
    if(cntry %in% WHO_regions$Entity){
      rc <- c(rc, as.character(WHO_regions$WHO.region.code[WHO_regions$Entity==cntry]))
    } else if (cntry %in% WHO_regions$Country.code){
      rc <- c(rc, as.character(WHO_regions$WHO.region.code[WHO_regions$Country.code==cntry]))
    } else {
      rc <- c(rc,NA)
    }
  }
  return(rc)
}


#' @export
#' @name fix_country_name
#' @title fix_country_name
#' @description Change a country name to its ISO_3166_1 alpha-3 code
#' @param country_name The name of the country
#' @param verbose logical
#' @return return ISO country code
fix_country_name <- function(country_name,verbose=TRUE){
  country_name = taxdat::standardize_string(country_name)
  fname <- system.file("extdata","country_aliases.csv",package = "taxdat")
  if(nchar(fname) == 0){
    if(verbose){
      warning("Could not load country_aliases.csv from the package directory.  Try reinstalling the package or contacting the maintainer.")
    }
    return(NA)
  }
  country_aliases  = tryCatch(
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
  country_aliases[,2] = sapply(taxdat::standardize_string(country_aliases[,2]),function(x){x[[1]]})
  rc = rep('UNK',length(country_name))
  for(country_idx in 1:length(country_name)){
    if(country_name[country_idx] %in% country_aliases[,2]){
      rc[country_idx] <- country_aliases[country_name[country_idx] == country_aliases[,2],1]
    } else if(country_name[country_idx] %in% country_aliases[,1]){
      rc[country_idx] <- country_name[country_idx]
    }
  }
  if(verbose){
    mapply(
      new=rc,
      old=country_name,
      function(new,old){
        if(is.na(old)){
          message("ISO_A1 ",old," is not a valid region")
        } else if(is.na(new)){
          message("ISO_A1 ",old," could not be located in the lookup table maintained by the package")
        } else if(old != new){
          message("The ISO_A1 was changed from ",old," to ",new," using the lookup table maintained by the package")
        }
      }
    )
  }
  return(rc)
}


#' @export
#' @name lookup_WorldPop_region
#' @title lookup_WorldPop_region
#' @description Determine which WorldPop raster to look at for population of a country
#' @param location The name of the country
#' @param verbose Whether to print detailed error messages
#' @return return string of WorldPop region code
lookup_WorldPop_region <- function(location,verbose=TRUE){
  if('character' %in% class(location)){
    fname <- system.file("extdata","country_worldpop_regions.csv",package = "taxdat")
    if(nchar(fname) == 0){
      if(verbose){
        warning("Could not load country_worldpop_regions.csv from the package directory.  Try reinstalling the package or contacting the maintainer.")
      }
      return(NA)
    }
    worldpop_regions = tryCatch(
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
    if(location %in% worldpop_regions[,'country']){
      return(worldpop_regions[location == worldpop_regions[,'country'],'region_code'])
    }
    if(location %in% worldpop_regions[,'country_code']){
      return(worldpop_regions[location == worldpop_regions[,'country_code'],'region_code'])
    }
    
    if(verbose){
      warning(paste("Could not find WorldPop data for country",location))
      return(NA)
    }
  } else if('sf' %in% class(location)){
    all_Worldpop_regions <- c("AMR","AFR","OCE","ASI")
    intersects = c()
    for(region in all_Worldpop_regions){
      files <- list.files(paste("Layers","pop",region,sep='/'))
      # files <- files[grepl('adj',files)]
      files <- files[grepl('tif$',files)]
      files <- files[1]
      raster_layer <- raster::raster(paste("Layers","pop",region,files,sep='/'))
      intersects[region] = sf::st_intersects(
        sf::st_as_sfc(sf::st_bbox(raster_layer)),
        sf::st_as_sfc(sf::st_bbox(location$geometry)),
        sparse = FALSE
      )
    }
    if(sum(intersects) == 1){
      return(names(intersects)[intersects])
    } else {
      stop("Not yet written")
    }
  }
}


#' @name standardize_string
#' @title standardize_string
#' @param string character string
#' @return standardize string
standardize_string <- function(string){
  # if(is.na(string)){return(NA)}
  string = as.character(string)
  string = strsplit(string,'|',fixed=TRUE)
  string = lapply(string,function(x){gsub(' ','',x)})
  string = lapply(string,function(x){
    if(length(x) == 0){
      return(x)
    }
    for(i in 1:length(x)){
      if(Encoding(x[i]) != 'unknown'){
        x[i] = iconv(from=Encoding(x[i]),to='ASCII//TRANSLIT',x[i])
      } else {
        x[i] = iconv(from='UTF-8',to='ASCII//TRANSLIT',x[i])
      }
    }
    return(x)
  })
  string = lapply(string,function(x){gsub('[[:punct:]]','',x)})
  string = lapply(string,function(x){toupper(x)})
  string[sapply(string,length) == 0] = ''
  return(string)
}



