#' @import methods dplyr
library(methods)
library(dplyr)
################################################################
######################Taxonomy Data Parser######################
################################################################
#' Taxonomy Data Parser
################################################################
##  Note: #' at the beginning of the line means to pass to R
##  Markdown
#'
#' The Taxonomy Data Parser package loads and handles Taxonomy Data.
#' It provides 1 category of functions:
#'   Taxonomy Parsing
#'
#' @section Taxonomy Parsing Functions:
##
#' @details Taxonomy parsing functions are designed to read flat
#'   files from a filesystem storing taxonomy data.  If the file
#'   system is called taxonomy.directory, then the following
#'   subdirectories must exist:
#' \itemize{
#' \item taxonomy.directory/Description - files must end in _DESC.csv
#' \item taxonomy.directory/Location    - files must end in _LOC.csv
#' \item taxonomy.directory/EpiCurves   - files must end in _EPI.csv
#' \item taxonomy.directory/Population  - files must end in _POP.csv
#' }
#' The files Description and Population files are regular .csv files,
#'   and the Location and Epi files are transposed .csv files.
################################################################
################################################################
##TODO:
##  1. Only read in relevant epi files in read_taxonomy_data
##  2. Specify the types of all columns in code, so the warnings
##     go away in read_taxonomy_csv and read_transposed_taxonomy_csv
##  3. Comment the code more effectively
##  4. Parallelize the reading.
################################################################

################################################################
#' @name read_taxonomy_csv
#' @title read_taxonomy_csv
#' @description This function reads a csv file which is column major.
#'   It will also fail without throwing an error, so it can be used on
#'   missing data
##  args:
#' @param filename A string for the relative or absolute path to the
#'   file to be read.  The file should be column major
##  vals:
#' @return A data.frame containing the data from \code{filename} or a
#'   data.frame with a single column missing=\code{filename})
################################################################

read_taxonomy_csv= function(filename,verbose=FALSE){
  if(!file.exists(filename)){
    if(verbose){
      warning(paste(filename,": Does not exist"),immediate.=FALSE)
    }
    return(data.frame(missing=filename,stringsAsFactors = FALSE))
  }
  output.data  = tryCatch(
    read.csv(
      filename,
      sep=',',
      header=TRUE,
      stringsAsFactors=FALSE,
      na.strings = "",
      colClasses = 'character',
      quote = "\"",
      check.names = FALSE,
      row.names=NULL
    ),
    warning = function(w){
      if(length(w$message > 0)){
        if(grepl(pattern="ncomplete final line",x=w$message)){
          return(suppressWarnings(
            read.csv(
              filename,
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
        warning(paste(filename,":",w$message),immediate.=TRUE)
      }
      return(w)
    },
    error = function(e){
      if(verbose){
        warning(paste(filename,":",e),immediate.=TRUE)
      }
      return(e)
    }
  )
  if(length(class(output.data))>0){
    if(class(output.data)[[1]] != "data.frame"){
      output.data = data.frame(
        missing=filename,
        stringsAsFactors = FALSE
      )
    }
  }
  return(output.data)
}

################################################################
#' @name read_transposed_taxonomy_csv
#' @title read_transposed_taxonomy_csv
#' @description This function reads a csv file which is row major.  It
#'   will also fail without throwing an error, so it can be used on
#'   missing data
##  args:
#' @param filename A string for the relative or absolute path to the
#'   file to be read.  The file should be row major
##  vals:
#' @return A data.frame containing the data from \code{filename} or a
#'   data.frame with a single column missing=\code{filename})
################################################################

read_transposed_taxonomy_csv = function(filename,verbose=FALSE){
  if(!file.exists(filename)){
    if(verbose){
      warning(paste(filename,": Does not exist"),immediate.=FALSE)
    }
    return(data.frame(missing=filename,stringsAsFactors = FALSE))
  }
  output.data  = tryCatch(
    as.data.frame(
      t(read.csv(
        filename,
        sep=',',
        header=FALSE,
        stringsAsFactors=FALSE,
        na.strings = "",
        colClasses = 'character',
        quote = "\"",
        check.names = FALSE
      )),
      stringsAsFactors=FALSE
    ),
    warning = function(w){
      if(length(w$message > 0)){
        if(grepl(pattern="ncomplete final line",x=w$message)){
          return(suppressWarnings(
            as.data.frame(
              t(read.csv(
                filename,
                sep=',',
                header=FALSE,
                stringsAsFactors=FALSE,
                na.strings = "",
                colClasses = 'character',
                quote = "\"",
                check.names = FALSE
              )),
              stringsAsFators = FALSE
            )))
        }
      }
      if(verbose){
        warning(paste(filename,":",w$message),immediate.=TRUE)
      }
      return(w)
    },
    error = function(e){
      if(verbose){
        warning(paste(filename,":",e),immediate.=TRUE)
      }
      return(e)
    }
  )
  if(length(class(output.data))>0){
    if(class(output.data)[[1]] != "data.frame"){
      warning(paste(
        "Output date is of type",
        class(output.data),
        "instead of data.frame."
      ))
      output.data = data.frame(
        missing=filename,stringsAsFactors = FALSE
      )
    } else{
      colnames(output.data) = as.character(unlist(output.data[1,]))
      output.data = output.data[-1,]
    }
  }
  return(output.data)
}

################################################################
#' @name safe.function
#' @title safe.function
#' @description This function runs a function, and returns either
#'   the return of that function, or a default return if it fails
##  args:
#' @param function The function to run safely
#' @param \dots The normal arguments to the function
#' @param default The value the function should return if it fails
##  vals:
#' @return The return value of the function if the function runs,
#'   or \code{default} otherwise
################################################################
safe.function <- function(fxn,...,default=warning("The function failed to run")){
  tryCatch(
    fxn(...),
    error = function(e){
      warning(e$message)
      return(default)
    }
  )
}

##Include methods for reading each particular type of file
################################################################
#' @name read_description_csv
#' @title read_description_csv
#' @description This function reads a csv file containing a
#'   description file.
##  args:
#' @param filename A string for the relative or absolute path to the
#'   file to be read.  The file format is described in the
#'   documentation
##  vals:
#' @return A \code{data.frame} containing the data from
#'   \code{filename} or a single column missing with the filename
#'   listed
################################################################

description_coltypes = c(
  uid = "integer",
  source = "character",
  source_uid = "character",
  source_url = "character",
  contact = "character",
  contact_email = "character",
  is_public = "integer",
  mou_dsa_notes = "character",
  contains_pii = "integer",
  irb_protocol = "character",
  owner = "character",
  owner_email = "character",
  source_file = "character",
  who_region = "character",
  ISO_A1 = "character",
  ISO_A2_L1 = "character",
  ISO_A2_L2 = "character",
  ISO_A2_L3 = "character",
  ISO_A2_L4 = "character",
  ISO_A2_L5 = "character",
  ISO_A2_L6 = "character",
  ISO_A2_L7 = "character",
  ISO_A2_L8 = "character",
  ISO_A2_L9 = "character",
  suspected_case_def = "character",
  confirmed_case_def = "character",
  day_start = "Date",
  day_end = "Date",
  primary_time_criteria = "character",
  deaths = "integer",
  cases = "integer",
  strains = "character",
  tet_res = "integer",
  sul_res = "integer",
  cip_res = "integer",
  az_res = "integer",
  humanitarian_crisis_assoc = "integer",
  reactive_vaccination = "integer",
  prev_vaccination = "integer",
  notes = "character"
)

read_description_csv = function(filename){
  rc <- read_transposed_taxonomy_csv(filename)
  for(column in colnames(rc)){
    if(!is.na(description_coltypes[column])){
      if(description_coltypes[column] == "Date"){
        rc[[column]] <- safe.function(as.Date,x=rc[[column]],default=as.Date(NA))
        # rc[[column]] <- as.Date(rc[[column]])
      } else {
        rc[[column]] <- as(rc[[column]],description_coltypes[column])
      }
    }
    
  }
  old.ncol = ncol(rc)
  if(old.ncol == 1){
    return(rc)
  }
  rc <- rc[,!is.na(colnames(rc))]
  rc <- rc[,!(colnames(rc) == "")]
  if(ncol(rc) != old.ncol){
    warning(paste("Columns were removed from file",filename,"during the reading process.  They are presumed to be empty"))
  }
  return(rc)
}



################################################################
#' @name read_epi_csv
#' @title read_epi_csv
#' @description This function reads a csv file containing a epi file
##  args:
#' @param filename A string for the relative or absolute path to the
#'   file to be read.  The file format is described in the
#'   documentation
##  vals:
#' @return A \code{data.frame} containing the data from
#'   \code{filename} or a single column missing with the filename
#'   listed
################################################################
epi_coltypes = c(
  TL = "Date",
  TR = "Date",
  TL_onset = "Date",
  TR_onset = "Date",
  TL_clinic = "Date",
  TR_clinic = "Date",
  TL_death = "Date",
  TR_death = "Date",
  ISO_A1 = "character",
  ISO_A2_L1 = "character",
  ISO_A2_L2 = "character",
  ISO_A2_L3 = "character",
  ISO_A2_L4 = "character",
  ISO_A2_L5 = "character",
  ISO_A2_L6 = "character",
  ISO_A2_L7 = "character",
  ISO_A2_L8 = "character",
  ISO_A2_L9 = "character",
  lat_case = "character",
  long_case = "character",
  sCh = "numeric",
  cCh = "numeric",
  deaths_L = "numeric",
  deaths_R = "numeric",
  sCh_L = "numeric",
  sCh_R = "numeric",
  cCh_L = "numeric",
  cCh_R = "numeric",
  age_U = "numeric",
  sex_U = "numeric",
  vac_U = "numeric"
)

read_epi_csv = function(filename,verbose=FALSE){
  rc <- read_taxonomy_csv(filename)
  for(column in colnames(rc)){
    if(!is.na(epi_coltypes[column])){
      if(epi_coltypes[column] == "Date"){
        #rc[[column]] <- as.Date(rc[[column]])
        rc[[column]] <- safe.function(as.Date,x=rc[[column]],default=as.Date(NA))
      } else {
        rc[[column]] <- as(rc[[column]],epi_coltypes[column])
      }
    }
  }
  old.ncol = ncol(rc)
  if(old.ncol == 1){
    return(rc)
  }
  rc <- rc[,!is.na(colnames(rc))]
  rc <- rc[,!(colnames(rc) == "")]
  if(verbose && (ncol(rc) != old.ncol)){
    warning(paste("Columns were removed from file",filename,"during the reading process.  They are presumed to be empty"))
  }
  return(rc)
}

################################################################
#' @name read_location_csv
#' @title read_location_csv
#' @description This function reads a csv file containing a location file
##  args:
#' @param filename A string for the relative or absolute path to the
#'   file to be read.  The file format is described in the
#'   documentation
##  vals:
#' @return A \code{data.frame} containing the data from
#'   \code{filename} or a single column missing with the filename
#'   listed
################################################################

location_coltypes = c(
  name = "character",
  cent_lat = "numeric",
  cent_long = "numeric",
  isISO_A1 = "numeric",
  isISO_A2_L1 = "numeric",
  isISO_A2_L2 = "numeric",
  isISO_A2_L3 = "numeric",
  isISO_A2_L4 = "numeric",
  isISO_A2_L5 = "numeric",
  isISO_A2_L6 = "numeric",
  isISO_A2_L7 = "numeric",
  isISO_A2_L8 = "numeric",
  isISO_A2_L9 = "numeric",
  gis_file = 'character',
  gis_file_2 = 'character',
  gis_file_3 = 'character',
  gis_file_4 = 'character',
  gis_file_5 = 'character',
  gis_start_1 = 'Date',
  gis_start_2 = 'Date',
  gis_start_3 = 'Date',
  gis_start_4 = 'Date',
  gis_start_5 = 'Date',
  gis_end_1 = 'Date',
  gis_end_2 = 'Date',
  gis_end_3 = 'Date',
  gis_end_4 = 'Date',
  gis_end_5 = 'Date',
  enclosed_by = "character",
  notes = "character"
)

read_location_csv = function(filename){
  rc <- read_transposed_taxonomy_csv(filename)
  for(column in colnames(rc)){
    if(!is.na(location_coltypes[column])){
      if(location_coltypes[column] == "Date"){
        # rc[[column]] <- as.Date(rc[[column]])
        rc[[column]] <- safe.function(as.Date,x=rc[[column]],default=as.Date(NA))
      } else {
        rc[[column]] <- as(rc[[column]],location_coltypes[column])
      }
    }
  }
  if(nrow(rc) > 1){
    return(data.frame(missing=filename,stringsAsFactors = FALSE))
  }
  
  old.ncol = ncol(rc)
  if(old.ncol == 1){
    return(rc)
  }
  rc <- rc[,!is.na(colnames(rc))]
  rc <- rc[,!(colnames(rc) == "")]
  if(ncol(rc) != old.ncol){
    warning(paste("Columns were removed from file",filename,"during the reading process.  They are presumed to be empty"))
  }
  ## rc <- rc %>% mutate(is_public = is_public == 1)
  return(rc)
}

################################################################
#' @name read_population_csv
#' @title read_population_csv
#' @description This function reads a csv file containing a population
#'   file
##  args:
#' @param filename A string for the relative or absolute path to the
#'   file to be read.  The file format is described in the
#'   documentation
##  vals:
#' @return A \code{data.frame} containing the data from
#'   \code{filename} or a single column missing with the filename
#'   listed
################################################################
population_coltypes = c(
  TL = "Date",
  TR = "Date",
  pop = "numeric",
  source = "character"
)
read_population_csv = function(filename){
  rc <- read_taxonomy_csv(filename)
  for(column in colnames(rc)){
    if(!is.na(population_coltypes[column])){
      if(population_coltypes[column] == "Date"){
        # rc[[column]] <- as.Date(rc[[column]])
        rc[[column]] <- safe.function(as.Date,x=rc[[column]],default=as.Date(NA))
      } else {
        rc[[column]] <- as(rc[[column]],population_coltypes[column])
      }
    }
  }
  old.ncol = ncol(rc)
  if(old.ncol == 1){
    return(rc)
  }
  rc <- rc[,!is.na(colnames(rc))]
  rc <- rc[,!(colnames(rc) == "")]
  if(length(ncol(rc)) == 0){
    browser()
  }
  if(ncol(rc) != old.ncol){
    warning(paste("Columns were removed from file",filename,"during the reading process.  They are presumed to be empty"))
  }
  
  return(rc)
}

################################################################
#' @name filter_description_data
#' @title filter_description_data
#' @description This function selects from a data.frame based on user
#'   provided filters
##  args:
#' @param data A data.frame to filter
#' @param ... As many string arguments as desired.
#' \itemize{
#' \item "who_region == 'AFR'"
#' \item 'ISO_A1 %in% c("COD","NGA")'
#' \item 'source != "ProMED"'
#' }
#' Each filter is applied sequentially, so only data that matches all
#'   filters will be returned.
#'
##  vals:
#' @return A data.frame with the filters applied
################################################################

filter_description_data = function(data,...){
  ##print(paste('(',paste(...,sep=') &( '),')',sep=''))
  if(!missing(...)){
    data = data %>% filter_(paste('(',paste(...,sep=') & ('),')',sep=''))
  }
  ##print(data)
  ##print("finished")
  return(data)
}


################################################################
#' @name read_description_taxonomy
#' @title read_description_taxonomy
#' @export read_description_taxonomy
#' @description This function will pull all of the description
#'   information.  It reads all of the description files, and combines
#'   them into a single data.frame
##  args:
#' @param taxonomy.directory A string for the path for the directory
#'   described above.
#' @param ... A sequence of filters used to filter the description
#'   files.  See \code{filter_description_data} for details
#' @param uids A vector of uids to read the description files for.
#'   This parameter is optional.  If missing, this function will read
#'   all present uids.
##  vals:
#' @return A data.frame containing the data read from the directory
#'   \code{taxonomy.directory} filtered by the filters \code{...}
################################################################
read_description_taxonomy = function(taxonomy.directory,...,uids){
  if(!missing(uids)){
    all.description.files = paste("CHOLERA",uids,"_DESC.csv",sep='')
    stop("The argument uids is not yet implemented")
  } else {
    all.description.files = list.files(
      paste(taxonomy.directory,"Description",sep='/'),
      no..=TRUE,
      recursive = TRUE,
      include.dirs=FALSE
    )
  }
  all.description.files = lapply(
    all.description.files,
    function(file){
      paste(taxonomy.directory,"Description",file,sep='/')
    }
  )
  
  ##Read all of the data from our description files, and turn them
  ##  into a single data.table
  all.description.data = lapply(
    all.description.files,
    function(file){
      read_description_csv(file)
    }
  )
  ##This next line makes sure the files close after reading.
  # closeAllConnections()
  all.description.data = bind_rows(all.description.data)
  ##This next line ensures that we treat uid as an integer
  all.description.data = all.description.data %>%
    #' @importFrom dplyr mutate
    mutate(uid = as.integer(uid))
  ##We now select only the data we want from the description.data
  all.description.data = filter_description_data(
    all.description.data,
    ...
  )
}


################################################################
#' @name read_epi_taxonomy
#' @title read_epi_taxonomy
#' @export read_epi_taxonomy
#' @description This function will pull all of the epi case
#'   information.  It reads all of the epi files, and combines them
#'   into a single data.frame
##  args:
#' @param taxonomy.directory A string for the path for the directory
#'   described above.
#' @param columns From the final data, which columns to select before
#'   returning the data.
#' @param uids A vector of uids to read the description files for.
#'   This parameter is optional.  If missing, this function will read
#'   all present uids.
#' @return A data.frame containing the specified data read from the
#'   directory \code{taxonomy.directory} filtered by the filters
#'   \code{...}
################################################################
read_epi_taxonomy = function(taxonomy.directory,uids,verbose=FALSE){
  if(!missing(uids)){
    ##This will need to account for public and private somehow
    public.dir <- paste(taxonomy.directory,"EpiCurves","Public",sep='/')
    private.dir <- paste(taxonomy.directory,"EpiCurves","Restricted",sep='/')
    all.epi.files = paste("CHOLERA",uids,"_EPI.csv",sep='')
    public.files = list.files(public.dir)[list.files(public.dir) %in% all.epi.files]
    private.files = list.files(private.dir)[list.files(private.dir) %in% all.epi.files]
    missing.files = all.epi.files[!(all.epi.files %in% c(public.files,private.files))]
    if(verbose){
      warning("There are", length(missing.files), "missing.")
    }
    all.epi.files = c(paste("Public",public.files,sep='/'),paste("Restricted",private.files,sep='/'))
  } else {
    all.epi.files = list.files(
      paste(taxonomy.directory,"EpiCurves",sep='/'),
      no..=TRUE,
      recursive = TRUE,
      include.dirs=FALSE
    )
  }
  all.epi.files = lapply(
    all.epi.files,
    function(file){
      paste(taxonomy.directory,"EpiCurves",file,sep='/')
    }
  )
  
  ## check which files are csvs
  are_csvs <- sapply(all.epi.files,function(my_file) endsWith(my_file,"csv"),simplify = TRUE)
  
  all.epi.data = lapply(all.epi.files[are_csvs],read_epi_csv)
  all.epi.files = all.epi.files[are_csvs]
  # closeAllConnections()
  
  ##Insert a row into the epi data containing the uid.  We need this
  ##  to join with the description data
  all.epi.uids = lapply(
    all.epi.files,
    function(file){
      unlist(strsplit(file,'_'))[1]
    }
  )
  all.epi.uids = lapply(
    all.epi.uids,
    function(uid){
      data.frame(unlist(strsplit(uid,'CHOLERA'))[2],stringsAsFactors = FALSE)
    }
  )
  ##Consider putting the as.numeric here
  for(idx in 1:length(all.epi.uids)){
    all.epi.data[[idx]] = mutate(all.epi.data[[idx]],'uid'=unlist(all.epi.uids[[idx]]))
  }
  # all.epi.data = mapply(
  #   uid=all.epi.uids,
  #   table=all.epi.data,
  #   function(uid,table){
  #     return(mutate(table,'uid'=unlist(uid)))
  #   }
  # )
  ##all.epi.data = Map(
  ##  function(uid,table){
  ##    return(mutate(table,'uid'=unlist(uid)))
  ##  },
  ##  uid=all.epi.uids,
  ##  table=all.epi.data
  ##)
  
  ##We need to transform our data from a list of data.tables into a
  ##  single data.table.
  all.epi.data = bind_rows(all.epi.data)
  all.epi.data = all.epi.data %>%
    #' @importFrom dplyr mutate
    mutate(uid = as.integer(uid))
  return(all.epi.data)
}

################################################################
#' @name read_taxonomy_data
#' @title read_taxonomy_data
#' @export read_taxonomy_data
#' @description This function is the main function in this package.
#'   It reads the taxonomy files from the filesystem, and combines
#'   them all into a single data.frame
##  args:
#' @param taxonomy.directory A string for the path for the directory
#'   described above.
#' @param columns From the final data, which columns to select before
#'   returning the data.
#' @param ... A sequence of filters used to filter the description
#'   files.  See \code{filter_description_data} for details
##  vals:
#' @return A data.frame containing the specified \code{columns} of the
#'   data read from the directory \code{taxonomy.directory} filtered
#'   by the filters \code{...}
################################################################
read_taxonomy_data = function(
  taxonomy.directory = 'taxonomy-verified',
  columns=NULL,
  ...
){
  ##Start by getting lists of all of the appropriate description files
  ##  Everything else will depend on description files, so there's no
  ##  need to get the other files yet
  all.description.data <- read_description_taxonomy(
    taxonomy.directory = taxonomy.directory,
    ...
  )
  
  ##Get relevent uids
  relevent_uids = unique(all.description.data$uid)
  
  ##Now we do the same thing for the epi files
  ##Read all of the data from our epi files
  
  all.epi.data <- read_epi_taxonomy(
    taxonomy.directory = taxonomy.directory,
    uids = relevent_uids
  )
  
  ##Now we join all the epi files together
  join.columns = c('uid')
  all.data = inner_join(
    all.description.data,
    all.epi.data,
    by = setNames(join.columns,join.columns)
  )
  
  names(all.data)[ grepl(pattern='\\.x$',names(all.data),perl=TRUE)] =
    gsub(
      '\\.x$',
      '.desc',
      names(all.data)[ grepl(pattern='\\.x$',names(all.data))]
    )
  
  names(all.data)[ grepl(pattern='\\.y$',names(all.data),perl=TRUE)] =
    gsub(
      '\\.y$',
      '',
      names(all.data)[ grepl(pattern='\\.y$',names(all.data))]
    )
  
  ##Fix this to use all available ISO levels
  all.locations = data.frame(
    data = apply(
      # select_(all.data,.dots = c("who_region","ISO_A1",sort(names(all.data)[(!endsWith(names(all.data),"desc")) & (startsWith(names(all.data),'ISO_A2'))]))),
      select_(
        all.data,
        .dots = c(
          names(all.data)[grepl('^who_region$',names(all.data))],
          names(all.data)[grepl('^ISO_A1$',names(all.data))],
          sort(names(all.data)[grepl('^ISO_A2_L[1234567890]*$',names(all.data))])
        )
      ),
      1,
      function(x){
        gsub('(_NA)+$','',paste(x,collapse='_'))
      }
    ),
    stringsAsFactors = FALSE
  )
  
  all.locations = all.locations %>%
    mutate(location=data) %>%
    #' @importFrom dplyr select
    select(location)
  unique.locations = all.locations %>%
    #' @importFrom dplyr group_by
    group_by(location) %>%
    #' @importFrom dplyr summarize
    summarize()
  
  ##all.locations = lapply(locations,function(...){data.table(location=...)})
  ##all.locations = bind_rows(locations)
  all.data = all.data %>% bind_cols(all.locations)
  location.description.files = unique.locations %>%
    #' @importFrom dplyr mutate
    mutate(
      location = paste(
        taxonomy.directory,
        '/Location/',
        location,
        "_LOC.csv",
        sep=''
      )
    ) %>%
    select(location)
  location.population.files =  unique.locations %>%
    #' @importFrom dplyr mutate
    mutate(
      location = paste(
        taxonomy.directory,
        '/Population/',
        location,
        "_POP.csv",
        sep=''
      )
    ) %>%
    select(location)
  
  ##So, location population is more complicated, because we need to
  ##  join on TL,TR... this will likely involve something hard
  # closeAllConnections()
  ##apply(location.population.files,1,read_taxonomy_csv)
  # closeAllConnections()
  
  if(nrow(location.population.files) > 0){
    location.population.data = apply(
      location.population.files,
      1,
      function(x){read_population_csv(x[[1]])}
    )
    
    location.population.data = mapply(
      location=unique.locations[[1]],
      table=location.population.data,
      function(location,table){
        #' @importFrom dplyr mutate
        return(mutate(table,'location'=location))
      },
      SIMPLIFY = FALSE
    )
    
    location.population.data = bind_rows(location.population.data)
    
    ##Consider doing something smarter here.
    join.columns = c("location","TL","TR");
    join.columns = join.columns[join.columns %in% colnames(all.data)]
    join.columns = join.columns[
      join.columns %in% colnames(location.population.data)
    ]
    
    all.data = all.data %>%
      left_join(location.population.data,by=join.columns)
    names(all.data)[ grepl(pattern='\\.y$',names(all.data),perl=TRUE)] =
      gsub(
        '\\.y$',
        '.pop',
        names(all.data)[ grepl(pattern='\\.y$',names(all.data))]
      )
    
    names(all.data)[ grepl(pattern='\\.x$',names(all.data),perl=TRUE)] =
      gsub(
        '\\.x$',
        '',
        names(all.data)[ grepl(pattern='\\.x$',names(all.data))]
      )
  }
  
  if(nrow(location.description.files) > 0){
    location.description.data = apply(
      location.description.files,
      1,
      function(x){
        read_location_csv(x[[1]])
      }
    )
    ##This requires location descriptions to only have a single row
    location.description.data = bind_rows(location.description.data)
    ##locations = bind_rows(all.locations)
    location.description.data = location.description.data %>%
      bind_cols(unique.locations)
    location.description.data = bind_rows(location.description.data)
    ##Then we can bind everything together.
    join.columns = c("location");
    join.columns = join.columns[join.columns %in% colnames(all.data)]
    join.columns = join.columns[
      join.columns %in% colnames(location.description.data)
    ]
    ##all.data = all.data %>%
    ##  left_join(
    ##    location.description.data,by=join.columns,suffix=c('','.loc')
    ##  )
    all.data = all.data %>%
      left_join(location.description.data,by=join.columns)
    names(all.data)[ grepl(pattern='\\.y$',names(all.data),perl=TRUE)] =
      gsub(
        '\\.y$',
        '.loc',
        names(all.data)[ grepl(pattern='\\.y$',names(all.data))]
      )
    names(all.data)[ grepl(pattern='\\.x$',names(all.data),perl=TRUE)] =
      gsub(
        '\\.x$',
        '',
        names(all.data)[ grepl(pattern='\\.x$',names(all.data))]
      )
  }
  
  
  ##Only take the columns which have some amount of data in them.
  all.data = select(
    all.data,
    which(summarise_all(all.data,funs(sum(!is.na(.)))) > 0)
  )
  
  if((!missing(columns)) && (length(columns) > 0)){
    print(columns)
    try(
      return(select(all.data,one_of(columns))),silent=TRUE
    )
    try(
      return(select(all.data,starts_with(columns))),silent=TRUE
    )
    warning("Could not find the columns\n",immediate.=TRUE);
  }
  return(all.data)
}

################################################################
#' @name aggregate_taxonomy_data
#' @title aggregate_taxonomy_data
#' @export aggregate_taxonomy_data
#' @description This function groups data by certain fields and sums over places where those fields match.
##  args:
#' @param data a \code{tbl_dt(data.frame} containing the data.)
#' @param ISO_level spatial level to aggreate_to.  Either 0 for country level, a positive integer for ISO_A2_L?, 'official' to the nearest official shapefile, or Inf, for no aggregation
#' @param temporal_aggregate_time_unit Currently does nothing.  Intended to allow for considering observations at multiple time aggregates (or none)
#' @param aggregate_columns which columns to aggregate over.
#' @param time_combine If 'strict', require time periods to match end to end to make an observation.  Otherwise, just assume all grouped observations are the same.
#' @param max_overlap In order to shunt cases into one year instead of dividing them, how many days can be removed from a year.
#' @param min_total_length In order to shunt cases into one year instead of dividing them, how many days does an observation need to cover?
#' @param filter_NA_cases Whether or not to remove cases which are NA (as opposed to 0)
#' @return A \code{tbl_dt(data.frame} the aggregated data.)
################################################################
aggregate_taxonomy_data = function(
  data,
  ISO_level=2,
  temporal_aggregate_time_unit = 'year',
  aggregate_columns = 'sCh',
  observation=TRUE,
  min_total_length = 60,
  max_overlap = 8,
  filter_NA_cases = TRUE,
  time_combine = 'none'
){
  ##Notes:
  ####This function does not make observations out of time points quite correctly.  In the future we should do the following steps in order:
  # a - group the data by by uid and location
  # b - combine data at a particular uid/location into time intervals by combining adjacent intervals
  # c - ungroup by location, and now group by uid/time interval
  # d - combine data at a particular uid/time interval into unions of locations by grouping
  # e - track the percent area of the shape covered by the aggregation
  # f - Within each time interval, decide whether to collect that interval entirely into a particular time unit based on the length of the whole observation
  # g - Aggregate time as decided above, keeping track of the fraction of the year involved and adding cases appropriately.
  aggregate_to_start <- time_unit_to_start_function(temporal_aggregate_time_unit)
  aggregate_to_end <- time_unit_to_end_function(temporal_aggregate_time_unit)
  
  
  #Filter out NA case values
  if(filter_NA_cases){
    data <- data %>%
      #' @importFrom dplyr group_by
      group_by(uid) %>%
      #' @importFrom dplyr filter_
      filter_(
        .dots = paste('!is.na(',aggregate_columns,')')
      )
  }
  if(temporal_aggregate_time_unit == "None"){
    stop("Not yet written")
  }
  time_change_func = time_unit_to_aggregate_function(temporal_aggregate_time_unit)
  ## Aggregate by time:
  ###### For now, the grouping columns are all spatial columns, and 'uid'. (This will change once time aggregation is done)
  names(aggregate_columns) = NULL
  grouping_columns <- c('uid','who_region','ISO_A1',names(data)[grepl('^ISO_A2_L[1234567890]*$',names(data))])
  ###### Remove columns we don't want to deal with.  This could be changed into an option later. And should get moved to the end
  data <- data %>% select_(.dots = c('TL','TR',grouping_columns,aggregate_columns))
  #### Define ttL and ttR, the first and last time unit the observation covers.
  data %>%
    #' @importFrom dplyr group_by_
    group_by_(.dots=c('TL','TR',grouping_columns)) %>%
    #importFrom dplyr summarize_
    summarize_(.dots = setNames(
      paste("ifelse(all(is.na(",aggregate_columns,")),NA,sum(",aggregate_columns,",na.rm=T))"),
      aggregate_columns
    )) %>%
    ungroup %>%
    #' @importFrom dplyr group_by_
    group_by_(.dots=c('TL','TR',grouping_columns)) %>%
    mutate(
      ttL = time_change_func(TL),
      ttR = time_change_func(TR)
    )  %>%
    ungroup ->
    data
  #### Divide into single time unit and multiple time unit observations
  data_single_year = data %>% filter(ttL == ttR)
  data_multi_year = data %>% filter(ttL != ttR)
  #### Allocate the multiple time unit observations into one observation per time unit
  if(nrow(data_multi_year) > 0){
    data_multi_year %>%
      #' @importFrom dplyr group_by_
      group_by_(.dots = c('TL','TR','ttL','ttR',grouping_columns)) %>%
      do({
        # if(length(.$ttL) != 1){stop("Impossible")}
        # if(length(.$ttR) != 1){stop("Impossible")}
        if(any(is.na(c(.$ttL,.$ttR)))){
          browser()
        }
        if(length(.$ttL) > 1){ browser()}
        tmp <- data.frame(t = .$ttL:.$ttR)
        # if((nrow(tmp) > 1) & (!is.na(.$sCh)) &(any(.$sCh > 0))){browser()}
        tmp$TL = if_else(aggregate_to_start(tmp$t) > .$TL,aggregate_to_start(tmp$t),.$TL)
        tmp$TR = if_else(aggregate_to_end(tmp$t) < .$TR,aggregate_to_end(tmp$t),.$TR)
        if(!all(tmp$TL <= tmp$TR)){browser()}
        #### tfrac is the proportion of the this time unit this observation covers
        tmp$tfrac = (as.numeric((tmp$TR - tmp$TL),'days')+1)/(as.numeric((aggregate_to_end(tmp$t) - aggregate_to_start(tmp$t)),'days') + 1)
        #### tdur is the duration of the observation within the time unit
        tmp$tdur = (as.numeric((tmp$TR - tmp$TL),'days')+1)
        #### tprop is the proporition of the observation within this time unit
        tmp$tprop = (as.numeric((tmp$TR - tmp$TL),'days')+1)/(as.numeric((.$TR - .$TL),'days') + 1)
        ## This is to adjust so that single day periods get kept with the rest of their observation if that observation is at least a week long.
        ## First find the number of points where the first and last day are the same
        tmp$t = .$ttL
        tmp[[aggregate_columns]] = diff(c(0,round(cumsum(.[[aggregate_columns]] * tmp$tprop))))
        tmp$obs_TL = .$TL
        tmp$obs_TR = .$TR
        tmp
      }) %>%
      ungroup() %>%
      select_(.dots=c(grouping_columns,aggregate_columns,'t','TL','TR','obs_TL','obs_TR')) ->
      data_multi_year
  }
  
  #### Make the single time unit data have the same info as the multi time unit
  if(nrow(data_single_year) > 0){
    data_single_year %>%
      #' @importFrom dplyr group_by_
      mutate(
        t = ttL,
        obs_TL = TL,
        obs_TR = TR,
        tfrac = (as.numeric((TR - TL),'days')+1)/(as.numeric((aggregate_to_end(t) - aggregate_to_start(t)),'days') + 1),
        #### tdur is the duration of the observation within the time unit
        tdur = as.numeric(TR-TL,'days') + 1,
        #### tprop is the proporition of the observation within this time unit
        tprop = 1
      ) %>%
      ungroup() %>%
      select_(.dots=c(grouping_columns,aggregate_columns,'t','TL','TR','obs_TL','obs_TR')) ->
      data_single_year
  }
  
  #### Recombine
  #' @importFrom dplyr bind_rows
  data = bind_rows(data_single_year,data_multi_year)
  
  #### Group data together by time year
  ###### Note that this fails when the time units don't work out
  if(!filter_NA_cases){
    warning(
      "This does not work right now.  Building time units does not account for suspected vs deaths reports"
    )
  }
  
  if(time_combine == 'strict'){
    data %>%
      #' @importFrom dplyr ungroup
      ungroup() %>%
      #' @importFrom dplyr group_by_
      group_by_(.dots = grouping_columns) %>%
      do({
        changed = TRUE
        new = .
        new$tmp_obs_TL = new$obs_TL
        new$tmp_obs_TR = new$obs_TR
        iter = 0
        #### Connect greedily until no more reports are left to connect
        while(changed){
          old = new
          iter = iter + 1
          # print(iter)
          total_idx = length(which(new$obs_TL %in% (new$obs_TR + 1)))
          if(total_idx>0){
            for(idx1 in which(new$obs_TL %in% (new$TR + 1))){
              # print(paste(idx1,'/',total_idx))
              if(length(which(new$obs_TL[idx1] == (new$TR+1)) ) <= 0){stop("Bad")}
              idx2 = which(new$obs_TL[idx1] == (new$TR+1))
              new$tmp_obs_TL[idx1] = min(new$obs_TL[c(idx1,idx2)],new$tmp_obs_TL[idx1])
              new$tmp_obs_TL[idx2] = min(new$obs_TL[c(idx1,idx2)],new$tmp_obs_TL[idx2])
              new$tmp_obs_TR[idx2] = max(new$obs_TR[c(idx1,idx2)],new$tmp_obs_TR[idx2])
              new$tmp_obs_TR[idx1] = max(new$obs_TR[c(idx1,idx2)],new$tmp_obs_TR[idx1])
            }
          }
          total_idx = length(which(new$obs_TR %in% (new$TL - 1)))
          if(total_idx > 0){
            for(idx1 in which(new$obs_TR %in% (new$TL - 1))){
              # print(paste(idx1,'/',total_idx))
              if(length(which(new$obs_TR[idx1] == (new$TL-1)) ) <= 0){browser()}
              idx2 = which(new$obs_TR[idx1] == (new$TL - 1))
              # if(length(unique(new$tmp_obs_TL[idx2])) > 1){browser()}
              # if(length(unique(new$tmp_obs_TR[idx2])) > 1){browser()}
              new$tmp_obs_TL[idx1] = min(new$obs_TL[c(idx1,idx2)],new$tmp_obs_TL[idx1])
              new$tmp_obs_TL[idx2] = min(new$obs_TL[c(idx1,idx2)],new$tmp_obs_TL[idx2])
              new$tmp_obs_TR[idx2] = max(new$obs_TR[c(idx1,idx2)],new$tmp_obs_TR[idx2])
              new$tmp_obs_TR[idx1] = max(new$obs_TR[c(idx1,idx2)],new$tmp_obs_TR[idx1])
            }
            new$obs_TL = new$tmp_obs_TL
            new$obs_TR = new$tmp_obs_TR
          }
          if(isTRUE(all.equal(old,new))){
            changed = FALSE
          }
        }
        if(any(.$obs_TL < new$obs_TL)){
          browser()
        }
        new
      }) ->
      data
  } else if(time_combine == 'unstrict'){
    
    data %>%
      #' @importFrom dplyr ungroup
      ungroup() %>%
      #' @importFrom dplyr group_by_
      group_by_(.dots = grouping_columns) %>%
      mutate(
        obs_TL = min(TL),
        obs_TR = max(TR),
        TL = ymd(mapply(lhs = obs_TL, rhs = aggregate_to_start(t),function(lhs,rhs){paste(max(c(lhs,rhs)))})),
        TR = ymd(mapply(lhs = obs_TR, rhs = aggregate_to_end(t),function(lhs,rhs){paste(min(c(lhs,rhs)))})),
        obs_t = ymd(mapply(tl = obs_TL,tr = obs_TR,function(tl,tr){paste(mean(c(tl,tr)))})),
        tdur = TR-TL + 1,
        obs_tdur = obs_TR - obs_TL + 1
      ) %>%
      mutate(
        t = ifelse(
          (tdur < max_overlap) & (obs_tdur > min_total_length),
          t + (t < obs_t) - (t > obs_t),
          t
        )
      ) -> data
  } else if(time_combine == 'none') {
    
  } else {
    stop("time_combine must be one of 'strict', 'unstrict', or 'none'")
  }
  
  #### Now that we have time units properly done, t becomes a grouping column.
  grouping_columns = c(grouping_columns,'t')
  
  #### We now need to move partial time units to the rest of their report if they meet the criteria given by
  ####    max_overlap and min_total_length
  data %>%
    group_by_(.dots=c(grouping_columns,'obs_TL','obs_TR')) %>%
    summarize_(
      .dots = setNames(
        c('min(TL)','max(TR)',paste('sum(',aggregate_columns,')')),
        c("TL","TR",aggregate_columns)
      )
    ) %>%
    ungroup %>%
    group_by_(.dots = grouping_columns) %>%
    mutate(
      tdur = TR - TL + 1,
      obs_tdur = obs_TR - obs_TL + 1,
      #' @importFrom lubridate ymd
      obs_t = time_change_func(ymd(mapply(tl=obs_TL,tr=obs_TR,function(tl,tr){paste(mean(c(tl,tr)))})))
    ) -> data
  
  #### Do the adjustments only if the criterion are met
  data %>%
    ungroup() %>%
    mutate(
      t = ifelse(
        (obs_t == t) | (obs_tdur < min_total_length) | (tdur > max_overlap),
        t,
        ifelse(
          obs_t > t,
          t+1,
          t-1
        )
      )
    ) %>%
    group_by_(.dots=grouping_columns) %>%
    summarize_(.dots = setNames(
      paste("ifelse(all(is.na(",aggregate_columns,")),NA,sum(",aggregate_columns,",na.rm=T))"),
      aggregate_columns
    )) ->
    data
  
  ## Time aggregation is finished
  ## Starting Spatial Aggregation
  data$iso_level = apply(!is.na(data[,grepl('ISO_A',colnames(data))]),1,sum)
  
  #### Picking which spatial columns to aggregate on based on input:
  if(is.null(ISO_level)){
    grouping_columns = c('uid','t')
  } else if(ISO_level == 'oficial'){
    data %>%
      ungroup %>%
      mutate(
        ISO_A2_L1 = ifelse(
          (isISO_A2_L1 == 1) | grepl('|',location,fixed=TRUE) | !is.na(gis_file),
          ISO_A2_L1,
          NA
        ),
        ISO_A2_L2 = ifelse(
          (isISO_A2_L2 == 1) | grepl('|',location,fixed=TRUE) | !is.na(gis_file),
          ISO_A2_L2,
          NA
        ),
        ISO_A2_L3 = ifelse(
          (isISO_A2_L3 == 1) | grepl('|',location,fixed=TRUE) | !is.na(gis_file),
          ISO_A2_L3,
          NA
        ),
        ISO_A2_L4 = ifelse(
          (isISO_A2_L4 == 1) | grepl('|',location,fixed=TRUE) | !is.na(gis_file),
          ISO_A2_L4,
          NA
        ),
        ISO_A2_L5 = ifelse(
          (isISO_A2_L5 == 1) | grepl('|',location,fixed=TRUE) | !is.na(gis_file),
          ISO_A2_L5,
          NA
        )
      ) ->
      tmp
  } else if (ISO_level == 0){
    grouping_columns <- c('uid','t','who_region','ISO_A1')
  } else if(is.finite(ISO_level)){
    grouping_columns <- c('uid','t','who_region','ISO_A1',paste("ISO_A2_L",1:ISO_level,sep=''))
  } else {
    grouping_columns <- c('uid','t','who_region','ISO_A1',names(data)[grep('^ISO_A2_L[1234567890]*$',names(data))])
  }
  if(!all(grouping_columns %in% names(data))){
    warning("Not all grouping columns are present in case data")
    grouping_columns = grouping_columns[grouping_columns %in%  names(data)]
  }
  warning("In the process of modifying this function.")
  browser()
  data %>%
    group_by_(.dots=grouping_columns) %>%
    do({
      tmp = .
      tmp$iso_level = list(unique(.$iso_level))
      tmp$max_iso_level = min(c(max(.$iso_level),.$iso_level))
    })
  summarize(iso_level = list(unique(iso_level)))
  ## Final Aggregation
  #### We have everything done, so its just a group_by and a summarize from here
  data %>%
    ungroup() %>%    group_by_(.dots=grouping_columns) %>%
    summarize_(.dots = setNames(
      paste("ifelse(all(is.na(",aggregate_columns,")),NA,sum(",aggregate_columns,",na.rm=T))"),
      aggregate_columns
    )) ->
    rc
  return(rc)
}

#' @export
#' @name case_definition_to_column_name
#' @title case_definition_to_column_name
#' @description Turns human readable types of cholera case definitions into taxdat codes
#' @param type string of type
#' @param database Whether or not we're using the database
#' @return string of column names in the data taxonomy data frame.
case_definition_to_column_name = function(type,database=FALSE,sql=FALSE){
  if((!database) & (!sql)){
    warning("The svn column names are deprecated, please use database column names.")
    changer <- c(
      'suspected' = 'sCh',
      'confirmed' = 'cCh',
      "presence"=c("sCh","sCh_R","sCh_L","cCh","cCh_L","cCh_R","deaths","deaths_L","deaths_R")
    )
  } else if((database) & (!sql)){
    changer <- c(
      'suspected' = 'attributes.fields.suspected_cases',
      'confirmed' = 'attributes.fields.confirmed_cases',
      "presence"=c(
        "attributes.fields.suspected_cases",
        "attributes.fields.suspected_cases_R",
        "attributes.fields.suspected_cases_L",
        "attributes.fields.confirmed_cases",
        "attributes.fields.confirmed_cases_L",
        "attributes.fields.confirmed_cases_R",
        "attributes.fields.deaths",
        "attributes.fields.deaths_L",
        "attributes.fields.deaths_R"
      )
    )
  } else if((!database) & (sql)){
    changer <- c(
      "suspected" = "suspected_cases",
      "confirmed" = "confirmed_cases",
      "presence"=c(
        "suspected_cases",
        "suspected_cases_R",
        "suspected_cases_L",
        "confirmed_cases",
        "confirmed_cases_L",
        "confirmed_cases_R",
        "deaths",
        "deaths_L",
        "deaths_R"
      )
    )
  }
  return(changer[type])
}

#' @export
#' @name time_unit_to_start_function
#' @title time_unit_to_start_function
#' @description Turns human readable time units into a functions that convert time units to the start of the time unit
#' @param type string of type
#' @return function to convert dates to the right thing
time_unit_to_start_function <- function(unit){
  
  # Remove the 's' at the end of the unit 
  unit <- gsub("s$", "", unit)
  
  changer = list(
    'year' = function(x){
      return(as.Date(paste(x,'01','01',sep='-'),format='%Y-%m-%d'))
    },
    'isoweek' = function(x){return(stop("Not yet written"))}
  )
  return(changer[[unit]])
}

#' @export
#' @name time_unit_to_end_function
#' @title time_unit_to_end_function
#' @description Turns human readable time units into a functions that convert time units to the end of the time unit
#' @param type string of type
#' @return function to convert dates to the right thing
time_unit_to_end_function <- function(unit){
  
  # Remove the 's' at the end of the unit 
  unit <- gsub("s$", "", unit)
  
  changer = list(
    'year' = function(x){
      return(as.Date(paste(x,'12','31',sep='-'),format='%Y-%m-%d'))
    },
    'isoweek' = function(x){return(stop("Not yet written"))}
  )
  return(changer[[unit]])
}
#' @export
#' @name time_unit_to_aggregate_function
#' @title time_unit_to_aggregate_function
#' @description Turns the
#' @param unit Human readable unit of time aggregation
#' @return function to convert dates to the right thing
#' @importFrom lubridate year
time_unit_to_aggregate_function <- function(unit){
  
  # Remove the 's' at the end of the unit 
  unit <- gsub("s$", "", unit)
  
  changer = list(
    'year' = lubridate::year,
    'isoweek' = function(x){return(stop("Not yet written"))}
  )
  return(changer[[unit]])
}

## print("Part 1")
## Rprof()
## read_taxonomy_data()
## Rprof(NULL)
## print(summaryRprof()$sampling.time)
## print("Part 2")
## Rprof()
## read_taxonomy_data('taxonomy-verified',columns='deaths')
## Rprof(NULL)
## print(summaryRprof()$sampling.time)
## print("Part 3")
## Rprof()
## read_taxonomy_data('taxonomy-verified',columns='deaths.x', "who_region == 'AFR'")
## Rprof(NULL)
## print(summaryRprof()$sampling.time)
## print("Part 4")
## Rprof()
## read_taxonomy_data('taxonomy-verified',columns='deaths.x', "!(source == 'ProMED')","deaths == '1'")
## Rprof(NULL)
## print(summaryRprof()$sampling.time)
## print("Part 6")
## Rprof()
## read_taxonomy_data('taxonomy-verified',columns='ISO_A1','ISO_A1 %in% c("MWI","NGA")')
## Rprof(NULL)
## print(summaryRprof()$sampling.time)
## print("Part 7")
## Rprof()
## read_taxonomy_data('taxonomy-verified',columns='pygmy_shrew')
## Rprof(NULL)
## print(summaryRprof()$sampling.time)
## print("Part 8")
## Rprof()
## read_taxonomy_data('taxonomy-verified',columns=c("who_region","ISO_A1"))
## Rprof(NULL)
## print(summaryRprof()$sampling.time)
## Rprof()
## test = read_taxonomy_data('taxonomy-working/working-entry1/',columns = c('TL','TR','location','sCh','cCh'))
## Rprof(NULL)
## print(summaryRprof()$sampling.time)


## JSON API interface to database
#' @name read_taxonomy_data_database
#' @title read_taxonomy_data_database
#' @export read_taxonomy_data_database
#' @description This function accesses the cholera-taxonomy stored
#'   at https://staging.cholera-taxonomy.middle-distance.com pulls
#'   data based on function parameters, links it together, and
#'   transforms it into a simple features object (sf).
#' @param username The username for a user of the database
#' @param api_key A working api.key for the user of the database
#' @param locations A vector of locations to pull observations from (should be in the form who_region::ISO_L1::ISO_A2_...)
#' @param time_left First time for observations
#' @param time_right Last time for observations
#' @param uids unique observation collections ids to pull
#' @param website Which website to pull from (default is cholera-taxonomy.middle-distance.com)
#' @return An sf object containing data pulled from the database
read_taxonomy_data_database <- function(username, 
                                        api_key, 
                                        locations = NULL, 
                                        time_left = NULL, 
                                        time_right = NULL, 
                                        uids = NULL, 
                                        website = "https://api.cholera-taxonomy.middle-distance.com/"){
  ## Before we start, I want to explain some weird syntax that will come up:
  ## #' @importFrom package function
  ## The above line is the preferred way of importing a function from a package.
  ## It works in the context of R's autodocumenter (roxygen).
  ## It works similarly to the following more sensible code:
  ## toJSON = jsonlite::toJSON
  ## except that it handles conflicts better by producing a warning, and
  ## tells the package about the dependency.
  
  
  ## First, we want to set up the https POST request.
  ## We make a list containing the arguments for the request:
  ## If the API changes, we will just need to change this list
  api_type = ""
  if(is.null(uids)){
    api_type = "by_location"
    if(length(locations == 1)){
      locations = c(locations,locations)
    }
    
    ## Prevent continents, or too many countries
    if(any(!grepl('::',locations))){
      stop("Trying to pull data for a continent is not allowed")
    }
    #' @importFrom stringr str_count
    if((sum(str_count(string = unique(locations),pattern='::') == 1) > 2)){
      stop("Trying to pull data for more than 2 countries at a time is not allowed")
    }
    
    https_post_argument_list = list(
      email=username,
      api_key=api_key,
      locations=gsub('::',' ',locations),
      time_left=time_left,
      time_right=time_right
    )
  } else if(is.null(locations) && is.null(time_left) && is.null(time_right)){
    api_type = "by_observation_collections"
    https_post_argument_list = list(
      email=username,
      api_key=api_key,
      observation_collection_ids = uids
    )
  } else {
    stop("Not supported")
  }
  
  website = paste0(website,"/api/v1/observations/",api_type)
  
  #' @importFrom jsonlite toJSON
  ## Every object in R is a vector, even the primitives.  For example, c(1,5,6) is of type
  ## integer.  Because of this, we need to explicitly tell the JSON parser to treat vectors
  ## of length 1 differently.  The option for this is auto_unbox = T
  json = toJSON(https_post_argument_list,auto_unbox = T)
  #' @importFrom httr POST
  #' @importFrom httr add_headers
  ## Message prints a message to the user.  It's somewhere between a warning and a normal print.
  ## In this case, this function might take a while to run, so we let the user know up front.
  message("Fetching results from JSON API")
  
  ## This is the line that actually fetches the results.
  ## The syntax for adding headers is a little weird.  The function add_headers takes named arguments
  ## and returns whatever the arguments to POST are supposed to be.
  ## body is the body
  ## encode is the transformation to perform on the body to make it into text
  results = POST(
    website,
    add_headers("Content-Type" = "application/json"),
    body=json,
    encode='form'
  )
  
  ## Now we process the status code to make sure that things are working correctly
  #' @importFrom httr status_code
  code = status_code(results)
  ## Right now, anything that isn't correct is an error
  if(code != 200){
    stop(paste('Error: Status Code',code))
  }
  
  ## Next we extract just the content of the results
  #' @importFrom httr content
  original_results_data = content(results)
  ## This returns something correct, but the formatting is really
  ## odd.  It is a little messy, but instead of debugging the
  ## formatting, for now I'm converting to json and back, which
  ## fixes the problems.
  jsondata = rjson::toJSON(original_results_data)
  #' @importFrom jsonlite validate
  if(!validate(jsondata)){
    stop("Could not validate json response")
  }
  #' @importFrom jsonlite fromJSON
  results_data = fromJSON(jsondata)
  
  ## Now we have the results of the api data as a nested list.
  ## We want to do the following in no particular order
  ## for the observations, we want to turn them into a data frame
  ## with one row per observation for the location_periods, we want
  ## to turn them into a geometry object and link them to the
  ## observations
  
  ## We start with the observations
  if(                                                      # The | operator is logical or
    (!("observations" %in% names(results_data)))           | # The results should have observations
    (!("data" %in% names(results_data[['observations']]))) | # The observations should have data
    (length(results_data[['observations']]) > 1)             # The data should be the only thing in observations
  ){
    stop("Could not parse results properly.  Contact package maintainer")
  }
  results_data[['observations']] = results_data[['observations']][['data']]
  ## jsonlite's flatten
  #' @importFrom jsonlite flatten
  if(!is.data.frame(results_data[['observations']])){
    results_data[['observations']] = as.data.frame(results_data[['observations']])
  }
  results_data[['observations']] = flatten(results_data[['observations']])
  
  observation_collections_present <- FALSE
  if(
    ("observation_collections" %in% names(results_data))           && # The results should have observations
    ("data" %in% names(results_data[['observation_collections']])) && # The observations should have data
    (length(results_data[['observation_collections']]) == 1)          # The data should be the only thing in observations
  ){
    results_data[['observation_collections']] = results_data[['observation_collections']][['data']]
    if(!is.data.frame(results_data[['observation_collections']])){
      results_data[['observation_collections']] = as.data.frame(results_data[['observation_collections']])
    }
    results_data[['observation_collections']] = flatten(results_data[['observation_collections']])
    observation_collections_present <- TRUE
  }
  
  ## Check to make sure that the number of ids and number of rows match
  if(!length(unique(results_data$observations$id)) == nrow(results_data$observations)){
    stop("Could not parse results properly.  Contact package maintainer")
  }
  
  ## Now we want to handle the location periods
  ## We need to process these individually, so we'll loop over
  ## location periods to extract the geojsons
  ## We use the original_results_data here, since the formatting
  ## transformation we did earlier prevents this code from working
  tmp_results = original_results_data[['location_periods']][['data']]
  all_locations = list() # This will be a list of the geojson objects
  if(length(tmp_results) > 0){
    for(idx in 1:length(tmp_results)){
      ## We process the geojson in three pieces.
      ## 1. Convert to json string
      ## 2. Convert to sf object
      ## 3. Add to location list
      message(paste(idx,'/',length(tmp_results)))
      ## Ignore NULL elements.  Undefined list elements default to
      ## NULL anyway
      if(is.null(tmp_results[[idx]]$attributes$geojson)){
        all_locations[[idx]] = st_sf(geometry = st_sfc(st_point()))
        next;
      }
      unformatted_geojson = tmp_results[[idx]][['attributes']][['geojson']]
      json_geojson = jsonlite::toJSON(unformatted_geojson,auto_unbox = TRUE) # 1.
      sf_geojson = geojsonsf::geojson_sf(json_geojson) # 2.
      all_locations[[idx]] = sf_geojson #3.
    }
  }
  ## reduce_sf_vector turns a list of sf objects into a single sf
  ## object containing the same information
  locations_sf = taxdat::reduce_sf_vector(all_locations)
  ## We are going to take our properly formatted geojson files and
  ## replace the badly formatted ones
  results_data$location_periods$data$geojson = NULL
  results_data$location_periods$data$attributes$geojson = NULL
  if(!is.data.frame(results_data$location_periods$data)){
    results_data$location_periods$data <- as.data.frame(results_data$location_periods$data)
  }
  results_data$location_periods = flatten(results_data$location_periods$data)
  if(nrow(results_data$location_periods) > 0){
    results_data$location_periods$sf_id = 1:nrow(results_data$location_periods)
  }
  results_data$observations$attributes.location_period_id = as(results_data$observations$attributes.location_period_id,class(results_data$location_periods$id))
  
  ## We then join (as in sql) by the location_periods with the
  ## observations by location_period_id
  all_results <- results_data$observations
  if(
    observation_collections_present &&
    (nrow(all_results) > 0) &&
    (nrow(results_data$observation_collections) > 0)
  ){
    all_results <- dplyr::left_join(
      results_data$observations,
      results_data$observation_collections,
      by=c(
        'relationships.observation_collection.data.id' = 'id' # lhs column name = rhs column name
      )
    )
  }
  if(
    (nrow(all_results) > 0) &&
    (nrow(results_data$location_periods) > 0)
  ){
    all_results = dplyr::left_join(
      all_results,
      results_data$location_periods,
      by=c(
        'attributes.location_period_id' = 'id' # lhs column name = rhs column name
      )
    )
  }
  
  geoinput <- st_sf(geometry=st_sfc(st_point(1.*c(NA,NA))))$geometry
  if(nrow(all_results) == 0){
    geoinput <- geoinput[0]
  }
  all_results$geojson = geoinput
  all_results$geojson[!is.na(all_results$sf_id)] = locations_sf$geometry[all_results[!is.na(all_results$sf_id),][['sf_id']] ]
  return(sf::st_sf(all_results,sf_column_name = 'geojson'))
}

#' @title Pull taxonomy data
#' @description Pulls data from the taxonomy database
#'
#' @param username taxonomy username
#' @param api_key A working api.key for the user of the database
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#' @param website Which website to pull from (default is cholera-taxonomy.middle-distance.com)
#' @param source whether to pull data from the website or using sql on idmodeling2. 
#' Needs to be one of 'api' or 'sql'.
#' 
#' @details This is a wrapper which calls either read_taxonomy_data_database or
#' read_taxonomy_data_sql depending on the source that the user specifies.
#' @return An sf object containing data pulled from the database
#' @export
pull_taxonomy_data <- function(username,
                               password,
                               locations = NULL,
                               time_left = NULL,
                               time_right = NULL,
                               uids = NULL, 
                               website = "https://api.cholera-taxonomy.middle-distance.com/",
                               source) {
  
  if (missing(source) | is.null(source))
    stop("No source specified to pull taxonomy data, please specify one of 'api' or 'sql'.")
  
  if (source == 'api') {
    if (missing(username) | missing(password) | is.null(username) | is.null(password))
      stop("Trying to pull data from API, please provide username and api_key.")
    
    # Return API data pull
    rc <- read_taxonomy_data_database(username = username,
                                      api_key = password,
                                      locations = locations,
                                      time_left = time_left,
                                      time_right = time_right,
                                      uids = uids,
                                      website = website)
    
  } else if (source == 'sql') {
    if (missing(username) | missing(password) | is.null(username) | is.null(password))
      stop("Trying to pull data using sql on idemodelin2, please provide database username and password.")
    
    # Return SQL data pull
    rc <- read_taxonomy_data_sql(username = username,
                                 password = password,
                                 locations = locations,
                                 time_left = time_left,
                                 time_right = time_right,
                                 uids = uids)
    rc$attributes.fields.suspected_cases <- rc$suspected_cases
    rc$attributes.fields.confirmed_cases <- rc$confirmed_cases
    rc$attributes.fields.location_id <- rc$location_id
    rc$attributes.location_period_id <- rc$location_period_id
    
  } else {
    stop("Parameter 'source' needs to be one of 'api' or 'sql'.")
  }
  
  if(nrow(rc) == 0) {
    if (!is.null(uids)) {
      err_mssg <- paste("in uids", paste(uids, collapse = ","))
    } else if (!is.null(locations)) {
      err_mssg <- paste("in locations", paste(locations, collapse = ","))
    } else {
      err_mssg <- ""
    }
    stop("Didn't find any data ", err_mssg, " in time range [", 
         ifelse(is.null(time_left), "-Inf", as.character(time_left)), " - ",
         ifelse(is.null(time_right), "-nf", as.character(time_right)), "]")
  }
  return(rc)
}

#' @title Taxonomy SQL data pull
#' @description Extracts data for a given set of country using SQL from the taxonomy
#' postgresql database stored on idmodeling2
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#' 
#' @details Code follows taxdat::read_taxonomy_data_database template.
#' @return An sf object containing data extracted from the database
#' @export
read_taxonomy_data_sql <- function(username,
                                   password,
                                   locations = NULL,
                                   time_left = NULL,
                                   time_right = NULL,
                                   uids = NULL) {
  
  if (missing(username) | missing(password))
    stop("Please provide username and password to connect to the taxonomy database.")
  
  # Connect to database
  conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                               host = "db.cholera-taxonomy.middle-distance.com",
                               dbname = "CholeraTaxonomy_production",
                               user = username,
                               password = password,
                               port = "5432")
  
  # Build query for observations
  obs_query <- paste("SELECT observations.id::text, observations.observation_collection_id::text, observations.time_left, observations.time_right,", 
                     "observations.suspected_cases, observations.confirmed_cases, observations.deaths, observations.location_period_id::text, observations.location_id::text,",
                     "observations.phantom, observations.primary
                     FROM observations left join location_hierarchies on observations.location_id = location_hierarchies.descendant_id")
  
  cat("-- Pulling data from taxonomy database with SQL \n")
  
  # Add filters
  if (any(c(!is.null(locations), 
            !is.null(time_left),
            !is.null(time_right), 
            !is.null(uids)))) {
    obs_query <- paste(obs_query, "\n WHERE ")
  } else {
    warning("No filters specified on data pull, pulling all data.")
  }
  
  if (!is.null(time_left)) {
    time_left_filter <- paste0("time_left >= '", format(time_left, "%Y-%m-%d"), "'")
  } else {
    time_left_filter <- NULL
  }
  
  if (!is.null(time_right)) {
    time_right_filter <- paste0("time_right <= '", format(time_right, "%Y-%m-%d"), "'")
  } else {
    time_right_filter <- NULL
  }
  
  if (!is.null(locations)) {
    if(all(is.numeric(locations))){
      locations_filter <- paste0("ancestor_id in ({locations*})")
    } else {
      stop("SQL access by location name is not yet implemented")
    }
  } else {
    locations_filter <- NULL
  }
  
  if (!is.null(uids)) {
    uids_filter <- paste0("observation_collection_id IN ({uids*})")
  } else {
    uids_filter <- NULL
  }
  
  # Combine filters
  filters <- c(time_left_filter, time_right_filter, 
               locations_filter, uids_filter) %>% 
    paste(collapse = " AND ")
  
  # Run query for observations
  obs_query <- glue::glue_sql(paste(obs_query, filters, ";"), .con = conn)
  observations <- DBI::dbGetQuery(conn = conn, obs_query)
  if(nrow(observations) == 0){
    stop(paste0("No observations found using query ||",obs_query,"||"))
  }
  
  # Pull location_periods
  u_lps <- unique(observations$location_period_id)    # unique location period ids
  u_lps <- u_lps[!is.na(u_lps)]
  if(all(u_lps == as.numeric(u_lps))){
    u_lps <- as.numeric(u_lps)
  } else {
    stop("Location period id exceeds max integer in R, and glue doesn't work on int64s")
  }
  lp_query <- glue::glue_sql("SELECT id as location_period_id, geojson FROM location_periods
                             WHERE id IN ({u_lps*});", .con = conn)
  location_periods <- DBI::dbGetQuery(conn = conn, lp_query)
  
  # Get missing geometries
  location_period_issues <- location_periods %>%   
    filter(is.na(geojson) | geojson == "{}")
  
  # Get unique valid geojsons
  location_periods <- location_periods  %>%   
    filter(!is.na(geojson), geojson != "{}") %>% 
    group_by(location_period_id) %>% 
    slice(1)
  
  # Convert to sf object
  location_periods.sf <- purrr::map(location_periods$geojson, ~try(geojsonsf::geojson_sf(.), silent = F))
  
  # Get errors
  errors <- purrr::map2(location_periods.sf, seq_along(location_periods.sf), ~ if (inherits(.x, "try-error")) .y) %>% 
    unlist()
  if (length(errors) > 0) {
    cat("Found unreadable geojson for location periods:", str_c(errors, collapse = ", "))
    location_periods.sf <- location_periods.sf[-errors]
    location_periods <- location_periods[-errors, ]
  }
  
  # extract geometries and metadata
  location_periods.sf <- do.call(rbind, location_periods.sf) %>% 
    mutate(location_period_id = location_periods$location_period_id,
           location_name = purrr::map_chr(location_periods$geojson, ~ jsonlite::parse_json(.)[["name"]] %>% 
                                            ifelse(is.null(.), NA, .)),
           times = ifelse(is.na(location_name), NA, str_extract(location_name, "([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}-[0-9]{2}-[0-9]{2})")),
           location_name = ifelse(is.na(location_name), NA, str_replace_all(str_replace(location_name, str_c("_", times, "_SHP"), ""), "_", "::"))
    ) %>% 
    select(-times) %>% 
    rename(geojson = geometry)
  
  # Combine observations and geojsons
  res <- right_join(location_periods.sf, observations, by = "location_period_id")
  
  return(res)
}
