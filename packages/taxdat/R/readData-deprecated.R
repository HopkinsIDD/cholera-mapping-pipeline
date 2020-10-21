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
  .Deprecated(msg = "read_taxonomy_csv is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_taxonomy_csv")
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
  .Deprecated(msg = "read_transposed_taxonomy_csv is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_transposed_taxonomy_csv")
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
  .Deprecated(msg = "safe.function is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "safe.function")
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
  .Deprecated(msg = "read_description_csv is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_description_csv")
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
  .Deprecated(msg = "read_epi_csv is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_epi_csv")
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
  .Deprecated(msg = "read_location_csv is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_location_csv")
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
  .Deprecated(msg = "read_population_csv is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_population_csv")
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
  .Deprecated(msg = "filter_description_data is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "filter_description_data")
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
  .Deprecated(msg = "read_description_taxonomy is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_description_taxonomy")
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
  .Deprecated(msg = "read_epi_taxonomy is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_epi_taxonomy")
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
  .Deprecated(msg = "read_taxonomy_data is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "read_taxonomy_data")
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
  .Deprecated(msg = "aggregate_taxonomy_data is deprecated because it is no longer used by the cholera mapping pipeline.", package = "taxdat", old = "aggregate_taxonomy_data")
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

