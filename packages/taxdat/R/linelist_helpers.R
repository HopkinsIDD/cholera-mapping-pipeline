#' @export
#' @name linelist2phantom
#' @title linelist2phantom
#' @description Takes a line ist and adds daily primary phantom observations for locations and dates not present in the linelist
#' @param linelist A linelist.  The following columns are required:
#'   - location
#'   - TL 
#'   - TR
#'   - at least one of 
#'     - sCh (suspected cases)
#'     - cCh (confirmed cases)
#'     - deaths
#' @param assumed_complete_location The location below which the data is assumed complete (e.g., "AFR::ETH" for a linelist representing the whole country of Ethiopia)
#' @param assumed_complete_TL The starting time for the linelist data (defaults to the first time reported in the linelist)
#' @param assumed_complete_TR The ending time for the linelist data (defaults to the first time reported)
#' @param location_columns The names of one or more columns containing locations (e.g., "Location" or c("ISO_A1", "ISO_A2_L1")). If only one column is given, delimit location levels by '::'
#' @param case_columns One or more columns containing case values across which to accumulate. Defaults to sCh, cCh, and deaths.
#' @return A data_frame with the following columns
#'  - TL
#'  - TR
#'  - case_column
#'  - each of the location_columns
#'  - each of the confirmed_cases
linelist2phantom <- function(
  linelist,
  assumed_complete_location,
  assumed_complete_TL,
  assumed_complete_TR,
  location_columns = "Location",
  case_columns = c('sCh','cCh', 'deaths')
){
  if(any(sapply(linelist,class) == c('factor'))){
    badness <- which(sapply(linelist,class) == 'factor')
    for(bad in badness){
      linelist[[bad]] <- as.character(linelist[[bad]])
    }
  }
  ## Check names
  names2check <- c('TL', 'TR')
  missing_names <- names2check[!names2check %in% names(linelist)]
  if (length(missing_names) > 0){
    stop(paste("linelist requires columns named",paste(missing_names,collapse = ", ")))
  }
  if(!any(location_columns %in% names(linelist))){
    stop(paste("At least one of ",paste(location_columns, sep=", "),"is required"))
  }
  location_columns <- location_columns[location_columns %in% names(linelist)]
  for(col in location_columns){
    linelist[[col]] <- as.character(linelist[[col]])
  }
  if(!any(case_columns %in% names(linelist))){
    stop(paste("At least one of ",paste(case_columns,sep=", "),"is required"))
  }
  case_columns <- case_columns[case_columns %in% names(linelist)]
  if(class(linelist$TL) == 'character'){
    linelist$TL = lubridate::ymd(linelist$TL)
  }
  if(class(linelist$TR) == 'character'){
    linelist$TR = lubridate::ymd(linelist$TR)
  }
  if(any(linelist$TL != linelist$TR)){
    stop("Linelist data should have the same TL and TR")
  }
  linelist <- tidyr::unite(linelist,"Location",!!location_columns,sep='::',na.rm=TRUE)
  original_linelist <- linelist
  linelist <- dplyr::select(linelist,Location,!!names2check,!!case_columns)
  test <- dplyr::bind_rows(dplyr::group_map(
    dplyr::group_by(
      linelist,
      Location,
      TL,
      TR
    ),
    .f = expand_all_locations,
    assumed_complete_location = assumed_complete_location
  ))
  ## time range
  if(missing(assumed_complete_TL)){
    assumed_complete_TL <- min(linelist$TL)
  }
  if(missing(assumed_complete_TR)){
    assumed_complete_TR <- max(linelist$TR)
  }
  # last_time <- max(linelist$TR)
  # Get range of all observed times
  time_range <- assumed_complete_TL + lubridate::days(0:as.numeric(lubridate::day(lubridate::days(assumed_complete_TR - assumed_complete_TL))))

  # Compute sums by location TL and TR
  test <- dplyr::ungroup(
    dplyr::summarize_all(
      dplyr::group_by(test,TL,TR,Location,lowest_level),
      list(~ sum(.,na.rm=T))
    )
  )
  
  # Create combination of all locations and times
  all_combinations <- tidyr::expand(
    test, 
    TL = time_range,
    tidyr::nesting(Location,lowest_level)
    )
  all_combinations <- dplyr::mutate(
    all_combinations,
    TR = TL
    )
  
  # Expand to cover all times for all locations
  test <- dplyr::full_join(test,
    all_combinations
  )

  # Fill in 0s for all locations for which no linelist information was available
  # at a given time
  for(cc in case_columns){
    cc <- rlang::sym(cc)
    test <- dplyr::mutate(test,!!cc := ifelse(is.na(!!cc),0,!!cc))
  }
  test <- dplyr::summarize_at(
    dplyr::group_by(
      dplyr::ungroup(test),
      Location,
      TL,
      TR
    ),
    .vars = case_columns,
    .funs = list(~sum(.))
  )
  test <- dplyr::select(test,Location,!!names2check,!!case_columns)
  test$Primary = TRUE
  original_linelist$Primary = FALSE
  test$Phantom = TRUE
  original_linelist$Phantom = FALSE
  if(length(location_columns) > 1){
    linelist <- dplyr::bind_rows(original_linelist,test)
    linelist <- tidyr::separate(linelist, "Location",location_columns,sep='::')
  } else {
    linelist <- dplyr::bind_rows(original_linelist,test)
    linelist[[location_columns]] <- linelist$Location
    if(!(location_columns == "Location")){
      linelist$Location <- NULL
    }
  }
  return(linelist)
}

#' @name na_smart_sum
#' @title na_smart_sum
#' @description Helper function to sum vectors with NA values correctly
#' @param x vector to sum
#' @return
na_smart_sum <- function(x){
  if(all(is.na(x))){
    return(as.numeric(NA))
  } else {
    return(sum(x, na.rm = TRUE))
  }
}

#' @name expand_all_locations
#' @title expand_all_locations
#' @description Helper function to get full list of locations
#' @param .x dataframe for group_map
#' @param .y groups for group_map
#' @param assumed_complete_location string with assumed complete location
#' @param all_locations list of unique locations
#' @return
expand_all_locations = function(.x, .y, assumed_complete_location, all_locations){
  # Compute some of all data columns for specific combination of location, TL and TR
  .x <- dplyr::summarize_all(.x, na_smart_sum)
  # Get all the names of all location levels in location period
  all_location <- strsplit(.y$Location, "::")[[1]]
  # Create consistent location hierarchy
  for(i in length(all_location):1){
    all_location[i] <- paste(all_location[1:i],collapse='::')
  }
  # Keep only locations that are either the assumed_complete_location or locations 
  # children of the latter 
  all_location <- all_location[grepl(assumed_complete_location,all_location)]
  if(length(all_location) > 0){
    # Create full dataframe of the observations at all location levels
    .x <- dplyr::bind_rows(
    lapply(
      1:length(all_location),
      function(x){
        .x
      }))
    # Fill in location information
    .x$Location <- all_location
    .x$lowest_level <- FALSE
    .x$lowest_level[length(all_location)] <- TRUE
    .x$TL <- .y$TL
    .x$TR <- .y$TR
  } else {
    .x$Location <- all_location
    .x$lowest_level <- FALSE
    .x$lowest_level[length(all_location)] <- TRUE
    .x <- .x[.x, ]
  }
  return(.x)
}
