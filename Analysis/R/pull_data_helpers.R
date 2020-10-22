#' @title Rename cholera data columns
#' @description Renames the columns of the data pulled either from the the 
#' API staging database or by SQL from taxdat 
#'
#' @param database_df Data who's columns are to be modified
#' @param source Whether the source is the staging database (sing the API) or taxdat (using SQL).
#' @details source is one of 'api' or 'sql'
#' @return the renamed dataframe
#' @export
rename_database_fields <- function(database_df,
                                   source = "api") {
  
  if (source == "api") {
    new_database_df <- database_df %>%
      dplyr::rename(
        TL = attributes.time_left,
        TR = attributes.time_right,
        is_primary = attributes.primary,
        is_phantom = attributes.phantom,
        locationPeriod_id = attributes.id,
        OC_UID = relationships.observation_collection.data.id,
        location_name = attributes.location_name
      )
  } else if (source == "sql") {
    new_database_df <- database_df %>%
      dplyr::rename(
        TL = time_left,
        TR = time_right,
        is_primary = primary,
        is_phantom = phantom,
        locationPeriod_id = location_period_id,
        OC_UID = observation_collection_id,
        location_name = location_name
      )
  } else {
    stop("Source needs to be one of 'api', 'sql', found ", source)
  }
  # names(new_database_df) <- gsub("attributes.fields.", "", names(new_database_df))
  # names(new_database_df) <- gsub("attributes.", "", names(new_database_df))
  return(new_database_df)
}




