#' @export
#' @name get_country_shp_from_stan_input
#' @title get_country_shp_from_stan_input
#' @description get_country_shp_from_stan_input
#' @param config the pathway to the config file
#' @param cache the environment variable cache
#' @param cholera_directory the cholera mapping directory 
#' @return the country-level shape files from the stan input 
get_country_shp_from_stan_input <- function(config, cache, cholera_directory){
  # First get the locationPeriod_id associated with each WHO observation 
  get_sf_cases_resized(name="sf_cases_resized",config=config, cache=cache, cholera_directory=cholera_directory)
  who_annual_cases <- cache[["sf_cases_resized"]]
  who_annual_cases_from_db <- NULL
  who_annual_cases_from_db <- taxdat::pull_output_by_source(who_annual_cases, "%WHO Annual Cholera Reports%",
                                                            database_api_key_rfile = stringr::str_c(cholera_directory, "/Analysis/R/database_api_key.R"))
  WHO_locationPeriod_id <- who_annual_cases_from_db$locationPeriod_id
  WHO_loctime <- who_annual_cases_from_db$loctime
  WHO_OC_UID <- who_annual_cases_from_db$OC_UID

  # Get the year of analysis
  config_file <- yaml::read_yaml(paste0(cholera_directory, "/", config))
  analysis_years <- lubridate::year(config_file$start_time):lubridate::year(config_file$end_time)

  # Then get the shape files for each locationPeriod_id from the stan input 
  shp_from_stan_input <- cache[["sf_cases_resized"]] %>%
    filter( locationPeriod_id %in% WHO_locationPeriod_id &
            loctime %in% WHO_loctime &
            OC_UID %in% WHO_OC_UID) %>%
    mutate(t = match(lubridate::year(TL), analysis_years)) %>%
    dplyr::select(t)

  # Warning if the number of shape files is different from the number of analysis years
  if(nrow(shp_from_stan_input) != length(analysis_years)){warning("The country-level shape files are partially (not for all the years of analysis) retrieved from the stan input. ")}

  return(shp_from_stan_input)
}
