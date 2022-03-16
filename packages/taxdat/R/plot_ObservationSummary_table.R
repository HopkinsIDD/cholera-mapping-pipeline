#' @include plot_cache_function.R

#' @name get_sf_cases_resized_no_cache
#' @description load stan output
#' @param config 
#' @param cache 
#' @return covar cube
get_sf_cases_resized_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config_filename)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  sf_cases_resized <- taxdat::read_file_of_type(file_names[["stan_input"]], "sf_cases_resized")
  require(bit64)
  require(sf)
  return(sf_cases_resized)
}
# cache the results
get_sf_cases_resized <- cache_fun_results(name = "sf_cases_resized", fun = get_sf_cases_resized_no_cache,
                                          overwrite = T, config = config)

#' @name plot_ObservationSummary_table
#' @title plot_ObservationSummary_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return table with observation statistic summary
plot_ObservationSummary_table <- function(config, cache, cholera_directory) {
  
  get_sf_cases_resized(config, cache, cholera_directory)
  
  obs_stats <- tibble::as_tibble(sf_cases_resized)
  obs_stats <- dplyr::mutate(obs_stats, year = lubridate::year(TL))
  alldf <- tibble::as_tibble(obs_stats)
  alldf <- dplyr::mutate(alldf, year = "all")
  obs_stats <- rbind(obs_stats, alldf)
  obs_stats <- dplyr::group_by(obs_stats, year)
  obs_stats <- dplyr::summarize(obs_stats,
                         n_obs = dplyr::n(),
                         n_cases = sum(attributes.fields.suspected_cases),
                         n_lp  = length(unique(locationPeriod_id)),
                         u_lps  = paste(sort(unique(locationPeriod_id)), collapse = ","),
                         n_OCs  = length(unique(OC_UID)),
                         u_OCs  = paste(sort(unique(OC_UID)), collapse = ",")
  )
  
  ObservationSummary_table <- obs_stats %>%
    dplyr::select(-dplyr::contains("u_")) %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
    dplyr::rename(year=year, `Observations`=n_obs, `Suspected cases`=n_cases, `Location periods`=n_lp, `Observation collections`=n_OCs)
  
  return(ObservationSummary_table)
  
}