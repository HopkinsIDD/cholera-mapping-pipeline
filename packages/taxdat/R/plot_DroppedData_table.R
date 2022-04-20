#' @include plot_cache_function.R

#' @name get_stan_input_no_cache
#' @title get_stan_input_no_cache
#' @description load stan_input object based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return stan_input object
get_stan_input_no_cache <- function(config, cache, cholera_directory) {
  load(config[["file_names"]][["stan_input"]])
  require(bit64)
  require(sf)
  return(stan_input)
}
get_stan_input <- cache_fun_results("stan_input", get_stan_input_no_cache,overwrite = T,config=config)

#' @name get_sf_cases_resized_no_cache
#' @title get_sf_cases_resized_no_cache
#' @description load sf object (i.e.,sf_cases_resized) from stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return sf_cases_resized object
get_sf_cases_resized_no_cache <- function(config, cache, cholera_directory) {
  get_stan_input(name="stan_input",
                 cache=cache,
                 config = paste0(params$cholera_directory, params$config), 
                 cholera_directory = params$cholera_directory)
  return(cache[["stan_input"]]$sf_cases_resized)
}
get_sf_cases_resized <- cache_fun_results("sf_cases_resized", get_sf_cases_resized_no_cache,overwrite = T,config=config)

#' @name plot_DroppedData_table
#' @title plot_ObservationSummary_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return table with observation statistic summary
plot_DroppedData_table <- function(config, cache, cholera_directory) {
  get_stan_input(config, cache, cholera_directory)
  get_sf_cases_resized(config, cache, cholera_directory)
  get_preprocessed_data(config,cache,cholera_directory)
  get_sf_cases(config,cache,cholera_directory)
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
  
  used_obs_stats <- obs_stats[order(obs_stats$year), ]
  all_obs_stats <- taxdat::get_obs_stats(sf_cases)
  all_obs_stats <- all_obs_stats[order(all_obs_stats$year), ]
  dropped_obs_stats <- all_obs_stats %>% 
    mutate(n_obs = all_obs_stats$n_obs - used_obs_stats$n_obs) %>% 
    mutate(n_cases = all_obs_stats$n_cases - used_obs_stats$n_cases) %>% 
    mutate(n_lp = all_obs_stats$n_lp - used_obs_stats$n_lp) %>% 
    mutate(n_OCs = all_obs_stats$n_OCs - used_obs_stats$n_OCs)
  for (i in 1:nrow(all_obs_stats)){
    dropped_obs_stats$u_lps[i] <- paste(sort(unique(setdiff(stringr::str_split(all_obs_stats$u_lps[i], ',')[[1]], 
                                                            stringr::str_split(used_obs_stats$u_lps[i], ',')[[1]]))), collapse = ",")
    dropped_obs_stats$u_OCs[i] <- paste(sort(unique(setdiff(stringr::str_split(all_obs_stats$u_OCs[i], ',')[[1]], 
                                                            stringr::str_split(used_obs_stats$u_OCs[i], ',')[[1]]))), collapse = ",")
  }
  Dropped_obs_table <- dropped_obs_stats %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
    dplyr::rename(year=year, `Number of dropped observations`=n_obs, `Number of dropped suspected cases`=n_cases, `Number of dropped location periods`=n_lp, `Number of dropped  observation collections`=n_OCs)
 
  return(Dropped_obs_table)
}