#' @include plot_cache_function.R
#' @export
#' @name plot_DroppedData_table
#' @title plot_ObservationSummary_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param aesthetic whether to use the kableExtra package to return a kable object
#' @return table with observation statistic summary
plot_DroppedData_table <- function(config, cache, cholera_directory, aesthetic = TRUE) {
  get_sf_cases_resized(
    name="sf_cases_resized",
    cache=cache,
    config = config,
    cholera_directory =cholera_directory)

  obs_stats <- tibble::as_tibble(cache[["sf_cases_resized"]])
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
  #get all obs_stats
  get_stan_input(name="stan_input",config=config, cache=cache, cholera_directory=cholera_directory)

  all_obs_stats <- tibble::as_tibble(cache[["sf_cases"]])
  all_obs_stats <- dplyr::mutate(all_obs_stats, year = lubridate::year(TL))
  alldf <- tibble::as_tibble(all_obs_stats)
  alldf <- dplyr::mutate(alldf, year = "all")
  all_obs_stats <- rbind(all_obs_stats, alldf)
  all_obs_stats <- dplyr::group_by(all_obs_stats, year)
  all_obs_stats <- dplyr::summarize(all_obs_stats,
                                n_obs = dplyr::n(),
                                n_cases = sum(attributes.fields.suspected_cases),
                                n_lp  = length(unique(locationPeriod_id)),
                                u_lps  = paste(sort(unique(locationPeriod_id)), collapse = ","),
                                n_OCs  = length(unique(OC_UID)),
                                u_OCs  = paste(sort(unique(OC_UID)), collapse = ",")
  )
  
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
  
  if(aesthetic){
    dropped_obs_stats %>%
      dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
      dplyr::rename(year=year, `Number of dropped observations`=n_obs, `Number of dropped suspected cases`=n_cases, `Number of dropped location periods`=n_lp, `Number of dropped  observation collections`=n_OCs) %>%
      kableExtra::kable(col.names = c("year", "# dropped observations", "# dropped suspected cases", "# dropped location periods",  "dropped location periods", "# dropped  observation collections",  "dropped observation collections")) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
      kableExtra::kable_paper(full_width = F) %>%
      kableExtra::row_spec(nrow(dropped_obs_stats), bold = T)
  }else{
    dropped_obs_stats 
  }
  
}
