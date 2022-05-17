#' @include plot_cache_function.R
#' @export
#' @name plot_ObservationSummary_table
#' @title plot_ObservationSummary_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return table with observation statistic summary
plot_ObservationSummary_table <- function(config, cache, cholera_directory) {
  
  get_sf_cases_resized(
    name="sf_cases_resized",
    cache=cache,
    config = config,
    cholera_directory = cholera_directory)
  
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
  
obs_stats %>%
    dplyr::select(-dplyr::contains("u_")) %>%
    dplyr::mutate_if(is.numeric, function(x) {format(x , big.mark=",")}) %>%
    dplyr::rename(year=year, `Observations`=n_obs, `Suspected cases`=n_cases, `Location periods`=n_lp, `Observation collections`=n_OCs)%>%
    kableExtra::kable(col.names = c("year", "# observations", "# suspected cases", "# location periods", "# observation collections")) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = T) %>%
    kableExtra::row_spec(nrow(obs_stats), bold = T)

}