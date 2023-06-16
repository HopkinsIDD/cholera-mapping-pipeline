#' @include plot_cache_function.R

#' @export
#' @name get_genquant_no_cache
#' @description load stan output
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return genquant
get_genquant_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory, "/", config))
  file_names <- taxdat::get_filenames(config, cholera_directory)
  genquant <- taxdat::read_file_of_type(file_names[["stan_genquant"]], "chol_gen")
  require(bit64)
  require(sf)
  return(genquant)
}
# cache the results
#' @export
#' @name get_genquant
get_genquant <- cache_fun_results(
  name = "genquant", fun = get_genquant_no_cache,
  overwrite = F, config = config
)

#' @export
#' @name get_genquant_summarized_cases_no_cache
#' @title get_genquant_summarized_cases_no_cache
#' @description loaded mondeled mean annual cases from genquant output
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param admin_level  select admin 0,1,2 or all
#' @return the summarized cases
get_genquant_summarized_cases_no_cache <- function(config,
cache,
admin_level = c(0, 1, 2)) {
    if (!all(admin_level %in% c(0, 1, 2))) {
        stop("admin_level must in 0, 1, 2.")
    }
  cases <- cache[["genquant"]]$summary(
    variables = "location_cases_output",
    c(
      posterior::default_summary_measures(),
      posterior::default_convergence_measures()
    ),
    .cores = 4
  ) %>%
    mutate(
      id = str_extract(variable, "[0-9]+") %>% as.numeric(),
      location_period_id = cache[["stan_input"]]$fake_output_obs$locationPeriod_id[id],
      TL = cache[["stan_input"]]$fake_output_obs$TL[id]
    )
    return(cases)
}
# cache the results
#' @export
#' @name get_genquant_summarized_cases
get_genquant_summarized_cases <- cache_fun_results(
    "genquant_summarized_cases",
    get_genquant_summarized_cases_no_cache,
    overwrite = FALSE,
    config = config,
)
