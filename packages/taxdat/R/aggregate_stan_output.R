#' @include plot_cache_function.R
#' @include get_stan_output.R
#' @include get_genquant.R

#' @export
#' @name get_modeled_cases_no_cache
#' @title get_modeled_cases_no_cache
#' @description extract modeled cases from model.rand
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return  modeled_cases
get_modeled_cases_no_cache <- function(config, cache, cholera_directory, ...) {
  get_genquant(name="genquant",cache=cache,config=config,cholera_directory = cholera_directory)
  varnames <- dimnames(cache[['genquant']]$draws())[[3]]
  modeled_cases <- as.array(cache[['genquant']]$draws())[, , grepl("grid_case", varnames),drop=FALSE]
  return(modeled_cases)
}
# cache the results
#' @export
#' @name get_modeled_cases
get_modeled_cases <- cache_fun_results("modeled_cases", get_modeled_cases_no_cache,
                                       overwrite = F, config = config)

#' @export
#' @name get_modeled_observed_cases_no_cache
#' @title get_modeled_observed_cases_no_cache
#' @description extrac modeled cases from model.rand
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return  modeled_cases
get_modeled_observed_cases_no_cache <- function(config, cache, cholera_directory, ...) {
  get_genquant(name="genquant",cache=cache,config=config,cholera_directory = cholera_directory)
  varnames <- dimnames(cache[['genquant']]$draws())[[3]]
  modeled_observed_cases <- as.array(cache[['genquant']]$draws())[, , grepl("modeled_cases", varnames),drop=FALSE]
  return(modeled_observed_cases)
}
# cache the results
#' @export
#' @name get_modeled_observed_cases
get_modeled_observed_cases <- cache_fun_results("modeled_observed_cases", get_modeled_observed_cases_no_cache,
                                       overwrite = F, config = config)

#' @export
#' @name get_modeled_rates_no_cache
#' @title get_modeled_rates_no_cache
#' @description extrac modeled rates (from log lambda) from model.rand
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return  modeled_rates
get_modeled_rates_no_cache <- function(config, cache, cholera_directory, ...) {
  get_genquant(name="genquant",cache=cache,config=config,cholera_directory = cholera_directory)
  varnames <- dimnames(cache[['genquant']]$draws())[[3]]
  modeled_rates <- exp(as.array(cache[['genquant']]$draws())[, , grepl("log_lambda", varnames),drop=FALSE])
  return(modeled_rates)
}
# cache the results
#' @export
#' @name get_modeled_rates
get_modeled_rates <- cache_fun_results("modeled_rates", get_modeled_rates_no_cache,
                                       overwrite = F, config = config)

#' @export
#' @name aggregate_modeled_cases_by_chain_no_cache
#' @title aggregate_modeled_cases_by_chain_no_cache
#' @description get the mean of the modeled cases by chain
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return  modeled cases mean for each grid cell by times
aggregate_modeled_cases_by_chain_no_cache <- function(config,cholera_directory,cache,funs = "mean") {
  get_modeled_cases(name="modeled_cases",
                    cache=cache,
                    config = config,
                    cholera_directory = cholera_directory
  )
  modeled_cases_by_chain <- apply(cache[["modeled_cases"]], 2, funs)

  return(modeled_cases_by_chain)
}
# cache the results
#' @export
#' @name aggregate_modeled_cases_by_chain
aggregate_modeled_cases_by_chain <- cache_fun_results(name="modeled_cases_by_chain", 
                                                      aggregate_modeled_cases_by_chain_no_cache,
                                                      overwrite = F, 
                                                      config = config,
                                                      cholera_directory=cholera_directory)

#' @export
#' @name aggregate_modeled_rates_by_chain_no_cache
#' @title aggregate_modeled_rates_by_chain_no_cache
#' @description get the mean of the modeled rates by chain
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return  modeled rates mean for each grid cell by times
aggregate_modeled_rates_by_chain_no_cache <- function(modeled_rates, funs = "mean") {
  modeled_rates_by_chain <- apply(modeled_rates, 2, funs)
  return(modeled_rates_by_chain)
}
# cache the results
#' @export
#' @name aggregate_modeled_rates_by_chain
aggregate_modeled_rates_by_chain <- cache_fun_results("modeled_rates_by_chain", aggregate_modeled_rates_by_chain_no_cache,
                                                      overwrite = F, config = config)

#' @export
#' @name aggregate_modeled_cases_by_chain_gridtime_no_cache
#' @description aggregate the modeled cases by chain 
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param funs the function to aggregate the modeled cases
#' @return modeled cases across iterations by chains and grid*time
aggregate_modeled_cases_by_chain_gridtime_no_cache <- function(config,cholera_directory,cache,funs = "mean"){
  get_modeled_cases(name="modeled_cases",
                    cache=cache,
                    config = config,
                    cholera_directory = cholera_directory
  )
  
  aggregated_modeled_cases_by_chain_gridtime <-  apply(cache[["modeled_cases"]], c(2,3), funs)
  
  return(aggregated_modeled_cases_by_chain_gridtime)
  }
# cache the results
#' @export
#' @name aggregate_modeled_cases_by_chain_gridtime
aggregate_modeled_cases_by_chain_gridtime <- cache_fun_results("aggregated_modeled_cases_by_chain_gridtime",
                                                               aggregate_modeled_cases_by_chain_gridtime_no_cache,
                                                               overwrite=F,
                                                               config=params$config)


#' @export
#' @name aggregate_modeled_observed_cases_by_chain_gridtime_no_cache
#' @description aggregate the modeled cases by chain
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param funs the function to aggregate the modeled cases
#' @return modeled cases across iterations by chains and grid*time
aggregate_modeled_observed_cases_by_chain_gridtime_no_cache <- function(config,cholera_directory,cache,funs = "mean"){
  get_modeled_observed_cases(name="modeled_observed_cases",
                    cache=cache,
                    config = config,
                    cholera_directory = cholera_directory
  )
  
  aggregated_modeled_observed_cases_by_chain_gridtime <-  apply(cache[["modeled_observed_cases"]], c(2,3), funs)
  
  return(aggregated_modeled_observed_cases_by_chain_gridtime)
}
# cache the results
#' @export
#' @name aggregate_modeled_observed_cases_by_chain_gridtime
aggregate_modeled_observed_cases_by_chain_gridtime <- cache_fun_results("aggregated_modeled_observed_cases_by_chain_gridtime",
                                                               aggregate_modeled_observed_cases_by_chain_gridtime_no_cache,
                                                               overwrite=F,
                                                               config=params$config)


#' @export
#' @name aggregate_modeled_rates_by_gridtime_no_cache
#' @description aggregate the modeled rates for each grid cell
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param funs the function to aggregate the modeled rates
#' @return modeled rates mean across iterations by chains and grid*time
aggregate_modeled_rates_by_chain_gridtime_no_cache <- function(modeled_rates, funs = "mean"){
  aggregated_modeled_rates_by_chain_gridtime <- apply(modeled_rates, c(2,3), funs)
  return(aggregated_modeled_rates_by_chain_gridtime)
}

# cache the results
#' @export
#' @name aggregate_modeled_rates_by_chain_gridtime
aggregate_modeled_rates_by_chain_gridtime <- cache_fun_results("aggregated_modeled_rates_by_chain_gridtime",aggregate_modeled_rates_by_chain_gridtime_no_cache,
                                                               overwrite=F,config=config,cholera_directory = cholera_directory)

#' @export
#' @name aggregate_modeled_cases_by_gridtime_no_cache
#' @description aggregate the modeled cases by grid*time
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param funs the function to aggregate the modeled cases
#' @return modeled cases mean across chains and iterations by grid*timee
aggregate_modeled_cases_by_gridtime_no_cache <- function(cache,config,cholera_directory, funs = "mean"){
  get_modeled_cases(name="modeled_cases",
                    cache=cache,
                    config = config,
                    cholera_directory = cholera_directory
  )
  
  aggregated_modeled_cases_by_gridtime <- apply(cache[["modeled_cases"]], c(3), funs)
  return(aggregated_modeled_cases_by_gridtime)
}

# cache the results
#' @export
#' @name aggregate_modeled_cases_by_gridtime
aggregate_modeled_cases_by_gridtime <- cache_fun_results("aggregated_modeled_cases_by_gridtime",aggregate_modeled_cases_by_gridtime_no_cache,
                                                               overwrite=F,config=config)

#' @export
#' @name aggregate_modeled_rates_by_gridtime_no_cache
#' @description aggregate the modeled rates by grid*time
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param funs the function to aggregate the modeled rates
#' @return modeled rates mean across chains and iterations by grid*timee
aggregate_modeled_rates_by_gridtime_no_cache <- function(cache,config,cholera_directory, funs = "mean"){
  get_modeled_rates(name="modeled_rates",
                    cache=cache,
                    config = config,
                    cholera_directory = cholera_directory
  )
  
  aggregated_modeled_rates_by_gridtime <- apply(cache[["modeled_rates"]], c(3), funs)
  return(aggregated_modeled_rates_by_gridtime)
}

# cache the results
#' @export
#' @name aggregate_modeled_rates_by_gridtime
aggregate_modeled_rates_by_gridtime <- cache_fun_results("aggregated_modeled_rates_by_gridtime",aggregate_modeled_rates_by_gridtime_no_cache,
                                                         overwrite=F,config=config)

#' @export
#' @name aggregate_averaged_modeled_cases_by_time_across_country_no_cache
#' @description aggregate the averaged modeled cases across chains and iterations by grid*time over the country
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param average_funs the function to get the averaged modeled cases across chains and iterations
#' @return modeled cases mean across chains and iterations by grid*timee over the country
aggregate_averaged_modeled_cases_by_time_across_country_no_cache <- function(config, cache,cholera_directory, average_funs = "mean"){
  get_modeled_cases(name="modeled_cases",
                    cache=cache,
                    config = config,
                    cholera_directory = cholera_directory
  )
  
  aggregated_modeled_cases_by_gridtime <- apply(cache[["modeled_cases"]], c(3), average_funs)
  analysis_years <- lubridate::year(config$start_time):lubridate::year(config$end_time)

  aggregated_averaged_modeled_cases_by_time_across_country<-
    data.frame(aggregated_modeled_cases_by_gridtime)%>%
    dplyr::mutate(analysis_years = rep(analysis_years,each=length(aggregated_modeled_cases_by_gridtime)/length(analysis_years)))%>%
    dplyr::group_by(analysis_years)%>%
    dplyr::summarise(total_cases_by_year_over_country = round(sum(aggregated_modeled_cases_by_gridtime),0))
  
  return(aggregated_averaged_modeled_cases_by_time_across_country)
}

# cache the results
#' @export
#' @name aggregate_averaged_modeled_cases_by_time_across_country
aggregate_averaged_modeled_cases_by_time_across_country <- cache_fun_results("aggregated_averaged_modeled_cases_by_time_across_country",aggregate_modeled_cases_by_gridtime_no_cache,
                                                         overwrite=F,config=config)

#' @export
#' @name aggregate_averaged_modeled_rates_by_time_across_country_no_cache
#' @description aggregate the averaged modeled rates across chains and iterations by grid*time over the country
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param average_funs the function to get the averaged modeled rates across chains and iterations
#' @param aggregate_funs the function to get the aggregated averaged modeled rates over country
#' @return modeled rates mean across chains and iterations by grid*timee over the country
aggregate_averaged_modeled_rates_by_time_across_country_no_cache <- function(config, cache,cholera_directory, average_funs = "mean",aggregate_funs="mean"){
  get_modeled_rates(name="modeled_rates",
                    cache=cache,
                    config = config,
                    cholera_directory = cholera_directory
  )
  
  aggregated_modeled_rates_by_gridtime <- apply(cache[["modeled_rates"]], c(3), average_funs)
  analysis_years <- lubridate::year(config$start_time):lubridate::year(config$end_time)
  
  aggregated_averaged_modeled_rates_by_time_across_country<-
    data.frame(aggregated_modeled_rates_by_gridtime)%>%
    dplyr::mutate(analysis_years = rep(analysis_years,each=length(aggregated_modeled_rates_by_gridtime)/length(analysis_years)))%>%
    dplyr::group_by(analysis_years)%>%
    dplyr::summarise_each( aggregate_funs)
  
  return(aggregated_averaged_modeled_rates_by_time_across_country)
}

# cache the results
#' @export
#' @name aggregate_averaged_modeled_rates_by_time_across_country
aggregate_averaged_modeled_rates_by_time_across_country <- cache_fun_results("aggregated_averaged_modeled_rates_by_time_across_country",aggregate_averaged_modeled_rates_by_time_across_country_no_cache,
                                                                             overwrite=F,config=config)