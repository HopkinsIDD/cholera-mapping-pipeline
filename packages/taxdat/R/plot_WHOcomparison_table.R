#' @include plot_cache_function.R
#' 
#' @export
#' @name plot_WHOcomparison_table
#' @title plot_WHOcomparison_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param aesthetic whether to return the kable object
#' @return table with who comparison statistics
plot_WHOcomparison_table <- function(config, cache, cholera_directory, aesthetic = TRUE) {
  get_sf_cases_resized(name="sf_cases_resized",config=config, cache=cache, cholera_directory=cholera_directory)
  who_annual_cases <- cache[["sf_cases_resized"]]
  
  if(!"data_fidelity" %in% names(cache)){
    cache[["data_fidelity"]] <- get_data_fidelity(cache = cache, cholera_directory = cholera_directory, config = config)
  }
  chains <- cache[["data_fidelity"]] %>% 
    filter(str_detect(variable, "tfrac", negate = T)) %>% 
    group_by(variable) %>% 
    summarise(modeled = paste0(format(round(mean(`modeled cases`),0),big.mark=","),"(",format(round(quantile(`modeled cases`,prob=0.025),0),big.mark=","),"-",format(round(quantile(`modeled cases`,prob=0.975),0),big.mark=","),")"))
  
  who_annual_cases$modeled <-   chains$modeled
  who_annual_cases$observed <- who_annual_cases$attributes.fields.suspected_cases # fix me
  who_annual_cases_from_db <- NULL

  who_annual_cases_from_db <- taxdat::pull_output_by_source(who_annual_cases, "%WHO Annual Cholera Reports%",
                                                              database_api_key_rfile = stringr::str_c(cholera_directory, "/Analysis/R/database_api_key.R"))
 
  if(!is.null(who_annual_cases_from_db)) {
    if(aesthetic){
      who_annual_cases_from_db %>%
        as.data.frame() %>%
        dplyr::select(OC_UID, TL, TR, observed, modeled) %>%
        dplyr::mutate_if(is.numeric, function(x) {format(round(x) , big.mark=",")}) %>%
        kableExtra::kable(col.names = c("OC id", "start time", "end time", "# Observed cases", "# Modeled Cases (2.5%-97.5%)")) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped"))
    }else{
      who_annual_cases_from_db %>%
        as.data.frame() %>%
        dplyr::select(OC_UID, TL, TR, observed, modeled)
      return(who_annual_cases_from_db)
    }
    
  }
}
