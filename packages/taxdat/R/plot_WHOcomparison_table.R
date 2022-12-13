#' @include plot_cache_function.R
#' 
#' @export
#' @name plot_WHOcomparison_table
#' @title plot_WHOcomparison_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param WHO_data_is_country_level whether the WHO data is at the country level
#' @param aesthetic whether to return the kable object
#' @return table with who comparison statistics
plot_WHOcomparison_table <- function(config, cache, cholera_directory, WHO_data_is_country_level = TRUE, aesthetic = TRUE) {
  get_sf_cases_resized(name="sf_cases_resized",config=config, cache=cache, cholera_directory=cholera_directory)
  who_annual_cases <- cache[["sf_cases_resized"]]
  
  ### Stashed code 
  # if(!"data_fidelity" %in% names(cache)){
  #   cache[["data_fidelity"]] <- get_data_fidelity(cache = cache, cholera_directory = cholera_directory, config = config)
  # }
  # chains <- cache[["data_fidelity"]] %>% 
  #   filter(str_detect(variable, "tfrac", negate = T)) %>% 
  #   group_by(variable) %>% 
  #   summarise(modeled = paste0(format(round(mean(`modeled cases`),0),big.mark=","),
  #                             "(",format(round(quantile(`modeled cases`,prob=0.025),0),big.mark=","),
  #                             "-",format(round(quantile(`modeled cases`,prob=0.975),0),big.mark=","),
  #                             ")")
  #             )
  # who_annual_cases$modeled <-   chains$modeled

  ## if WHO_data_is_country_level 
  if(WHO_data_is_country_level){
    ### First get the non-na grid cells and their associated time 
    grid_time <- cache$covar_cube_output$sf_grid %>% 
      filter(long_id %in% cache$covar_cube_output$non_na_gridcells) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(t)
    
    ### Get the grid-level modeled cases 
    list_of_draws <- rstan::extract(cache$model.rand)
    grid_cases_tmp <- t(list_of_draws$grid_cases)
    yearly_total_modeled_cases <- unlist(lapply(unique(grid_time$t), 
                                                function(x){
                                                  yearly_vector <- apply(grid_cases_tmp[which(grid_time$t == x), ], 2, sum)
                                                  paste0(   format(round(mean(yearly_vector),0),big.mark=","),
                                                            "(", format(round(quantile(yearly_vector, prob=0.025),0),big.mark=","),
                                                            "-", format(round(quantile(yearly_vector, prob=0.975),0),big.mark=","),
                                                            ")"
                                                        )
                                                }
                                              )
                                        )

    ### Get the WHO table and combine them 
    who_annual_cases <- who_annual_cases %>% rename(observed = attributes.fields.suspected_cases)
    who_annual_cases_from_db <- NULL

    who_annual_cases_from_db <- taxdat::pull_output_by_source(who_annual_cases, "%WHO Annual Cholera Reports%",
                                                              database_api_key_rfile = stringr::str_c(cholera_directory, "/Analysis/R/database_api_key.R")) %>%
                                mutate(modeled = yearly_total_modeled_cases)
  }else{
    ### get the distribution of the observation-level modeled cases 
    get_genquant(name="genquant",cache=cache,config=config,cholera_directory = cholera_directory)
    varnames <- dimnames(cache[['genquant']]$draws())[[3]]
    modeled_observed_cases <- as.array(cache[['genquant']]$draws())[, , grepl("^modeled_cases", varnames),drop=FALSE]
    dim(modeled_observed_cases) <- c(dim(modeled_observed_cases)[1] * dim(modeled_observed_cases)[2], dim(modeled_observed_cases)[3])
    modeled_obs_level_cases <- apply(modeled_observed_cases, 2, function(x){
      paste0( format(round(mean(x),0),big.mark=","),
              "(", format(round(quantile(x,prob=0.025),0),big.mark=","),
              "-", format(round(quantile(x,prob=0.975),0),big.mark=","),
              ")"
            )
      })
    who_annual_cases$modeled <- modeled_obs_level_cases

    ### Get the WHO table and combine them 
    who_annual_cases <- who_annual_cases %>% rename(observed = attributes.fields.suspected_cases)
    who_annual_cases_from_db <- NULL

    who_annual_cases_from_db <- taxdat::pull_output_by_source(who_annual_cases, "%WHO Annual Cholera Reports%",
                                                              database_api_key_rfile = stringr::str_c(cholera_directory, "/Analysis/R/database_api_key.R"))
  }

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
