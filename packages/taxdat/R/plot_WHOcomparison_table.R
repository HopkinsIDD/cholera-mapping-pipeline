#' @include plot_cache_function.R
#' 
#' @export
#' @name plot_WHOcomparison_table
#' @title plot_WHOcomparison_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param observation_level_modeled_cases whether the WHO data is at the country level
#' @param allow_data_pull whether to pull WHO data from the database 
#' @param add_other_source whether to add non-WHO country annual cases data
#' @param aesthetic whether to return the kable object
#' @return table with who comparison statistics
plot_WHOcomparison_table <- function(config, cache, cholera_directory, observation_level_modeled_cases = TRUE, 
                                     allow_data_pull = TRUE, add_other_source = FALSE, aesthetic = TRUE) {
  get_sf_cases_resized(name="sf_cases_resized",config=config, cache=cache, cholera_directory=cholera_directory)
  who_annual_cases <- cache[["sf_cases_resized"]] %>% sf::st_drop_geometry()
  
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

  ## if to get the sum of the grid-level modeled cases  
  if(!observation_level_modeled_cases){
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

    if(allow_data_pull){
      who_annual_cases_from_db <- taxdat::pull_output_by_source(who_annual_cases, "%WHO Annual Cholera Reports%",
                                                                database_api_key_rfile = stringr::str_c(cholera_directory, "/Analysis/R/database_api_key.R")) %>%
                                  mutate(modeled = yearly_total_modeled_cases)
    }else{
      who_annual_cases_from_db <- who_annual_cases %>% filter(stringr::str_length(OC_UID) == 3) %>%
        mutate(modeled = yearly_total_modeled_cases)
    }

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

    if(allow_data_pull){
      who_annual_cases_from_db <- taxdat::pull_output_by_source(who_annual_cases, "%WHO Annual Cholera Reports%",
                                                              database_api_key_rfile = stringr::str_c(cholera_directory, "/Analysis/R/database_api_key.R"))
    }else{
      who_annual_cases_from_db <- who_annual_cases %>% filter(stringr::str_length(OC_UID) == 3)
    }
    
  }

  ### Only need certain variables 
  who_annual_cases_from_db <- who_annual_cases_from_db %>%
        as.data.frame() %>%
        dplyr::select(OC_UID, TL, TR, observed, modeled)

  ### Whether add more rows that compare modeled cases with other annual country-level observed cases 
  if(add_other_source){
    sf_cases_country_level <- cache[["sf_cases"]] %>%
      sf::st_drop_geometry() %>%
      filter(stringr::str_count(location_name, "::") == 1 & stringr::str_length(OC_UID) != 3)
    country_level_ids <- unique(sf_cases_country_level$locationPeriod_id)
    country_level_agg_cases <- cache[["stan_input"]]$sf_cases_resized %>% 
      sf::st_drop_geometry() %>%
      filter(locationPeriod_id %in% country_level_ids & stringr::str_length(OC_UID) != 3) %>%
      mutate(observed = attributes.fields.suspected_cases, modeled = NA) %>%
      select(OC_UID, TL, TR, observed, modeled)
    
    who_annual_cases_from_db <- rbind(who_annual_cases_from_db, country_level_agg_cases) %>%
      arrange(TL, desc(TR))
  }

  ### Whether make it pretty 
  if(!is.null(who_annual_cases_from_db)) {
    if(aesthetic){
      who_annual_cases_from_db %>%
        dplyr::mutate_if(is.numeric, function(x) {format(round(x) , big.mark=",")}) %>%
        kableExtra::kable(col.names = c("OC id", "start time", "end time", "# Observed cases", "# Modeled Cases (2.5%-97.5%)")) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped"))
    }else{
      return(who_annual_cases_from_db)
    }
    
  }
}
