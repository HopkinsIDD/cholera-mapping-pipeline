#' @include plot_cache_function.R
#' 
#' @export
#' @name plot_modeled_cases_by_chain_time
#' @title plot_modeled_cases_by_chain_time
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return table with modeled cases by time and chain
plot_modeled_cases_by_chain_time <- function(config, cache, cholera_directory) {
  
     get_stan_input(name="stan_input",cache=cache,config=config,cholera_directory=cholera_directory)
     stan_input<-cache[["stan_input"]]
     stan_input$sf_grid <- stan_input$sf_grid %>%
      dplyr::ungroup() %>%
      dplyr::select(t,id)
     
    get_stan_model_nchain(name="nchain",cache=cache,config=config,cholera_directory=cholera_directory)
    nchain<-cache[["nchain"]] 
    cases_chains<-aggregate_modeled_cases_by_chain_gridtime_no_cache(
                                              config=config,cholera_directory=cholera_directory,
                                              cache=cache)
    
    stan_input$sf_grid[paste('cases','chain',seq_len(nchain),sep='_')] <- t(cases_chains)

    config_file<-yaml::read_yaml(paste0(cholera_directory,config))
    analysis_years <- lubridate::year(config_file$start_time):lubridate::year(config_file$end_time)
    obs_years <- min(lubridate::year(cache[["sf_cases_resized"]]$TL)):max(lubridate::year(cache[["sf_cases_resized"]]$TR))
    
    if(params$drop_nodata_years & !all(analysis_years %in% obs_years)){
      drop_year_ix <- which(!analysis_years %in% obs_years)
      message(paste("Dropping", paste(analysis_years[drop_year_ix], collapse = ", "), "from cases_chains"))
      stan_input$sf_grid <- dplyr::filter(stan_input$sf_grid, !(t %in% drop_year_ix))
    }
    
    sf_grid_wider <- sf::st_drop_geometry(stan_input$sf_grid)
    

    by_years <- sf_grid_wider %>%
      dplyr::group_by(t) %>%
      dplyr::summarise(dplyr::across(dplyr::contains("cases_chain"), sum)) %>%
      dplyr::mutate(t = as.character(t))
    mai <- by_years %>%
      dplyr::summarise(dplyr::across(dplyr::contains("cases_chain"), mean)) %>%
      dplyr::mutate(t = "mean annual cases")
    
    dplyr::bind_rows(by_years, mai) %>%
      kableExtra::kable(col.names = c("time slice", paste("chain", 1:nchain))) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped"))
    
  }
