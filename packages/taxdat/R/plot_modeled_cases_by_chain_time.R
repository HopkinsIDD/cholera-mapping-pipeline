#' @include plot_cache_function.R
#' 
#' @export
#' @name plot_modeled_cases_by_chain_time
#' @title plot_modeled_cases_by_chain_time
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @param aesthetic whether to return the kable object
#' @return table with modeled cases by time and chain
plot_modeled_cases_by_chain_time <- function(config, cache, cholera_directory, aesthetic = TRUE) {
  
    get_stan_input(name="stan_input",cache=cache,config=config,cholera_directory=cholera_directory)
    stan_input<-cache[["stan_input"]]
    stan_input$sf_grid <- stan_input$sf_grid %>%
    dplyr::ungroup() %>%
    dplyr::select(t,id)
    
    get_sf_cases_resized(name = "sf_cases_resized", cache = cache, config = config, cholera_directory = cholera_directory)
    cache[["niter_per_chain"]] <- get_stan_model_niter_per_chain_no_cache(cache = cache,config = config,cholera_directory = cholera_directory)
    cache$nchain <- get_stan_model_nchain_no_cache(config=config, cache=cache, cholera_directory=cholera_directory)
    nchain <- cache$nchain
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
    
    tab <- dplyr::bind_rows(by_years, mai)
    if(aesthetic){
      tab %>% 
        kableExtra::kable(col.names = c("time slice", paste("chain", 1:nchain))) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped"))
    }else{
      return(tab)
    }
    
  }
