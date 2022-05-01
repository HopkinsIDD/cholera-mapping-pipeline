#' @export
#' @name get_data_fidelity
#' @title get_data_fidelity
#' @description add
#' @param genquant_filenames genquant_filenames
#' @return
get_data_fidelity <- function(cache,config,cholera_directory){
       
        
        get_stan_model_nchain(name="nchain",cache=cache,config=config,cholera_directory = cholera_directory)
        nchain<-cache[["nchain"]]
        
        cache[["modeled_cases_chain_mean"]]<-aggregate_modeled_observed_cases_by_chain_gridtime_no_cache(cache=cache,config=config,cholera_directory = cholera_directory)
        modeled_cases_chain_mean<-cache[["modeled_cases_chain_mean"]]
        
        get_stan_data(name="stan_data",cache=cache,config=config,cholera_directory=cholera_directory)
        stan_data<-cache[["stan_data"]]
        
        actual_cases <- matrix(stan_data$y, nrow(modeled_cases_chain_mean), ncol(modeled_cases_chain_mean), byrow=TRUE)
        get_stan_input(name="stan_input",cache=cache,config=config,cholera_directory=cholera_directory)
        
        dimnames(actual_cases) <- dimnames(modeled_cases_chain_mean)
        modeled_cases_chain_mean <- reshape2::melt(modeled_cases_chain_mean)
        actual_cases <- reshape2::melt(actual_cases)
        actual_cases$censoring <- rep(stan_data$censoring_inds, each = nchain)
        
        actual_cases$oc_uid <- rep(cache[["stan_input"]]$sf_cases_resized$OC_UID, 
                                   each = nchain) #newly added
        actual_cases$oc_year <- rep(paste0(format(cache[["stan_input"]]$sf_cases_resized$TL, '%Y'),
                                           "_",
                                           format(cache[["stan_input"]]$sf_cases_resized$TR, '%Y')),
                                    each = nchain) #newly added and updated on 21-12-14
        
        #get_preprocessed_stan(name="preprocessed_stan_data",cache=cache,config=params$config,cholera_directory=params$cholera_directory)
        #get_preprocessed_stan_no_cache(cache=cache,config=params$config,cholera_directory=params$cholera_directory)
        
        #preprocessed_stan_data<-cache[["preprocessed_stan_data"]]
        
        obs_tfrac=data.frame(
          obs = stan_data$map_obs_loctime_obs,
          tfrac = stan_data$tfrac
        ) %>%
          dplyr::group_by(obs) %>%
          dplyr::summarize(tfrac = sum(tfrac))
        actual_cases$tfrac<-1
        actual_cases[stringr::str_detect(actual_cases$parameters,"tfrac"),]$tfrac<-rep(obs_tfrac$tfrac,each=nchain)
        
        comparison <- dplyr::left_join(modeled_cases_chain_mean, actual_cases, by = c(chains = "chains", parameters = "parameters"))
        names(comparison)[3:4] <- c("modeled cases", "actual cases")
      
  return(comparison)
}