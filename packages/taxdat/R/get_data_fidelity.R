#' @export
#' @name get_data_fidelity
#' @title get_data_fidelity
#' @description add
#' @param genquant_filenames genquant_filenames
#' @return
get_data_fidelity <- function(stan_input_filenames, 
                              genquant_filenames){
  
  if (length(stan_input_filenames) != length(genquant_filenames))
    stop("Need to provide same number of stan_input and stan_output files")
  data_fidelity <- NULL

    if(file.exists(file_names["initial_values"])){
      data_fidelity <- taxdat::get_data_fidelity(stan_input_filenames = file_names["stan_input"],
                                                 model_output_filenames = file_names["stan_output"])
      
    }else{
      #for old runs without initial_values output files
      stan_input_filenames = file_names["stan_input"]
      model_output_filenames = file_names["stan_output"]
      if (length(stan_input_filenames) != length(model_output_filenames))
        stop("Need to provide same number of stan_input and stan_output files")
      
      rc <- list()
      layer_index <- 1
      for (i in 1:length(model_output_filenames)) {
        filename <- model_output_filenames[i]
        model.rand <- read_file_of_type(filename, "model.rand")
        nchain <- dim(MCMCvis::MCMCchains(model.rand, params='lp__'))[1] / niter_per_chain
        
        stan_data <- read_file_of_type(stan_input_filenames[i], "stan_input")$stan_data
        modeled_cases <- as.array(model.rand)[, , grepl("modeled_cases", names(model.rand)), drop = FALSE]
        modeled_cases_chain_mean <- apply(modeled_cases, c(2, 3), mean)
        actual_cases <- matrix(stan_data$y, nrow(modeled_cases_chain_mean), ncol(modeled_cases_chain_mean), byrow=TRUE)
        dimnames(actual_cases) <- dimnames(modeled_cases_chain_mean)
        modeled_cases_chain_mean <- reshape2::melt(modeled_cases_chain_mean)
        actual_cases <- reshape2::melt(actual_cases)
        actual_cases$censoring <- rep(stan_data$censoring_inds, each = nchain)
        actual_cases$oc_uid <- rep(taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$sf_cases_resized$OC_UID, 
                                   each = nchain) #newly added
        actual_cases$oc_year <- rep(paste0(format(taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$sf_cases_resized$TL, '%Y'),
                                           "_",
                                           format(taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$sf_cases_resized$TR, '%Y')),
                                    each = nchain) #newly added and updated on 21-12-14
        
        stan_data<-read_file_of_type(file_names["data"],"stan_data")
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
        rc[[filename]] <- comparison
        names(rc)[[layer_index]] <- paste(
          paste(filename_to_stubs(filename)[2:3], collapse = " "),
          "\niterations: Chain", filename_to_stubs(filename)[5])
        layer_index <- layer_index + 1
      }
    }
  
  return(rc)
}