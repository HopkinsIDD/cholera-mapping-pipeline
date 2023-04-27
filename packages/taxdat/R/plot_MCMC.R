#' @include plot_cache_function.R

#' @export
#' @name plot_chain_convergence
#' @title plot_chain_convergence
#' @description add
#' @param name of the input object which is model.rand
#' @param cache the cache environment
#' @param config 
#' @param cholera_directory
#' @param model_output_filenames model output filenames
#' @param pars parameters for which to display traceplots
#' @param render default is TRUE
#' @return ggplot object with traceplots by parameter
plot_chain_convergence <- function(name, 
                                   cache, 
                                   config, 
                                   cholera_directory, 
                                   model_output_filenames,
                                   pars = c("alpha", "rho", "log_std_dev_w", "eta", "std_dev_w", "od_param","sigma_eta","beta","lambda"),
                                   render = T){
  if(name %in% names(cache)){
    model.rand <- cache[[name]]
  }else{
    model.rand <- read_file_of_type(model_output_filenames,"model.rand")
  }
  
  time_effect_param <- tolower(yaml::read_yaml(paste0(cholera_directory,"/",config))$time_effect)
  if(time_effect_param == 'no' | time_effect_param == 'false'){
    warning("There is no time effect, meaning the eta parameter(s) will not be shown. ")
  }
  # pars<-pars[unlist(lapply(pars,function(x){any(str_detect(names(model.rand),x))}))]
  
  pars <- pars[pars %in% unique(stringr::str_extract(names(model.rand), "[[a-z]*_*]*[a-z]+"))]
  #subset a random sample of w for trace plot
  get_stan_input(name="stan_input",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
  smooth_grid_N<-cache[["stan_input"]]$stan_data$smooth_grid_N
  w_random_indx <- sample(smooth_grid_N,size=5)
  w_pars<- c(unlist((lapply(w_random_indx,FUN = function(x){return(paste0("w[",x,"]"))}))))

  if (render) {
    rstan::traceplot(model.rand, pars = c(pars,w_pars))
  }
}

# plot parameter posteriors
#' @export
#' @name plot_MCMCpars
#' @description plot the parameter posteriors 
#' @param name name of the input object which is model.rand from stan output
#' @param cache cached environment where we store model.rand
#' @param pars a list of parameters which we want to display
#' @return ggplot object

plot_MCMCpars <- function(name, 
                          cache, 
                          pars = c("alpha", "rho", "log_std_dev_w", "eta", "std_dev_w", "od_param","sigma_eta","beta","lambda"),
                          config, 
                          cholera_directory) {
  model.rand <- cache[[name]]
  
  time_effect_param <- tolower(yaml::read_yaml(paste0(cholera_directory,"/",config))$time_effect)
  if(time_effect_param == 'no' | time_effect_param == 'false'){
    warning("There is no time effect, meaning the eta parameter(s) will not be shown. ")
  }
  # pars<-pars[unlist(lapply(pars,function(x){any(str_detect(names(model.rand),x))}))]

  pars <- pars[pars %in% unique(stringr::str_extract(names(model.rand), "[[a-z]*_*]*[a-z]+"))]
  #subset a random sample of w for trace plot
  get_stan_input(name="stan_input",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
  smooth_grid_N<-cache[["stan_input"]]$stan_data$smooth_grid_N
  w_random_indx <- sample(smooth_grid_N,size=5)
  w_pars<- c(unlist((lapply(w_random_indx,FUN = function(x){return(paste0("w[",x,"]"))}))))
  
  plot <- rstan::plot(model.rand, pars = c(pars,w_pars))
  return(plot)
}

# plot  Gelman-Rubin Rhat
#' @export
#' @name plot_Rhat
#' @description plot the Rhat of the model
#' @param name name of the input object which is model.rand
#' @param cache the cached environment
#' @param rhat_thresh the threshold for rhat
#' @return ggplot object

plot_Rhat <- function(name, cache,rhat_thresh=1.05){
  model.rand<-cache[[name]]
  fit_summary <- rstan::summary(model.rand)
  rhats <- tibble::tibble(Rhat = round(fit_summary$summary[which(str_detect(row.names(fit_summary$summary), "modeled_cases")), "Rhat"], 2)) %>%
    dplyr::mutate(x=dplyr::row_number())
  rhat_thresh <- rhat_thresh
  frac_above <- sum(rhats$Rhat > rhat_thresh)/nrow(rhats)
  plot <- ggplot2::ggplot(rhats, ggplot2::aes(x = x, y = Rhat)) +
    ggplot2::xlab("Obs. ID") +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = rhat_thresh, col = "red") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(glue::glue("Fraction above threshold: {format(round(frac_above*100, 2))}%"))
  return(plot)
}

# plot spatial random effect
#' @export
#' @name plot_w_mean
#' @description plot the mean w distribution across all iterations
#' @param cache the cached environment
#' @param config the config
#' @param cholera_directory the cholera directory
#' @return plot object 

plot_w_mean <- function(cache, config, cholera_directory) {
    get_model_rand(name="model.rand",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
    get_stan_input(name="stan_input",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
    w_data <-  rstan::extract(cache[["model.rand"]],pars="w")%>%
      reshape2::melt()
    w_data$chain <- rep(1:4,each=1000)
    w_mean_by_chain <- w_data %>%
      group_by(Var2,chain) %>%
      summarise(mean = mean(value)) # QZ: mean across chain and iterations    
    sf_tmp <- cache[["stan_input"]]$sf_grid %>% subset(t == 1)
    w_sf <-data.frame()
    for(chain_idx in unique(w_mean_by_chain$chain)){
    sf_tmp$w_mean <- w_mean_by_chain[w_mean_by_chain$chain==1,]$mean
    sf_tmp$chain<-chain_idx
    w_sf <- rbind(w_sf,sf_tmp)
    }
    plot <- w_sf %>% ggplot2::ggplot(.) + ggplot2::geom_sf(ggplot2::aes(fill = w_mean))+ggplot2::scale_fill_gradientn(colours=c("blue","white","red"),guide = ggplot2::guide_colorbar(label.theme=ggplot2::element_text(angle=45)))+taxdat::map_theme() +facet_wrap(~chain)
    return(plot)
}

# plot  Gelman-Rubin Rhat by admin units
#' @export
#' @name plot_Rhat_by_admin
#' @description plot the Rhat of the model by admin units
#' @param cache the cached environment
#' @param config config file name
#' @param cholera_directory the cholera directory
#' @param rhat_thresh the threshold for rhat
#' @return ggplot object
plot_Rhat_by_admin <- function(cache,config,cholera_directory,rhat_thresh=1.05){
  get_genquant(name="genquant",cache=cache,config=config,cholera_directory = cholera_directory)
  get_output_shapefiles(name="output_shapefiles", cache=cache,config=config,cholera_directory = cholera_directory)
  
  tot_rhats <- cache[["genquant"]]$summary(variables = "location_total_cases_output", 
                                           c(posterior::default_summary_measures(),
                                             posterior::default_convergence_measures()),
                                           .cores = 4) %>% 
    mutate(id = str_extract(variable, "[0-9]+") %>% as.numeric(),
           admin_level = cache[["output_shapefiles"]]$admin_level[id])
  rhat_text<-tot_rhats%>%group_by(admin_level)%>%summarize(fraction_above_threshold=paste0(round(length(rhat[rhat>rhat_thresh])/n()*100,2),"%"))
  
  plot<-ggplot2::ggplot(tot_rhats, aes(x=rhat)) + 
    ggplot2::geom_histogram(color="darkblue", fill="lightblue")+
    ggplot2::geom_vline(xintercept=rhat_thresh, colour="red", linetype = "longdash")+
    ggplot2::theme_bw()+
    ggplot2::facet_wrap(~admin_level,ncol=1,scales = 'free_y')+
    geom_text(
      data=rhat_text,
      mapping=aes(x = -Inf, y=-Inf,label = paste("Fraction above threshold:",fraction_above_threshold)),
      hjust   = -0.1,
      vjust   = -15
    )+
    ylab("")
  return(plot)
}
