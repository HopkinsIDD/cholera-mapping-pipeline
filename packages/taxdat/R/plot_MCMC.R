#' @include plot_cache_function.R

#' @export
#' @name plot_chain_convergence
#' @description plot the chain convergence
#' @param name of the input object which is model.rand
#' @param cache the cache environment
#' @param pars a list of parameters which we want to display

plot_chain_convergence <- function(cache, name, pars= c("rho", "betas", "log_std_dev_w", "eta")){
  model.rand <- cache[[name]]
  plot<- rstan::traceplot(model.rand, pars = pars)
  return(plot)
}

# plot parameter posteriors
#' @export
#' @name plot_MCMCpars
#' @description plot the parameter posteriors 
#' @param name name of the input object which is model.rand from stan output
#' @param cache cached environment where we store model.rand
#' @param pars a list of parameters which we want to display
#' @return ggplot object

plot_MCMCpars <- function(name,cache,pars) {
  model.rand <- cache[[name]]
  plot <- rstan::plot(model.rand, pars =  c("rho", "betas", "log_std_dev_w", "eta"))
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