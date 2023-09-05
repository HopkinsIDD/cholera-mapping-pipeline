#' @export
#' @name plot_posterior_coverage_cdr

plot_posterior_coverage_cdr <- function(config, cache, cholera_directory){
  
  config_list <-  yaml::read_yaml(paste0(cholera_directory, "/", config))

  config_list$file_names$stan_genquant_filename <- paste0(cholera_directory,"/Analysis/data/", config_list$file_names$stan_genquant_filename)
  config_list$file_names$stan_input_filename <- paste0(cholera_directory,"/Analysis/data/", config_list$file_names$stan_input_filename)

  gen_obs <- taxdat:::postprocess_gen_obs(config_list = config_list)
  
  colors_admin_levels <- function(){c("#CAE0C9", "#99CCFF", "#FF9999", "#CC9900", "#FF9933",'black')}
  
  gen_obs$country <- config_list$countries_name
  
  plot <- taxdat::plot_posterior_coverage(gen_obs)
  
  return(plot)
}