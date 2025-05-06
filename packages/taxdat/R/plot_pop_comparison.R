#' @export
#' @name plot_pop_comparison

plot_pop_comparison <- function(config, cache, cholera_directory){
  
  #load stan_input, output_shapefiles and genquant file
  config_file <- yaml::read_yaml(paste(cholera_directory,config,sep = '/'))
  period <- seq(lubridate::year(config_file$start_time),lubridate::year(config_file$end_time))
  get_genquant(name="genquant",cache=cache,config=config,cholera_directory = cholera_directory)
  get_stan_input(name="stan_input",cache=cache,config = config,cholera_directory = cholera_directory)
  get_output_shapefiles(name="output_shapefiles", config = config,cache=cache,cholera_directory = cholera_directory)
  
  #get the pop from genquant file
  pop_by_year <- cache[["genquant"]]$summary(variables = "pop_loctimes_output",
                                             c(posterior::default_summary_measures(),
                                               posterior::default_convergence_measures()),
                                             .cores = 1) %>% 
    dplyr::mutate(id = stringr::str_extract(variable, "[0-9]+") %>% as.numeric(),
                  location_period_id = cache[["stan_input"]]$fake_output_obs$locationPeriod_id[id],
                  TL = cache[["stan_input"]]$fake_output_obs$TL[id],
                  wpp_pop = NA) %>% 
    dplyr::filter(stringr::str_detect(location_period_id,"ADM0")) %>% 
    dplyr::inner_join(cache[["output_shapefiles"]]) %>% 
    dplyr::arrange(TL)
  
  # load pop data from world pop csv file
  data("WHO_regions", package = "taxdat")
  
  afr_regions <- WHO_regions %>% 
    filter(WHO.region %in% c("Africa","Eastern Mediterranean"))
  
  # Load UN population estimates
  data("WPP2024", package = "taxdat")
  
  wpp_pop <- afr_regions %>% 
    dplyr::mutate(
      ISO3_code = Country.code
    ) %>% 
    dplyr::select(ISO3_code) %>% 
    dplyr::inner_join(
      WPP2024 %>% dplyr::filter(Time %in% period) %>% dplyr::select(ISO3_code, Time, PopTotal) %>% dplyr::mutate(PopTotal = 1000*PopTotal)
    ) %>% 
    dplyr::filter(ISO3_code == config_file$countries_name) %>%
    dplyr::arrange(Time)
  
  pop_by_year$wpp_pop <- wpp_pop$PopTotal
  
  # plot comparison
  
  plot <- ggplot2::ggplot(data=pop_by_year %>% 
                            dplyr::rename("pipeline_pop" = "median") %>% 
                            dplyr::mutate(difference_percentage = paste0(round(100*(pipeline_pop-wpp_pop)/wpp_pop,2),"%")) %>% 
                            tidyr::pivot_longer(cols= c(pipeline_pop,wpp_pop),
                                                names_to = "Source",
                                                values_to = "population") %>% 
                            dplyr::mutate(difference_percentage = replace(difference_percentage,Source =="wpp_pop","")),
                          ggplot2::aes(
                            x= lubridate::year(TL),
                            y= population)) +
    ggplot2::geom_bar(
      ggplot2::aes(
        fill = Source
      ),
      stat = 'identity',
      position = ggplot2::position_dodge()) + 
    ggplot2::geom_text(
      ggplot2::aes(
        label = difference_percentage
      ),
      vjust = 1,nudge_x = -.2
    ) +
    ggplot2::labs(
      y="Population",
      x="Year"
    ) + 
    ggplot2::scale_fill_manual(values = c("gold1", "steelblue3"))+
    ggplot2::theme(
      axis.title=ggplot2::element_text(size=14,face="bold")
    ) +
    ggplot2::theme_classic(
      base_size = 12
    )
  return(plot)
}