library(dplyr)
library(sf)
library(taxdat)

## ECL: I'm sure these functions must exist in taxdat but they got renamed so I'm just keeping this here for the model_run_report_censoring.Rmd (which may be obsolete anyways)
get_aggregated_data <- function(covar_data_filename,
                                stan_input_filename) {
  
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  stan_input <- read_file_of_type(stan_input_filename, "stan_input")
  
  # Aggregated data
  aggregated_data <- inner_join(
    tibble(
      obs = seq_along(stan_input$stan_data$y),
      y = stan_input$stan_data$y,
      censoring = stan_input$stan_data$censoring_inds
    ),
    tibble(
      obs = stan_input$stan_data$map_obs_loctime_obs,
      lp = stan_input$stan_data$u_loctime[stan_input$stan_data$map_obs_loctime_loc],
      tfrac = stan_input$stan_data$tfrac
    )
  ) %>%
    inner_join(covar_cube_output$location_periods_dict %>%
                 group_by(location_period_id, loctime_id, t) %>%
                 summarise(n_cell = n()),
               by = c("lp" = "loctime_id")) %>%
    mutate(location_period_id = as.character(location_period_id))
  
  return(aggregated_data)
}

plot_censored_data <- function(modeled_cases) {
  
  ccdfs <- modeled_cases %>% 
    distinct(modeled) %>% 
    unlist() %>% 
    map_df(function(x) {
      vals <- seq(0, 5e3, by = 1)
      tibble(mean_rate = x, 
             vals = vals,
             ccdf = 1-ppois(vals, x))
    })
  
  p_censored <- modeled_cases %>% 
    filter(censoring == "right-censored") %>% 
    inner_join(ccdfs, by = c("modeled" = "mean_rate")) %>% 
    filter(vals < 3 * cases) %>% 
    ggplot(aes(x = vals, y = ccdf, group = modeled)) +
    geom_line() +
    geom_vline(aes(xintercept = cases), lty = 2, size = .4) +
    facet_wrap(~location_period_id, scales = "free_x") +
    theme_bw() +
    ggtitle("Modeled complementary CDF (P(Y >=y)) by location period id") +
    labs(x = "Line: modeled CCDF. Vertical lines: right-censored observations")
  p_censored
}

plot_full_cases <- function(modeled_cases) {
  
  p_full <- modeled_cases %>% 
    filter(censoring != "right-censored") %>% 
    ggplot(aes(x = cases, y= modeled)) +
    geom_abline(aes(intercept = 0, slope = 1)) +
    geom_point() +
    theme_bw() +
    ggtitle("Modeled vs. observed cases \nfor full observations (tfrac > 95%) \nby location period id") +
    labs(x = "Observed cases", y = "Modeled cases")
  p_full
}
