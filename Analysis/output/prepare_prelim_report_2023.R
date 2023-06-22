# This script combines post-processed results for preliminary report

# Preamble ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(rstan)
library(cmdstanr)
library(optparse)
library(foreach)
library(rmapshaper)


# Load data ---------------------------------------------------------------

# Lakes for plotting
lakes_sf <- get_lakes()

# ADM0 MAI results
mai_adm0_combined <- bind_rows(
  readRDS("Analysis/output/processed_outputs/mai_adm0_postprocessing_test.rds") %>% 
    mutate(period = "2011-2015"),
  readRDS("Analysis/output/processed_outputs/mai_adm0_postprocessing_test.rds") %>% 
    mutate(period = "2016-2020") %>% 
    mutate(mean = map_dbl(mean, ~ . * 10^runif(1, -1, 1)))
)


# ADM2 MAI results
mai_adm2_combined <- bind_rows(
  readRDS("Analysis/output/processed_outputs/mai_adm2_postprocessing_test.rds") %>% 
    mutate(period = "2011-2015"),
  readRDS("Analysis/output/processed_outputs/mai_adm2_postprocessing_test.rds") %>% 
    mutate(period = "2016-2020") %>% 
    mutate(mean = mean * runif(nrow(.), .1, 10))
)


# Get unique shapefiles
adm0_sf <- mai_adm0_combined %>% 
  group_by(location_period_id) %>% 
  slice(1) %>% 
  select(-period)

adm2_sf <- mai_adm2_combined %>% 
  group_by(location_period_id) %>% 
  slice(1) %>% 
  select(-period)

# Compute stats -----------------------------------------------------------

compute_rate_changes <- function(df) {
  df %>% 
    st_drop_geometry() %>% 
    select(location_period_id, mean, period) %>% 
    pivot_wider(values_from = "mean",
                names_from = "period") %>% 
    mutate(rate_ratio = `2016-2020`/`2011-2015`,
           rate_diff = `2016-2020` - `2011-2015`)
}

mai_adm0_changes <- compute_rate_changes(mai_adm0_combined)
mai_adm2_changes <- compute_rate_changes(mai_adm2_combined)

# Figure 1. MAI national --------------------------------------------------

p_fig1 <- output_plot_map(sf_obj = mai_adm0_combined,
                          lakes_sf = lakes_sf,
                          country_borders = adm0_sf,
                          fill_var = "mean",
                          fill_color_scale_type = "rates") +
  facet_wrap(~ period) +
  ggtitle(str_glue("Mean mean annual incidence rate\nat national level"))


# Figure 2. MAI ADM2 ------------------------------------------------------

p_fig2 <- output_plot_map(sf_obj = mai_adm2_combined,
                          lakes_sf = lakes_sf,
                          country_borders = adm0_sf,
                          fill_var = "mean",
                          fill_color_scale_type = "rates") +
  facet_wrap(~ period) +
  ggtitle(str_glue("Mean mean annual incidence rate\nat national level"))



# Figure 3. MAI Ratio -----------------------------------------------------

p_fig3 <- inner_join(adm0_sf,
                     mai_adm0_changes) %>% 
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  country_borders = adm0_sf,
                  fill_var = "rate_ratio",
                  fill_color_scale_type = "rates") +
  scale_fill_gradient2(midpoint = 0) +
  ggtitle(str_glue("Mean mean annual incidence rate ratios\nat national level"))


# Figure 4. MAI scatter plot ----------------------------------------------

p_fig4 <- mai_adm0_changes %>% 
  ggplot(aes(x = `2011-2015`, y = `2016-2020`)) +
  geom_abline() +
  geom_point() +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10()

