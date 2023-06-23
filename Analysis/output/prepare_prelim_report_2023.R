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
library(taxdat)
options(bitmapType='cairo')

# Load data ---------------------------------------------------------------

# Lakes for plotting
lakes_sf <- get_lakes()

# ADM0 MAI results
mai_adm0_combined <- bind_rows(
  readRDS("C:/Users/zheng/Cholera/map/processed_outputs/mai_adm0__2011_2015.rds") %>% 
    mutate(period = "2011-2015"),
  readRDS("C:/Users/zheng/Cholera/map/processed_outputs/mai_adm0__2016_2020.rds") %>% 
    mutate(period = "2016-2020") %>% 
    mutate(mean = map_dbl(mean, ~ . * 10^runif(1, -1, 1)))
)

# ADM2 MAI results
mai_adm2_combined <- bind_rows(
  readRDS("C:/Users/zheng/Cholera/map/processed_outputs/mai_adm2__2011_2015.rds") %>% 
    mutate(period = "2011-2015"),
  readRDS("C:/Users/zheng/Cholera/map/processed_outputs/mai_adm2__2016_2020.rds") %>% 
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

# Create country level shapefiles for African continent -----------------------------------------------------------
# sf::sf_use_s2(FALSE)
# load("C:/Users/zheng/Cholera/cholera-mapping-pipeline/packages/taxdat/data/WHO_regions.rdata")
# afr <- WHO_regions %>% subset(WHO.region.code=="AFR" | Country.code %in% c("SOM","DJI","LBY","SDN","EGY"))
# afr_sf<-data.frame()
# for ( idx in seq(nrow(afr))) {
#   sf_tmp <- geodata::gadm(country = afr$Country.code[idx], 
#                 level = 0, 
#                 path = tempdir()) %>%
#     sf::st_as_sf()
#   afr_sf <- rbind(afr_sf,sf_tmp)
# }
# sf::st_write(afr_sf,"packages/taxdat/data/afr_sf.shp")

# Figure 1. MAI national --------------------------------------------------

afr_sf<-sf::st_read("packages/taxdat/data/afr_sf.shp")
mai_adm0_combined <- mai_adm0_combined %>% 
  mutate(iso = substr(location_period_id,1,3))
country_in_progress_2011_2015 <- afr_sf[!afr_sf$GID_0 %in% mai_adm0_combined[mai_adm0_combined$period=="2011-2015",]$iso,]
country_in_progress_2016_2020 <- afr_sf[!afr_sf$GID_0 %in% mai_adm0_combined[mai_adm0_combined$period=="2016-2020",]$iso,]

mai_adm0_NA_2011_2015 <- mai_adm0_combined[1,] %>% 
  slice(rep(1:n(), each = nrow(country_in_progress_2011_2015))) %>% 
  mutate(
    mean = NA,
    q5 = NA,
    q95 = NA,
    shapeName = country_in_progress_2011_2015$COUNTRY,
    iso= country_in_progress_2011_2015$GID_0,
    geom=country_in_progress_2011_2015$geometry,
    period = '2011-2015'
  )
mai_adm0_NA_2016_2020 <- mai_adm0_combined[1,] %>% 
  slice(rep(1:n(), each = nrow(country_in_progress_2016_2020))) %>% 
  mutate(
    mean = NA,
    q5 = NA,
    q95 = NA,
    shapeName = country_in_progress_2016_2020$COUNTRY,
    iso= country_in_progress_2016_2020$GID_0,
    geom=country_in_progress_2016_2020$geometry,
    period = '2016-2020'
  )

mai_adm0_combined<- rbind(mai_adm0_combined,mai_adm0_NA_2011_2015,mai_adm0_NA_2016_2020)

p_fig1 <- output_plot_map(sf_obj = mai_adm0_combined,
                          lakes_sf = lakes_sf,
                          country_borders = afr_sf,
                          fill_var = "mean",
                          fill_color_scale_type = "rates") +
  facet_wrap(~ period) +
  ggtitle(str_glue("Mean mean annual incidence rate\nat national level"))

ggsave(p_fig1,
       file = "Analysis/output/figures/Figure_1_MAI_admin0.png",
       width = 10,
       height = 8, 
       dpi = 300)

# Figure 2. MAI ADM2 ------------------------------------------------------
mai_adm2_combined <- mai_adm2_combined %>% 
  mutate(iso = substr(location_period_id,1,3))
mai_adm2_combined <- rbind(mai_adm2_combined,mai_adm0_NA_2011_2015,mai_adm0_NA_2016_2020)

p_fig2 <- output_plot_map(sf_obj = mai_adm2_combined,
                          lakes_sf = lakes_sf,
                          country_borders = afr_sf,
                          fill_var = "mean",
                          fill_color_scale_type = "rates") +
  facet_wrap(~ period) +
  ggtitle(str_glue("Mean mean annual incidence rate\nat second administrative level"))

ggsave(p_fig2,
       file = "Analysis/output/figures/Figure_2_MAI_admin2.png",
       width = 10,
       height = 8, 
       dpi = 300)

# Figure 3. MAI Ratio -----------------------------------------------------
# add countries that are in progress (either aren't approved nor no intend to run)
mai_adm0_changes_sf<-inner_join(adm0_sf,
           mai_adm0_changes) 

mai_adm0_changes_sf <- mai_adm0_changes_sf %>% 
  mutate(iso = substr(location_period_id,1,3))
country_in_progress <- afr_sf[!afr_sf$GID_0 %in% mai_adm0_changes_sf$iso,]

mai_adm0_changes_sf_NA <- mai_adm0_changes_sf[1,] %>% 
  slice(rep(1:n(), each = nrow(country_in_progress))) %>% 
  mutate(
    mean = NA,
    q5 = NA,
    q95 = NA,
    shapeName = country_in_progress$COUNTRY,
    iso= country_in_progress$GID_0,
    geom=country_in_progress$geometry,
    `2011-2015`=NA,
    `2016-2020` =NA
  )

mai_adm0_changes_sf<- rbind(mai_adm0_changes_sf,mai_adm0_changes_sf_NA)

p_fig3 <- mai_adm0_changes_sf %>% 
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  country_borders = afr_sf,
                  fill_var = "rate_ratio",
                  fill_color_scale_type = "rates") +
  scale_fill_gradient2(midpoint = 2) +
  ggtitle(str_glue("Mean mean annual incidence rate ratios\nat national level"))
ggsave(p_fig3,
       file = "Analysis/output/figures/Figure_3_MAI_ratio_admin0.png",
       width = 10,
       height = 8, 
       dpi = 300)

# Figure 4. MAI scatter plot ----------------------------------------------
#assign who regions to the 
mai_adm0_changes <- mai_adm0_changes %>% 
  mutate(iso = substr(location_period_id,1,3))
mai_adm0_changes<-get_AFRO_region(mai_adm0_changes,ctry_col = "iso")

p_fig4 <- mai_adm0_changes %>% 
  ggplot(aes(x = `2011-2015`, y = `2016-2020`,col=AFRO_region)) +
  geom_abline() +
  geom_point() +
  theme_bw() +
  labs(col="WHO regions")+
  scale_x_log10(limits=c(1e-8,1e-02)) +
  scale_y_log10(limits=c(1e-8,1e-02)) 

ggsave(p_fig4,
       file = "Analysis/output/figures/Figure_4_MAI_scatter_plot.png",
       width = 10,
       height = 8, 
       dpi = 300)
