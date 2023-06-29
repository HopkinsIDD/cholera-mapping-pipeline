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
library(cowplot)
library(flextable)

options(bitmapType='cairo')

# User-supplied options
opt_list <- list(
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", help = "Output directory")
)

opt <- parse_args(OptionParser(option_list = opt_list))

# Load data ---------------------------------------------------------------

# Lakes for plotting
lakes_sf <- get_lakes()

# List of all countries with intended run
# Sheet Data Entry Tracking Feb 2022 from spreadsheet that can be downloaded from
# https://docs.google.com/spreadsheets/d/17MtTdUlC2tNLk3QPYdTzgFqiPacUg4rbi8cVTYoOaZE/edit#gid=1264119518

intended_runs <- read_csv("Analysis/output/Data Entry Coordination - Data Entry Tracking - Feb 2022.csv") %>% 
  janitor::clean_names() %>% 
  filter(country != "GMB") %>% 
  mutate(isocode = case_when(str_detect(country, "TZA") ~ "TZA",
                             T ~ country))

# Shapefiles for ADM0 countries in AFR
if(!file.exists("packages/taxdat/data/afr_sf_cleaned.shp")) {
  afr_sf <- sf::st_read("packages/taxdat/data/afr_sf.shp") %>% 
    # Remove wholes 
    nngeo::st_remove_holes() %>% 
    # Remove small islands 
    ms_filter_islands(min_area = 1e9) %>% 
    ms_simplify(keep = 0.05,
                keep_shapes = FALSE)
  
  st_write(afr_sf, "packages/taxdat/data/afr_sf_cleaned.shp")
} else {
  afr_sf <- sf::st_read("packages/taxdat/data/afr_sf_cleaned.shp")
}

afr_sf <- afr_sf %>% 
  janitor::clean_names() %>% 
  mutate(country_name = country,
         country = gid_0,
         intended_run = gid_0 %in% intended_runs$isocode)

afr_sf %>% 
  ggplot(aes(fill = intended_run)) +
  geom_sf() +
  taxdat::map_theme()

# ADM0 MAI results
mai_adm0_combined <- bind_rows(
  readRDS(str_glue("{opt$output_dir}/mai_adm0__2011_2015.rds")) %>% 
    mutate(period = "2011-2015"),
  readRDS(str_glue("{opt$output_dir}/mai_adm0__2016_2020.rds")) %>% 
    mutate(period = "2016-2020")
) %>% 
  mutate(country = str_extract(location_period_id, "[A-Z]{3}"),
         log10_rate_per_1e5 = log10(mean * 1e5))

# ADM2 MAI results
mai_adm2_combined <- bind_rows(
  readRDS(str_glue("{opt$output_dir}/mai_adm2__2011_2015.rds")) %>% 
    mutate(period = "2011-2015"),
  readRDS(str_glue("{opt$output_dir}/mai_adm2__2016_2020.rds")) %>% 
    mutate(period = "2016-2020")
) %>% 
  mutate(country = str_extract(location_period_id, "[A-Z]{3}"),
         log10_rate_per_1e5 = log10(mean * 1e5))


# Risk categories
risk_categories_adm2_combined <- bind_rows(
  readRDS(str_glue("{opt$output_dir}/test_postprocessing_test_risk_categories.rds")) %>% 
    mutate(period = "2011-2015"),
  readRDS(str_glue("{opt$output_dir}/test_postprocessing_test_risk_categories.rds")) %>% 
    mutate(period = "2016-2020")
)  %>% 
  filter(admin_level == "ADM2") %>% 
  mutate(country = str_extract(location_period_id, "[A-Z]{3}"))


# # Get unique shapefiles
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
    select(location_period_id, mean, period, country) %>% 
    pivot_wider(values_from = "mean",
                names_from = "period") %>% 
    mutate(rate_ratio = `2016-2020`/`2011-2015`,
           log10_rate_ratio = log10(rate_ratio),
           rate_diff = `2016-2020` - `2011-2015`)
}

mai_adm0_changes <- compute_rate_changes(mai_adm0_combined)
mai_adm2_changes <- compute_rate_changes(mai_adm2_combined)

# Figure 1. MAI national --------------------------------------------------

p_fig1 <- output_plot_map(sf_obj = mai_adm0_combined,
                          lakes_sf = lakes_sf,
                          all_countries_sf = afr_sf,
                          fill_var = "log10_rate_per_1e5",
                          fill_color_scale_type = "rates") +
  facet_wrap(~ period) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = "right") +
  guides(fill = guide_colorbar("Incidence rate\n[cases/100,000 people]"))


ggsave(p_fig1,
       file = "Analysis/output/figures/Figure_1_MAI_admin0.png",
       width = 12,
       height = 8, 
       dpi = 600)

# Figure 2. MAI ADM2 ------------------------------------------------------

p_fig2 <- output_plot_map(sf_obj = mai_adm2_combined,
                          lakes_sf = lakes_sf,
                          all_countries_sf = afr_sf,
                          fill_var = "log10_rate_per_1e5",
                          fill_color_scale_type = "rates") +
  facet_wrap(~ period) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = "right") +
  guides(fill = guide_colorbar("Incidence rate\n[cases/100,000 people]"))


ggsave(p_fig2,
       file = "Analysis/output/figures/Figure_2_MAI_admin2.png",
       width = 12,
       height = 8, 
       dpi = 600)

# Figure 3. MAI Ratio -----------------------------------------------------

p_fig3 <- inner_join(adm0_sf,
                     mai_adm0_changes) %>% 
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "log10_rate_ratio",
                  fill_color_scale_type = "ratio")  +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar("Ratio of incidence rates\n[2016-2020/2011-2015]"))

ggsave(p_fig3,
       file = "Analysis/output/figures/Figure_3_MAI_ratio_admin0.png",
       width = 10,
       height = 8, 
       dpi = 600)

# Figure 3_2. MAI Ratio at admin2-----------------------------------------------------

p_fig3_2 <- inner_join(adm2_sf,
                       mai_adm2_changes) %>% 
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "log10_rate_ratio",
                  fill_color_scale_type = "ratio") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar("Ratio of incidence rates\n[2016-2020/2011-2015]"))

ggsave(p_fig3_2,
       file = "Analysis/output/figures/Figure_3_2_MAI_ratio_admin2.png",
       width = 10,
       height = 8, 
       dpi = 600)

# Figure 4. MAI scatter plot ----------------------------------------------
#assign who regions to the 
mai_adm0_changes <- taxdat::get_AFRO_region(mai_adm0_changes, 
                                            ctry_col = "country")

afr_sf <- taxdat::get_AFRO_region(afr_sf, 
                                  ctry_col = "country")

p_fig4_scatter <- mai_adm0_changes %>% 
  ggplot(aes(x = log10(`2011-2015`*1e5), 
             y =  log10(`2016-2020`*1e5), 
             col = AFRO_region)) +
  geom_abline() +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = country),
                            min.segment.length = 0,
                            max.overlaps = Inf) +
  theme_bw() +
  guides(color = guide_legend("WHO regions")) +
  scale_x_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2),
                     labels = formatC(10^(seq(-2, 2)),
                                      digits = 1,
                                      format = "fg", 
                                      big.mark = ",")) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2),
                     labels = formatC(10^(seq(-2, 2)),
                                      digits = 1,
                                      format = "fg", 
                                      big.mark = ",")) +
  labs(x = "Incidence rate 2011-2015\n[cases per 100,000/year]",
       y = "Incidence rate 2016-2020\n[cases per 100,000/year]")

p_regions <- ggplot(afr_sf, aes(fill = AFRO_region)) +
  geom_sf() +
  taxdat::map_theme() +
  guides(fill = "none") +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank())


p_fig4 <- ggdraw() +
  draw_plot(p_fig4_scatter) +
  draw_plot(p_regions, x = 0.765, y = .7, width = .27, height = .27)


ggsave(p_fig4,
       file = "Analysis/output/figures/Figure_4_MAI_scatter_plot.png",
       width = 10,
       height = 7, 
       dpi = 600)



# Table -------------------------------------------------------------------

mai_adm0_combined %>% 
  st_drop_geometry() %>% 
  mutate(text = str_c(
    formatC(mean*1e5, digits = 1, format = "f"),
    " (",
    formatC(q5*1e5, digits = 1, format = "f"),
    "-",
    formatC(q95*1e5, digits = 1, format = "f"),
    ")"
  )) %>% 
  select(text, period, country) %>% 
  pivot_wider(values_from = "text",
              names_from = "period") %>% 
  inner_join(mai_adm0_changes %>% 
               mutate(rate_diff = formatC(rate_diff*1e5 , digits = 1, format = "f"),
                      rate_ratio = formatC(rate_ratio, digits = 1, format = "f")) %>% 
               select(country, rate_ratio, rate_diff)) %>% 
  flextable() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  save_as_docx(path = "Analysis/output/figures/Table_1.docx")




# Figure 5. ADM2 risk categories ------------------------------------------


p_fig5 <- output_plot_map(sf_obj = risk_categories_adm2_combined, 
                          lakes_sf = lakes_sf,
                          all_countries_sf = afr_sf,
                          fill_var = "risk_cat",
                          fill_color_scale_type = "risk category") +
  facet_wrap(~period) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = "right") +
  guides(fill = guide_colorbar("Risk category"))


ggsave(p_fig5,
       file = "Analysis/output/figures/Figure_2_MAI_admin2.png",
       width = 12,
       height = 8, 
       dpi = 600)

