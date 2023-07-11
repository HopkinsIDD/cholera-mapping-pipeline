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
library(ggsn)

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
         intended_run = gid_0 %in% intended_runs$isocode) %>% 
  st_crop(st_bbox(st_sfc(
    st_point(c(-18.8, 36.6)), 
    st_point(c(52.2, -36.5)),
    crs = 4326)))

afr_sf %>% 
  ggplot(aes(fill = intended_run)) +
  geom_sf() +
  taxdat::map_theme()

# ADM0 MAI results
mai_adm0_combined <- bind_rows(
  readRDS(str_glue("{opt$output_dir}/mai_adm0_2011_2015.rds")) %>% 
    mutate(period = "2011-2015"),
  readRDS(str_glue("{opt$output_dir}/mai_adm0_2016_2020.rds")) %>% 
    mutate(period = "2016-2020")
) %>% 
  mutate(country = str_extract(location_period_id, "[A-Z]{3}"),
         log10_rate_per_1e5 = log10(mean * 1e5))

# ADM2 MAI results
mai_adm2_combined <- bind_rows(
  readRDS(str_glue("{opt$output_dir}/mai_adm2_2011_2015.rds")) %>% 
    mutate(period = "2011-2015"),
  readRDS(str_glue("{opt$output_dir}/mai_adm2_2016_2020.rds")) %>% 
    mutate(period = "2016-2020")
) %>% 
  mutate(country = str_extract(location_period_id, "[A-Z]{3}"),
         log10_rate_per_1e5 = log10(mean * 1e5))


# # Risk categories
# risk_categories_adm2_combined <- bind_rows(
#   readRDS(str_glue("{opt$output_dir}/test_postprocessing_test_risk_categories.rds")) %>% 
#     mutate(period = "2011-2015"),
#   readRDS(str_glue("{opt$output_dir}/test_postprocessing_test_risk_categories.rds")) %>% 
#     mutate(period = "2016-2020")
# )  %>% 
#   filter(admin_level == "ADM2") %>% 
#   mutate(country = str_extract(location_period_id, "[A-Z]{3}"))


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
  guides(fill = guide_legend("Risk category"))


ggsave(p_fig5,
       file = "Analysis/output/figures/Figure_5_risk_categories_map_plot.png",
       width = 12,
       height = 8, 
       dpi = 600)

# Figure 6. ADM2 risk categories changes ----------------------------------


adm2_pop_at_risk <- risk_categories_adm2_combined %>% 
  st_drop_geometry() %>% 
  filter(admin_level == "ADM2") %>%
  mutate(country = get_country_from_filename(location_period_id)) %>% 
  {
    x <- .
    bind_rows(x, x %>% mutate(country = "overall")) %>% 
      mutate(country = factor(country, levels = c("overall", sort(unique(x$country)))))
  } %>% 
  group_by(risk_cat, country, period) %>% 
  summarise(pop = sum(pop)) %>% 
  group_by(country, period) %>% 
  complete(risk_cat = get_risk_cat_dict()) %>%
  mutate(risk_cat = factor(risk_cat, levels = get_risk_cat_dict())) %>% 
  mutate(prop = pop/sum(pop, na.rm = T))


p_fig6 <- adm2_pop_at_risk %>% 
  filter(country == "overall") %>% 
  ggplot(aes(x = risk_cat, y = pop, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = .7) +
  theme_bw() +
  scale_fill_manual(values = c("#7B3D91", "#EB9028")) +
  scale_y_continuous(breaks = c(0, 10^(seq(6, 10))),
                     labels = formatC(c(0, 10^(seq(0, 4))),
                                      format = "f",
                                      digits = 0,
                                      big.mark = ","), 
                     minor_breaks = 10^(seq(6, 10, by = .5))) +
  labs(y = "Population in ADM2 units\nin risk category [millions]", x = "Risk category")


ggsave(p_fig6,
       file = "Analysis/output/figures/Figure_6_population_at_risk_AMD2_plot.png",
       width = 8,
       height = 6, 
       dpi = 600)



# # Sandbox final figures ---------------------------------------------------
# # Sandbox to try out figures for the paper
# 
# 
# # Figure with zooms
# # Set bounding boxes of areas of interest for the zooms
# zoom_windows <-list(
#   zoom_west_coast = st_sfc(
#     st_point(c(-15.9, 4.2)), 
#     st_point(c(-7.1, 12.7)),
#     crs = 4326),
#   zoom_lake_chad = st_sfc(
#     st_point(c(11.9, 9.7)), 
#     st_point(c(16.5, 15.5)),
#     crs = 4326),
#   zoom_lake_tangayika = st_sfc(
#     st_point(c(26.9, -8.5)), 
#     st_point(c(31.4, 0)),
#     crs = 4326),
#   zoom_lake_malawi = st_sfc(
#     st_point(c(32.1, -17.5)), 
#     st_point(c(36.5, -9.3)),
#     crs = 4326)
# )
# 
# mai_adm2_combined <- nngeo::st_remove_holes(mai_adm2_combined)
# 
# zoom_plots <- map(zoom_windows, function(x) {
#   p_fig2 +  
#     geom_sf(inherit.aes = F,
#             data = x, alpha = 0, col = "red") +
#     coord_sf(xlim = st_coordinates(x)[,'X'], 
#              ylim = st_coordinates(x)[,'Y'],
#              datum = 4326, 
#              expand = TRUE) +
#     scalebar(data = st_crop(mai_adm2_combined, st_bbox(x)),
#              dist = 75,
#              dist_unit = "km",
#              transform = TRUE,
#              model = "WGS84",
#              facet.var = c("period"),
#              facet.lev = c("2016-2020"),
#              st.size = 2,
#              st.color = "white",
#              st.dist = .025,
#              border.size = .1,
#              box.color = "black"
#     ) +
#     guides(fill = "none")  +
#     theme(strip.text = element_blank(),
#           panel.border = element_rect(color = "black", fill = NA),
#           panel.spacing = unit(2, "lines"))
# })
# 
# p_zooms <- cowplot::plot_grid(plotlist = zoom_plots,
#                               ncol = 1,
#                               rel_heights = c(1.2, 1, 1.2, 1.2)) +
#   theme(plot.background = element_rect(fill = "white"))
# 
# ggsave(p_zooms, filename = "Analysis/output/figures/mai_zooms.png",
#        height = 10, width = 8, dpi = 600)
# 
# 
# zoom_windows_sf <- zoom_windows %>% 
#   map(~st_as_sfc(st_bbox(.))) %>% 
#   bind_rows() %>% 
#   pivot_longer(cols = everything(),
#                names_to = "aoi",
#                values_to = "geom") %>% 
#   st_set_geometry("geom")
# 
# p_fig2_v2 <- output_plot_map(sf_obj = mai_adm2_combined,
#                              lakes_sf = lakes_sf,
#                              all_countries_sf = afr_sf,
#                              fill_var = "log10_rate_per_1e5",
#                              fill_color_scale_type = "rates") +
#   geom_sf(data = zoom_windows_sf,
#           alpha = 0,
#           color = "red",
#           inherit.aes = F,
#           linewidth = .8) +
#   facet_wrap(~ period) +
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size = 15),
#         panel.spacing = unit(9, "lines"),
#         legend.position = "right") +
#   guides(fill = guide_colorbar("Incidence rate\n[cases/100,000 people]")) +
#   coord_sf(xlim = c(NA, NA), ylim = c(NA, NA))
# 
# 
# p_fig2_v2_comb <- cowplot::ggdraw(p_fig2_v2) +
#   cowplot::draw_plot(p_zooms +
#                        theme(plot.background = element_blank(),
#                              panel.background = element_blank()), 
#                      x = .37,
#                      y = .16,
#                      width = .16,
#                      height = .68,
#                      hjust = 0,
#                      vjust = 0)
# 
# ggsave(p_fig2_v2_comb,
#        file = "Analysis/output/figures/Figure_2_MAI_admin2_v2.png",
#        width = 18,
#        height = 10, 
#        dpi = 600)
# 
# 
# 
# # Spatial autocorrelation of changes
# library(rgeoda)
# mai_changes_sf <- inner_join(adm2_sf,
#                              mai_adm2_changes) %>% 
#   filter(!is.na(log10_rate_ratio))
# 
# queen_w <- queen_weights(mai_changes_sf)
# 
# lisa_fit <- local_moran(queen_w, mai_changes_sf %>% 
#                           st_drop_geometry() %>%
#                           ungroup() %>% 
#                           select(log10_rate_ratio), 
#                         permutations = 9999,
#                         significance_cutoff = 0.025)
# 
# p.adjust(lisa_pvalues(lisa_fit), "fdr") >= 0.0025
# 
# sum(lisa_pvalues(lisa_fit) < 0.025)
# sum(p.adjust(lisa_pvalues(lisa_fit), "fdr") <= 0.025)
# 
# mai_changes_sf$lisa <- lisa_values(lisa_fit)
# mai_changes_sf$lisa_cats <- factor(lisa_labels(lisa_fit)[1+lisa_clusters(lisa_fit)],
#                                    levels = lisa_labels(lisa_fit))
# 
# mai_changes_sf$lisa_cats[p.adjust(lisa_pvalues(lisa_fit), "fdr") >= 0.025] <- lisa_labels(lisa_fit)[1]
# sf_use_s2(FALSE)
# 
# p_lisa_clusters <- output_plot_map(mai_changes_sf,
#                                    all_countries_sf = all_country_sf,
#                                    lakes_sf = lakes_sf,
#                                    fill_var = "lisa_cats",
#                                    fill_color_scale_type = "lisa cluster")
# 
# ggsave(p_lisa_clusters,
#        file = "Analysis/output/figures/Figure_MAI_ratio_lisa_clusters.png",
#        width = 10,
#        height = 8, 
#        dpi = 600)
# 
# 
# mai_changes_regions <- mai_changes_sf %>% 
#   group_by(lisa_cats) %>% 
#   summarise(n = n()) %>% 
#   # Remove wholes 
#   nngeo::st_remove_holes() %>% 
#   # Remove small islands 
#   ms_filter_islands(min_area = 1e9) 
# 
# p_lisa_regions <- p_fig3_2 + 
#   geom_sf(data = mai_changes_regions %>% 
#             filter(lisa_cats != "Not significant"),
#           inherit.aes = F,
#           aes(color = lisa_cats),
#           linewidth = .6,
#           alpha = 0) +
#   scale_color_manual(values = lisa_cat_colors[-1])
# 
# ggsave(p_lisa_regions,
#        file = "Analysis/output/figures/Figure_MAI_ratio_admin2_with_clusters.png",
#        width = 10,
#        height = 8, 
#        dpi = 600)
