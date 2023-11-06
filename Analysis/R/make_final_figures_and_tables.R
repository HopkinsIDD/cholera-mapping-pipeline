# This script combines postprocessed outputs and makes final figures


# Preamble ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(cmdstanr)
library(optparse)
library(rmapshaper)
library(taxdat)
library(cowplot)

# User-supplied options
opt_list <- list(
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", help = "Output directory with postprocessed objects"),
  make_option(opt_str = c("-x", "--prefix_p1"), type = "character",
              default = "postprocessing_test_2011_2015", help = "Prefix of output files for first period"),
  make_option(opt_str = c("-y", "--prefix_p2"), type = "character",
              default = "postprocessing_test_2016_2020", help = "Prefix of output files for second period"),
  make_option(opt_str = c("-p", "--out_prefix"), type = "character",
              default = "postprocessing_test", help = "Prefix for output figures and tables"),
  make_option(opt_str = c("-d", "--out_dir"), type = "character",
              default = "./Analysis/output/figures", help = "Output directory for figures")
)

opt <- parse_args(OptionParser(option_list = opt_list))


prefix_list <- list(
  "2011-2015" = opt$prefix_p1,
  "2016-2020" = opt$prefix_p2
)

# Functions ---------------------------------------------------------------

#' combine_period_output
#'
#' @param prefix_list 
#' @param output_name 
#' @param output_dir 
#'
#' @return
#' @export
#'
#' @examples
combine_period_output <- function(prefix_list,
                                  output_name,
                                  output_dir) {
  
  res <- map_df(1:length(prefix_list), function(j) {
    readRDS(str_glue("{output_dir}/{prefix_list[j]}_{output_name}.rds")) %>% 
      ungroup() %>% 
      mutate(period = names(prefix_list)[j])
  })
  
  res
}

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

# Load data ---------------------------------------------------------------

# Total number of cases by region
cases_by_region <- combine_period_output(prefix_list = prefix_list,
                                         output_name = "mai_cases_by_region",
                                         output_dir = opt$output_dir) %>% 
  mutate(AFRO_region = str_remove(variable, "country_cases_") %>% 
           str_replace("_", " ") %>% 
           str_to_title(),
         AFRO_region = factor(AFRO_region, 
                              levels = get_AFRO_region_levels()))

# Mean annual incidence rates by region
rates_by_region <- combine_period_output(prefix_list = prefix_list,
                                         output_name = "mai_rates_by_region",
                                         output_dir = opt$output_dir) %>% 
  mutate(AFRO_region = str_remove(variable, "country_rates_"))  %>% 
  mutate(AFRO_region = str_remove(variable, "country_cases_") %>% 
           str_replace("_", " ") %>% 
           str_to_title(),
         AFRO_region = factor(AFRO_region, 
                              levels = get_AFRO_region_levels()))

# Overall rates
rates_overall <- combine_period_output(prefix_list = prefix_list,
                                         output_name = "mai_rates_all",
                                         output_dir = opt$output_dir)

# Gridded cases
grid_cases <- combine_period_output(prefix_list = prefix_list,
                                    output_name = "mai_grid_cases",
                                    output_dir = opt$output_dir)


# Number of people living in different risk categories
risk_pop_adm2 <- combine_period_output(prefix_list = prefix_list,
                                       output_name = "risk_categories",
                                       output_dir = opt$output_dir) %>% 
  filter(admin_level == "ADM2") %>% 
  get_AFRO_region(ctry_col = "country")  %>% 
  mutate(AFRO_region = factor(AFRO_region, 
                              levels = get_AFRO_region_levels()))


# Mean annual incidence rates at ADM2 level
mai_adm_all <- combine_period_output(prefix_list = prefix_list,
                                     output_name = "mai",
                                     output_dir = opt$output_dir) %>% 
  mutate(run_id = str_c(country, period, sep = "-"),
         log10_rate_per_1e5 = log10(mean * 1e5))

# Get unique spatial locations
u_space_sf <- mai_adm_all %>% 
  group_by(location_period_id) %>%
  slice(1)

# Combine results with no-w runs
mai_adm <- bind_rows(
  mai_adm_all %>% filter(admin_level == "ADM0", run_id %in% get_no_w_runs()),
  mai_adm_all %>% filter(admin_level == "ADM2", !(run_id %in% get_no_w_runs()))
)

# Compute change map
mai_change_adm <- inner_join(u_space_sf,
                             compute_rate_changes(mai_adm)) %>% 
  ungroup()

# Compute changes at ADM0 level
mai_adm0_changes <-  mai_adm_all %>% 
  filter(admin_level == "ADM0") %>% 
  compute_rate_changes() %>% 
  ungroup() %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  mutate(AFRO_region = factor(AFRO_region, 
                              levels = get_AFRO_region_levels()))

# Compute changes for regions as well
mai_region_changes <- rates_by_region %>% 
  mutate(country = variable) %>% 
  rename(location_period_id = variable) %>% 
  compute_rate_changes() %>% 
  mutate(AFRO_region = str_remove(country, "country_rates_") %>% 
           str_replace("_", " ") %>% 
           str_to_title(),
         AFRO_region = factor(AFRO_region, 
                              levels = get_AFRO_region_levels()))

# Changes at the continent level
mai_all_changes <- rates_overall %>% 
  mutate(country = variable) %>% 
  rename(location_period_id = variable) %>% 
  compute_rate_changes() %>% 
  mutate(country = "SSA")
  

# Get intended runs
intended_runs <- get_intended_runs()

# Spatial data
data(afr_sf, package = "taxdat")

afr_sf <- afr_sf %>% 
  janitor::clean_names() %>% 
  mutate(country_name = country,
         intended_run = country %in% intended_runs$isocode) %>% 
  filter(country_name != "YEM") %>% 
  st_crop(st_bbox(st_sfc(
    st_point(c(-18.8, 36.6)), 
    st_point(c(52.2, -36.5)),
    crs = 4326))) %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  mutate(AFRO_region = factor(AFRO_region, 
                              levels = get_AFRO_region_levels()))

# Lakes for plots
lakes_sf <- get_lakes()

# Figure 1: cases ---------------------------------------------------------

# Figure 1A: gridded cases by time period
p_fig1A <- output_plot_map(sf_obj = grid_cases %>% 
                             mutate(log10_cases = log10(mean)),
                           lakes_sf = lakes_sf,
                           all_countries_sf = afr_sf,
                           fill_var = "log10_cases",
                           fill_color_scale_type = "cases",
                           cholera_dir = opt$cholera_dir) +
  facet_wrap(~ period) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.1, .3)) +
  guides(fill = guide_colorbar("Mean annual incidence \n[cases/year]"))

ggsave(p_fig1A,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1A.png"),
       width = 10,
       height = 6, 
       dpi = 300)

# Fig. 1C: cases by region and time period
p_fig1C <- cases_by_region %>% 
  ggplot(aes(x = mean, y = period, fill = AFRO_region)) +
  geom_bar(stat = "identity")  +
  theme_bw() +
  labs(x = "Mean annual cholera incidence [cases/year]", y = "Time period")

ggsave(plot = p_fig1C,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1C.png"),
       width = 8,
       height = 2.5,
       dpi = 300)


# Figure 1
P_fig1 <- cowplot::plot_grid(
  p_fig1A,
  p_fig1C +
    theme(plot.margin = unit(c(1, 5, 1, 5), "lines")),
  nrow = 2,
  labels = "auto",
  rel_heights = c(1, .3)
)  + theme(panel.background = element_rect(fill = "white"))

ggsave(plot = P_fig1,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1.png"),
       width = 10,
       height = 7,
       dpi = 300)


# Figure 2: changes between periods ------------------------------------------

# Figure 2A: ADM2 rate maps
p_fig2A <- output_plot_map(sf_obj = mai_adm,
                           lakes_sf = lakes_sf,
                           all_countries_sf = afr_sf,
                           fill_var = "log10_rate_per_1e5",
                           fill_color_scale_type = "rates") +
  facet_wrap(~ period) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.1, .3)) +
  guides(fill = guide_colorbar("Mean annual incidence rate\n[cases/100,000 people]"))

ggsave(plot = p_fig2A,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2A.png"),
       width = 10,
       height = 6,
       dpi = 300)

# Figure 2b: Rate change map
p_fig2B <- output_plot_map(sf_obj = mai_change_adm,
                           lakes_sf = lakes_sf,
                           all_countries_sf = afr_sf,
                           fill_var = "log10_rate_ratio",
                           fill_color_scale_type = "ratio") +
  theme(legend.position = "right") +
  guides(fill = guide_colorbar("Ratio of incidence rates\n[2016-2020/2011-2015]")) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.2, .3))

ggsave(p_fig2B,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2B.png"),
       width = 7,
       height = 6, 
       dpi = 600)


# Figure 3C: national-level scatterplot
p_fig2C_scatter <- mai_adm0_changes %>% 
  ggplot(aes(x = log10(`2011-2015`*1e5), 
             y =  log10(`2016-2020`*1e5), 
             col = AFRO_region)) +
  geom_abline(lty = 2, lwd = .2) +
  geom_point() +
  geom_point(data = mai_region_changes,
             pch = 5) +
  ggrepel::geom_label_repel(data = 
                              bind_rows(
                                mai_region_changes %>% 
                                  mutate(country = AFRO_region), 
                                mai_adm0_changes 
                              ),
                            aes(label = country),
                            min.segment.length = 0,
                            max.overlaps = Inf) +
  geom_point(data = mai_all_changes,
             pch = 3,
             size = 4,
             color = "black") +
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

p_fig_2C_regions <- ggplot(afr_sf, aes(fill = AFRO_region)) +
  geom_sf() +
  taxdat::map_theme() +
  guides(fill = "none") +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank())

p_fig2C <- ggdraw() +
  draw_plot(p_fig2C_scatter) +
  draw_plot(p_fig_2C_regions, x = 0.765, y = .7, width = .27, height = .27)


ggsave(p_fig2C,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2C.png"),
       width = 10,
       height = 7, 
       dpi = 600)


# Figure 2
p_fig2 <- plot_grid(
  p_fig2A,
  plot_grid(p_fig2B,
            p_fig2C  +
              theme(plot.margin = unit(c(2, 1, 2, 1), "lines")),
            nrow = 1,
            labels = c("b", "c"),
            rel_widths = c(1, 1)),
  nrow = 2,
  labels = c("a", NULL),
  rel_heights = c(1, 1)
)  + theme(panel.background = element_rect(fill = "white"))

ggsave(plot = p_fig2,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2.png"),
       width = 12,
       height = 10,
       dpi = 300)


# Figure 3: population at risk --------------------------------------------

# Fig. 3A: Population at risk map
p_fig3A <- output_plot_map(sf_obj = risk_pop_adm2 %>% 
                             filter(period == "2016-2020"), 
                lakes_sf = lakes_sf,
                all_countries_sf = afr_sf,
                fill_var = "risk_cat",
                fill_color_scale_type = "risk category") +
  scale_fill_viridis_d(direction = -1) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.2, .3)) +
  guides(fill = guide_legend("Risk category"))


ggsave(p_fig3A,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3A.png"),
       width = 7,
       height = 6, 
       dpi = 600)

# Fig. 3B: People per risk category
# This is at the ADM2 level per country
p_fig3B <- risk_pop_adm2 %>% 
  bind_rows(risk_pop_adm2 %>% mutate(AFRO_region = "all")) %>%
  st_drop_geometry() %>% 
  mutate(AFRO_region = factor(AFRO_region, 
                              levels = c("all", get_AFRO_region_levels()))) %>% 
  group_by(AFRO_region, risk_cat, period) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  complete(AFRO_region,
           risk_cat,
           period) %>% 
  ggplot(aes(x = risk_cat, y = pop, fill = AFRO_region, alpha = period, group = period)) +
  geom_bar(stat = "identity", position = position_dodge(.5), width = .4) +
  theme_bw() +
  facet_wrap(~AFRO_region) +
  labs(x = "Incidence risk category", y = "ADM2 population at risk") +
  scale_alpha_manual(values = c(.3, 1))

ggsave(plot = p_fig3B,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3B.png"),
       width = 12,
       height = 7,
       dpi = 300)


# Figure 2
p_fig3 <- plot_grid(
  p_fig3A,
  p_fig3B +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)),
  nrow = 1,
  labels = "auto",
  rel_widths = c(1, 1.5)
)  + theme(panel.background = element_rect(fill = "white"))

ggsave(plot = p_fig3,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3.png"),
       width = 13,
       height = 5,
       dpi = 300)
