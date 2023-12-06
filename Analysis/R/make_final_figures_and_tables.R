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
              default = "./Analysis/output/processed_outputs/", 
              help = "Output directory with postprocessed objects"),
  make_option(opt_str = c("-x", "--prefix_p1"), type = "character",
              default = "postprocessing_test_2011_2015", 
              help = "Prefix of output files for first period"),
  make_option(opt_str = c("-y", "--prefix_p2"), type = "character",
              default = "postprocessing_test_2016_2020", 
              help = "Prefix of output files for second period"),
  make_option(opt_str = c("-p", "--out_prefix"), type = "character",
              default = "postprocessing", 
              help = "Prefix for output figures and tables"),
  make_option(opt_str = c("-d", "--out_dir"), type = "character",
              default = "./Analysis/output/figures",
              help = "Output directory for figures"),
  make_option(opt_str = c("-f", "--bundle_filename"), type = "character",
              default = "./Analysis/output/data_bundle_for_figures_test.rdata", 
              help = "Data bundle to avoid re-processing"),
  make_option(opt_str = c("-r", "--redo"), type = "logical",
              default = FALSE, 
              help = "Redo re-processing")
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

# Compute change statistics (could package into function)
merge_ratio_draws <- function(df1, df2) {
  u_lps <- intersect(unique(df1$location_period_id), unique(df2$location_period_id))
  n_variables <- length(u_lps)
  u_draws <- unique(df1$.draw)
  n_draws <- length(u_draws)
  ratios <- array(NA, dim = c(n_variables, n_draws, n_draws))
  
  # Compute ratio samples
  for (i in 1:n_variables) {
    ind2 <- df2$location_period_id == u_lps[i]
    ind <- df1$location_period_id == u_lps[i]
    for (j in 1:n_draws) {
      ratios[i, j, ] <- df2$value[ind2 & df2$.draw == u_draws[j]]/
        df1$value[ind]
    }
    cat("Done", i, "/", n_variables, "\n")
  }
  
  ratio_stats <- array(NA, dim = c(n_variables, 3))
  colnames(ratio_stats) <- c("mean", "q2.5", "q97.5")
  
  # Compute ratio stats
  for (i in 1:n_variables) {
    ratio_stats[i, "mean"] <- mean(ratios[i, , ])
    ratio_stats[i, "q2.5"] <- quantile(ratios[i, , ], 0.025)
    ratio_stats[i, "q97.5"] <- quantile(ratios[i, , ], 0.975)
  }
  
  as_tibble(ratio_stats) %>% 
    mutate(location_period_id = u_lps)
}



# Second post-processing step ---------------------------------------------

if (opt$redo | !file.exists(opt$bundle_filename)) {
  
  ## Cases by region and continent ---------------------------------------
  cases_continent <- combine_period_output(prefix_list = prefix_list,
                                           output_name = "mai_simulated_cases_all",
                                           output_dir = opt$output_dir)
  
  cases_by_region <- combine_period_output(prefix_list = prefix_list,
                                           output_name = "mai_cases_by_region",
                                           output_dir = opt$output_dir) %>% 
    mutate(AFRO_region = str_remove(variable, "country_cases_") %>% 
             str_replace("_", " ") %>% 
             str_to_title(),
           AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels()))
  
  ## Rates by region and continent ---------------------------------------
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
  
  ## Gridded cases ---------------------------------------
  grid_cases <- combine_period_output(prefix_list = prefix_list,
                                      output_name = "mai_grid_cases",
                                      output_dir = opt$output_dir)
  
  
  ## Population at risk ---------------------------------------
  
  # Number of people living in different risk categories
  risk_pop_adm2 <- combine_period_output(prefix_list = prefix_list,
                                         output_name = "risk_categories",
                                         output_dir = opt$output_dir) %>% 
    filter(admin_level == "ADM2") %>% 
    get_AFRO_region(ctry_col = "country")  %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels())) %>% 
    st_drop_geometry()
  
  
  
  # Number of people living in different risk categories using the "Lancet method"
  pop_at_risk <- combine_period_output(prefix_list = prefix_list,
                                       output_name = "pop_at_risk",
                                       output_dir = opt$output_dir) %>% 
    filter(admin_level == "ADM2") %>% 
    get_AFRO_region(ctry_col = "country")  %>% 
    mutate(AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels()))
  
  
  unpack_pop_at_risk <- function(df) {
    risk_cat_map <- get_risk_cat_dict()
    names(risk_cat_map) <- janitor::make_clean_names(risk_cat_map) %>% 
      str_remove("x")
    
    
    df %>% 
      mutate(risk_cat = str_extract(variable, str_c(rev(names(risk_cat_map)), collapse = "|")),
             risk_cat = risk_cat_map[risk_cat] %>% factor(levels = risk_cat_map),
             admin_level = str_c("ADM", str_extract(variable, "(?<=adm)[0-9]+"))
      )
  }
  
  # Population at risk by WHO-AFRO region
  pop_at_risk_regions <- combine_period_output(prefix_list = prefix_list,
                                               output_name = "pop_at_risk_by_region",
                                               output_dir = opt$output_dir) %>% 
    mutate(AFRO_region = str_extract(variable, "(?<=tot_pop_risk_)(.)*(?=_adm)") %>% 
             str_replace("_", " ") %>% 
             str_to_title(),
           AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels())) %>% 
    unpack_pop_at_risk()
  
  
  # Total population at risk
  pop_at_risk_all <- combine_period_output(prefix_list = prefix_list,
                                           output_name = "pop_at_risk_all",
                                           output_dir = opt$output_dir)  %>% 
    unpack_pop_at_risk()
  
  ## ADM2 level stats ---------------------------------------
  # Mean annual incidence rates at ADM2 level
  mai_adm_all <- combine_period_output(prefix_list = prefix_list,
                                       output_name = "mai",
                                       output_dir = opt$output_dir) %>% 
    mutate(run_id = str_c(country, period, sep = "-"),
           log10_rate_per_1e5 = log10(mean * 1e5))
  
  # Get unique spatial locations
  u_space_sf <- mai_adm_all %>% 
    group_by(location_period_id) %>%
    slice(1) %>% 
    ungroup() %>% 
    select(shapeName, admin_level, country, shp_id, location_period_id)
  
  mai_adm_all <- mai_adm_all %>% 
    st_drop_geometry() %>% 
    as_tibble()
  
  # Combine results with no-w runs
  mai_adm <- bind_rows(
    mai_adm_all %>% filter(admin_level == "ADM0", run_id %in% get_no_w_runs()),
    mai_adm_all %>% filter(admin_level == "ADM2", !(run_id %in% get_no_w_runs()))
  ) %>% 
    st_drop_geometry() %>% 
    as_tibble()
  
  # Compute change map
  mai_change_adm <- compute_rate_changes(mai_adm)
  
  ## ADM2 mai ratio stats ---------------------------------------
  # Random draws
  random_draws <- sample(1:1000, 100)
  
  mai_draws_p1 <- readRDS(str_glue("{opt$output_dir}/{prefix_list[1]}_mai_draws.rds")) %>%
    ungroup() %>% 
    select(.draw, location_period_id, value, country) %>% 
    filter(.draw %in% random_draws)
  
  mai_draws_p2 <- readRDS(str_glue("{opt$output_dir}/{prefix_list[2]}_mai_draws.rds")) %>%
    ungroup() %>% 
    select(.draw, location_period_id, value, country) %>% 
    filter(.draw %in% random_draws)
  
  mai_change_stats <- map_df(unique(mai_draws_p1$country), function(x) {
    cat("--- ", x, "\n")
    
    merge_ratio_draws(
      df1 = filter(mai_draws_p1, country == x),
      df2 = filter(mai_draws_p2, country == x)
    ) %>% 
      mutate(country = x)
  })
  
  mai_change_stats <- mai_change_stats %>%
    inner_join(u_space_sf %>% 
                 select(country, location_period_id, shp_id, admin_level), .)
  
  saveRDS(mai_change_stats, file = str_glue("{opt$output_dir}/mai_ratio_stats.rds"))
  
  
  mai_change_stats <- mai_change_stats %>% 
    st_drop_geometry() %>% 
    as_tibble()
  
  ## Changes between periods ---------------------------------------
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
  
  
  # Combine mai change data
  combined_mai_changes <- bind_rows(
    # AFRO regions
    mai_region_changes %>% 
      mutate(country = AFRO_region,
             region = AFRO_region), 
    # ADM2 level
    mai_adm0_changes %>% 
      mutate(region = AFRO_region),
    # Continent level
    mai_all_changes %>% 
      mutate(country = "SSA", 
             region = "SSA")
  ) %>% 
    mutate(
      # !! change rate values to 1e-1/100'000 for display
      across(
        c("2011-2015", "2016-2020"), 
        function(x) {
          x[x < 1e-6] <- 1e-6
          x
        }),
      admin_level = case_when(
        str_detect(location_period_id, "country") ~ "region",
        location_period_id == "tot" ~ "continent",
        TRUE ~ "ADM2"),
      admin_level = factor(admin_level, levels = c("continent", "region", "ADM2")),
      region = factor(region, levels = c("SSA", get_AFRO_region_levels()))
    )
  
  
  ## Get intended runs ---------------------------------------
  intended_runs <- get_intended_runs()
  
  ## Spatial data  ---------------------------------------
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
  
  
  ## Generated observations ------
  gen_obs <- combine_period_output(prefix_list = prefix_list,
                                   output_name = "gen_obs",
                                   output_dir = opt$output_dir) %>% 
    mutate(admin_level = str_c("ADM", admin_level)) %>% 
    get_AFRO_region(ctry_col = "country") %>% 
    mutate(AFRO_region = factor(AFRO_region, levels = get_AFRO_region_levels()))
  
  ## Save data  ---------------------------------------
  save(list = ls(), file = opt$bundle_filename)
  
} else {
  cat("---- Loading pre-computed data. \n")
  load(opt$bundle_filename)
}

# Figure 1: cases ---------------------------------------------------------

## Figure 1A: gridded cases by time period --------------------------------

# Gridded maps of cases
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

# Save
ggsave(p_fig1A,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1A.png"),
       width = 10,
       height = 6, 
       dpi = 300)

## Figure 1B: cases by region and time period --------------------------------
# Horizontal barplot of cases by region
p_fig1B <- cases_by_region %>% 
  mutate(AFRO_region = factor(AFRO_region, levels = get_AFRO_region_levels())) %>% 
  ggplot(aes(x = mean, y = period, fill = AFRO_region)) +
  geom_bar(stat = "identity")  +
  geom_errorbarh(data = cases_continent,
                 inherit.aes = F,
                 aes(xmin = q2.5, xmax = q97.5, y = period), 
                 height = .2) +
  scale_x_continuous(
    labels = function(x) {
      formatC(x, digits = 0, big.mark = "'", format = "f")
    }) +
  scale_fill_manual(values = colors_afro_regions()) +
  theme_bw() +
  theme(legend.key.size = unit(.2, units = "in")) +
  labs(x = "Mean annual cholera incidence [cases/year]", 
       y = "Time period",
       fill = "WHO region")

# Save figure
ggsave(plot = p_fig1B,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1B.png"),
       width = 8,
       height = 2.5,
       dpi = 300)

# Map of WHO regions
p_fig_BC_regions <- ggplot(afr_sf, aes(fill = AFRO_region)) +
  geom_sf() +
  taxdat::map_theme() +
  guides(fill = "none") +
  scale_fill_manual(values = colors_afro_regions()) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank())


## Assemble Figure 1 ---------

p_fig1 <- cowplot::plot_grid(
  p_fig1A,
  p_fig1B +
    theme(plot.margin = unit(c(2, 9, 1, 3), "lines")),
  nrow = 2,
  labels = "auto",
  rel_heights = c(1, .3)
)  + theme(panel.background = element_rect(fill = "white"))

p_fig1_v2 <- ggdraw() +
  draw_plot(p_fig1) +
  draw_plot(p_fig_BC_regions, x = 0.76, y = .02, width = .23, height = .23)

# Save
ggsave(plot = p_fig1_v2,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_1.png"),
       width = 10,
       height = 7.5,
       dpi = 300)


# Figure 2: changes between periods ------------------------------------------

## Figure 2A: national-level scatterplot  ---------
p_fig2A <- combined_mai_changes %>% 
  ggplot(aes(x = log10(`2011-2015`*1e5), 
             y =  log10(`2016-2020`*1e5), 
             col = region)) +
  geom_abline(lty = 2, lwd = .2) +
  geom_point(aes(pch = admin_level, alpha = admin_level), size = 2) +
  ggrepel::geom_label_repel(
    aes(label = country, size = admin_level, alpha = admin_level),
    min.segment.length = 0,
    nudge_x = 0,
    # nudge_y = .1,
    max.overlaps = Inf, 
    xlim = c(-1, 2.2),
    ylim = c(-1, 2.2)) +
  scale_size_manual(values = c(6, 4, 2.5)) +
  scale_shape_manual(values = c(15, 17, 16)) +
  scale_alpha_manual(values = c(1, 1, .75)) +
  scale_color_manual(values = c("black", colors_afro_regions())) +
  theme_bw() +
  guides(color = guide_legend("WHO regions")) +
  scale_x_continuous(limits = c(-1.1, 2.3),
                     breaks = seq(-1, 2),
                     labels = formatC(10^(seq(-1, 2)),
                                      digits = 1,
                                      format = "fg", 
                                      big.mark = ",")) +
  scale_y_continuous(limits = c(-1.1, 2.3),
                     breaks = seq(-1, 2),
                     labels = formatC(10^(seq(-1, 2)),
                                      digits = 1,
                                      format = "fg", 
                                      big.mark = ",")) +
  labs(x = "Incidence rate 2011-2015\n[cases per 100,000/year]",
       y = "Incidence rate 2016-2020\n[cases per 100,000/year]") +
  guides(color = "none", size = "none", shape = "none", alpha = "none")


# Save
ggsave(p_fig2A,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2A.png"),
       width = 8,
       height = 7, 
       dpi = 150)


## Figure 2B: rate ratio maps ---------

# Compute significant changes in terms of quantiles of rate ratios
mai_adm2_change_stats <- mai_change_stats %>% 
  filter(admin_level == "ADM2") %>% 
  mutate(change_direction = case_when(
    q2.5 > 1 ~ "increase",
    q97.5 < 1 ~ "decrease",
    T ~ "no change"
  ))#,
# change_direction = factor(change_direction, levels = c("increase", "no change", "decrease")))

#  Rate change map
p_fig2B <- mai_change_adm %>% 
  inner_join(u_space_sf, .) %>%
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "log10_rate_ratio",
                  fill_color_scale_type = "ratio") +
  # Add the significance information
  geom_sf(data = mai_adm2_change_stats  %>% 
            inner_join(u_space_sf, .) %>% 
            filter(change_direction == "no change"),
          inherit.aes = F,
          aes(color = change_direction),
          alpha = 0,
          lwd = .05) +
  geom_sf(data = mai_adm2_change_stats  %>% 
            inner_join(u_space_sf, .) %>% 
            filter(change_direction != "no change"),
          inherit.aes = F,
          aes(color = change_direction),
          alpha = 0,
          lwd = .05) +
  theme(legend.position = "right") +
  scale_color_manual(values = c("blue", "red", "darkgray")) +
  guides(fill = guide_colorbar("Ratio of incidence rates\n[2016-2020/2011-2015]"),
         color = guide_legend("Change significance")) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.2, .3))

# Save
ggsave(p_fig2B,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2B.png"),
       width = 7,
       height = 6, 
       dpi = 150)


## Assemble Figure 2B ------

p_fig2 <- plot_grid(
  p_fig2A +
    theme(plot.margin = unit(c(2.5, 1.5, 2.5, 1.5), "lines")),
  p_fig2B +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
  nrow = 1,
  labels = "auto"#,
)  + theme(panel.background = element_rect(fill = "white"))


# Save
ggsave(plot = p_fig2,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2.png"),
       width = 14,
       height = 6,
       dpi = 300)


# Figure 3: population at risk --------------------------------------------

## Fig. 3A: ADM2 level risk category map --------
p_fig3A <- risk_pop_adm2 %>% 
  filter(period == "2016-2020") %>% 
  inner_join(u_space_sf, .) %>% 
  output_plot_map(sf_obj = ., 
                  lakes_sf = lakes_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "risk_cat",
                  fill_color_scale_type = "risk category") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = c(.2, .3)) +
  guides(fill = guide_legend("Risk category"))


# Save
ggsave(p_fig3A,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3A.png"),
       width = 7,
       height = 6, 
       dpi = 300)


## Fig. 3B: People per risk category --------

# Uncertainty bounds for totals
risk_pop_all <- pop_at_risk_all %>% 
  filter(period == "2016-2020", 
         admin_level == "ADM2",
         risk_cat != "<1") %>% 
  select(risk_cat, mean, q2.5, q97.5)

# Values by AFRO region
risk_pop_regions <- pop_at_risk_regions %>% 
  filter(period == "2016-2020", 
         admin_level == "ADM2",
         risk_cat != "<1") %>% 
  select(AFRO_region, risk_cat, mean, q2.5, q97.5)

p_fig3B <- risk_pop_regions %>%
  ggplot(aes(y = risk_cat, x = mean)) +
  geom_bar(aes(fill = AFRO_region), stat = "identity", width = .5) +
  geom_errorbar(data = risk_pop_all,
                inherit.aes = F,
                aes(xmin = q2.5, xmax = q97.5, y = risk_cat), width = 0.2) +
  geom_point(data = risk_pop_all, aes(x = mean)) +
  theme_bw() +
  scale_fill_manual(values = colors_afro_regions()) +
  scale_x_continuous(labels = function(x) {formatC(x/1e6)}) +
  labs(y = "Incidence risk category", x = "ADM2 population at risk [millions]")


# Save
ggsave(plot = p_fig3B,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3B.png"),
       width = 12,
       height = 7,
       dpi = 300)


## Fig. 3C: Change in risk categories ----
endemicity_df_v2 <- risk_pop_adm2 %>% 
  mutate(high_risk = risk_cat %in% get_risk_cat_dict()[3:6],
         low_risk = risk_cat %in% get_risk_cat_dict()[1]) %>% 
  group_by(country, location_period_id) %>% 
  summarise(
    endemicity = case_when(
      sum(high_risk) == 2 ~ "high-both",
      sum(low_risk) == 2 ~ "low-both",
      sum(high_risk) == 1 ~ "high-either",
      T ~ "mix"
    ),
    pop = max(pop)
  ) %>% 
  mutate(endemicity = factor(endemicity, levels = c("high-both", "high-either",
                                                    "mix", "low-both")))  

# Figure
p_fig3C <- endemicity_df_v2 %>% 
  inner_join(u_space_sf, .) %>% 
  output_plot_map(sf_obj = .,
                  lakes_sf = lakes_sf,
                  all_countries_sf = afr_sf,
                  fill_var = "endemicity",
                  fill_color_scale_type = "endemicity",
                  border_width = .03) +
  theme(legend.position = c(.2, .3)) +
  guides(fill = guide_legend("'Endemicity'"))

# Save
ggsave(p_fig3C,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3C_risk_cat.png"),
       width = 12,
       height = 10, 
       dpi = 150)


## Fig. 3D: Change in risk categories barplot --------

p_fig3D <- endemicity_df_v2 %>% 
  ungroup() %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  mutate(AFRO_region = factor(AFRO_region, 
                              levels = get_AFRO_region_levels())) %>% 
  group_by(AFRO_region, endemicity) %>% 
  summarise(pop = sum(pop)) %>% 
  ggplot(aes(y = endemicity, x = pop, fill = AFRO_region)) +
  geom_bar(stat = "identity", width = .5) +
  theme_bw() +
  scale_fill_manual(values = colors_afro_regions()) +
  scale_x_continuous(labels = function(x) {formatC(x/1e6)}) +
  scale_y_discrete(limits = rev) +
  labs(y = "Change in risk category", x = "ADM2 population at risk [millions]")


# Save
ggsave(plot = p_fig3D,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3D.png"),
       width = 12,
       height = 7,
       dpi = 300)

## Assemble Figure 3 ----

p_fig3 <- plot_grid(
  p_fig3A +
    theme(plot.margin = unit(c(0, 0, 0, -3), units = "lines")),
  p_fig3B +
    theme(plot.margin = unit(c(2, 1, 2, 2), units = "lines"),
          legend.position = c(.75, .6)),
  p_fig3C +
    theme(plot.margin = unit(c(0, 0, 0, -3), units = "lines")),
  p_fig3D +
    guides(fill = "none") +
    theme(plot.margin = unit(c(2, 1, 2, 2), units = "lines")),
  nrow = 2,
  labels = "auto",
  rel_widths = c(1.3, 1),
  align = "v",
  axis = "lr") +
  theme(panel.background = element_rect(fill = "white"))


# Save
ggsave(plot = p_fig3,
       filename = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_3.png"),
       width = 12,
       height = 10,
       dpi = 600)


# Supplementary figures ---------------------------------------------------


# Fraction by categories for supplement
p_endemicity_v2 <- endemicity_df_v2  %>%
  group_by(country) %>% 
  complete(endemicity = unique(endemicity_df_v2$endemicity)) %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  group_by(AFRO_region, country, endemicity) %>%
  summarise(n = sum(!is.na(location_period_id)),
            pop = sum(pop[!is.na(location_period_id)])) %>% 
  group_by(AFRO_region, country) %>%
  mutate(frac = pop/sum(pop)) %>% 
  group_by(country) %>% 
  mutate(
    frac_other = frac[endemicity == "mix"],
    frac_high = sum(frac[endemicity %in% c("high-both", "high-either")]),
    frac_low = sum(frac[endemicity %in% c("low-both")])
  ) %>% 
  ungroup() %>% 
  mutate(endemicity = forcats::fct_rev(endemicity),
         country = factor(country, levels = unique(country[order(frac_high, frac_other)])),
  ) %>% 
  ggplot(aes(y = country, x = frac, fill = endemicity)) +
  geom_bar(stat = "identity") +
  facet_grid(AFRO_region ~., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = rev(taxdat:::colors_endemicity())) +
  theme_bw() +
  labs(x = "Fraction of population")

ggsave(p_endemicity_v2,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_fig_2C_endemicity_risk_cat_10.png"),
       width = 6,
       height = 7, 
       dpi = 150)

## Model fit ----


# Scatter plot of ADM0 level units
p_data_adm0_scatter <- gen_obs %>%
  filter(admin_level == "ADM0", censoring == "full") %>% 
  group_by(country, period, loctime_comb) %>% 
  mutate(mean_obs = mean(observation)) %>% 
  slice(1) %>% 
  ggplot(aes(x = mean_obs+1, y = mean+1)) +
  geom_abline(lty = 2, lwd = .5, col = "red") +
  geom_point(alpha = .7) +
  geom_errorbar(aes(ymin = q2.5+1, ymax = q97.5+1), alpha = .5) +
  facet_grid(. ~ AFRO_region) +
  theme_bw() +
  # coord_cartesian(xlim = c(0, 1e4), ylim = c(0, 1e4)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Mean of ADM0 observed number of cases", y = "Modeled")


ggsave(p_data_adm0_scatter,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_valiation_adm0_scatter.png"),
       width = 10,
       height = 3, 
       dpi = 300)

# Scatter plot of all admin units for mean of observations
p_data_scatter <- gen_obs %>%
  filter(censoring == "full") %>% 
  group_by(country, period, loctime_comb) %>% 
  mutate(mean_obs = mean(observation)) %>% 
  slice(1) %>% 
  ggplot(aes(x = mean_obs+1, y = mean+1)) +
  geom_abline(lty = 2, lwd = .5, col = "red") +
  geom_point(alpha = .2) +
  geom_errorbar(aes(ymin = q2.5+1, ymax = q97.5+1), alpha = .1) +
  facet_grid(admin_level ~ period) +
  theme_bw() +
  # coord_cartesian(xlim = c(0, 1e4), ylim = c(0, 1e4)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Observed number of cases", y = "Modeled")

ggsave(p_data_scatter,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_validation_scatter.png"),
       width = 8,
       height = 8, 
       dpi = 300)


# Scatter plot all censored admin units
p_data_scatter_censored <- gen_obs %>%
  filter(censoring == "right-censored") %>% 
  ggplot(aes(x = observation+1, y = mean+1)) +
  geom_abline(lty = 2, lwd = .5, col = "red") +
  geom_point(alpha = .2) +
  geom_errorbar(aes(ymin = q2.5+1, ymax = q97.5+1), alpha = .1) +
  facet_grid(admin_level ~ period) +
  theme_bw() +
  # coord_cartesian(xlim = c(0, 1e4), ylim = c(0, 1e4)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Observed number of cases", y = "Modeled")

ggsave(p_data_scatter_censored,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_validation_censored_scatter.png"),
       width = 8,
       height = 8, 
       dpi = 300)


# Subnational level coverage plot
p_coverage <- gen_obs %>%
  filter(admin_level != "ADM0") %>% 
  mutate(admin_level = str_extract(admin_level, "[0-9]") %>% as.numeric()) %>% 
  plot_posterior_coverage(with_period = TRUE)

ggsave(p_coverage,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_validation_coverage.png"),
       width = 12,
       height = 10, 
       dpi = 300)

# National level coverage plot
p_coverage_adm0 <- gen_obs %>%
  group_by(period, country, loctime_comb) %>% 
  mutate(observation = mean(observation)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(admin_level == "ADM0") %>% 
  mutate(admin_level = str_extract(admin_level, "[0-9]") %>% as.numeric()) %>% 
  plot_posterior_coverage(with_period = TRUE)

ggsave(p_coverage_adm0,
       file = str_glue("{opt$out_dir}/{opt$out_prefix}_supfig_validation_adm0_coverage.png"),
       width = 12,
       height = 10, 
       dpi = 300)
