# This script post-processes results from a set of country runs


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

# User-supplied options
opt_list <- list(
  make_option(c("-d", "--config_dir"), 
              default = "./Analysis/cholera-configs/postprocessing_test_2016_2020",
              action ="store", type = "character", help = "Directory"),
  make_option(opt_str = c("-p", "--prefix"), type = "character",
              default = NULL, help = "Prefix to use in output file names"),
  make_option(opt_str = c("-s", "--suffix"), type = "character",
              default = NULL, help = "Suffix to use in output file names"),
  make_option(opt_str = c("-e", "--error_handling"), type = "character",
              default = "stop", help = "Error handling"),
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", help = "Output directory"),
  make_option(opt_str = c("-c", "--cholera_dir"), type = "character",
              default = "./", help = "Cholera mapping pipeline directory")
)

opt <- parse_args(OptionParser(option_list = opt_list))


suffix <- opt$config_dir %>% 
  # Remove tailing / to ensure non-empty string
  stringr::str_remove("/$") %>% 
  stringr::str_split("/") %>% 
  .[[1]] %>% 
  last()

if (!is.null(opt$suffix)) {
  suffix <- paste(suffix, opt$suffix, sep = "_")
}

# Make prefix for all files
prefix <- make_prefix_from_config_dir(config_dir = opt$config_dir,
                                      prefix = opt$prefix)


# Get spatial data for plots ----------------------------------------------

# Get lakes
lakes_sf <- get_lakes()

# All the country-level shapefiles for overlay
all_country_sf <- read_output(
  fun_name = "adm0_sf",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds") %>% 
  mutate(intended_run = TRUE)

# All the data shapfiles for spatial coverage
all_shapefiles <- read_output(
  fun_name = "shapefiles",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds") 

# All the obveration counts
all_obs_counts <- read_output(
  fun_name = "obs_counts",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds") 


# Get outputs -------------------------------------------------------------

# Get the total number of cases
mai_overall_stats <- read_output(
  fun_name = "mai_cases_all",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")


# Get the number of cases by WHO region
mai_region_case_stats <- read_output(
  fun_name = "mai_cases_by_region",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")

mai_region_rates_stats <- read_output(
  fun_name = "mai_rates_by_region",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")

# Get the MAI summary at all admin levels 
mai_stats <- read_output(
  fun_name = "mai",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")

# Get the coefficient of variation summary at all admin levels 
cov_stats <- read_output(
  fun_name = "cov",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")

# Get the MAI rates summary at space grid level 
mai_grid_rates_stats <- read_output(
  fun_name = "mai_grid_rates",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")


# Get the MAI cases summary at space grid level 
mai_grid_cases_stats <- read_output(
  fun_name = "mai_grid_cases",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")

# Get the risk category by location at all admin levels 
risk_categories <- read_output(
  fun_name = "risk_categories",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")

# Get the population at risk in each risk category by country 
# This is the sum of gridcells with risk categories
pop_at_risk <- read_output(
  fun_name = "pop_at_risk",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")


# Get the generated observations to plot posterior retrodictive checks
gen_obs <- read_output(
  fun_name = "gen_obs",
  prefix = prefix,
  suffix = opt$suffix,
  output_dir = opt$output_dir,
  output_file_type = "rds")


# Figure 1: Mean annual incidence ADM0 ------------------------------------

mai_adm0 <- mai_stats %>% 
  filter(admin_level == "ADM0") %>% 
  rmapshaper::ms_simplify(keep = 0.05,
                          keep_shapes = FALSE)
# Save
save_file_generic(res = mai_adm0,
                  res_file = str_glue("{opt$output_dir}/mai_adm0_{suffix}.rds"))

p_fig1 <- output_plot_map(sf_obj = mai_adm0 %>% 
                            mutate(log10_rate_per_1e5 = log10(mean * 1e5)),
                          lakes_sf = lakes_sf,
                          all_countries_sf = all_country_sf,
                          fill_var = "log10_rate_per_1e5",
                          fill_color_scale_type = "rates",
                          cholera_dir = opt$cholera_dir) +
  ggtitle(str_glue("Mean mean annual incidence rate\nat national level"))

ggsave(p_fig1,
       file = str_glue("{opt$output_dir}/figure_mai_adm0_{suffix}.png"),
       width = 10,
       height = 8, 
       dpi = 300)

# Figure 2: Mean annual incidence ADM2 ------------------------------------

mai_adm2 <- mai_stats %>% 
  filter(admin_level == "ADM2") %>% 
  rmapshaper::ms_simplify(keep = 0.05,
                          keep_shapes = FALSE)

save_file_generic(res = mai_adm2,
                  res_file = str_glue("{opt$output_dir}/mai_adm2_{suffix}.rds"))

p_fig2 <- output_plot_map(sf_obj = mai_adm2 %>% 
                            mutate(log10_rate_per_1e5 = log10(mean * 1e5)),
                          lakes_sf = lakes_sf,
                          all_countries_sf = all_country_sf,
                          fill_var = "log10_rate_per_1e5",
                          fill_color_scale_type = "rates",
                          cholera_dir = opt$cholera_dir)  +
  ggtitle(str_glue("Mean mean annual incidence rate\n at ADM2 level"))


ggsave(p_fig2,
       file = str_glue("{opt$output_dir}/figure_mai_adm2_{suffix}.png"),
       width = 10,
       height = 8, 
       dpi = 300)


# Figure 3: Grid-level rates ----------------------------------------------

p_fig3 <- output_plot_map(sf_obj = mai_grid_rates_stats %>% 
                            mutate(log10_rate_per_1e5 = log10(mean * 1e5)),
                          lakes_sf = lakes_sf,
                          all_countries_sf = all_country_sf,
                          fill_var = "log10_rate_per_1e5",
                          fill_color_scale_type = "rates",
                          cholera_dir = opt$cholera_dir)

ggsave(p_fig3,
       file = str_glue("{opt$output_dir}/figure_mai_grid_rates_{suffix}.png"),
       width = 10,
       height = 8, 
       dpi = 300)

# Figure 4: Grid-level cases ----------------------------------------------

p_fig4 <- output_plot_map(sf_obj = mai_grid_cases_stats %>% 
                            mutate(log10_cases = log10(mean)),
                          lakes_sf = lakes_sf,
                          all_countries_sf = all_country_sf,
                          fill_var = "log10_cases",
                          fill_color_scale_type = "cases",
                          cholera_dir = opt$cholera_dir)

ggsave(p_fig4,
       file = str_glue("{opt$output_dir}/figure_mai_grid_cases_{suffix}.png"),
       width = 10,
       height = 8, 
       dpi = 300)



# Figure 5: risk categories -----------------------------------------------

p_fig5 <- output_plot_map(sf_obj = risk_categories, 
                          lakes_sf = lakes_sf,
                          all_countries_sf = all_country_sf,
                          fill_var = "risk_cat",
                          fill_color_scale_type = "risk category",
                          cholera_dir = opt$cholera_dir) +
  facet_wrap(~admin_level)

ggsave(p_fig5,
       file = str_glue("{opt$output_dir}/figure_risk_categories_{suffix}.png"),
       width = 15,
       height = 8, 
       dpi = 300)


# Figure 6: population at risk at ADM 2 -----------------------------------

adm2_pop_at_risk <- risk_categories %>% 
  st_drop_geometry() %>% 
  filter(admin_level == "ADM2") %>%
  mutate(country = get_country_from_string(location_period_id)) %>% 
  {
    x <- .
    bind_rows(x, x %>% mutate(country = "overall")) %>% 
      mutate(country = factor(country, levels = c("overall", sort(unique(x$country)))))
  } %>% 
  group_by(risk_cat, country) %>% 
  summarise(pop = sum(pop)) %>% 
  group_by(country) %>% 
  complete(risk_cat = get_risk_cat_dict()) %>%
  mutate(risk_cat = factor(risk_cat, levels = get_risk_cat_dict())) %>% 
  mutate(prop = pop/sum(pop, na.rm = T))

p_fig6 <- adm2_pop_at_risk %>% 
  ggplot(aes(x = risk_cat, y = prop, fill = risk_cat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country) +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(x = "Proportion in risk category", y = "Risk category")

ggsave(p_fig6,
       file = str_glue("{opt$output_dir}/figure_risk_categories_prop_ADM2_{suffix}.png"),
       width = 18,
       height = 8, 
       dpi = 300)



# Figure 7: Coefficient of variation ------------------------------------------------

mai_cov <- mai_stats %>% 
  filter(admin_level == "ADM0") %>% 
  st_drop_geometry() %>% 
  select(location_period_id, mai = mean) %>% 
  inner_join(cov_stats %>% 
               filter(admin_level == "ADM0") %>% 
               st_drop_geometry() %>% 
               select(location_period_id, cov = mean)) %>% 
  mutate(country = get_country_from_string(location_period_id))


p_cov <- ggplot(mai_cov, aes(x = cov, y = mai * 1e5)) +
  geom_label(aes(label = country)) +
  theme_bw() +
  labs(x = "Coefficient of variation [-]", y = "Mean annual incidence [cases per 100,000/year]")


ggsave(p_cov,
       file = str_glue("{opt$output_dir}/figure_mai_cov_{suffix}.png"),
       width = 7,
       height = 6, 
       dpi = 300)



# Figure 8: posterior coverage --------------------------------------------

p_coverage <- plot_posterior_coverage(gen_obs) 

ggsave(p_coverage,
       file = str_glue("{opt$output_dir}/figure_posterior_coverage_{suffix}.png"),
       width = 10,
       height = 8, 
       dpi = 300)



# Figure 9: data shapefiles -----------------------------------------------

p_fig9 <- all_shapefiles %>% 
  mutate(admin_level = factor(admin_level, levels = 0:6)) %>% 
  output_plot_map(sf_obj = ., 
                  lakes_sf = lakes_sf,
                  all_countries_sf = all_country_sf,
                  fill_var = "admin_level",
                  fill_color_scale_type = "admin levels",
                  border_width = .3,
                  border_color = "gray",
                  lake_alpha = 1,
                  country_border_color = "black",
                  country_border_width = 1,
                  cholera_dir = opt$cholera_dir)

ggsave(p_fig9,
       file = str_glue("{opt$output_dir}/figure_data_spatial_coverage_{suffix}.png"),
       width = 10,
       height = 8, 
       dpi = 300)


# Table with obs counts per admin level -----------------------------------

all_obs_counts %>% 
  mutate(admin_level = str_c("ADM", admin_level)) %>% 
  bind_rows(all_obs_counts %>% mutate(admin_level = "all")) %>% 
  group_by(country, admin_level, imputed) %>% 
  summarise(n_loc = n(),
            n_obs = sum(n_obs))
