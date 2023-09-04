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
              default = "./Analysis/cholera-configs/postprocessing_test",
              action ="store", type = "character", help = "Directory"),
  make_option(opt_str = c("-r", "--redo"), type = "logical",
              default = T, help = "redo final outputs"),
  make_option(opt_str = c("-i", "--redo_interm"), type = "logical",
              default = T, help = "redo intermediate"),
  make_option(opt_str = c("-j", "--redo_auxilliary"), type = "logical",
              default = F, help = "redo auxilliary files"),
  make_option(opt_str = c("-v", "--verbose"), type = "logical",
              default = T, help = "Print statements"),
  make_option(opt_str = c("-p", "--prefix"), type = "character",
              default = NULL, help = "Prefix to use in output file names"),
  make_option(opt_str = c("-s", "--suffix"), type = "character",
              default = NULL, help = "Suffix to use in output file names"),
  make_option(opt_str = c("-e", "--error_handling"), type = "character",
              default = "stop", help = "Error handling"),
  make_option(opt_str = c("-x", "--data_dir"), type = "character",
              default = "./cholera-mapping-output/", help = "Directory with all data"),
  make_option(opt_str = c("-y", "--interm_dir"), type = "character",
              default = "./Analysis/output/interm/", help = "Intermediate outputs directory"),
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", help = "Output directory"),
  make_option(opt_str = c("-c", "--cholera_dir"), type = "character",
              default = "cholera-mapping-pipeline", help = "Cholera mapping pipeline directory")
)

opt <- parse_args(OptionParser(option_list = opt_list))


# Create directories if they don't exist
purrr::walk(c("interm_dir", "output_dir"), function(x) {
  if (!dir.exists(opt[[x]])){
    dir.create(opt[[x]])
  }
})

if (!dir.exists(opt$data_dir)) {
  stop("Data directory ", opt$data_dir, " does not exist")
}

suffix <- opt$config_dir %>% 
  # Remove tailing / to ensure non-empty string
  stringr::str_remove("/$") %>% 
  stringr::str_split("/") %>% 
  .[[1]] %>% 
  last()

if (!is.null(opt$suffix)) {
  suffix <- paste(suffix, opt$suffix, sep = "_")
}

# Get spatial data for plots ----------------------------------------------

# Get lakes
lakes_sf <- get_lakes()

# All the country-level shapefiles for overlay
all_country_sf <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_sf,
  fun_name = "adm0_sf",
  fun_opts = NULL,
  postprocess_fun = tidy_shapefiles,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose) %>% 
  mutate(intended_run = TRUE)


# All the data shapfiles for spatial coverage
all_shapefiles <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_lp_shapefiles,
  fun_name = "shapefiles",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose) 

# All the obveration counts
all_obs_counts <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_lp_obs_counts,
  fun_name = "obs_counts",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose) 


# Get outputs -------------------------------------------------------------

# Get the total number of cases
mai_overall_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mai_adm0_cases,
  fun_name = "mai_adm0_draws",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws,
  postprocess_fun_opts = list(col = "country_cases"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the number of cases by WHO region
mai_region_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mai_adm0_cases,
  fun_name = "mai_adm0_draws_region",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws_by_region,
  postprocess_fun_opts = list(col = "country_cases"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the MAI summary at all admin levels 
mai_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mean_annual_incidence,
  fun_name = "mai",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the coefficient of variation summary at all admin levels 
cov_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_coef_of_variation,
  fun_name = "cov",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the MAI rates summary at space grid level 
mai_grid_rates_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_grid_mai_rates,
  fun_name = "mai_grid_rates",
  postprocess_fun = collapse_grid,
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the MAI cases summary at space grid level 
mai_grid_cases_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_grid_mai_cases,
  postprocess_fun = collapse_grid,
  fun_name = "mai_grid_cases",
  post_process_fun = collapse_grid,
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the risk category by location at all admin levels 
risk_categories <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_risk_category,
  fun_name = "risk_categories",
  fun_opts = list(cum_prob_thresh = 0.95),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the population at risk in each risk category by country 
# This is the sum of gridcells with risk categories
pop_at_risk_grid <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_pop_at_risk,
  fun_name = "pop_at_risk_grid",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the generated observations to plot posterior retrodictive checks
gen_obs <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_gen_obs,
  fun_name = "gen_obs",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


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
       width = 10,
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
