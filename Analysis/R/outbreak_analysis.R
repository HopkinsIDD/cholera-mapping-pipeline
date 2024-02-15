# This script aims at quantifying the relative risk of observing outbreaks
# by 10-year risk category

# Preamble ----------------------------------------------------------------

library(tidyverse)
library(cmdstanr)
library(sf)
library(taxdat)


# Functions ---------------------------------------------------------------

getADM2Units <- function(adm1_lp, all_lps) {
  
  adm2_lps <- all_lps %>% str_subset(str_extract(adm1_lp, "[A-Z]{3}"))
  
  ind <- str_detect(adm2_lps, str_split(adm1_lp, "ADM1")[[1]][2] %>% 
                      str_remove("_1") %>% 
                      str_c(., "\\."))
  adm2_lps[ind]
}


# Data --------------------------------------------------------------------

if (file.exists("Analysis/notebooks/endemicity_validated.rds")) {
  endemicity <- readRDS("Analysis/notebooks/endemicity_validated.rds")
} else {
  # This is simply the geopackage from the endemicity object from make_final_figures_and_tables.R
  endemicity <- st_read("Analysis/output/endemicity.gpkg") %>% st_make_valid()
  saveRDS(endemicity, "Analysis/notebooks/endemicity_validated.rds")
}


# Sitreps -----------------------------------------------------------------
# This assumes all the data extraction folders are in folder data_extraction in notebooks
sitrep_dirs <- dir("Analysis/notebooks/data_extraction", full.names = TRUE) %>% 
  str_subset("gpkg", negate = T)

# Map over dirst
sitrep_files <- map(sitrep_dirs, ~ dir(., full.names = T)) %>% 
  unlist()

# Read in all files
outbreaks <- map_df(sitrep_files, function(x){
  layers <- st_layers(x)
  map_df(layers$name, function(y) {
    st_read(x, layer = y) %>% 
      mutate(source_layer = y,
             admin_level = as.character(admin_level))
  })
})

# Save for further use
st_write(outbreaks, "Analysis/notebooks/data_extraction/combined_extraction.gpkg",
         append = FALSE)

# Processing admin levels -------------------------------------------------

# Get set of countries with intersection
outbreaks <- st_join(outbreaks, taxdat::afr_sf %>% select(country))

# Get unique country/admin level combinations
country_admin_levels <- outbreaks %>% 
  st_drop_geometry() %>% 
  filter(!is.na(country)) %>% 
  distinct(country, admin_level) %>% 
  as_tibble()

# Get all shapefiles for these
# This assumes ssa_admin_units.gpkg exists in the project root directory.
shps <- st_read("ssa_admin_units.gpkg", 
                query = str_glue("SELECT * FROM ssa_admin_units WHERE country IN ('{str_c(country_admin_levels$country, collapse = \"','\")})')")
) %>% 
  mutate(admin_level = str_extract(admin_level, "[0-2]"))

# Join on shapefile and admin level
joins <- map_df(
  1:nrow(country_admin_levels), 
  function(x) {
    # Subset outbreak data to this country and admin level
    outbreaks %>% 
      inner_join(country_admin_levels[x, ]) %>% 
      # Run spatial join
      st_join(
        shps %>% 
          inner_join(
            country_admin_levels[x, ] %>% 
              # Overwrite admin_level if admin3
              mutate(admin_level = case_when(admin_level == "3" ~ "2",
                                             T ~ admin_level)
              )) %>% 
          select(-country, -admin_level),
        .,
        left = FALSE)
  })


# Save for later use
st_write(joins, "Analysis/notebooks/data_extraction/admin_unit_extractions.gpkg",
         append = FALSE)


# Get nested locations
u_joins <- joins %>% 
  group_by(location_period_id, admin_level) %>% 
  slice(1)


all_adm2_lps <- shps$location_period_id[shps$admin_level == "2"]

# These are the joins on which to compute mappings
final_joins <-
  bind_rows(
    u_joins %>% 
      filter(admin_level != "1") %>% 
      mutate(adm2_lps = map(location_period_id, ~.)),
    u_joins %>% 
      filter(admin_level == "1") %>% 
      mutate(adm2_lps = map(location_period_id, ~ getADM2Units(., all_adm2_lps)))
  ) %>% 
  arrange(location_period_id)

# Stats by admin level
final_joins %>% 
  st_drop_geometry() %>% 
  distinct(location_period_id, admin_level) %>% 
  count(admin_level) %>% mutate(frac = n/sum(n))

# Counts for paper
obs_outbreaks <- final_joins %>% 
  st_drop_geometry() %>% 
  filter(admin_level != "1") %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  {
    x <- .
    bind_rows(
      x,
      x %>% mutate(AFRO_region = "overall")
    )
  } %>% 
  distinct(location_period_id, AFRO_region) %>% 
  inner_join(endemicity %>% st_drop_geometry()) %>% 
  count(AFRO_region, endemicity) %>% 
  group_by(AFRO_region) %>% 
  mutate(frac = n/sum(n))

print(obs_outbreaks)


# Countries with outbreaks in sustained low regions
final_joins %>% 
  st_drop_geometry() %>% 
  filter(admin_level != "1") %>% 
  distinct(location_period_id) %>% 
  inner_join(endemicity %>% st_drop_geometry()) %>% 
  filter(endemicity == "sustained low risk") %>% 
  distinct(country)

non_obs_outbreaks <- endemicity %>% 
  st_drop_geometry() %>% 
  mutate(in_outbreak = location_period_id %in% unlist(final_joins$adm2_lps)) %>% 
  filter(!in_outbreak) %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  {
    x <- .
    bind_rows(
      x,
      x %>% mutate(AFRO_region = "overall")
    )
  } %>% 
  distinct(location_period_id, AFRO_region) %>% 
  inner_join(endemicity %>% st_drop_geometry()) %>% 
  count(AFRO_region, endemicity) %>% 
  group_by(AFRO_region) %>% 
  mutate(frac = n/sum(n))

# Prepare data for stan model ---------------------------------------------

endemicity <- endemicity %>% 
  mutate(in_outbreak = location_period_id %in% unlist(final_joins$adm2_lps))

# chisq.test(table(endemicity$endemicity, endemicity$in_outbreak))

# Data for analysis
dat <- endemicity %>% 
  st_drop_geometry()  %>% 
  as_tibble() %>% 
  get_AFRO_region(ctry_col = "country")


# Unique set of ADM2 units
u_adm2 <- endemicity$location_period_id %>% sort()

# Set of observations at ADM2 (outbreak presence)
adm2_obs <- final_joins$location_period_id[final_joins$admin_level != "1"] %>% sort()
adm2_non_obs <- dat$location_period_id[!dat$in_outbreak] %>% sort()

# Set of upper level ADM units
map_adm2_other <- final_joins %>% 
  filter(admin_level == "1") %>% 
  pull(adm2_lps) %>% 
  unlist() %>% 
  map_dbl(~which(u_adm2 == .))

# Starts and ends for mapping between adm2 and other adm units
map_adm_start_ends <- final_joins %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  filter(admin_level == "1") %>% 
  mutate(size_adm2 = map_dbl(adm2_lps, ~ length(.)),
         starts = c(1,  1+cumsum(size_adm2[-length(size_adm2)])),
         ends = cumsum(size_adm2)) %>% 
  select(location_period_id, admin_level, size_adm2, starts, ends)


# Unique upper admin units
u_countries <- dat %>% pull(country) %>% unique() %>% sort()
u_regions <- dat %>% pull(AFRO_region) %>% unique() %>% sort()


# Covariates
X <- model.matrix(data = dat %>% 
                    mutate(endemicity = factor(endemicity) %>% 
                             forcats::fct_relevel("sustained low risk")), 
                  ~ endemicity)

get_indices <- function(vec, ref_vec) {
  map_dbl(vec, ~ which(ref_vec == .))
}

stan_data <- list(
  N = length(u_adm2),
  M = ncol(X),
  N_obs = nrow(final_joins),
  N_obs_adm2 = length(adm2_obs),
  N_non_obs_adm2 = sum(!dat$in_outbreak),
  N_obs_other_adm = nrow(map_adm_start_ends),
  K = length(map_adm2_other),
  U = length(u_regions),
  C = length(u_countries),
  L = 1,
  ind_obs_adm2 = get_indices(adm2_obs, u_adm2),
  ind_non_obs_adm2 = get_indices(adm2_non_obs, u_adm2),
  map_adm2_other = map_adm2_other,
  map_adm_starts = map_adm_start_ends$starts,
  map_adm_ends = map_adm_start_ends$ends,
  map_adm2_countries = get_indices(dat$country, u_countries),
  map_adm2_upper = get_indices(dat$AFRO_region, u_regions),
  map_country_upper = dat %>% 
    distinct(country, AFRO_region) %>% 
    arrange(country) %>% 
    pull(AFRO_region) %>% 
    get_indices(u_regions),
  X = X
)


save(outbreaks, final_joins, stan_data, dat, u_countries, u_regions,
     obs_outbreaks, non_obs_outbreaks,
     file = "Analysis/notebooks/outbreak_analysis_data.rdata")

# Stan model --------------------------------------------------------------


# Data for stan
stan_model <- cmdstan_model("Analysis/Stan/outbreak_analysis_multilevel_hierarchical.stan")


stan_fit <- stan_model$sample(
  data = stan_data,
  iter_warmup = 250,
  iter_sampling = 1000,
  parallel_chains = 4,
  refresh = 100
)

stan_fit$save_object("Analysis/notebooks/stan_fit.rds")

stan_genquant <- stan_model$generate_quantities(
  stan_fit,
  data = stan_data,
  parallel_chains = 4
)

stan_genquant$save_object("Analysis/notebooks/stan_genquant.rds")

# Save for figures --------------------------------------------------------

beta_levels <- c("history of moderate risk", "history of high risk", "sustained high risk")

# Stats of log OR of outbreak observation
logOR_stats <- bind_rows(
  stan_fit$summary("r_beta", mean, taxdat:::custom_quantile2) %>% 
    mutate(AFRO_region = u_regions[str_extract(variable, "(?<=\\[)[0-9](?=,)") %>% as.numeric()],
           param = colnames(stan_data$X)[as.numeric(str_extract(variable, "(?<=,)[0-9](?=\\])"))] %>% 
             str_remove("endemicity") %>% 
             factor(levels = beta_levels)),
  stan_fit$summary("mu_beta", mean, taxdat:::custom_quantile2) %>% 
    mutate(param = colnames(stan_data$X)[str_extract(variable, "(?<=\\[)[0-9](?=\\])") %>% as.numeric()] %>% 
             str_remove("endemicity") %>% 
             factor(levels = beta_levels),
           AFRO_region = "overall")
) %>% 
  filter(param != "(Intercept)") %>% 
  mutate(what = "2011-2020 cholera risk category and \n outbreak occurrence in 2022-2023",
         AFRO_region = factor(AFRO_region, levels = c("overall", get_AFRO_region_levels())))

logOR_stats %>% 
  mutate(
    txt = str_c(
      formatC(mean, format = "f", digits = 2),
      " (",
      formatC(q2.5, format = "f", digits = 2),
      "-",
      formatC(q97.5, format = "f", digits = 2),
      ")"
    )
  ) %>% 
  select(AFRO_region, param, txt)

# Stats of OR of outbreak observation
OR_stats <- bind_rows(
  stan_genquant$summary("r_odd_ratios", mean, median, taxdat:::custom_quantile2) %>% 
    mutate(AFRO_region = u_regions[str_extract(variable, "(?<=\\[)[0-9](?=,)") %>% as.numeric()],
           param = colnames(stan_data$X)[as.numeric(str_extract(variable, "(?<=,)[0-9](?=\\])"))+1] %>% 
             str_remove("endemicity") %>% 
             factor(levels = beta_levels)),
  stan_genquant$summary("odd_ratios", mean, median, taxdat:::custom_quantile2) %>% 
    mutate(param = colnames(stan_data$X)[as.numeric(str_extract(variable, "(?<=\\[)[0-9](?=\\])"))+1] %>% 
             str_remove("endemicity") %>% 
             factor(levels = beta_levels),
           AFRO_region = "overall")
) %>% 
  filter(param != "(Intercept)") %>% 
  mutate(what = "2011-2020 cholera risk category and \n outbreak occurrence in 2022-2023",
         AFRO_region = factor(AFRO_region, levels = c("overall", get_AFRO_region_levels())))

OR_stats %>% 
  mutate(
    txt = str_c(
      formatC(median, format = "f", digits = 2),
      " (",
      formatC(q2.5, format = "f", digits = 2),
      "-",
      formatC(q97.5, format = "f", digits = 2),
      ")"
    )
  ) %>% 
  select(AFRO_region, param, txt)

baseline_prob_stats <- bind_rows(
  stan_genquant$summary("r_baseline_prob", mean, median, taxdat:::custom_quantile2) %>% 
    mutate(AFRO_region = u_regions[str_extract(variable, "(?<=\\[)[0-9](?=\\])") %>% as.numeric()]),
  stan_genquant$summary("baseline_prob", mean, median, taxdat:::custom_quantile2) %>% 
    mutate(AFRO_region = "overall")
) %>% 
  mutate(param = "baseline \n (ref: sustained low risk)",
         what = "baseline outbreak\nprobability",
         AFRO_region = factor(AFRO_region, levels = c("overall", get_AFRO_region_levels())))



ICC_stats <- stan_genquant$summary("ICC", mean, taxdat:::custom_quantile2) %>% 
  mutate(AFRO_region = u_regions[str_extract(variable, "(?<=\\[)[0-9](?=,)") %>% as.numeric()],
         param = colnames(stan_data$X)[as.numeric(str_extract(variable, "(?<=,)[0-9](?=\\])"))] %>% 
           str_remove("endemicity") %>% 
           factor(levels = c("(Intercept)", beta_levels)))


param_by_country <- stan_genquant$summary("c_beta", mean, median, taxdat:::custom_quantile2,
                                          .cores = 4) %>% 
  mutate(country = u_countries[str_extract(variable, "(?<=\\[)[0-9]+(?=,)") %>% as.numeric()],
         param = colnames(stan_data$X)[str_extract(variable, "(?<=,)[0-9](?=\\])") %>% as.numeric()] %>% 
           str_remove("endemicity") %>% 
           factor(levels = c("(Intercept)", beta_levels))) %>% 
  get_AFRO_region(ctry_col = "country") %>% 
  mutate(AFRO_region = factor(AFRO_region, levels = get_AFRO_region_levels())) 

save(final_joins, logOR_stats, OR_stats, baseline_prob_stats, param_by_country, ICC_stats,
     file = "Analysis/notebooks/recent_cholera_outbreaks_res.rdata")

# Plots -------------------------------------------------------------------
# 
# 
# library(bayesplot)
# 
# stan_fit$summary("pred_obs_prob_other_adm") %>% 
#   bind_cols(final_joins %>% 
#               filter(admin_level == "1")) %>% 
#   ggplot(aes(y = location_period_id, x = mean)) +
#   geom_point() +
#   geom_errorbarh(aes(xmin = q5, xmax = q95), height = 0) +
#   facet_wrap(~country, scales = "free_y")
# 
# pd <- position_dodge(.3)
# beta_levels <- c("(Intercept)", "history of moderate risk", "history of high risk", "sustained high risk")
# 
# 
# stan_fit$summary("c_beta") %>% 
#   mutate(country = u_countries[str_extract(variable, "(?<=\\[)[0-9]+(?=,)") %>% as.numeric()],
#          param = colnames(X)[str_extract(variable, "(?<=,)[0-9](?=\\])") %>% as.numeric()] %>% 
#            str_remove("endemicity") %>% 
#            factor(levels = beta_levels)) %>% 
#   get_AFRO_region(ctry_col = "country") %>% 
#   mutate(AFRO_region = factor(AFRO_region, levels = get_AFRO_region_levels())) %>% 
#   ggplot(aes(x = param, y = mean, ymin = q5, ymax = q95, color = AFRO_region)) +
#   geom_point(position = pd) +
#   geom_errorbar(width = 0, position = pd) +
#   geom_hline(aes(yintercept = 0), lty = 3, lwd = .6) +
#   facet_wrap(~ country) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   scale_color_manual(values = taxdat::colors_afro_regions()) +
#   labs(x = "", y = "value")
# 
# 
# stan_fit$summary("r_beta") %>% 
#   mutate(AFRO_region = u_regions[str_extract(variable, "(?<=\\[)[0-9](?=,)") %>% as.numeric()],
#          param = colnames(X)[str_extract(variable, "(?<=,)[0-9](?=\\])") %>% as.numeric()] %>% 
#            str_remove("endemicity") %>% 
#            factor(levels = beta_levels)) %>%
#   bind_rows(stan_fit$summary("mu_beta") %>% 
#               mutate(param = colnames(X)[str_extract(variable, "(?<=\\[)[0-9](?=\\])") %>% as.numeric()] %>% 
#                        str_remove("endemicity") %>% 
#                        factor(levels = beta_levels),
#                      AFRO_region = "overall")) %>% 
#   mutate(what = case_when(param == "(Intercept)" ~ "baseline logit-prob\n(sustained low risk)",
#                           T ~ "log(OR)"),
#          AFRO_region = factor(AFRO_region, levels = c("overall", get_AFRO_region_levels()))) %>% 
#   ggplot(aes(x = param, y = mean, ymin = q5, ymax = q95, color = AFRO_region)) +
#   geom_point(position = pd) +
#   geom_errorbar(width = 0, position = pd) +
#   geom_hline(aes(yintercept = 0), lty = 3, lwd = .6) +
#   facet_grid(. ~ what, scales = "free", space = "free") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   scale_color_manual(values = c("overall" = "black", taxdat::colors_afro_regions())) +
#   labs(x = "", y = "value")
# 
# stan_fit$draws(c("beta[2]", "beta[3]", "beta[4]")) %>% 
#   mcmc_intervals(transformations = exp) +
#   scale_x_log10()
# 
# 
# 
# stan_fit$summary("r_logit_phi") %>% 
#   mutate(AFRO_region = u_regions) %>% 
#   ggplot(aes(y = AFRO_region, x = 1/(1+exp(-mean)))) +
#   geom_errorbarh(aes(xmin = 1/(1+exp(-q5)), xmax = 1/(1+exp(-q95)))) +
#   geom_label(aes(label = AFRO_region)) +
#   theme_bw()
# 
# stan_fit$summary("log_phi") %>% 
#   mutate(country = u_countries) %>% 
#   ggplot(aes(y = country, x = exp(mean))) +
#   geom_errorbarh(aes(xmin = exp(q5), xmax = exp(q95))) +
#   geom_label(aes(label = country)) +
#   theme_bw()
# 
# # Map
# prob_stats <- stan_fit$summary("pred_prob", .cores = 4) 
# 
# p_map <- prob_stats %>% 
#   bind_cols(endemicity, .) %>% 
#   ggplot(aes(fill = mean)) +
#   geom_sf() +
#   scale_fill_viridis_c() +
#   taxdat::map_theme()
# 
# ggsave(p_map, 
#        filename = "Analysis/notebooks/prob_map.png", 
#        width = 10, 
#        height = 8, 
#        dpi = 300)
# 
# true_prob_stats <- stan_fit$summary("log_prob", .cores = 4) 
# 
# p_map2 <- true_prob_stats %>% 
#   bind_cols(endemicity, .) %>% 
#   ggplot(aes(fill = exp(mean))) +
#   geom_sf() +
#   scale_fill_viridis_c() +
#   taxdat::map_theme()
# 
# ggsave(p_map2, 
#        filename = "Analysis/notebooks/true_prob_map.png", 
#        width = 10, 
#        height = 8, 
#        dpi = 300)
# 
# 
# prob_stats %>% 
#   bind_cols(endemicity, .) %>% 
#   st_write("Analysis/notebooks/pred_prob.gpkg")
# 


