# Standalone case burden scaling process with country specific report generated


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
library(rmarkdown)
library(knitr)
library(yaml)

# User-supplied options
opt_list <- list(
  make_option(c("-d", "--config_dir"),
              default = "./scaling_test/scaling_test_configs",
              action ="store", type = "character", help = "Directory"),
  make_option(opt_str = c("-r", "--redo"), type = "logical",
              default = T, help = "redo final outputs"),
  make_option(opt_str = c("-i", "--redo_interm"), type = "logical",
              default = F, help = "redo intermediate"),
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
              default = "./scaling_test/scaling_test_output/", help = "Directory with all data"),
  make_option(opt_str = c("-y", "--interm_dir"), type = "character",
              default = "./scaling_test/scaling_test_output/", help = "Intermediate outputs directory"),
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./scaling_test/scaling_test_output/", help = "Output directory"),
  make_option(opt_str = c("-c", "--cholera_dir"), type = "character",
              default = "cholera-mapping-pipeline", help = "Cholera mapping pipeline directory"),
  make_option(opt_str = c("-n", "--n_draws"), type = "numeric",
              default = 10, help = "Number of draws to save from rate/cases grids"),
  make_option(opt_str = c("-w", "--scaling_input_dir"), type = "character",
              default = "./scaling_test/scaling_test_input/", help = "Directory with case burden scaling inputs"),
  make_option(opt_str = c("-k", "--case_filter_draws"), type = "numeric",
              default = 1000, help = paste("Number of draws to keep per country in",
                                           "postprocess_admin_cases_draws(), set to the smallest",
                                           "draw count produced across country",
                                           "configs' Stan fits")),
  make_option(opt_str = c("-m", "--rmd_template"), type = "character",
              default = "./scaling_burden_report.Rmd",
              help = "Path to the Rmd template used to render each country's HTML report")
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


# K. Case burden adjustment (cCh), this is where the scaling pipeline starts

# Admin-unit-level case draws (ADM0 and all subnational levels),
# unaggregated, the input for the case burden adjustment pipeline.
# Country-level scaling factors below (positivity, p_u5, care-seeking) are
# joined on "country" only, so combine_draws() applies each country's
# factors to every admin unit of that country; reporting_ratio is a single
# global scalar applied to every row with no join at all (see
# get_config_reporting_ratio())
mai_admin_case_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_admin_cases_draws,
  fun_name = "mai_cases_by_admin_unit_draws",
  fun_opts = list(filter_draws = opt$case_filter_draws),
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

base_case_draws <- mai_admin_case_draws
n_draws <- dplyr::n_distinct(base_case_draws$.draw)

sid <- opt$scaling_input_dir

# Country-specific severity-by-age-class scalars (proportion of cases that
# are moderate-to-severe, among under-5s and among 5-and-overs): two
# GLOBAL scalars (NOT per-country, see get_config_severity_scalars()),
# applied identically to every country
severity_scalars <- get_config_severity_scalars(opt$config_dir)

# Reporting-completeness ratio (ratio of reported sCh to all
# medically-attended sCh): a single global scalar, applied to every country and admin unit
reporting_ratio <- get_config_reporting_ratio(opt$config_dir)

# Test positivity and care-seeking posteriors, global (from systematic
# review), not country-specific, so these have no "country" column;
# combine_draws() applies a shared .draw value to every country.
positivity_draws <- readr::read_csv(file.path(sid, "prop_pos_adj_est.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(positivity = x) %>%
  align_draws_to_reference(n_draws)
care_seek_mild <- readr::read_csv(file.path(sid, "prop_sought_general_est.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(care_seeking = x) %>%
  align_draws_to_reference(n_draws)
care_seek_severe <- readr::read_csv(file.path(sid, "prop_sought_severe_est.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(care_seeking = x) %>%
  align_draws_to_reference(n_draws)

# Country lookup + country-level under-5 proportion posteriors, from the
# case age-distribution model (fit separately by fit_cholera_age_model.R)
country_lookup <- readr::read_csv(file.path(sid, "country_lookup.csv"))
country_id_vec <- readr::read_csv(file.path(sid, "country_id_vec.csv"))$country_id

# Countries with case draws from the main pipeline but no
# age-split data to fit the age-distribution model on get the
# model's p_epidemic_posterior fallback instead of a country-specific value
missing_countries <- setdiff(unique(base_case_draws$country), country_lookup$country)

p_u5_draws <- dplyr::bind_rows(
    postprocess_country_p_u5(sid, country_lookup, country_id_vec = country_id_vec),
    postprocess_country_p_u5_fallback(sid, missing_countries)
  ) %>%
  align_draws_to_reference(n_draws)

# Divide: reported / ratio = all medically-attended sCh. reporting_ratio is
# a single global scalar, so every row (every country, every admin unit,
# every draw) is divided by the same value
medically_attended_sCh <- scale_by_reporting_ratio(base_case_draws, "admin_cases", reporting_ratio)

# Scale by test positivity (global posterior, broadcast to every admin unit)
medically_attended_cCh <- combine_draws(medically_attended_sCh, "admin_cases",
                                        positivity_draws, "positivity", "cCh")

# Country-specific mild vs. moderate-to-severe split: severity_u5/
# severity_o5 are global scalars, weighted per country and per draw by
# that country's posterior proportion of cases under 5 (p_u5_draws)
severity_props <- compute_severity_proportions(p_u5_draws, severity_scalars$severity_u5, severity_scalars$severity_o5)

cCh_mild <- combine_draws(medically_attended_cCh, "cCh", severity_props, "prop_mild", "cCh_mild")
cCh_severe <- combine_draws(medically_attended_cCh, "cCh", severity_props, "prop_severe", "cCh_severe")

# Divide (not multiply): care_seeking is P(a symptomatic person seeks care),
# so cCh_mild/cCh_severe (cases among those who sought care) / care_seeking
# = all cases regardless of care-seeking -- same logic as
# scale_by_reporting_ratio() dividing by reporting_ratio above.
all_cCh_mild <- combine_draws(cCh_mild, "cCh_mild", care_seek_mild, "care_seeking", "all_cCh_mild", op = `/`)
all_cCh_severe <- combine_draws(cCh_severe, "cCh_severe", care_seek_severe, "care_seeking", "all_cCh_severe", op = `/`)

# Sum mild + severe to get total confirmed cases per country
all_cCh <- combine_draws(all_cCh_mild, "all_cCh_mild", all_cCh_severe, "all_cCh_severe", "all_cCh", op = `+`)

# Save every intermediate and final output
purrr::iwalk(
  list(mai_admin_case_draws = mai_admin_case_draws,
       medically_attended_sCh = medically_attended_sCh,
       medically_attended_cCh = medically_attended_cCh,
       cCh_mild = cCh_mild,
       cCh_severe = cCh_severe,
       all_cCh_mild = all_cCh_mild,
       all_cCh_severe = all_cCh_severe,
       all_cCh = all_cCh),
  ~ save_file_generic(.x, make_std_output_name(opt$output_dir, fun_name = .y,
                                                prefix = opt$prefix, suffix = opt$suffix,
                                                file_type = "rds"), file_type = "rds")
)


# K2. Geometry for maps -------------------------------------------------------
#
# mai_admin_case_draws et al. above only carry case counts, not polygons --
# these two shapefile fetches add the geometry the section-L maps need: ADM0
# country borders (background/border context on every map) and per-admin-
# level polygons for the actual choropleths. This script skips sections A-J
# entirely (it's K/L only), so unlike postprocess_results.R, all_country_sf
# isn't already available here and has to be fetched too, not just
# all_admin_sf. Same taxdat functions/caching postprocess_results.R's own
# section A uses for all_country_sf; all_admin_sf is fetched the same way
# postprocess_admin_cases_draws() does internally (get_output_sf_reload()),
# via a small wrapper, for every admin level rather than just ADM0. Lakes
# are optional -- skipped silently if the shapefile isn't available locally.

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
  verbose = opt$verbose)

opt$redo_auxilliary <- FALSE

get_all_admin_sf <- function(config_list, redo_aux = FALSE) {
  taxdat::get_output_sf_reload(config_list = config_list, redo = redo_aux)
}

all_admin_sf <- run_all(
  config_dir = opt$config_dir,
  fun = get_all_admin_sf,
  fun_name = "all_admin_sf",
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
  verbose = opt$verbose)

lakes_sf <- tryCatch(taxdat::get_lakes(), error = function(e) NULL)


# L. Case burden adjustment -- country-specific scaling burden reports -------
#
# Renders one self-contained HTML report per country present in
# mai_admin_case_draws, to <output_dir>/scaling_burden_report_<country>_<suffix>.html.
# Each report has: every setting from every config file matched to that
# country, the scaling funnel and net-effect figures, density comparisons of
# admin_cases vs. all_cCh split out by admin level AND time period (not
# pooled), the "scale factors used" tables/plots, and the per-step
# validation checks (ratio-matches-expected-factor, row-count agreement,
# mild+severe == total) restricted to that country.
#
# scaling_burden_report.Rmd is fully self-contained -- it defines and calls
# its own table/figure-building functions, nothing is sourced from another
# file -- so this loop just calls rmarkdown::render() once per country,
# passing the raw pipeline outputs through as params. postprocess_results.R
# has the identical loop in its own embedded K/L section, by design, so the
# two scripts can't produce divergent reports.

if (!file.exists(opt$rmd_template)) {
  warning("Report template not found at ", opt$rmd_template,
          " -- skipping country report generation entirely")
} else {

  countries_present <- sort(unique(mai_admin_case_draws$country))

  purrr::walk(countries_present, function(cty) {
    tryCatch({
      message("Rendering scaling burden report for country: ", cty)

      out_file <- stringr::str_glue("scaling_burden_report_{cty}.html")

      rmarkdown::render(
        input = opt$rmd_template,
        output_file = out_file,
        output_dir = opt$output_dir,
        params = list(
          country = cty,
          config_dir = opt$config_dir,
          mai_admin_case_draws = mai_admin_case_draws,
          medically_attended_sCh = medically_attended_sCh,
          medically_attended_cCh = medically_attended_cCh,
          cCh_mild = cCh_mild,
          cCh_severe = cCh_severe,
          all_cCh_mild = all_cCh_mild,
          all_cCh_severe = all_cCh_severe,
          all_cCh = all_cCh,
          positivity_draws = positivity_draws,
          severity_props = severity_props,
          care_seek_mild = care_seek_mild,
          care_seek_severe = care_seek_severe,
          reporting_ratio = reporting_ratio,
          severity_scalars = severity_scalars,
          p_u5_draws = p_u5_draws,
          all_admin_sf = all_admin_sf,
          all_country_sf = all_country_sf,
          lakes_sf = lakes_sf,
          generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
        ),
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      message("  -> wrote ", file.path(opt$output_dir, out_file))
    }, error = function(e) {
      warning("Report generation FAILED for country ", cty, ": ", conditionMessage(e))
    })
  })
}
