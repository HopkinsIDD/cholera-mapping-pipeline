# fit_cholera_age_model.R
#
# Standalone, non-interactive fit of the case age-distribution model used by
# the case burden adjustment (postprocess_results.R, section K). Extracted
# from cholera_age_model_fits.qmd, which fits and compares eight model
# variants for exploration -- this script fits only the one that was kept,
# cad_model4_country_random_endemicity_random.stan ("Country (random) +
# Endemicity (random)"), and writes exactly what the pipeline needs into
# scaling_input_dir:
#   - fit_cad_model4_country_random_endemicity_random.rds  (the saved fit)
#   - country_lookup.csv     (country, region, country_id, region_id)
#   - country_id_vec.csv     (country_id per age-model row, for averaging
#                             a country's own draws across its years)
#
# Run this before postprocess_results.R, any time the underlying age data
# or model changes. Not part of run_all()'s per-country-config loop -- this
# is a single global model, fit once, not per country.

library(dplyr)
library(tidyr)
library(readr)
library(cmdstanr)
library(optparse)
library(countrycode)

`%||%` <- function(a, b) if (is.null(a)) b else a

opt_list <- list(
  make_option(c("-a", "--age_csv_path"), type = "character",
              default = "./Analysis/outputs/cholera_age_data.csv",
              help = "Country-year age-split case counts (see cholera_age_distribution.qmd)"),
  make_option(c("-t", "--totals_csv_path"), type = "character",
              default = "./Analysis/outputs/cholera_totals_2016_2020.csv",
              help = "Optional 2016-2020 totals, for the 5-year endemicity lookback"),
  make_option(c("-s", "--stan_dir"), type = "character",
              default = "./Analysis/Stan/", help = "Directory containing the .stan model files"),
  make_option(c("-o", "--scaling_input_dir"), type = "character",
              default = "./Analysis/scaling_input/",
              help = "Output directory (shared with postprocess_results.R)"),
  make_option(c("-g", "--config"), type = "character", default = NULL,
              help = paste("Path to any one country's config yml (its scaling: section is",
                           "identical across countries by construction -- see",
                           "write_batch_mapping_config_general.R). Sampling settings below are",
                           "used only as fallback defaults if this is not given.")),
  make_option(c("-r", "--refit"), type = "logical", default = FALSE,
              help = "Re-fit even if a cached fit .rds already exists"),
  make_option(c("--chains"), type = "integer", default = 4),
  make_option(c("--parallel_chains"), type = "integer", default = 4),
  make_option(c("--iter_warmup"), type = "integer", default = 1000),
  make_option(c("--iter_sampling"), type = "integer", default = 1000),
  make_option(c("--adapt_delta"), type = "numeric", default = 0.95),
  make_option(c("--seed"), type = "integer", default = 123)
)
opt <- parse_args(OptionParser(option_list = opt_list))

if (!dir.exists(opt$scaling_input_dir)) {
  dir.create(opt$scaling_input_dir, recursive = TRUE)
}

# Sampling settings: config$scaling (a country config's scaling: section)
# takes precedence over the CLI defaults above when --config is given, so a
# single source of truth (the config yml) drives both the age model fit and
# the case burden adjustment downstream in postprocess_results.R.
if (!is.null(opt$config)) {
  cfg <- yaml::read_yaml(opt$config)
  scaling_cfg <- cfg$scaling
  if (is.null(scaling_cfg)) {
    stop("--config was given but that config has no scaling: section -- ",
         "regenerate it with write_batch_mapping_config_general.R after adding ",
         "severity_u5/severity_o5/age_model_* to params_df.")
  }
  opt$chains          <- scaling_cfg$age_model_chains          %||% opt$chains
  opt$parallel_chains <- scaling_cfg$age_model_parallel_chains %||% opt$parallel_chains
  opt$iter_warmup      <- scaling_cfg$age_model_iter_warmup     %||% opt$iter_warmup
  opt$iter_sampling     <- scaling_cfg$age_model_iter_sampling    %||% opt$iter_sampling
  opt$adapt_delta      <- scaling_cfg$age_model_adapt_delta     %||% opt$adapt_delta
  opt$seed             <- scaling_cfg$age_model_seed            %||% opt$seed
} else {
  message("--config not given -- using this script's own CLI defaults/flags for sampling settings, ",
          "not a config yml's scaling: section.")
}

model_name <- "cad_model4_country_random_endemicity_random"
stan_file <- file.path(opt$stan_dir, paste0(model_name, ".stan"))
fit_rds_path <- file.path(opt$scaling_input_dir, paste0("fit_", model_name, ".rds"))

if (!file.exists(stan_file)) {
  stop("Stan file not found: ", stan_file)
}

# Data ------------------------------------------------------------------

if (!file.exists(opt$age_csv_path)) {
  stop(
    "`", opt$age_csv_path, "` was not found. Render cholera_age_distribution.qmd ",
    "first (it writes this file as part of the \"Run the scraper if needed\" step)."
  )
}

#' to_iso3
#' Convert a country-name column to ISO3, reporting (not silently dropping)
#' anything that fails to match -- every downstream join in this pipeline
#' (severity, p_u5, case draws in postprocess_results.R section K) keys on
#' ISO3 codes like "BDI" (from get_country_from_string() on genquant
#' filenames), not full country names, so an unconverted "Burundi" would
#' never match and would silently fall through to the no-data fallback
#' regardless of how much real age-split data actually exists for it.
#'
#' custom_match resolves a handful of official/UN-style names that
#' commonly fail default dictionary matching -- explicit rather than left
#' to automatic fuzzy matching, since a WRONG silent match (e.g. a fuzzy
#' matcher once mapped "Niger" to Nigeria's code during testing of this
#' function) is far more dangerous than a flagged non-match: it produces no
#' warning at all and silently misattributes one country's data to another.
to_iso3 <- function(country_raw, source_label) {

  custom_match <- c(
    "C\u00f4te d\u2019Ivoire" = "CIV", "C\u00f4te d'Ivoire" = "CIV",
    "Democratic Republic of the Congo" = "COD",
    "Congo" = "COG", "Republic of the Congo" = "COG",
    "Iran (Islamic Republic of)" = "IRN", "Iran" = "IRN",
    "Tanzania (United Republic of)" = "TZA", "United Republic of Tanzania" = "TZA",
    "Republic of Korea" = "KOR"
  )

  iso3 <- countrycode::countrycode(country_raw, origin = "country.name",
                                    destination = "iso3c", warn = FALSE,
                                    custom_match = custom_match)

  unmatched <- unique(country_raw[is.na(iso3)])
  if (length(unmatched) > 0) {
    warning(
      length(unmatched), " distinct value(s) in ", source_label,
      " could not be matched to an ISO3 country code and will be dropped: ",
      paste(unmatched, collapse = ", "),
      " -- check for numeric country codes, merged/concatenated fields, ",
      "or non-standard names that need a custom_match entry in to_iso3()."
    )
  }

  mapping_check <- tibble::tibble(raw = country_raw, iso3 = iso3) %>%
    dplyr::distinct() %>%
    dplyr::arrange(raw)
  message("Country name -> ISO3 mapping used for ", source_label,
          " -- VERIFY THIS BY EYE, especially similarly-named/spelled countries",
          " (e.g. confirm Niger is not mapped to Nigeria's code):")
  print(mapping_check, n = Inf)

  iso3
}

cholera_age <- read_csv(
  opt$age_csv_path,
  col_types = cols(
    year = col_integer(), region = col_character(), country = col_character(),
    cases_le5 = col_integer(), cases_gt5 = col_integer(), .default = col_guess()
  )
) %>%
  mutate(country = to_iso3(country, opt$age_csv_path)) %>%
  filter(!is.na(country)) %>%
  mutate(n_known_age = cases_le5 + cases_gt5) %>%
  filter(n_known_age > 0)

message(dplyr::n_distinct(cholera_age$country), " distinct countries in ", opt$age_csv_path,
        " after ISO3 conversion: ", paste(sort(unique(cholera_age$country)), collapse = ", "))

# Endemicity classification (5-year lookback) ----------------------------

have_totals <- file.exists(opt$totals_csv_path)

presence_2021_2023 <- cholera_age %>%
  distinct(country, year) %>%
  filter(year %in% 2021:2023) %>%
  transmute(country, year, reported = TRUE)

if (have_totals) {
  presence_2016_2020 <- read_csv(
    opt$totals_csv_path,
    col_types = cols(year = col_integer(), country = col_character(), total_cases = col_integer())
  ) %>%
    mutate(country = to_iso3(country, opt$totals_csv_path)) %>%
    filter(!is.na(country)) %>%
    transmute(country, year, reported = TRUE)
  presence_history <- bind_rows(presence_2016_2020, presence_2021_2023)
} else {
  warning(
    "`", opt$totals_csv_path, "` not found -- proceeding with only 2021-2023 ",
    "reporting history. See cholera_age_distribution.qmd for what this file adds."
  )
  presence_history <- presence_2021_2023
}

target_years <- 2021:2024
countries_with_data <- sort(unique(cholera_age$country))

endemicity <- expand_grid(country = countries_with_data, target_year = target_years) %>%
  mutate(window_start = target_year - 5L, window_end = target_year - 1L) %>%
  rowwise() %>%
  mutate(
    n_years_reported = sum(
      presence_history$country == country &
        presence_history$year >= window_start &
        presence_history$year <= window_end
    )
  ) %>%
  ungroup() %>%
  mutate(endemic = as.integer(n_years_reported >= 3))

# Country / region indices ------------------------------------------------

model_data <- cholera_age %>%
  inner_join(endemicity, by = c("country", "year" = "target_year"))

n_dropped <- nrow(cholera_age) - nrow(model_data)
if (n_dropped > 0) {
  message(n_dropped, " row(s) had no matching endemicity classification and were dropped.")
}

country_lookup <- model_data %>%
  count(country, region) %>%
  group_by(country) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(country) %>%
  mutate(country_id = row_number()) %>%
  select(country, region, country_id)

region_lookup <- country_lookup %>%
  distinct(region) %>%
  arrange(region) %>%
  mutate(region_id = row_number())

country_lookup <- country_lookup %>% left_join(region_lookup, by = "region")

model_data <- model_data %>%
  left_join(country_lookup %>% select(country, country_id), by = "country")

C <- nrow(country_lookup)

message(C, " countries with age-split data across ", nrow(region_lookup), " regions.")

# Fit ---------------------------------------------------------------------

if (file.exists(fit_rds_path) && !isTRUE(opt$refit)) {
  message("Using cached fit: ", fit_rds_path, " (pass --refit TRUE to re-fit)")
} else {
  message("Compiling and fitting ", model_name, " (", stan_file, ") ...")

  stan_data <- list(
    N = nrow(model_data),
    C = C,
    country = model_data$country_id,
    n = model_data$n_known_age,
    y = model_data$cases_le5,
    endemic = model_data$endemic
  )

  mod <- cmdstan_model(stan_file)
  fit <- mod$sample(
    data = stan_data,
    chains = opt$chains,
    parallel_chains = opt$parallel_chains,
    iter_warmup = opt$iter_warmup,
    iter_sampling = opt$iter_sampling,
    adapt_delta = opt$adapt_delta,
    seed = opt$seed,
    refresh = 0
  )

  # fit$save_object() (not a plain saveRDS()) moves the CmdStan output CSVs
  # to sit next to the .rds, so the cached fit is self-contained and still
  # readable in a future session -- see cholera_age_model_fits.qmd's
  # fit-function chunk for the full explanation.
  fit$save_object(file = fit_rds_path)
}

# Persist the lookup + row-level country index -----------------------------
# Neither of these is written anywhere in cholera_age_model_fits.qmd -- both
# only exist in that notebook's in-session environment -- so the pipeline
# (a separate, non-interactive process) has no way to get them without this.

write_csv(country_lookup, file.path(opt$scaling_input_dir, "country_lookup.csv"))
write_csv(tibble(country_id = model_data$country_id),
          file.path(opt$scaling_input_dir, "country_id_vec.csv"))

message("Wrote fit, country_lookup.csv, and country_id_vec.csv to ", opt$scaling_input_dir)
