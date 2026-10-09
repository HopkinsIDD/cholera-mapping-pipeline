# Pull a run's observations on a machine with access to the taxonomy database,
# and cache the admin boundaries the run needs, so the run can then be done on
# a machine without that access (e.g. Yggdrasil compute nodes).
#
# Credentials: CHOLERA_SQL_USERNAME / CHOLERA_SQL_PASSWORD (data_source: sql) or
# CHOLERA_API_USERNAME / CHOLERA_API_KEY (data_source: api), or
# Analysis/R/database_api_key.R.
#
#   Rscript Analysis/R/pull_observations_local.R -c Analysis/configs/BDI_pilot.yml
#
# Then copy the .rds file and <layers>/admin_units/ to the cluster and run with
#   CHOLERA_OBSERVATIONS_RDS=/path/to/file.rds

option_list <- list(
  optparse::make_option(c("-c", "--config"), type = "character", help = "Run config"),
  optparse::make_option(c("-o", "--output"), default = NULL, type = "character",
                        help = "Output .rds (default Analysis/data/observations/<ISO>_<start>_<end>_<source>.rds)"),
  optparse::make_option(c("-l", "--layers_directory"), default = "Layers", type = "character",
                        help = "Layers directory; boundaries are cached in <layers>/admin_units")
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
if (is.null(opt$config)) stop("Give the run config with -c")

library(magrittr)
sf::sf_use_s2(FALSE)
config <- yaml::read_yaml(opt$config, eval.expr = TRUE)
config$countries_name <- taxdat::check_countries_name(config$countries_name)
config$data_source <- taxdat::check_data_source(config$data_source)

out <- opt$output
if (is.null(out)) {
  out <- file.path("Analysis", "data", "observations",
                   sprintf("%s_%s_%s_%s.rds", paste(config$countries_name, collapse = "-"),
                           config$start_time, config$end_time, config$data_source))
}

cases <- taxdat::pull_observations(config)
message("-- Pulled ", nrow(cases), " observations")
taxdat::save_observations_rds(cases, config, out)

cache_dir <- Sys.getenv("CHOLERA_AOI_CACHE_DIR", file.path(opt$layers_directory, "admin_units"))
levels <- sort(unique(c(0, if (is.null(config$summary_admin_levels)) 0:2 else config$summary_admin_levels)))
for (iso in config$countries_name) {
  taxdat::cache_admin_units(iso, admin_levels = levels, cache_dir = cache_dir)
}
cat("Observations:", out, "\nAdmin boundaries cached in:", cache_dir, "\n")
