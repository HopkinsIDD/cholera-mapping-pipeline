# Pre-compute and/or ingest covariates into the covariates database.
#
# Database connection: PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD.
# Covariates are given as dictionary abbreviations; population ("p") is always
# included first.
#
# Pre-compute only (no database; e.g. one Slurm array task per covariate):
#   Rscript Analysis/R/prepare_covariates_cmd.R -l Layers -r 20 -a BDI -c aw --precompute_only TRUE -n 8
# Load into the database (re-uses the pre-computed files):
#   Rscript Analysis/R/prepare_covariates_cmd.R -l Layers -r 20 -a BDI -c aw,dw

option_list <- list(
  optparse::make_option(c("-l", "--layers_directory"), default = "Layers", type = "character",
                        help = "Layers directory (holds covariate_dictionary.yml)"),
  optparse::make_option(c("-r", "--res_space"), default = 20, type = "numeric",
                        help = "Spatial resolution in km"),
  optparse::make_option(c("-t", "--res_time"), default = "1 years", type = "character",
                        help = "Temporal resolution"),
  optparse::make_option(c("-a", "--aoi"), default = "raw", type = "character",
                        help = "Area of interest: 'raw' or an ISO3 code"),
  optparse::make_option(c("-b", "--aoi_buffer_km"), default = 50, type = "numeric",
                        help = "Buffer around the area of interest, km"),
  optparse::make_option(c("-c", "--covar"), default = "", type = "character",
                        help = "Covariate abbreviations, comma separated (population is always added)"),
  optparse::make_option(c("-m", "--mode"), default = "ingest_missing", type = "character",
                        help = "ingest_missing | use_existing | reingest"),
  optparse::make_option(c("-n", "--n_cores"), default = 1, type = "integer",
                        help = "Processes for pre-computing files in parallel"),
  optparse::make_option(c("--precompute_only"), default = FALSE, type = "logical",
                        help = "Only fill the processed-file cache (no database access)")
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

library(magrittr)
sf::sf_use_s2(FALSE)
layers_dir <- normalizePath(opt$layers_directory, mustWork = TRUE)
if (Sys.getenv("CHOLERA_AOI_CACHE_DIR") == "") {
  Sys.setenv(CHOLERA_AOI_CACHE_DIR = file.path(layers_dir, "admin_units"))
}
covar_dict <- yaml::read_yaml(file.path(layers_dir, "covariate_dictionary.yml"))
covar_abbr <- strsplit(opt$covar, ",")[[1]]
master <- taxdat::default_master_grid_source(layers_dir)
aoi <- taxdat::get_aoi(taxdat::check_aoi(opt$aoi), buffer_km = opt$aoi_buffer_km,
                       snap_to = if (file.exists(master)) master else NULL)

if (opt$precompute_only) {
  # The grid file is enough; it was written by prepare_grid_cmd.R
  grid_file <- taxdat::grid_file_path(layers_dir, opt$res_space, taxdat::aoi_metadata(aoi)$aoi_name)
  if (!file.exists(grid_file)) {
    stop("Grid file ", grid_file, " not found; run prepare_grid_cmd.R first.")
  }
  grid <- list(full_grid_name = sprintf("grids.grid_%s_%s", opt$res_space, opt$res_space),
               grid_file = grid_file)
} else {
  grid <- taxdat::prepare_grid(res_space = opt$res_space, aoi = aoi, layers_dir = layers_dir,
                               ingest = FALSE)
}

covar_list <- taxdat::prepare_covariates(
  covar_abbr = covar_abbr, covar_dict = covar_dict, layers_dir = layers_dir,
  res_space = opt$res_space, res_time = opt$res_time, grid = grid, aoi = aoi,
  mode = opt$mode, n_cpus = opt$n_cores, precompute_only = opt$precompute_only)
cat("Covariates:", covar_list, "\n")
