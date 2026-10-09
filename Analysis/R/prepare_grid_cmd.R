# Build the master grid and a modelling grid in the covariates database.
#
# Database connection: PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD.
# Example (Burundi pilot, 20 km):
#   Rscript Analysis/R/prepare_grid_cmd.R -l Layers -r 20 -a BDI -b 50

option_list <- list(
  optparse::make_option(c("-l", "--layers_directory"), default = "Layers", type = "character",
                        help = "Layers directory (holds pop_old/, grids/, admin_units/)"),
  optparse::make_option(c("-r", "--res_space"), default = 20, type = "numeric",
                        help = "Grid resolution in km"),
  optparse::make_option(c("-i", "--ingest"), default = TRUE, type = "logical",
                        help = "Build missing grids (FALSE stops instead)"),
  optparse::make_option(c("-a", "--aoi"), default = "raw", type = "character",
                        help = "Area of interest: 'raw' or an ISO3 code"),
  optparse::make_option(c("-b", "--aoi_buffer_km"), default = 50, type = "numeric",
                        help = "Buffer around the area of interest, km"),
  optparse::make_option(c("-m", "--master_grid_source"), default = NULL, type = "character",
                        help = "WorldPop 1 km GeoTIFF (default: CHOLERA_MASTER_GRID_FILE or <layers>/pop_old/ppp_2020_1km_Aggregated.tif)")
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

library(magrittr)
sf::sf_use_s2(FALSE)
layers_dir <- normalizePath(opt$layers_directory, mustWork = TRUE)
if (Sys.getenv("CHOLERA_AOI_CACHE_DIR") == "") {
  Sys.setenv(CHOLERA_AOI_CACHE_DIR = file.path(layers_dir, "admin_units"))
}
master <- if (is.null(opt$master_grid_source)) taxdat::default_master_grid_source(layers_dir) else opt$master_grid_source

aoi <- taxdat::get_aoi(taxdat::check_aoi(opt$aoi), buffer_km = opt$aoi_buffer_km,
                       snap_to = if (file.exists(master)) master else NULL)
grid <- taxdat::prepare_grid(res_space = opt$res_space, aoi = aoi, layers_dir = layers_dir,
                             ingest = opt$ingest, master_grid_source = master)
cat("full_grid_name:", grid$full_grid_name, "\ngrid_file:", grid$grid_file, "\n")
