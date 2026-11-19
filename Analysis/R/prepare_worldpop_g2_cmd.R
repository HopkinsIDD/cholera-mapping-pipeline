# Convert downloaded WorldPop Global 2 yearly 1 km GeoTIFFs into the yearly
# NetCDF files the covariate ingestion reads (<layers>/pop/). No database.
#
#   Rscript Analysis/R/prepare_worldpop_g2_cmd.R -l Layers -y 2015:2030
#
# Defaults: sources in <layers>/worldpop_g2_raw (hpc/unige/tools/download_worldpop_g2.sh),
# land from CHOLERA_MASTER_GRID_FILE.

option_list <- list(
  optparse::make_option(c("-l", "--layers_directory"), default = "Layers", type = "character",
                        help = "Layers directory"),
  optparse::make_option(c("-s", "--raw_directory"), default = NULL, type = "character",
                        help = "Downloaded GeoTIFFs (default: <layers>/worldpop_g2_raw)"),
  optparse::make_option(c("-m", "--land_file"), default = NULL, type = "character",
                        help = "Land raster on the same lattice (default: CHOLERA_MASTER_GRID_FILE)"),
  optparse::make_option(c("-o", "--out_directory"), default = NULL, type = "character",
                        help = "Output directory (default: <layers>/pop)"),
  optparse::make_option(c("-y", "--years"), default = "2015:2030", type = "character",
                        help = "Years: a range (2015:2030) or a comma-separated list"),
  optparse::make_option(c("--overwrite"), default = FALSE, type = "logical",
                        help = "Rewrite existing files")
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

layers_dir <- normalizePath(opt$layers_directory, mustWork = TRUE)
raw_dir <- if (is.null(opt$raw_directory)) file.path(layers_dir, "worldpop_g2_raw") else opt$raw_directory
out_dir <- if (is.null(opt$out_directory)) file.path(layers_dir, "pop") else opt$out_directory
land_file <- if (is.null(opt$land_file)) taxdat::default_master_grid_source(layers_dir) else opt$land_file
years <- if (grepl(":", opt$years)) {
  b <- as.integer(strsplit(opt$years, ":")[[1]])
  seq(b[1], b[2])
} else {
  as.integer(strsplit(opt$years, ",")[[1]])
}

res <- taxdat::prepare_worldpop_g2(raw_dir = raw_dir, land_file = land_file, out_dir = out_dir,
                                   years = years, overwrite = opt$overwrite)
print(res[, c("year", "total", "settled_pixels", "pixels_off_land", "people_off_land")], row.names = FALSE)
cat("Done WorldPop Global 2:", nrow(res), "year(s) in", out_dir, "\n")
