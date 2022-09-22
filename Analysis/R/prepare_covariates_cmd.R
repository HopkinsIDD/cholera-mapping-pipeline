option_list <- list(
  optparse::make_option(c("-u", "--dbuser"), action = "store", default = Sys.getenv("USER"), type = "character", help = "Database user"),
  optparse::make_option(c("-d", "--cholera_covariates_directory"), action = "store", default = "./", type = "character", help = "Cholera directory"),
  optparse::make_option(c("-r", "--res_space"), action = "store", default = 20, type = "integer", help = "Spatial resolution"),
  optparse::make_option(c("-t", "--res_time"), action = "store", default = "1 year", type = "character", help = "Temporal resolution"),
  optparse::make_option(c("-i", "--ingest"), action = "store", default = T, type = "logical", help = "Flag to do ingestion, if false stops if covariates has not already been ingested"),
  optparse::make_option(c("-p", "--do_parallel"), action = "store", default = T, type = "logical", help = "Flag to do parallel pre-processing, this needs to be done prior to ingestion. Useful for temporal covariates."),
  optparse::make_option(c("-n", "--n_cores"), action = "store", default = parallel::detectCores() - 2, type = "integer", help = "Number of cores to use for parallel computation"),
  optparse::make_option(c("-o", "--ovrt_covar"), action = "store", default = T, type = "logical", help = "Flag to overwrite covariates when ingesting in database"),
  optparse::make_option(c("-x", "--ovrt_metadata_table"), action = "store", default = FALSE, type = "logical", help = "Flag to overwrite covariates metatdata table in database"),
  optparse::make_option(c("-m", "--redo_metadata"), action = "store", default = FALSE, type = "logical", help = "Flag to re-extract metadata information"),
  optparse::make_option(c("-c", "--covar"), action = "store", default = "aw", type = "character", help = "List of covariates to use, specified as abbreviations separated by commas"),
  optparse::make_option(c("-g", "--full_grid_name"), action = "store", default = "public.grid_20_20", type = "character", help = "Name of full grid"),
  optparse::make_option(c("-a", "--aoi_name"), action = "store", default = "raw", type = "character", help = "Are of interest")
)

parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(parser)

source(paste(stringr::str_replace(opt$cholera_covariates_directory, "/Layers", ""), "Analysis/R/prepare_covariates.R", sep = "/"))
prepare_covariates(
  dbuser = opt$dbuser,
  cholera_covariates_directory = opt$cholera_covariates_directory,
  res_space = opt$res_space,
  res_time = opt$res_time,
  ingest = opt$ingest,
  do_parallel = opt$do_parallel,
  n_cores = opt$n_cores,
  ovrt_covar = opt$ovrt_covar,
  ovrt_metadata_table = opt$ovrt_metadata_table,
  redo_metadata = opt$redo_metadata,
  covar = opt$covar,
  full_grid_name = opt$full_grid_name,
  aoi_name = opt$aoi_name
)
