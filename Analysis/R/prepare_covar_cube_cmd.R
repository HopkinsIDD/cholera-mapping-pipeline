option_list <- list(
  optparse::make_option(c("-l", "--covarlist"), action = "store", default = paste0("covar_list_", Sys.getenv("USER"), ".txt"), type="character", help = "Text file with the covariates to ingest"),
  optparse::make_option(c("-f", "--fname"), action = "store", default = "preprocessed_covar.Rda", type="character", help = "Output filename"),
  optparse::make_option(c("-u", "--dbuser"), action = "store", default =  Sys.getenv("USER"), type="character", help = "Database user"),
  optparse::make_option(c("-d", "--cholera_directory"), action = "store", default = "./", type="character", help = "Cholera directory"),
  optparse::make_option(c("-s", "--start_time"), action = "store", default = "2016-01-01", type="character", help = "Model start time"),
  optparse::make_option(c("-e", "--end_time"), action = "store", default = "2016-02-01", type="character", help = "Model end time"),
  optparse::make_option(c("-t", "--dt"), action = "store", default = "year", type="character", help = "Temporal aggregations"),
  optparse::make_option(c("-r", "--res_space"), action = "store", default = "20", type="character", help = "Spatial resolution")
)

parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(parser)

source(paste(opt$cholera_directory, "Analysis/R/prepare_covar_cube.R", sep = "/"))

## Parameters
# List of covariates
covar_list <- readLines(opt$covarlist)
temporal_aggregate_time_unit <- opt$dt

prepare_covar_cube(
  covar_list,
  opt$fname,
  opt$dbuser,
  opt$cholera_directory,
  opt$start_time,
  opt$end_time,
  opt$dt,
  opt$res_space
) 
