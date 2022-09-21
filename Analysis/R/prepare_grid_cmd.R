option_list <- list(
  optparse::make_option(c("-u", "--dbuser"), action = "store", default = Sys.getenv("USER"), type = "character", help = "Database user"),
  optparse::make_option(c("-d", "--cholera_directory"), action = "store", default = "../", type = "character", help = "Cholera directory"),
  optparse::make_option(c("-r", "--res_space"), action = "store", default = 20, type = "numeric", help = "Temporal aggregations"),
  optparse::make_option(c("-i", "--ingest"), action = "store", default = T, type = "logical", help = "Flag to do ingestion, if false stops if grid has not already been preprocessed")
)

parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(parser)

source(paste(opt$cholera_directory, "Analysis/R/prepare_grid.R", sep = "/"))

prepare_grid(
  dbuser = opt$dbuser,
  cholera_directory = opt$cholera_directory,
  res_space = opt$res_space,
  ingest = opt$ingest
)
