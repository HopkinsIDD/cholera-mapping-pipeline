option_list <- list(
  optparse::make_option(c("-c", "--input_cases_file"), action = "store", default = "prepared_cases_input.Rda", type="character", help = "Prepared cases input"),
  optparse::make_option(c("-i", "--input_covar_file"), action = "store", default = "prepared_covar_input.Rda", type="character", help = "Prepared covariate cube input"),
  optparse::make_option(c("-u", "--dbuser"), action = "store", default =  Sys.getenv("USER"), type="character", help = "Database user"),
  optparse::make_option(c("-d", "--cholera_directory"), action = "store", default = "../", type="character", help = "Cholera directory"),
  optparse::make_option(c("-s", "--timesteps"), action = "store", default = 1, type="integer", help = "Smooth covariate number time steps"),
  optparse::make_option(c("-n", "--ncore"), action = "store", default = parallel::detectCores() - 2, type="integer", help = "Number of cores to run"),
  optparse::make_option(c("-t", "--res_time"), action = "store", default = "year", type="character", help = "Temporal aggregations"),
  optparse::make_option(c("-a", "--cases_column"), action = "store", default = "attributes.fields.suspected_cases", type="character", help = "Column with cholera cases"),
  optparse::make_option(c("-x", "--covariate_choices"), action = "store", default = "distance.to.water", type="character", help = "Choice of covariates separated byu spaces"),
  optparse::make_option(c("-o", "--output_file"), action = "store", default = "prepared_stan_input.Rda", type="character", help = "Output filename")
)

parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(parser)

source(paste(opt$cholera_directory, "Analysis/R/prepare_stan_input.R", sep = "/"))

prepare_stan_input(
  opt$input_cases_file,
  opt$input_covar_file,
  opt$dbuser,
  opt$cholera_directory,
  opt$timesteps,
  opt$ncore,
  opt$res_time,
  opt$cases_column,
  opt$covariate_choices,
  opt$output_file
)