#!/usr/bin/Rscript
##  This scripts purpose is to collect all data required to model cholera incidence, and store it in a form usable by stan.
##  This script mainly uses functions from the package taxdat, stored in trunk/packages/taxdat.
# Libraries
### Control Variables:

### Construct some additional parameters based on the above
# Define relevent directories
# Name the output file

# Stan modeling section
print("*** STARTING STAN MODEL ***")

library(cmdstanr)
library(rstan)


# Get runtime parameters --------------------------------------------------

max_treedepth <- taxdat::check_max_treedepth(Sys.getenv("MAX_TREEDEPTH"))

cat("-- Running with runtime parameters:\n",
    paste0(
      "\t- ", 
      c("max_treedepth"), 
      ": ", 
      c(max_treedepth)
    )
)

# Run model ---------------------------------------------------------------

# Compile cmdstanr model
chol_model <- cmdstanr::cmdstan_model(stan_model_path,
                                      quiet = FALSE,
                                      force_recompile = F)

# Remove censoring inds that are character
initial_values_data$stan_data$censoring_inds <- NULL

cmdstan_fit <- chol_model$sample(
  seed = 1234,
  data = initial_values_data$stan_data,
  chains = nchain,
  parallel_chains = nchain,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  max_treedepth = max_treedepth,
  metric = "diag_e",
  adapt_delta = 0.8,
  init = initial_values_data$init.list,
  sig_figs = 10,
  save_warmup = F,
  refresh = 100,
  term_buffer = 100,
  step_size = 1
)

# Transform back to stanfit object
model.rand <- rstan::read_stan_csv(cmdstan_fit$output_files())

# Save output
save(model.rand, file = file_names[["stan_output"]])
