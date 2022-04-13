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
  iter_warmup = round(niter/2),
  iter_sampling = round(niter/2),
  max_treedepth = 15,
  init = initial_values_data$init.list,
  sig_figs = 5,
  save_warmup = F,
  refresh = 100
)

# Transform back to stanfit object
model.rand <- rstan::read_stan_csv(cmdstan_fit$output_files())

# Save output
save(model.rand, file = file_names[["stan_output"]])

# Run generated quantities ------------------------------------------------

# Compile cmdstanr gen quantities
gen_path <- stringr::str_replace(stan_model_path, "\\.", "_generate.")

chol_gen_model <- cmdstanr::cmdstan_model(gen_path,
                                          quiet = FALSE,
                                          force_recompile = F)

# Generate posterior probabilities
chol_gen <- chol_gen_model$generate_quantities(fitted_params = cmdstan_fit,
                                               data = initial_values_data$stan_data,
                                               parallel_chains = nchain)

# Save generated quantities
chol_gen$save_object(file = file_names[["stan_genquant"]])
