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

library(rstan)
library(cmdstanr)

# Run model ---------------------------------------------------------------

# Select model based on config and run level
model <- cmdstan_model(stan_model_path,
                       quiet = FALSE,
                       force_recompile = F)

# Correct to avoid passing strings
initial_values_data$stan_data$censoring_inds <- NULL

cmdstan_fit <- model$sample(
  data = initial_values_data$stan_data,
  seed = 1234,
  init =  initial_values_data$init.list,
  sig_figs = 5,
  chains = nchain,
  parallel_chains = nchain,
  threads_per_chain = NULL,
  iter_warmup = round(niter/2),
  iter_sampling = round(niter/2),
  max_treedepth = 15,
  save_warmup = F,
  refresh = 100,
  show_messages = F)

model.rand <- rstan::read_stan_csv(cmdstan_fit$output_files())

# Save output
save(model.rand, file = file_names[["stan_output"]])
