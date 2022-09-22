#!/usr/bin/Rscript
## This scripts purpose is to collect all data required to model cholera
## incidence, and store it in a form usable by stan.  This script mainly uses
## functions from the package taxdat, stored in trunk/packages/taxdat.  Libraries
## Control Variables:

### Construct some additional parameters based on the above Define relevent
### directories Name the output file

# Stan modeling section
print("*** STARTING STAN MODEL ***")

library(rstan)

# Run model ---------------------------------------------------------------
start_time <- Sys.time()
model.rand <- rstan::stan(
  file = stan_model_path, data = initial_values_data$stan_data,
  chains = nchain, iter = niter, pars = c("b", "t_rowsum", "vec_var"), include = FALSE,
  control = list(max_treedepth = 15), init = initial_values_data$init.list
)
end_time <- Sys.time()

elapsed_time <- end_time - start_time

# Save output
save(model.rand, elapsed_time, file = file_names[["stan_output"]])
