# This script runs the generated quantities block

# Stan generated quantites section
print("*** STARTING STAN GENERATED QUANTITIES ***")

library(cmdstanr)
library(rstan)

# Transform back to stanfit object
cmdstan_draws <- posterior::as_draws(model.rand) 

# Run generated quantities ------------------------------------------------

# Remove censoring inds that are character
initial_values_data$stan_data$censoring_inds <- NULL
# Remove u_loctime_combs because it may have different lengths
initial_values_data$stan_data$u_loctime_combs <- NULL

# Compile cmdstanr gen quantities
chol_gen_model <- cmdstanr::cmdstan_model(stan_genquant_path,
                                          quiet = FALSE,
                                          force_recompile = F)

# Generate posterior probabilities
chol_gen <- chol_gen_model$generate_quantities(fitted_params = cmdstan_draws,
                                               data = initial_values_data$stan_data,
                                               parallel_chains = nchain)

# Save generated quantities
chol_gen$save_object(file = file_names[["stan_genquant"]])
