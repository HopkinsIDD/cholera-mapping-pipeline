// Joshua's attempt at converting DAGAR to stan.
//Based on code from Abhi dagar_poisson[1].R
// 3/3/2020 Annual incidence model with time-varying covariates
// 12/15/2020 Flexible code allowing for specification of time-slice random effect and censoring
//            Time-slice random effect can be specified as:
//              - 0-centered prior, no reference and no sum-to-zero constraint
//              - Autocorrelated (increments ~ N(0, sigma)) with sum-to-zero constraint 
data {
  int <lower=1> N; //length of non-NA grid cells (space and time)
  int <lower=1> N_edges;
  int <lower=1> smooth_grid_N; //size of smooth grid (# non-NA cells * (timesteps+1))
  
  //The adjacency matrix node1 should be sorted and lower triangular
  int <lower=1, upper=smooth_grid_N> node1[N_edges]; //column 1 of the adjacency matrix
  int <lower=1, upper=smooth_grid_N> node2[N_edges]; //column 2 of the adjacency matrix
  vector<lower=0, upper=smooth_grid_N>[smooth_grid_N] diag; //rowSums of directed adjacency matrix
  
  real <lower=1> pop[N]; //population by cell over all time points
  real <lower=0, upper=1> meanrate;
  
  int <lower=0> M; //number of observations
  int <lower=0> y[M];      //observed counts
  int <lower=0, upper=M> M_full;    // number of observations that cover a full modeling time slice
  int <lower=0, upper=M> M_left;    // number of left-censored observations (open lower bound on the observation)
  int <lower=0, upper=M> M_right;   // number of right-censored observations (open upper bound on the observation)
  
  int <lower=1, upper=M> ind_full[M_full];    // indexes of full observations
  int <lower=1, upper=M> ind_left[M_left];    // indexes of left-censored observations
  int <lower=1, upper=M> ind_right[M_right];   // indexes of right-censored observations
  
  int<lower=1> T; // number of time slices
  int <lower=0> L; // number of location periods (space and time)
  
  int <lower=M> K1; // the length of the mapping of observations to location periods and times
  int <lower=L> K2; // the length of the mapping of location periods to gridcells
  int <lower=0, upper=M> map_obs_loctime_obs[K1]; // The observation side of the mapping from observations to location/times
  int <lower=0, upper=L> map_obs_loctime_loc[K1]; // The location side of the mapping from observations to location/times
  real <lower=0, upper=1> tfrac[K1]; // The time fraction side of the mapping from observations to location/times
  int <lower=0, upper=L> map_loc_grid_loc[K2]; // the location side of the mapping from locations to gridcells
  int <lower=0, upper=N> map_loc_grid_grid[K2]; // the gridcell side of the mapping from locations to gridcells
  
  // For output summaries
  int <lower=0> L_output; // number of location periods (space and time)
  int <lower=0> M_output; // number of location periods (space and time)
  int <lower=L_output> K1_output; // the length of the mapping of observations to location periods and times
  int <lower=L_output> K2_output; // the length of the mapping of location periods to gridcells
  int <lower=0, upper=M_output> map_output_obs_loctime_obs[K1_output]; // The observation side of the mapping from observations to location/times
  int <lower=0, upper=L_output> map_output_obs_loctime_loc[K1_output]; // The location side of the mapping from observations to location/times
  int <lower=0, upper=L_output> map_output_loc_grid_loc[K2_output]; // the location side of the mapping from locations to gridcells
  int <lower=0, upper=N> map_output_loc_grid_grid[K2_output]; // the gridcell side of the mapping from locations to gridcells
  
  int <lower=0,upper=smooth_grid_N> map_smooth_grid[N]; //vector with repeating smooth_grid_N indexes repeating 1:N
  
  // Covariate stuff
  int ncovar; // Number of covariates
  matrix[N,ncovar] covar; // Covariate matrix
  int<lower=0> beta_sigma_scale;
  real<lower=0> sigma_eta_scale; // the scale of inter-annual variability
  
  // Options
  // Censoring of cases with tfracs bellow threshold
  int<lower=0, upper=1> do_censoring;
  // Random effect for each time slice
  int<lower=0, upper=1> do_time_slice_effect;
  // Autocorrelation between time slice random effects
  int<lower=0, upper=1> do_time_slice_effect_autocor;
  // Weight likelihoods by expected number of cases
  int<lower=0, upper=1> use_weights; 
  // Prior for high values of rho
  int<lower=0, upper=1> use_rho_prior;
  int<lower=0, upper=1> use_pop_weight; 
  real<lower=0> pop_weight[K2 * use_pop_weight];
  real<lower=0> pop_weight_output[K2_output * use_pop_weight];
  
  // If time slice effect pass indicator function for years without data
  vector<lower=0, upper=1>[N*do_time_slice_effect] has_data_year;
  matrix[N*do_time_slice_effect + 2 * (do_time_slice_effect != 1), T*do_time_slice_effect + 2*(do_time_slice_effect != 1)] mat_grid_time; // The time side of the mapping from locations/times to grid (2x2 in case of missing just so it's easy to create)
}

transformed data {
  vector<lower=0>[N] logpop;//populations by timestep
  real small_N = .001 * smooth_grid_N;
  real<lower=0> weights[M*(1-do_censoring)*use_weights]; //a function of the expected offset for each observation used to downwight the likelihood
  real log_meanrate = log(meanrate);
  real <lower=1> pop_loctimes[L]; // pre-computed population in each location period
  real <lower=1> pop_loctimes_output[L_output]; // pre-computed population in each location period
  
  for(i in 1:N){
    logpop[i] = log(pop[i]);
  }
  
  for (i in 1:L) {
    pop_loctimes[i] = 0;
  }
  
  for (i in 1:K2) {
    pop_loctimes[map_loc_grid_loc[i]] += pop[map_loc_grid_grid[i]];
  }
  
  for (i in 1:L_output) {
    pop_loctimes_output[i] = 0;
  }
  
  for (i in 1:K2_output) {
    pop_loctimes_output[map_output_loc_grid_loc[i]] += pop[map_output_loc_grid_grid[i]];
  }
  
  // Compute observation likelihood weights 
  if (do_censoring == 0 && use_weights == 1) {
    for (i in 1:M) {
      weights[i] = 1;
    }
    for (i in 1:K1) {
      weights[map_obs_loctime_obs[i]] += meanrate * tfrac[i] * pop_loctimes[map_obs_loctime_loc[i]];
    }
    for (i in 1:M) {
      weights[i] = sqrt(weights[i]);
    }
  }
}

parameters {
  //real beta0; //the intercept
  
  real <lower=0, upper=1> rho; // Spatial correlation parameter
  real log_std_dev_w; // Precision of the spatial effects
  
  vector[smooth_grid_N] w; // Spatial Random Effect
  
  vector[T*do_time_slice_effect] eta_tilde; // yearly random effects
  real <lower=0> sigma_eta_tilde[do_time_slice_effect];
  
  // Covariate stuff
  vector[ncovar] betas;
}
generated quantities {
  real<lower=0> tfrac_modeled_cases[M]; //expected number of cases for each observation
  real log_lik[M]; // log-likelihood of observations
  vector[N] log_lambda; //local log rate
  vector<lower=0>[L] location_cases; //cases modeled in each (temporal) location.
  vector<lower=0>[L] location_cases_output; //cases modeled in each (temporal) location.
  vector<lower=0>[L] location_rates_output; //cases modeled in each (temporal) location.
  
  vector<lower=0>[N] grid_cases; //cases modeled in each gridcell and time point.
  vector[T*do_time_slice_effect] eta; // yearly random effects
  real<lower=0> modeled_cases[M]; //expected number of cases for each observation
  
  if (do_time_slice_effect == 1) {
    for(i in 1:T) {
      // scale yearly random effects
      eta[i] = sigma_eta_scale * sigma_eta_tilde[1] * eta_tilde[i];
    }
  }
  
  // log-rates without time-slice effects
  log_lambda =  w[map_smooth_grid] + log_meanrate + covar * betas;
  
  // Add time slice effects
  if (do_time_slice_effect == 1) {
    log_lambda += (mat_grid_time * eta) .* has_data_year;
  }
  
  grid_cases = exp(log_lambda + logpop);
  
  //calculate the expected number of cases by location
  
  // calculate number of cases for each location
  for(i in 1:L){
    location_cases[i] = 0;
  }
  for(i in 1:K2){
    if (use_pop_weight == 1) {
      location_cases[map_loc_grid_loc[i]] += grid_cases[map_loc_grid_grid[i]] * pop_weight[map_loc_grid_grid[i]];
    } else {
      location_cases[map_loc_grid_loc[i]] += grid_cases[map_loc_grid_grid[i]];
    }
  }
  
  // Output cases
  for(i in 1:L_output){
    location_cases_output[i] = 0;
  }
  for(i in 1:K2_output){
    if (use_pop_weight == 1) {
      location_cases_output[map_output_loc_grid_loc[i]] += grid_cases[map_output_loc_grid_grid[i]] * pop_weight_output[map_output_loc_grid_grid[i]];
    } else {
      location_cases_output[map_output_loc_grid_loc[i]] += grid_cases[map_output_loc_grid_grid[i]];
    }
  }
  
  for(i in 1:L_output){
    location_rates_output[i] = location_cases_output[i]/pop_loctimes_output[i];
  }
  
  //first initialize to 0
  for (i in 1:M) {
    modeled_cases[i] = 0;
  }
  
  //now accumulate
  for (i in 1:K1) {
    if (do_censoring == 1) {
      modeled_cases[map_obs_loctime_obs[i]] += location_cases[map_obs_loctime_loc[i]];
    } else {
      modeled_cases[map_obs_loctime_obs[i]] += tfrac[i] * location_cases[map_obs_loctime_loc[i]];
    }
  }
  
  // first initialize to 0
  for (i in 1:M) {
    tfrac_modeled_cases[i] = 0;
  }
  //now accumulate
  for (i in 1:K1) {
    tfrac_modeled_cases[map_obs_loctime_obs[i]] += tfrac[i] * location_cases[map_obs_loctime_loc[i]];
  }
  
  if (do_censoring == 0) {
    for (i in 1:M) {
      log_lik[i] = poisson_lpmf(y[i] | modeled_cases[i]);
    }
  } else {
    // full observations
    for (i in 1:M_full) {
      log_lik[ind_full[i]] = poisson_lpmf(y[ind_full[i]] | modeled_cases[ind_full[i]]);
    }
    // rigth-censored observations
    for(i in 1:M_right){
      real lpmf;
      lpmf = poisson_lpmf(y[ind_right[i]] | modeled_cases[ind_right[i]]);
      // heuristic condition to only use the PMF if Prob(Y>y| modeled_cases) ~ 0
      if ((y[ind_right[i]] < modeled_cases[ind_right[i]]) || ((y[ind_right[i]] > modeled_cases[ind_right[i]]) && (lpmf > -35))) {
        real lls[2];
        lls[1] = poisson_lccdf(y[ind_right[i]] | modeled_cases[ind_right[i]]);
        lls[2] = lpmf;
        log_lik[ind_right[i]] = log_sum_exp(lls);
      } else {
        log_lik[ind_right[i]] = lpmf;
      }
    }
  }
}
