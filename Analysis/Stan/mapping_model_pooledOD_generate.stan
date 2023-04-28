// This is the stan model for the cholera mapping pipeline.
// 
// The aim of the model is to produce inference on grid-level cholera rates.
//
// 
// https://discourse.mc-stan.org/t/test-soft-vs-hard-sum-to-zero-constrain-choosing-the-right-prior-for-soft-constrain/3884/31
functions {
  vector Q_sum_to_zero_QR(int N) {
    vector [2*N] Q_r;
    
    for(i in 1:N) {
      Q_r[i] = -sqrt((N-i)/(N-i+1.0));
      Q_r[i+N] = inv_sqrt((N-i) * (N-i+1));
    }
    return Q_r;
  }
  
  vector sum_to_zero_QR(vector x_raw, vector Q_r) {
    int N = num_elements(x_raw) + 1;
    vector [N] x;
    real x_aux = 0;
    
    for(i in 1:N-1){
      x[i] = x_aux + x_raw[i] * Q_r[i];
      x_aux = x_aux + x_raw[i] * Q_r[i+N];
    }
    x[N] = x_aux;
    return x;
  }
}
data {
  
  // Data sizes
  int <lower=1> N;     // length of non-NA grid cells (space and time)
  int <lower=1> smooth_grid_N; // size of smooth grid (# non-NA cells * (timesteps+1))
  int <lower=1> N_space; // length of non-NA grid cells (space only)
  int <lower=1> N_edges;       // number of edges between grid cells
  int <lower=1> T;     // number of time slices
  int <lower=0> L;     // number of location periods (space and time)
  int <lower=0> M;     // number of observations
  int <lower=M> K1;    // the length of the mapping of observations to location periods and times
  int <lower=L> K2;    // the length of the mapping of location periods to gridcells
  int <lower=0> ncovar;    // Number of covariates
  int<lower=1> N_admin_lev;    // the number of unique administrative levels in the data
  
  // Options
  int<lower=0, upper=1> do_censoring;       // Censoring of cases with tfracs bellow threshold
  int<lower=0, upper=1> do_time_slice_effect;            // Random effect for each time slice
  int<lower=0, upper=1> do_time_slice_effect_autocor;    // Autocorrelation between time slice random effects
  int<lower=0, upper=1> use_weights;        // Weight likelihoods by expected number of cases
  int<lower=0, upper=1> use_rho_prior;      // Prior for high values of autocorrelation of spatial random effects
  int<lower=0, upper=1> exp_prior;          // Double-exponential prior on covariate regression coefficients 
  int<lower=1, upper=3> obs_model;          // Observation model, 1:poisson, 2:quasi-poisson, 3:neg-binomial
  int<lower=0, upper=1> use_intercept;      // Whether to include an intercept in the model or not
  int<lower=0, upper=1> do_zerosum_cnst;    // Whether to enforce a 0-sum constraint on the yearly random effects
  int<lower=0, upper=1> do_infer_sd_eta;    // Whether to enforce a 0-sum constraint on the yearly random effects
  
  // Spatial adjacency
  // Note: The adjacency matrix node1 should be sorted and lower triangular
  int <lower=1, upper=smooth_grid_N> node1[N_edges];    // column 1 of the adjacency matrix
  int <lower=1, upper=smooth_grid_N> node2[N_edges];    // column 2 of the adjacency matrix
  vector<lower=0, upper=smooth_grid_N>[smooth_grid_N] diag;    // rowSums of directed adjacency matrix
  
  // Observations
  int <lower=0> y[M];    // observed counts of cholera cases
  int <lower=0, upper=M> M_full;    // number of observations that cover a full modeling time slice
  int <lower=0, upper=M> M_left;    // number of left-censored observations (open lower bound on the observation)
  int <lower=0, upper=M> M_right;   // number of right-censored observations (open upper bound on the observation)
  int <lower=1, upper=M> ind_full[M_full];      // indexes of full observations
  int <lower=1, upper=M> ind_left[M_left];      // indexes of left-censored observations
  int <lower=1, upper=M> ind_right[M_right];    // indexes of right-censored observations
  real <lower=0, upper=1> tfrac[K1];            // the time fraction side of the mapping from observations to location/times
  int <lower=0, upper=1> censored[K1]; // Whether data is censored in the mapping from observations to location/times
  
  // Mappings
  int <lower=0, upper=M> map_obs_loctime_obs[K1];    // the observation side of the mapping from observations to location/times
  int <lower=0, upper=L> map_obs_loctime_loc[K1];    // the location side of the mapping from observations to location/times
  int <lower=0, upper=L> map_loc_grid_loc[K2];       // the location side of the mapping from locations to gridcells
  int <lower=0, upper=N> map_loc_grid_grid[K2];      // the gridcell side of the mapping from locations to gridcells
  int <lower=0, upper=smooth_grid_N> map_smooth_grid[N];    // vector with repeating smooth_grid_N indexes repeating 1:N
  real <lower=0, upper=1> map_loc_grid_sfrac[K2];    // the population-weighed location spatial fraction covered by each gridcell
  int <lower=1, upper=N_space> map_spacetime_space_grid[N];  // map from spacextime grid cells to space-only grid
  int<lower=1, upper=N_admin_lev> map_obs_admin_lev[M];    // administrative level of each observation for observation model
  
  // Time slices
  vector<lower=0, upper=1>[N*do_time_slice_effect] has_data_year;
  // If time slice effect pass indicator function for years without data
  matrix[N*do_time_slice_effect + 2 * (do_time_slice_effect != 1), T*do_time_slice_effect + 2*(do_time_slice_effect != 1)] mat_grid_time; // The time side of the mapping from locations/times to grid (2x2 in case of missing just so it's easy to create)
  
  // Covariates
  real <lower=1> pop[N];               // population by cell over all time points
  real <lower=0, upper=1> meanrate;    // mean cholera rate used as offset
  matrix[N,ncovar] covar;              // covariate matrix
  
  // Priors
  int<lower=0> beta_sigma_scale;    // the scale of regression coefficients
  real<lower=0> sigma_eta_scale;    // the scale of temporal random effects sd
  
  // For output summaries
  //  Data sizes
  int <lower=0> L_output; // number of location periods (space and time)
  int <lower=0> L_output_space; // number of location periods (space and time)
  int <lower=0> M_output; // number of location periods (space and time)
  int <lower=L_output> K1_output; // the length of the mapping of observations to location periods and times
  int <lower=L_output> K2_output; // the length of the mapping of location periods to gridcells
  //  Mappings
  int <lower=0, upper=M_output> map_output_obs_loctime_obs[K1_output]; // The observation side of the mapping from observations to location/times
  int <lower=0, upper=L_output> map_output_obs_loctime_loc[K1_output]; // The location side of the mapping from observations to location/times
  int <lower=0, upper=L_output> map_output_loc_grid_loc[K2_output]; // the location side of the mapping from locations to gridcells
  int <lower=0, upper=N> map_output_loc_grid_grid[K2_output]; // the gridcell side of the mapping from locations to gridcells
  int <lower=0, upper=L_output_space> map_output_loctime_loc[L_output]; // Map from space x time location ids to space only location
  int <lower=0> map_output_loc_adminlev[L_output_space]; // Map from space location ids to admin level
  real<lower=0> map_loc_grid_sfrac_output[K2_output];
  //  Population at risk 
  int<lower=0> N_cat;    // Number of incidence categories. For now there are no checks whether categories are mutually exclusive or not
  real<lower=0> risk_cat_low[N_cat];    // lower bound of categories
  real<lower=0> risk_cat_high[N_cat];   // upper bound of categories
}

transformed data {
  vector<lower=0>[N] logpop;              // populations by timestep
  real log_meanrate = log(meanrate);
  real<lower=0> weights[M*(1-do_censoring)*use_weights];    // a function of the expected offset for each observation used to downwight the likelihood
  real <lower=0> pop_loctimes[L];        // pre-computed population in each location period
  int N_output_adminlev = max(map_output_loc_adminlev)+1;    // number of admin levels in output
  real <lower=0, upper=1> tfrac_censoring[K1]; // tfrac accounting for censoring
  int<lower=0, upper=1> do_overdispersion;    // derived option to know whether the models contain overdispertion or not
  vector[2*T] Q_r = Q_sum_to_zero_QR(T);      // this is for the 0-centered temporal random effects if used
  real eta_zerosum_raw_sigma = inv_sqrt(1 - inv(T));
  int<lower=0, upper=T> size_eta;
  int<lower=0, upper=1> size_sd_eta;
  
  for (i in 1:K1) {
    if (censored[i] == 1) {
      tfrac_censoring[i] = 1;  
    } else {
      tfrac_censoring[i] = tfrac[i];  
    }
  }
  
  for(i in 1:N){
    logpop[i] = log(pop[i]);
  }
  
  for (i in 1:L) {
    pop_loctimes[i] = 0;
  }
  
  for (i in 1:K2) {
    pop_loctimes[map_loc_grid_loc[i]] += pop[map_loc_grid_grid[i]] * map_loc_grid_sfrac[i];
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
  
  // If poisson likelihood then no overdispertion
  if (obs_model == 1) {
    do_overdispersion = 0;
  } else {
    do_overdispersion = 1;
  }
  
  if (do_time_slice_effect == 1) {
    if (do_zerosum_cnst == 1) {
      size_eta = T - 1;
      size_sd_eta = 0;
    } else {
      size_eta = T;
      size_sd_eta = 1;
    }
  } else {
    size_eta = 0;
    size_sd_eta = 0;
  }
}

parameters {
  // Intercept
  real alpha[use_intercept];    
  
  // Spatial random effects
  real <lower=0, upper=1> rho;    // spatial correlation parameter
  real log_std_dev_w;             // precision of the spatial effects
  vector[smooth_grid_N] w;        // spatial random effect
  
  // Temporal random effects
  vector[size_eta] eta_tilde;    // uncentered temporal random effects
  real <lower=0> sigma_eta[size_sd_eta];    // sd of temporal random effects
  
  // Covariate effects
  vector[ncovar] betas;
  
  // Overdispersion parameters
  vector<lower=0>[N_admin_lev*do_overdispersion] inv_od_param;
  // mean and sd of pooled od parameter for lower admin levels
  real<lower=0> h_inv_od_mu[do_overdispersion];   
  real<lower=0> h_inv_od_sd[do_overdispersion];
}
transformed parameters {
  vector[N_admin_lev*do_overdispersion] od_param;
  
  if (do_overdispersion == 1) {
    for (i in 1:N_admin_lev) {
      od_param[i] = 1/inv_od_param[i];
    }
  }
}
generated quantities {
  
  // Model estimates
  real<lower=0> tfrac_modeled_cases[M]; //expected number of cases for each observation
  real<lower=0> modeled_cases[M]; //expected number of cases for each observation
  real log_lik[M]; // log-likelihood of observations
  vector[N] grid_cases; //cases modeled in each gridcell and time point.
  vector[N] log_lambda; //local log rate
  vector[N_space] space_grid_rates; // mean annual incidence rates at grid level
  
  vector<lower=0>[L] location_cases; //cases modeled in each (temporal) location.
  vector[T*do_time_slice_effect] eta; // yearly random effects
  real sigma_eta_val;        // value of sigma_eta. This is either fixed to sigma_eta_scale if do_infer_sd_eta==0, or sigma_eta otherwise
  
  // Outputs at given admin levels
  vector<lower=0>[L_output] location_cases_output; //cases modeled in each (temporal) location.
  vector<lower=0>[L_output] location_rates_output; //rates modeled in each (temporal) location.
  vector<lower=0>[L_output_space] location_total_cases_output; //cases modeled in each location across time slices.
  vector<lower=0>[L_output_space] location_total_rates_output; //rates modeled in each location  across time slices.
  matrix<lower=0>[L_output_space, N_cat] location_risk_cat_num;    // number of people in each location in each risk category
  matrix<lower=0>[L_output_space, N_cat] location_risk_cat_prop;    // proportion of people in each location in each risk category
  int<lower=0> location_risk_cat[L_output_space] ;    // risk category for each space location
  matrix<lower=0>[N_cat, N_output_adminlev] tot_pop_risk;    // total number of people in admin units in each risk category by admin level
  
  // Data outputs to return (same for all samples)
  real <lower=0> pop_loctimes_output[L_output];    // population in each output location period
  real <lower=0> pop_loc_output[L_output_space];   // population in each output location (space only)
  
  for (i in 1:L_output) {
    pop_loctimes_output[i] = 0;
  }
  
  for (i in 1:K2_output) {
    pop_loctimes_output[map_output_loc_grid_loc[i]] += pop[map_output_loc_grid_grid[i]] * map_loc_grid_sfrac_output[i];
  }
  
  
  // ---- Part A: Grid-level rates and cases ----
  if (do_infer_sd_eta == 1) {
    sigma_eta_val = sigma_eta[1];
  } else {
    sigma_eta_val = sigma_eta_scale;
  }
  
  if (do_time_slice_effect == 1) {
    if (do_zerosum_cnst == 0) {
      for(i in 1:T) {
        // scale yearly random effects
        eta[i] = sigma_eta_val * eta_tilde[i];
      }
    } else {
      // QR decomposition method
      eta = sum_to_zero_QR(eta_tilde, Q_r);
    }    
  }
  
  // log-rates without time-slice effects
  log_lambda =  w[map_smooth_grid] + log_meanrate;
  
  if (use_intercept == 1) {
    log_lambda += alpha[1];
  }
  
  // covariates if applicable
  if (ncovar > 1) {
    log_lambda += covar * betas;
  }
  
  // Add time slice effects
  if (do_time_slice_effect == 1) {
    log_lambda += (mat_grid_time * eta) .* has_data_year;
  }
  
  grid_cases = exp(log_lambda + logpop);
  // --- End Part A ---
  
  // ----  Part B: Modeled number of cases for observed location-periods ----
  // calculate number of cases for each location
  for(i in 1:L){
    location_cases[i] = 0;
  }
  
  for(i in 1:K2){
    location_cases[map_loc_grid_loc[i]] += grid_cases[map_loc_grid_grid[i]] * map_loc_grid_sfrac[i];
  }
  
  // B.1: Modeled cases as used in observation model (depends on censoring)
  for (i in 1:M) {
    modeled_cases[i] = 0;
  }
  
  //now accumulate
  for (i in 1:K1) {
    if (do_censoring == 1) {
      modeled_cases[map_obs_loctime_obs[i]] += tfrac_censoring[i] * location_cases[map_obs_loctime_loc[i]];
    } else {
      modeled_cases[map_obs_loctime_obs[i]] += tfrac[i] * location_cases[map_obs_loctime_loc[i]];
    }
  }
  
  // B.2: Modeled cases accounting for tfrac (for reporting)
  for (i in 1:M) {
    tfrac_modeled_cases[i] = 0;
  }
  //now accumulate
  for (i in 1:K1) {
    tfrac_modeled_cases[map_obs_loctime_obs[i]] += tfrac[i] * location_cases[map_obs_loctime_loc[i]];
  }
  // ---  End Part B ---
  
  // ---- Part C: Modeled number of cases for output summary location-periods ----
  
  for(i in 1:L_output){
    location_cases_output[i] = 0;
  }
  
  for(i in 1:L_output_space){
    location_total_cases_output[i] = 0;
  }
  
  for(i in 1:K2_output){
    location_cases_output[map_output_loc_grid_loc[i]] += grid_cases[map_output_loc_grid_grid[i]] * map_loc_grid_sfrac_output[i];
  }
  
  {
    // This block computes the total cases and mean rates across time
    real tot_loc_pop[L_output_space]; // store the total exposed population across time slices
    
    for (i in 1:L_output_space) {
      tot_loc_pop[i] = 0;
    }
    
    // Compute total cases and total exposed population
    for (i in 1:L_output) {
      location_total_cases_output[map_output_loctime_loc[i]] += location_cases_output[i];
      tot_loc_pop[map_output_loctime_loc[i]] += pop_loctimes_output[i];
    }
    
    // Compute mean rates at each location
    for (i in 1:L_output_space) {
      location_total_rates_output[i] = location_total_cases_output[i]/tot_loc_pop[i];
    }
    
    // Compute average population in each output location (space only)
    for (i in 1:L_output_space) {
      pop_loc_output[i] = tot_loc_pop[i]/T;
    }
  }
  
  for(i in 1:L_output){
    location_rates_output[i] = location_cases_output[i]/pop_loctimes_output[i];
  }
  // ---  End Part C ---
  
  // ---- Part D: People at risk ----
  // This block computes the number of people at risk
  
  // First comput mean rates at grid level
  for (i in 1:N_space) {
    space_grid_rates[i] = 0;
  }
  
  for (i in 1:N) {
    //  We know that there are T time slices
    space_grid_rates[map_spacetime_space_grid[i]] += exp(log_lambda[i])/T;
  }
  
  {
    // Loop over space output locations and compute numbers at risk
    // Since there are T pixel/location intersections in K2_output we only add in the first.
    int check_done[N_space, L_output_space];
    
    for (i in 1:N_space) {
      for (j in 1:L_output_space) {
        check_done[i, j] = 0;
      }
    }
    
    // Initialize risk cat num
    for (i in 1:L_output_space) {
      for (j in 1:N_cat) {
        location_risk_cat_num[i, j] = 0;
      }
    }
    
    for (i in 1:K2_output) {
      int s = map_spacetime_space_grid[map_output_loc_grid_grid[i]];
      real r = space_grid_rates[s];
      int l = map_output_loctime_loc[map_output_loc_grid_loc[i]];  // which space location period we are in
      if (check_done[s, l] == 0) {
        for (j in 1:N_cat) {
          if (r >= risk_cat_low[j] && r < risk_cat_high[j]) {
            location_risk_cat_num[l, j] += pop[map_output_loc_grid_grid[i]] * map_loc_grid_sfrac_output[i]; 
          }
        }
        check_done[s, l] = 1;
      }
    }
    
    // Compute proportions
    for (i in 1:L_output_space) {
      for (j in 1:N_cat) {
        location_risk_cat_prop[i, j] = location_risk_cat_num[i, j]/pop_loc_output[i];
      }
    }
    
    // Determine risk category for each output location
    // Initialize to lowest risk category
    for (i in 1:L_output_space) {
      location_risk_cat[i] = 1;
    }
    
    // This algorithm assumes that risk categories are mutually exclusive and sorted
    // in increasing order
    for (i in 1:L_output_space) {
      for (j in 1:N_cat) {
        if (location_risk_cat_num[i, j] > 1e5 || location_risk_cat_prop[i, j] > .1) {
          location_risk_cat[i] = j;
        }
      }
    }
  }
  // --- End Part D ---
  
  // ---- Part E: Total population at risk ----
  
  // Initialize
  for (i in 1:N_cat) {
    for (j in 1:N_output_adminlev) {
      tot_pop_risk[i, j] = 0;
    }
  } 
  
  // Sum over locations
  for (i in 1:L_output_space) {
    int j = location_risk_cat[i];
    int k = map_output_loc_adminlev[i] + 1;
    tot_pop_risk[j, k] += pop_loc_output[i];
  }
  // ---  End Part E ---
  
  // ---- Part F: Log-likelihoods ----
  if (do_censoring == 0) {
    for (i in 1:M) {
      if (obs_model == 1) {
        // Poisson likelihood
        log_lik[i] = poisson_lpmf(y[i] | modeled_cases[i]);
      } else if (obs_model == 2) {
        // Quasi-poisson likelihood
        log_lik[i] = neg_binomial_2_lpmf(y[i] | modeled_cases[i], od_param[map_obs_admin_lev[i]] * modeled_cases[i]);
      } else {
        // Neg-binom likelihood
        log_lik[i] = neg_binomial_2_lpmf(y[i] | modeled_cases[i], od_param[map_obs_admin_lev[i]]);
      }
    }
    
  } else {
    // full observations
    for (i in 1:M_full) {
      if (obs_model == 1) {
        // Poisson likelihood
        log_lik[ind_full[i]] = poisson_lpmf(y[ind_full[i]] | modeled_cases[ind_full[i]]);
      } else if (obs_model == 2) {
        // Quasi-poisson likelihood
        log_lik[ind_full[i]] = neg_binomial_2_lpmf(y[ind_full[i]] | modeled_cases[ind_full[i]], od_param[map_obs_admin_lev[ind_full[i]]] * modeled_cases[ind_full[i]]);
      } else {
        // Neg-binom likelihood
        log_lik[ind_full[i]] = neg_binomial_2_lpmf(y[ind_full[i]] | modeled_cases[ind_full[i]], od_param[map_obs_admin_lev[ind_full[i]]]);
      }
    }
    // rigth-censored observations
    for(i in 1:M_right){
      real lpmf;
      if (obs_model == 1) {
        // Poisson likelihood
        lpmf = poisson_lpmf(y[ind_right[i]] | modeled_cases[ind_right[i]]);
      } else if (obs_model == 2) {
        // Quasi-poisson likelihood
        lpmf = neg_binomial_2_lpmf(y[ind_right[i]] | modeled_cases[ind_right[i]], od_param[map_obs_admin_lev[ind_right[i]]] * modeled_cases[ind_right[i]]);
      } else {
        // Neg-binom likelihood
        lpmf = neg_binomial_2_lpmf(y[ind_right[i]] | modeled_cases[ind_right[i]], od_param[map_obs_admin_lev[ind_right[i]]]);
      }
      
      // heuristic condition to only use the PMF if Prob(Y>y| modeled_cases) ~ 0
      if ((y[ind_right[i]] < modeled_cases[ind_right[i]]) || ((y[ind_right[i]] > modeled_cases[ind_right[i]]) && (lpmf > -35))) {
        real lls[2];
        if (obs_model == 1) {
          // Poisson likelihood
          lls[1] = poisson_lccdf(y[ind_right[i]] | modeled_cases[ind_right[i]]);
        } else if (obs_model == 2) {
          // Quasi-poisson likelihood
          lls[1] = neg_binomial_2_lccdf(y[ind_right[i]] | modeled_cases[ind_right[i]], od_param[map_obs_admin_lev[ind_right[i]]] * modeled_cases[ind_right[i]]);
        } else {
          // Neg-binom likelihood
          lls[1] = neg_binomial_2_lccdf(y[ind_right[i]] | modeled_cases[ind_right[i]], od_param[map_obs_admin_lev[ind_right[i]]]);
        }
        lls[2] = lpmf;
        log_lik[ind_right[i]] = log_sum_exp(lls);
      } else {
        log_lik[ind_right[i]] = lpmf;
      }
    }
  }
  // ---  End Part F ---
  
}
