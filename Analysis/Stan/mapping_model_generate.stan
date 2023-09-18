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
  
  // Continous version of the Negative binomial function
  // Replace the binomial coefficient with ratio of gamma functions
  // Following: https://stats.stackexchange.com/questions/310676/continuous-generalization-of-the-negative-binomial-distribution
  real neg_binomial_2_cont_lpdf(real x, real mu, real phi) {
    real ll;
    real log_gamma_ratio;    // the ratio of gamma functions replacing the binomial coefficient
    
    log_gamma_ratio = lgamma(x + phi) - lgamma(x + 1) - lgamma(phi);
    ll = log_gamma_ratio + x * log(mu) - phi * log1p(mu/phi) - x * log(mu + phi);
    
    return ll;
  }
  
  // Continous version of the Poisson distribution
  // Replace the factorial with a gamma function
  real poisson_cont_lpdf(real x, real lambda) {
    real ll;
    
    ll = -lgamma(x + 1) + x * log(lambda) - lambda;
    
    return ll;
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
  int <lower=0> L_combs;     // number of unique combinations of location periods (space and time) in observations
  int <lower=0> M;     // number of observations
  int <lower=M> K1;    // the length of the mapping of observations to location periods and times
  int <lower=L> K2;    // the length of the mapping of location periods to gridcells
  int <lower=L_combs> K3;    // the length of the mapping of location period combinations 
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
  int<lower=0, upper=1> do_spatial_effect;    // Whether to have a spatial random effect
  int<lower=0, upper=1> do_sd_w_mixture;    // Whether to have a spatial random effect
  
  // Spatial adjacency
  // Note: The adjacency matrix node1 should be sorted and lower triangular
  array[N_edges] int <lower=1, upper=smooth_grid_N> node1;    // column 1 of the adjacency matrix
  array[N_edges] int <lower=1, upper=smooth_grid_N> node2;    // column 2 of the adjacency matrix
  vector<lower=0, upper=smooth_grid_N>[smooth_grid_N] diag;    // rowSums of directed adjacency matrix
  
  // Observations
  array[M] int <lower=0> y;    // observed counts of cholera cases
  int <lower=0, upper=M> M_full;    // number of observations that cover a full modeling time slice
  int <lower=0, upper=M> M_left;    // number of left-censored observations (open lower bound on the observation)
  int <lower=0, upper=M> M_right;   // number of right-censored observations (open upper bound on the observation)
  array[M_full] int <lower=1, upper=M> ind_full;      // indexes of full observations
  array[M_left] int <lower=1, upper=M> ind_left;      // indexes of left-censored observations
  array[M_right] int <lower=1, upper=M> ind_right;    // indexes of right-censored observations
  array[K1] real <lower=0, upper=1> tfrac;            // the time fraction side of the mapping from observations to location/times
  array[K1] int <lower=0, upper=1> censored;          // Whether data is censored in the mapping from observations to location/times
  
  // Mappings
  array[K1] int <lower=0, upper=M> map_obs_loctime_obs;    // the observation side of the mapping from observations to location/times
  array[K1] int <lower=0, upper=L> map_obs_loctime_loc;    // the location side of the mapping from observations to location/times
  array[K2] int <lower=0, upper=L> map_loc_grid_loc;       // the location side of the mapping from locations to gridcells
  array[K2] int <lower=0, upper=N> map_loc_grid_grid;      // the gridcell side of the mapping from locations to gridcells
  array[N] int <lower=0, upper=smooth_grid_N> map_smooth_grid;    // vector with repeating smooth_grid_N indexes repeating 1:N
  array[K2] real <lower=0, upper=1> map_loc_grid_sfrac;    // the population-weighed location spatial fraction covered by each gridcell
  array[M] int<lower=1, upper=N_admin_lev> map_obs_admin_lev;    // administrative level of each observation for observation model
  array[N] int <lower=1, upper=N_space> map_spacetime_space_grid;  // map from spacextime grid cells to space-only grid
  array[K3] int <lower=1, upper=L> map_loctime_combs_loc;             // the location time side of the mapping from locations to unique location time combinations
  array[K3] int <lower=1, upper=L_combs> map_loctime_combs_comb;      // the unique combination side of the mapping from locations to unique location time combinations
  array[L_combs] int <lower=1, upper=N_admin_lev> map_u_loctime_combs_admin_lev;      // the unique combination side of the mapping from locations to unique location time combinations
  
  // Time slices
  vector<lower=0, upper=1>[N*do_time_slice_effect] has_data_year;
  // If time slice effect pass indicator function for years without data
  matrix[N*do_time_slice_effect + 2 * (do_time_slice_effect != 1), T*do_time_slice_effect + 2*(do_time_slice_effect != 1)] mat_grid_time; // The time side of the mapping from locations/times to grid (2x2 in case of missing just so it's easy to create)
  
  // Covariates
  array[N] real <lower=1> pop;               // population by cell over all time points
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
  array[K1_output] int <lower=0, upper=M_output> map_output_obs_loctime_obs; // The observation side of the mapping from observations to location/times
  array[K1_output] int <lower=0, upper=L_output> map_output_obs_loctime_loc; // The location side of the mapping from observations to location/times
  array[K2_output] int <lower=0, upper=L_output> map_output_loc_grid_loc; // the location side of the mapping from locations to gridcells
  array[K2_output] int <lower=0, upper=N> map_output_loc_grid_grid; // the gridcell side of the mapping from locations to gridcells
  array[L_output] int <lower=0, upper=L_output_space> map_output_loctime_loc; // Map from space x time location ids to space only location
  array[L_output_space] int <lower=0> map_output_loc_adminlev; // Map from space location ids to admin level
  array[K2_output] real<lower=0> map_loc_grid_sfrac_output;
  
  //  Population at risk 
  int<lower=0> N_cat;    // Number of incidence categories. For now there are no checks whether categories are mutually exclusive or not
  array[N_cat] real<lower=0> risk_cat_low;    // lower bound of categories
  array[N_cat] real<lower=0> risk_cat_high;   // upper bound of categories
  
  // Over-dispersion at adm0
  real<lower=0> adm0_od;
}

transformed data {
  vector<lower=0>[N] logpop;              // populations by timestep
  real log_meanrate = log(meanrate);
  array[M*(1-do_censoring)*use_weights] real<lower=0> weights;    // a function of the expected offset for each observation used to downwight the likelihood
  array[L] real<lower=0> pop_loctimes;        // pre-computed population in each location period
  array[K1] real<lower=0, upper=1> tfrac_censoring; // tfrac accounting for censoring
  int<lower=0, upper=1> do_overdispersion;    // derived option to know whether the models contain overdispertion or not
  vector[2*T] Q_r = Q_sum_to_zero_QR(T);      // this is for the 0-centered temporal random effects if used
  real eta_zerosum_raw_sigma = inv_sqrt(1 - inv(T));
  int<lower=0, upper=T> size_eta;
  int<lower=0, upper=1> size_sd_eta;
  int<lower=0, upper=smooth_grid_N> size_w; 
  int<lower=0, upper=1> size_lambda; 
  int<lower=0, upper=2> size_sigma_std_dev_w; 
  int N_output_adminlev = max(map_output_loc_adminlev)+1;    // number of admin levels in output
  
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
      if (do_infer_sd_eta == 1) {
        size_sd_eta = 1;
      } else {
        size_sd_eta = 0;
      }
    }
  } else {
    size_eta = 0;
    size_sd_eta = 0;
  }
  
  // Saptial random effect
  if (do_spatial_effect == 1) {
    size_w = smooth_grid_N;
    // Size of prior on std_dev_w
    if (do_sd_w_mixture == 1) {
      size_lambda = 1;
      size_sigma_std_dev_w = 2;
    } else {
      size_lambda = 0;
      size_sigma_std_dev_w = 1;
    }
  } else {
    size_w = 0;
    size_lambda = 0;
    size_sigma_std_dev_w = 0;
  }
}

parameters {
  // Intercept
  array[use_intercept] real alpha;    
  
  // Spatial random effects
  array[do_spatial_effect] real rho;    // spatial correlation parameter
  array[do_spatial_effect] real<lower=0> std_dev_w;             // precision of the spatial effects
  vector[size_w] w;        // spatial random effect
  array[size_lambda] real<lower=0, upper=1> lambda;
  
  // Temporal random effects
  vector[size_eta] eta_tilde;    // uncentered temporal random effects
  array[size_sd_eta] real <lower=0> sigma_eta;    // sd of temporal random effects
  
  // Covariate effects
  vector[ncovar] betas;
  
  // Overdispersion parameters
  vector<lower=0>[(N_admin_lev-1)*do_overdispersion] inv_od_param;
  array[size_sigma_std_dev_w] real<lower=0> sigma_std_dev_w;
  
  vector[M_right] raw_dummy_right;
}
transformed parameters {
  vector[N_admin_lev*do_overdispersion] od_param;
  
  if (do_overdispersion == 1) {
    od_param[1] = adm0_od;
    for (i in 2:N_admin_lev) {
      od_param[i] = 1/inv_od_param[(i-1)];
    }
  }
}
generated quantities {
  
  // Model estimates
  array[M] real<lower=0> tfrac_modeled_cases; //expected number of cases for each observation
  array[M] real<lower=0> modeled_cases; //expected number of cases for each observation
  array[M] real log_lik; // log-likelihood of observations
  vector[N] grid_cases; //cases modeled in each gridcell and time point.
  vector[N] log_lambda; //local log rate
  vector[N_space] space_grid_rates; // mean annual incidence rates at grid level
  
  
  // Outputs at given admin levels
  vector<lower=0>[L_output] location_cases_output;    //cases modeled in each (temporal) location.
  vector<lower=0>[L_output] location_rates_output;    //rates modeled in each (temporal) location.
  vector<lower=0>[L_output_space] location_total_cases_output;       //cases modeled in each location across time slices.
  vector<lower=0>[L_output_space] location_mean_cases_output;    //variance of cases modeled in each location across time slices.
  vector<lower=0>[L_output_space] location_variance_cases_output;    //variance of cases modeled in each location across time slices.
  vector<lower=0>[L_output_space] location_cov_cases_output;         //coefficient of variation of modeled in each location across time slices.
  vector<lower=0>[L_output_space] location_total_rates_output;       //rates modeled in each location  across time slices.
  matrix<lower=0>[L_output_space, N_cat] location_risk_cat_num;      // number of people in each location in each risk category
  matrix<lower=0>[L_output_space, N_cat] location_risk_cat_prop;     // proportion of people in each location in each risk category
  array[L_output_space] int<lower=0> location_risk_cat;           // risk category for each space location
  matrix<lower=0>[N_cat, N_output_adminlev] tot_pop_risk;    // total number of people in admin units in each risk category by admin level
  
  // Data outputs to return (same for all samples)
  array[L_output] real <lower=0> pop_loctimes_output;    // population in each output location period
  array[L_output_space] real <lower=0> pop_loc_output;   // population in each output location (space only)
  
  // Posterior observations
  array[L_combs] int <lower=0> gen_obs_loctime_combs;    // generated observation for each loctime combination
  
  for (i in 1:L_output) {
    pop_loctimes_output[i] = 0;
  }
  
  for (i in 1:K2_output) {
    pop_loctimes_output[map_output_loc_grid_loc[i]] += pop[map_output_loc_grid_grid[i]] * map_loc_grid_sfrac_output[i];
  }
  
  {
    
    // Start environment in which temporary variables are used for outputs
    vector[L] location_cases; //cases modeled in each (temporal) location.
    vector[T*do_time_slice_effect] eta; // yearly random effects
    real sigma_eta_val;        // value of sigma_eta. This is either fixed to sigma_eta_scale if do_infer_sd_eta==0, or sigma_eta otherwise
    
    
    // ---- Part A: Grid-level rates and cases ----
    if (size_sd_eta == 1) {
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
    log_lambda = rep_vector(log_meanrate, N);
    
    if (use_intercept == 1) {
      log_lambda += alpha[1];
    }
    
    // Add spatial effects
    if (do_spatial_effect == 1) {
      log_lambda += w[map_smooth_grid];
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
      array[L_output_space] real tot_loc_pop; // store the total exposed population across time slices
      
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
      array[N_space, L_output_space] int check_done;
      
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
    
    // ---- Part F: Incidence mean, variance and CoV ----
    // Compute variance of incidence across years with online algorithm
    // https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    // https://datascience.stackexchange.com/questions/25858/how-can-i-calculate-mean-and-variance-incrementally
    {
      vector[L_output_space] tmp_mean = rep_vector(1e-2, L_output_space);    // incremental mean
      vector[L_output_space] tmp_ssq = rep_vector(0, L_output_space);        // incremental sum of squares
      vector[L_output_space] tmp_cnt = rep_vector(0, L_output_space);        // count of observations
      
      for (i in 1:L_output) {
        int j = map_output_loctime_loc[i];
        real x = location_cases_output[i];
        if (tmp_cnt[j] == 0) {
          tmp_mean[j] = x;
          tmp_ssq[j] = 0;
          tmp_cnt[j] = 1;
        } else {
          real d1 = x - tmp_mean[j];
          real d2;
          // Update count
          tmp_cnt[j] = tmp_cnt[j] + 1;
          // Update mean
          tmp_mean[j] += d1/tmp_cnt[j];
          // Update sum of squares
          d2 = x - tmp_mean[j];
          tmp_ssq[j] += d1*d2;
        }
      }
      
      // set outputs
      for (i in 1:L_output_space) {
        location_mean_cases_output[i] = tmp_mean[i];
        location_variance_cases_output[i] = tmp_ssq[i]/(tmp_cnt[i] - 1);
        location_cov_cases_output[i] = sqrt(location_variance_cases_output[i])/tmp_mean[i];
      }
    }
    
    // ---  End Part F ---
    
    // ---- Part G: Log-likelihoods ----
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
        real dummy_right;
        int j = ind_right[i];
        
        dummy_right = y[j] + exp(raw_dummy_right[i]);
        
        if (obs_model == 1) {
          // Poisson likelihood
          log_lik[j] = poisson_cont_lpdf(dummy_right | modeled_cases[j]);
        } else if (obs_model == 2) {
          // Quasi-poisson likelihood
          log_lik[j] = neg_binomial_2_cont_lpdf(dummy_right | modeled_cases[j], od_param[map_obs_admin_lev[j]] * modeled_cases[j]);
        } else {
          // Neg-binom likelihood
          log_lik[j] = neg_binomial_2_cont_lpdf(dummy_right | modeled_cases[j], od_param[map_obs_admin_lev[j]]);
        }
        
      }
    }
    
    // ---  End Part G ---
    
    // ---- Part H: Posterior observations ----
    // Generate observations for each unique location-period combination present in the data
    {
      array[L_combs] real u_loctime_comb_modeled_cases;
      
      for (i in 1:L_combs) {
        u_loctime_comb_modeled_cases[i] = 0;
      }
      
      for (i in 1:K3) {
        u_loctime_comb_modeled_cases[map_loctime_combs_comb[i]] += location_cases[map_loctime_combs_loc[i]];
      }
      
      // Generate the observations
      for (i in 1:L_combs) {
        gen_obs_loctime_combs[i] = neg_binomial_2_rng(u_loctime_comb_modeled_cases[i], od_param[map_u_loctime_combs_admin_lev[i]]);
      }
    }
    
    // ---  End Part H ---
  }
}
