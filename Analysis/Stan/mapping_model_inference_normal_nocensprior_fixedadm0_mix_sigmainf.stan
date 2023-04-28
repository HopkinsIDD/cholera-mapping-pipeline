// This is the stan model for the cholera mapping pipeline.
// 
// The aim of the model is to produce inference on grid-level cholera rates.
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
  int <lower=1> N_edges;       // number of edges between grid cells
  int <lower=1> T;     // number of time slices
  int <lower=0> L;     // number of location periods (space and time)
  int <lower=0> M;     // number of observations
  int <lower=M> K1;    // the length of the mapping of observations to location periods and times
  int <lower=L> K2;    // the length of the mapping of location periods to gridcells
  int <lower=0> ncovar;    // Number of covariates
  int<lower=1> N_admin_lev;    // the number of unique administrative levels in the data
  int<lower=1> N_countries;    // the number of unique administrative levels in the data
  int<lower=1> N_inv_od;    // the number of unique administrative levels in the data
  
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
  int <lower=0, upper=1> censored[K1];          // Whether data is censored in the mapping from observations to location/times
  
  // Mappings
  int <lower=0, upper=M> map_obs_loctime_obs[K1];    // the observation side of the mapping from observations to location/times
  int <lower=0, upper=L> map_obs_loctime_loc[K1];    // the location side of the mapping from observations to location/times
  int <lower=0, upper=L> map_loc_grid_loc[K2];       // the location side of the mapping from locations to gridcells
  int <lower=0, upper=N> map_loc_grid_grid[K2];      // the gridcell side of the mapping from locations to gridcells
  int <lower=0, upper=smooth_grid_N> map_smooth_grid[N];    // vector with repeating smooth_grid_N indexes repeating 1:N
  real <lower=0, upper=1> map_loc_grid_sfrac[K2];    // the population-weighed location spatial fraction covered by each gridcell
  // int<lower=1, upper=N_admin_lev> map_obs_admin_lev[M];    // administrative level of each observation for observation model
  int<lower=1, upper=N_admin_lev*N_countries> map_obs_country_admin_lev[M];    // administrative level of each observation for observation model
  // int<lower=1, upper=N_countries> comb_country_admin_lev_country[N_admin_lev*N_countries];    // administrative level of each observation for observation model
  // int<lower=1, upper=N_admin_lev> comb_country_admin_lev_admin_lev[N_admin_lev*N_countries];    // administrative level of each observation for observation model
  int<lower=0, upper=(N_admin_lev*N_countries) - N_countries> map_od_inv_od_param[N_admin_lev*N_countries];    // administrative level of each observation for observation model
  int<lower=1, upper=N_countries> map_grid_country[N];    // administrative level of each observation for observation model
  
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
  real mu_alpha;             // the mean of the intercept, if used
  real<lower=0> sd_alpha;    // the sd of the intercept, if used
  real<lower=0> mu_inv_od[N_inv_od];    // the means of the inverse over-dispersion parameters
  real<lower=0> sd_inv_od[N_inv_od];    // the sds of the inverse over-dispersion parameters
  real<lower=0> mu_sd_w;
  real<lower=0> sd_sd_w;
  
  // Debug
  int debug;
}

transformed data {
  vector<lower=0>[N] logpop;              // populations by timestep
  real log_meanrate = log(meanrate);
  real<lower=0> weights[M*(1-do_censoring)*use_weights];    // a function of the expected offset for each observation used to downwight the likelihood
  real <lower=0> pop_loctimes[L];        // pre-computed population in each location period
  real <lower=0, upper=1> tfrac_censoring[K1]; // tfrac accounting for censoring
  int<lower=0, upper=1> do_overdispersion;    // derived option to know whether the models contain overdispertion or not
  vector[2*T] Q_r = Q_sum_to_zero_QR(T);      // this is for the 0-centered temporal random effects if used
  real eta_zerosum_raw_sigma = inv_sqrt(1 - inv(T));
  int<lower=0, upper=T> size_eta;
  int<lower=0, upper=1> size_sd_eta;
  int<lower=1> N_countries_admin_lev = N_countries * N_admin_lev;
  int<lower=0,upper=N> ind_grid_country[N_countries, N];    // matrix of indices of countries
  int<lower=1, upper=N> N_ind_grid_country[N_countries];    // counter for countries
  
  {
    // Build ind_grid_country
    for (i in 1:N_countries) {
      N_ind_grid_country[i] = 0;
      for (j in 1:N) {
        ind_grid_country[i, j] = 0;
      }
    }
    
    for (i in 1:N) {
      N_ind_grid_country[map_grid_country[i]] += 1;
      ind_grid_country[map_grid_country[i],  N_ind_grid_country[map_grid_country[i]]] = i;
    }
  }
  
  
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
  vector[N_countries*use_intercept] alpha;    
  
  // Spatial random effects
  real <lower=0, upper=1> rho;    // spatial correlation parameter
  real<lower=0> std_dev_w;             // precision of the spatial effects
  vector<lower=-15, upper=15>[smooth_grid_N] w;        // spatial random effect
  real<lower=0, upper=1> lambda;
  
  // Temporal random effects
  matrix[N_countries, size_eta] eta_tilde;    // uncentered temporal random effects
  real <lower=0> sigma_eta[size_sd_eta];    // sd of temporal random effects
  
  // Covariate effects
  vector[ncovar] betas;
  
  // Overdispersion parameters
  vector<lower=0>[N_inv_od*do_overdispersion] inv_od_param;
  real<lower=0> sigma_std_dev_w[2];
}

transformed parameters {
  
  matrix[N_countries, T*do_time_slice_effect] eta;    // temporal random effects
  vector[M] modeled_cases;        // expected number of cases for each observation
  // real std_dev_w = exp(log_std_dev_w);    // sd of spatial random effects
  vector[N] grid_cases;       // cases modeled in each gridcell and time point
  real previous_debugs = 0;
  real sigma_eta_val;        // value of sigma_eta. This is either fixed to sigma_eta_scale if do_infer_sd_eta==0, or sigma_eta otherwise
  vector[N_countries_admin_lev*do_overdispersion] od_param;
  
  
  if (do_overdispersion == 1) {
    for (i in 1:N_countries_admin_lev) {
      if (map_od_inv_od_param[i] == 0) {
        // For admin level 0 i is assumed that overdispersion is fixed
        od_param[i] = 1e2;
      } else {
        od_param[i] = 1/inv_od_param[map_od_inv_od_param[i]];
      }
    }
  }
  
  if (size_sd_eta == 1) {
    sigma_eta_val = sigma_eta[1];
  } else {
    sigma_eta_val = sigma_eta_scale;
  }
  
  {
    vector[L] location_cases;    // cases modeled in each (temporal) location.
    vector[N] log_lambda;        // local log rate
    
    if (do_time_slice_effect == 1) {
      for (j in 1:N_countries) {
        if (do_zerosum_cnst == 0) {
          // scale yearly random effects
          eta[j, ] = sigma_eta_val * eta_tilde[j, ];
        } else {
          // QR decomposition method
          eta[j, ] = to_row_vector(sum_to_zero_QR(to_vector(eta_tilde[j, ]), Q_r));
        }    
      }
    }
    
    if (debug && (previous_debugs == 0)) {
      {
        int i = 1;
        if (eta[i,i] < - 9999) {
          print("eta is -inf");
          print("sigma eta scale is ", sigma_eta_scale, " at index ", i);
          print("sigma eta tilde is ", sigma_eta_val, " at index ", i);
          print("eta tilde is ", eta_tilde, " at index ", i);
        }
        if (is_nan(eta[i,i])) {
          print("eta is -inf");
          print("sigma eta scale is ", sigma_eta_scale, " at index ", i);
          print("sigma eta tilde is ", sigma_eta_val, " at index ", i);
          print("eta tilde is ", eta_tilde, " at index ", i);
        }
      }
    }
    
    
    // ---- A. Grid-level rates and cases ----
    
    // log-rates without time-slice effects
    log_lambda =  w[map_smooth_grid] + log_meanrate;
    
    if (use_intercept == 1) {
      log_lambda += alpha[map_grid_country];
    }
    
    // covariates if applicable
    if (ncovar > 1) {
      log_lambda += covar * betas;
    }
    
    // Add time slice effects
    if (do_time_slice_effect == 1) {
      for (i in 1:N_countries) {
        log_lambda[ind_grid_country[i, 1:N_ind_grid_country[i]]] += (mat_grid_time[ind_grid_country[i, 1:N_ind_grid_country[i]],] * to_vector(eta[i, ]));
      }
    }
    
    if (debug && (previous_debugs == 0)) {
      // for(i in 1:N)
      {
        int i = 1;
        if (log_lambda[i] < -1000) {
          print("lambda is 0 at index ", i);
          print("dagar prior is ", w[map_smooth_grid[i]], " at index ", i);
          print("log mean rate is ", log_meanrate, " at index ", i);
          print("covariate contribution is ", (covar * betas)[i], " at index ", i);
          print("has_data_year is ", has_data_year[i], " at index ", i);
          print("mat_grid_time is ", mat_grid_time[i], " at index ", i);
          print("eta is ", eta, " at index ", i);
          // print("Eta contribution is ", ((mat_grid_time * eta) .* has_data_year)[i], " at index ", i);
          print("alpha is ", alpha, " at index ", i);
          previous_debugs += 1;
        }
        if (is_nan(log_lambda[i])) {
          print("lambda is nan at index ", i);
          print("dagar prior is ", w[map_smooth_grid[i]], " at index ", i);
          print("log mean rate is ", log_meanrate, " at index ", i);
          print("covariate contribution is ", (covar * betas)[i], " at index ", i);
          print("has_data_year is ", has_data_year[i], " at index ", i);
          print("mat_grid_time is ", mat_grid_time[i], " at index ", i);
          print("eta is ", eta, " at index ", i);
          // print("Eta contribution is ", ((mat_grid_time * eta) .* has_data_year)[i], " at index ", i);
          previous_debugs += 1;
        }
      }
    }
    
    // Compute cases
    grid_cases = exp(log_lambda + logpop);
    
    if (debug && previous_debugs == 0) {
      // for(i in 1:N)
      {
        int i = 1;
        if (grid_cases[i] == 0) {
          print("grid cases is 0 at index ", i);
          print("log_lambda is ", log_lambda[i], " at index ", i);
          print("logpop is ", logpop[i], " at index ", i);
          previous_debugs += 1;
        }
        if (is_nan(grid_cases[i])) {
          print("grid cases is nan at index ", i);
          print("log_lambda is ", log_lambda[i], " at index ", i);
          print("logpop is ", logpop[i], " at index ", i);
          previous_debugs += 1;
        }
      }
    }
    
    // ---- B. Expected number of cases by location ----
    
    // calculate number of cases for each location
    for(i in 1:L){
      location_cases[i] = 0;
    }
    
    for(i in 1:K2){
      location_cases[map_loc_grid_loc[i]] += grid_cases[map_loc_grid_grid[i]] * map_loc_grid_sfrac[i];
      if (debug && (previous_debugs == 0)) {
        if (is_nan(location_cases[map_loc_grid_loc[i]])) {
          print("location : ", map_loc_grid_loc[i]);
          print("grid_cases : ", grid_cases[map_loc_grid_grid[i] ]);
          print("sfrac : ", map_loc_grid_sfrac[i]);
          previous_debugs += 1;
        }
      }
    }
    
    if (debug && (previous_debugs == 0)) {
      // for(i in 1:L)
      {
        int i = 1;
        if (location_cases[i] == 0) {
          print("location cases is 0 at index ", i);
          previous_debugs += 1;
        }
        if (is_nan(location_cases[i])) {
          print("location cases is nan at index ", i);
          previous_debugs += 1;
        }
      }
    }
    
    
    // ---- C. Expected number of cases by observation ----
    
    for (i in 1:M) {
      modeled_cases[i] = 0;
    }
    
    // now accumulate
    for (i in 1:K1) {
      if (do_censoring == 1) {
        modeled_cases[map_obs_loctime_obs[i]] += tfrac_censoring[i] * location_cases[map_obs_loctime_loc[i]];
      } else {
        modeled_cases[map_obs_loctime_obs[i]] += tfrac[i] * location_cases[map_obs_loctime_loc[i]];
      }
    }
    
    if (debug && (previous_debugs == 0)) {
      // for(i in 1:M)
      {
        int i = 1;
        if (is_nan(modeled_cases[i])) {
          print("modeled cases is nan at index ", i);
          previous_debugs += 1;
        }
        if (modeled_cases[i] == 0) {
          print("modeled cases is 0 at index ", i);
          previous_debugs += 1;
        }
      }
    }
  }
}

model {
  
  // ---- 1. Spatial prior ----
  {
    // DAGAR prior on random effects
    vector[smooth_grid_N] b; //
    vector[smooth_grid_N] vec_var; //
    vector[smooth_grid_N] std_dev; // Rescaled std_dev by std_dev_w
    vector[smooth_grid_N] t_rowsum; // only the rowsum of t is used
    
    // Construct w
    b = rho ./ (1 + (diag - 1) * rho * rho );
    vec_var = (1 - rho * rho) ./ (1 + (1. * diag - 1) * rho * rho);
    std_dev = std_dev_w * sqrt(vec_var);
    
    // Linear in number of edges
    for(i in 1:smooth_grid_N){
      t_rowsum[i] = 0;
    }
    for(i in 1:N_edges){
      t_rowsum[node1[i]] += w[node2[i]] * b[node1[i]];
    }
    
    // NOTE:  no prior on phi_raw, it is used to construct phi
    // the following computes the prior on phi on the unit scale with std_dev = 1
    w ~ normal(t_rowsum, std_dev);
    if (debug && (previous_debugs == 0)) {
      print("dagar", target());
    }
  }
  
  // prior on rho if provided
  if (use_rho_prior == 1) {
    rho ~ beta(5,1.5);
    if (debug && (previous_debugs == 0)) {
      print("rho", target());
    }
  }
  
  target += log_mix(lambda, normal_lpdf(std_dev_w |mu_sd_w, sigma_std_dev_w[1]), normal_lpdf(std_dev_w |0, sigma_std_dev_w[2]));
  lambda ~ beta(2, 1);
  
  sigma_std_dev_w[1] ~ normal(0, 1);
  sigma_std_dev_w[2] ~ normal(0, .5);
  
  
  if (debug && (previous_debugs == 0)) {
    print("dagar std", target());
  }
  
  
  // ---- 2. Temporal priors ----
  
  if (do_time_slice_effect == 1) {
    
    for (i in 1:N_countries) {
      if (do_time_slice_effect_autocor == 1) {
        // For the autocorrelated model sigma is the sd of the increments in the random effects
        real tau = 1/(sigma_eta_val)^2; // precision of the increments of the time-slice random effects
        // Autocorrelation on yearly random effects with 0-sum constraint
        // The increments of the time-slice random effects are assumed to have mean 0 and variance 1/tau
        // Sorbye and Rue (2014) https://doi.org/10.1016/j.spasta.2013.06.004
        target += (T-1.0)/2.0 * log(tau) - tau/2 * (dot_self(eta[i,2:T] - eta[i,1:(T-1)]));
        sum(eta[i,]) ~ normal(0, 0.001 * T); // soft sum to 0 constraint 
      } else {
        if (do_zerosum_cnst == 1) {
          eta_tilde[i,] ~ normal(0, eta_zerosum_raw_sigma);
        } else {
          eta_tilde[i,] ~ std_normal();
        }
      }
    }
    
    if (debug && (previous_debugs == 0)) {
      print("etas", target());
    }
    
    if (size_sd_eta == 1) {
      // prior on the time_slice random effects
      sigma_eta ~ normal(0, sigma_eta_scale);
    } 
  }    
  
  // ---- 3. Other priors ----
  
  if (use_intercept == 1) {
    // prior on intercept
    alpha ~ normal(mu_alpha, sd_alpha);
  }
  
  // prior on regression coefficients
  if (exp_prior == 0){
    betas ~ normal(0,beta_sigma_scale);
  } else {
    betas ~ double_exponential(0, beta_sigma_scale);
  }
  
  if (debug && (previous_debugs == 0)) {
    print("betas", target());
  }
  
  // Priors on the over-dispersion parameters
  
  if (do_overdispersion == 1) {
    inv_od_param ~ normal(mu_inv_od, sd_inv_od);
  }
  
  // ---- 4. Observations likelihood ----
  
  if (do_censoring == 1) {
    
    if (M_full > 0) {
      
      if (obs_model == 1) {
        // data model for estimated rates for full time slice observations
        target += poisson_lpmf(y[ind_full]| modeled_cases[ind_full]);
      } else if (obs_model == 2) {
        target += neg_binomial_2_lpmf(y[ind_full] | modeled_cases[ind_full], od_param[map_obs_country_admin_lev[ind_full]] .* modeled_cases[ind_full]);
      } else {
        target += neg_binomial_2_lpmf(y[ind_full] | modeled_cases[ind_full], od_param[map_obs_country_admin_lev[ind_full]]);
      }
      
      if (debug && (previous_debugs == 0)) {
        print("full obs", target());
      }
    }
    
    if (M_right > 0) {
      //data model for estimated rates for right-censored time slice observations
      //note that according to Stan the complementary CDF, or CCDF(Y|modeled_cases)
      // is defined as Pr(Y > y | modeled_cases),
      // we therefore add the probability Pr(Y = y|modeled_cases) to CCDF(y|modeled_cases)
      // to get Pr(Y >= y|modeled_cases)
      //https://mc-stan.org/docs/2_25/functions-reference/cumulative-distribution-functions.html
      
      vector[M_right] lp_censored;
      
      for(i in 1:M_right){
        real lpmf;
        
        if (obs_model == 1) {
          // Poisson likelihood
          lpmf = poisson_lpmf(y[ind_right[i]] | modeled_cases[ind_right[i]]);
        } else if (obs_model == 2) {
          // Quasi-poisson likelihood
          lpmf = neg_binomial_2_lpmf(y[ind_right[i]] | modeled_cases[ind_right[i]], od_param[map_obs_country_admin_lev[ind_right[i]]] * modeled_cases[ind_right[i]]);
        } else {
          // Neg-binom likelihood
          lpmf = neg_binomial_2_lpmf(y[ind_right[i]] | modeled_cases[ind_right[i]], od_param[map_obs_country_admin_lev[ind_right[i]]]);
        }
        
        // heuristic condition to only use the PMF if Prob(Y>y| modeled_cases) ~ 0
        if ((y[ind_right[i]] < modeled_cases[ind_right[i]]) || ((y[ind_right[i]] > modeled_cases[ind_right[i]]) && (lpmf > -35))) {
          real lls[2];
          if (obs_model == 1) {
            // Poisson likelihood
            lls[1] = poisson_lccdf(y[ind_right[i]] | modeled_cases[ind_right[i]]);
          } else if (obs_model == 2) {
            // Quasi-poisson likelihood
            lls[1] = neg_binomial_2_lccdf(y[ind_right[i]] | modeled_cases[ind_right[i]], od_param[map_obs_country_admin_lev[ind_right[i]]] * modeled_cases[ind_right[i]]);
          } else {
            // print("i: ", i, " y: ", y[ind_right[i]], " mcases: ", modeled_cases[ind_right[i]], " od: ", od_param[map_obs_country_admin_lev[ind_right[i]]]);
            // Neg-binom likelihood
            lls[1] = neg_binomial_2_lccdf(y[ind_right[i]] | modeled_cases[ind_right[i]], od_param[map_obs_country_admin_lev[ind_right[i]]]);
          }
          lls[2] = lpmf;
          lp_censored[i] = log_sum_exp(lls);
        } else {
          lp_censored[i] = lpmf;
        }
      }
      
      target += sum(lp_censored);
      
      if (debug && (previous_debugs == 0)) {
        print("right censored obs", target());
      }
      // add a 0-centered prior on the censored cases
      // for (idx in ind_right) {
        //   modeled_cases[idx] ~ cauchy(0, 2);
        // }
    }
  } else {
    if (use_weights == 1) {
      //data model for estimated rates
      for(i in 1:M){
        if (obs_model == 1) {
          // Poisson likelihood
          target += poisson_lpmf(y[i] | modeled_cases[i])/weights[i];
        } else if (obs_model == 2) {
          // Quasi-poisson likelihood
          target += neg_binomial_2_lpmf(y[i] | modeled_cases[i], od_param[map_obs_country_admin_lev[i]] * modeled_cases[i])/weights[i];
        } else {
          // Neg-binom likelihood
          target += neg_binomial_2_lpmf(y[i] | modeled_cases[i], od_param[map_obs_country_admin_lev[i]])/weights[i];
        }
        if (debug && (previous_debugs == 0)) {
          print("weighted obs", target());
        }
      }
    } else {
      if (obs_model == 1) {
        // Poisson likelihood
        target += poisson_lpmf(y | modeled_cases);
      } else if (obs_model == 2) {
        // Quasi-poisson likelihood
        target += neg_binomial_2_lpmf(y | modeled_cases, od_param[map_obs_country_admin_lev] .* modeled_cases);
      } else {
        // Neg-binom likelihood
        target += neg_binomial_2_lpmf(y | modeled_cases, od_param[map_obs_country_admin_lev]);
      }
      
      if (debug && (previous_debugs == 0)) {
        print("unweighted obs", target());
      }
    }
  }
}
