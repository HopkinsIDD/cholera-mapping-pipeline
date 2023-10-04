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
  int<lower=0, upper=1> do_infer_sd_eta;    // Whether to infer the sd of the temporal random effect
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
  real mu_alpha;             // the mean of the intercept, if used
  real<lower=0> sd_alpha;    // the sd of the intercept, if used
  array[N_admin_lev] real<lower=0> mu_inv_od;    // the means of the inverse over-dispersion parameters
  array[N_admin_lev] real<lower=0> sd_inv_od;    // the sds of the inverse over-dispersion parameters
  real<lower=0> mu_sd_w;
  real<lower=0> sd_sd_w;
  
  // Debug
  int debug;
  
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
  array[use_intercept] real<lower=-8, upper=8> alpha;    
  
  // Spatial random effects
  array[do_spatial_effect] real<lower=0, upper=.99> rho;    // spatial correlation parameter
  array[do_spatial_effect] real<lower=0> std_dev_w;             // precision of the spatial effects
  vector<lower=-15, upper=(-log_meanrate + log(2))>[size_w] w;        // spatial random effect
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
  
  vector[T*do_time_slice_effect] eta;    // temporal random effects
  vector[M] modeled_cases;        // expected number of cases for each observation
  // real std_dev_w = exp(log_std_dev_w);    // sd of spatial random effects
  vector[N] grid_cases;       // cases modeled in each gridcell and time point
  real previous_debugs = 0;
  real sigma_eta_val;        // value of sigma_eta. This is either fixed to sigma_eta_scale if do_infer_sd_eta==0, or sigma_eta otherwise
  vector[N_admin_lev*do_overdispersion] od_param;
  
  
  if (do_overdispersion == 1) {
    od_param[1] = adm0_od;
    for (i in 2:N_admin_lev) {
      od_param[i] = 1/inv_od_param[(i-1)];
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
    
    if (debug && (previous_debugs == 0)) {
      {
        int i = 1;
        if (eta[i] < - 9999) {
          print("eta is -inf");
          print("sigma eta scale is ", sigma_eta_scale, " at index ", i);
          print("sigma eta tilde is ", sigma_eta_val, " at index ", i);
          print("eta tilde is ", eta_tilde, " at index ", i);
        }
        if (is_nan(eta[i])) {
          print("eta is -inf");
          print("sigma eta scale is ", sigma_eta_scale, " at index ", i);
          print("sigma eta tilde is ", sigma_eta_val, " at index ", i);
          print("eta tilde is ", eta_tilde, " at index ", i);
        }
      }
    }
    
    
    // ---- A. Grid-level rates and cases ----
    
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
          print("Eta contribution is ", ((mat_grid_time * eta) .* has_data_year)[i], " at index ", i);
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
          print("Eta contribution is ", ((mat_grid_time * eta) .* has_data_year)[i], " at index ", i);
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
  if (do_spatial_effect == 1) {
    // DAGAR prior on random effects
    vector[smooth_grid_N] b; //
    vector[smooth_grid_N] vec_var; //
    vector[smooth_grid_N] std_dev; // Rescaled std_dev by std_dev_w
    vector[smooth_grid_N] t_rowsum; // only the rowsum of t is used
    
    // Construct w
    b = rho[1] ./ (1 + (diag - 1) * rho[1] * rho[1]);
    vec_var = (1 - rho[1] * rho[1]) ./ (1 + (1. * diag - 1) * rho[1] * rho[1]);
    std_dev = std_dev_w[1] * sqrt(vec_var);
    
    // Linear in number of edges
    for(i in 1:smooth_grid_N){
      t_rowsum[i] = 0;
    }
    for(i in 1:N_edges){
      t_rowsum[node2[i]] += w[node1[i]] * b[node2[i]];
    }
    
    // NOTE:  no prior on phi_raw, it is used to construct phi
    // the following computes the prior on phi on the unit scale with std_dev = 1
    w ~ normal(t_rowsum, std_dev);
    if (debug && (previous_debugs == 0)) {
      print("dagar", target());
    }
    
    // prior on rho if provided
    if (use_rho_prior == 1) {
      rho ~ beta(5,1.5);
      if (debug && (previous_debugs == 0)) {
        print("rho", target());
      }
    }
    
    if (do_sd_w_mixture == 1) {
      target += log_mix(lambda[1], normal_lpdf(std_dev_w |mu_sd_w, sigma_std_dev_w[1]), normal_lpdf(std_dev_w |0, sigma_std_dev_w[2]));
      lambda[1] ~ beta(1, 3);
      
      sigma_std_dev_w[1] ~ normal(0, 2);
      sigma_std_dev_w[2] ~ normal(0, .5);
    } else {
      std_dev_w ~ normal(5, 0.5);
      sigma_std_dev_w[1] ~ normal(0, 1);
    }
  }
  
  if (debug && (previous_debugs == 0)) {
    print("dagar std", target());
  }
  
  
  // ---- 2. Temporal priors ----
  
  if (do_time_slice_effect == 1) {
    
    if (do_time_slice_effect_autocor == 1) {
      // For the autocorrelated model sigma is the sd of the increments in the random effects
      real tau = 1/(sigma_eta_val)^2; // precision of the increments of the time-slice random effects
      // Autocorrelation on yearly random effects with 0-sum constraint
      // The increments of the time-slice random effects are assumed to have mean 0 and variance 1/tau
      // Sorbye and Rue (2014) https://doi.org/10.1016/j.spasta.2013.06.004
      target += (T-1.0)/2.0 * log(tau) - tau/2 * (dot_self(eta[2:T] - eta[1:(T-1)]));
      sum(eta) ~ normal(0, 0.001 * T); // soft sum to 0 constraint 
    } else {
      if (do_zerosum_cnst == 1) {
        eta_tilde ~ normal(0, eta_zerosum_raw_sigma);
        // sum(eta_tilde) ~ normal(0, 0.001 * T); // soft sum to 0 constraint 
      } else {
        eta_tilde ~ std_normal();
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
    inv_od_param ~ normal(mu_inv_od[2:N_admin_lev], sd_inv_od[2:N_admin_lev]);
  }
  
  // ---- 4. Observations likelihood ----
  
  if (do_censoring == 1) {
    
    if (M_full > 0) {
      
      if (obs_model == 1) {
        // data model for estimated rates for full time slice observations
        target += poisson_lpmf(y[ind_full]| modeled_cases[ind_full]);
      } else if (obs_model == 2) {
        target += neg_binomial_2_lpmf(y[ind_full] | modeled_cases[ind_full], od_param[map_obs_admin_lev[ind_full]] .* modeled_cases[ind_full]);
      } else {
        target += neg_binomial_2_lpmf(y[ind_full] | modeled_cases[ind_full], od_param[map_obs_admin_lev[ind_full]]);
      }
      
      if (debug && (previous_debugs == 0)) {
        print("full obs", target());
      }
    }
    
    if (M_right > 0) {
      vector[M_right] lp_censored;
      
      // Create constrained dummy variables
      
      for(i in 1:M_right){
        real lpmf;
        real dummy_right;
        int j = ind_right[i];
        
        dummy_right = y[j] + exp(raw_dummy_right[i]);
        
        if (obs_model == 1) {
          // Poisson likelihood
          lpmf = poisson_cont_lpdf(dummy_right | modeled_cases[j]);
        } else if (obs_model == 2) {
          // Quasi-poisson likelihood
          lpmf = neg_binomial_2_cont_lpdf(dummy_right | modeled_cases[j], od_param[map_obs_admin_lev[j]] * modeled_cases[j]);
        } else {
          // Neg-binom likelihood
          lpmf = neg_binomial_2_cont_lpdf(dummy_right | modeled_cases[j], od_param[map_obs_admin_lev[j]]);
        }
        
        // Add constrained likelihood and log-jacobian det for transformation
        target += lpmf + raw_dummy_right[i];
      }
      
      if (debug && (previous_debugs == 0)) {
        print("right censored obs", target());
      }
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
          target += neg_binomial_2_lpmf(y[i] | modeled_cases[i], od_param[map_obs_admin_lev[i]] * modeled_cases[i])/weights[i];
        } else {
          // Neg-binom likelihood
          target += neg_binomial_2_lpmf(y[i] | modeled_cases[i], od_param[map_obs_admin_lev[i]])/weights[i];
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
        target += neg_binomial_2_lpmf(y | modeled_cases, od_param[map_obs_admin_lev] .* modeled_cases);
      } else {
        // Neg-binom likelihood
        target += neg_binomial_2_lpmf(y | modeled_cases, od_param[map_obs_admin_lev]);
      }
      
      if (debug && (previous_debugs == 0)) {
        print("unweighted obs", target());
      }
    }
  }
}
