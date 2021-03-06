// Joshua's attempt at converting DAGAR to stan.
//Based on code from Abhi dagar_poisson[1].R
// 3/3/2020 Annual incidence model with time-varying covariates

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
  
  int <lower=0> L; // number of location periods (space and time)
  
  int <lower=M> K1; // the length of the mapping of observations to location periods and times
  int <lower=L> K2; // the length of the mapping of location periods to gridcells
  int <lower=0, upper=M> map_obs_loctime_obs[K1]; // The observation side of the mapping from observations to location/times
  int <lower=0, upper=L> map_obs_loctime_loc[K1]; // The location side of the mapping from observations to location/times
  real <lower=0, upper=1> tfrac[K1]; // The time fraction side of the mapping from observations to location/times
  int <lower=0, upper=L> map_loc_grid_loc[K2]; // the location side of the mapping from locations to gridcells
  int <lower=0, upper=N> map_loc_grid_grid[K2]; // the gridcell side of the mapping from locations to gridcells
  
  int <lower=0,upper=smooth_grid_N> map_smooth_grid[N]; //vector with repeating smooth_grid_N indexes repeating 1:N
  
  // Covariate stuff
  int ncovar; // Number of covariates
  matrix[N,ncovar] covar; // Covariate matrix
  int<lower=0> beta_sigma_scale;
}

transformed data {
  vector<lower=0>[N] logpop;//populations by timestep
  real small_N = .001 * smooth_grid_N;
  real<lower=0> weights[M]; //a function of the expected offset for each observation used to downwight the likelihood
  real log_meanrate = log(meanrate);
  real <lower=1> pop_loctimes[L]; // pre-computed population in each location period
  
  for(i in 1:N){
    logpop[i] = log(pop[i]);
  }
  
  for (i in 1:L) {
    pop_loctimes[i] = 0;
  }
  
  for (i in 1:K2) {
    pop_loctimes[map_loc_grid_loc[i]] += pop[map_loc_grid_grid[i]];
  }
  
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

parameters {
  //real beta0; //the intercept
  
  real <lower=0, upper=1> rho; // Spatial correlation parameter
  real log_std_dev_w; // Precision of the spatial effects
  
  vector[smooth_grid_N] w; // Spatial Random Effect
  
  // Covariate stuff
  vector[ncovar] betas;
  
}

transformed parameters {
  vector[N] log_lambda; //local log rate
  vector[smooth_grid_N] b; //
  vector[smooth_grid_N] vec_var; //
  vector[smooth_grid_N] t_rowsum; // only the rowsum of t is used
  vector[smooth_grid_N] std_dev; // Rescaled std_dev by std_dev_w
  vector<lower=0>[L] location_cases; //cases modeled in each (temporal) location.
  vector<lower=0>[N] grid_cases; //cases modeled in each gridcell and time point.
  // real w_sum;
  
  real<lower=0> modeled_cases[M]; //expected number of cases for each observation
  real<lower=0> std_dev_w;
  
  std_dev_w = exp(log_std_dev_w);
  
  // Construct w
  vec_var = (1 - rho * rho) ./ (1 + (1. * diag - 1) * rho * rho);
  b = rho ./ (1 + (diag - 1) * rho * rho );
  // Linear in number of edges
  for(i in 1:smooth_grid_N){
    t_rowsum[i] = 0;
  }
  for(i in 1:N_edges){
    t_rowsum[node1[i] ] += w[node2[i] ] * b[ node1[i] ];
  }
  
  log_lambda =  w[map_smooth_grid] + log_meanrate + covar * betas;
  grid_cases = exp(log_lambda + logpop);
  
  //calculate the expected number of cases by location
  
  // calculate number of cases for each location
  for(i in 1:L){
    location_cases[i] = 0;
  }
  for(i in 1:K2){
    location_cases[map_loc_grid_loc[i] ] += grid_cases[map_loc_grid_grid[i] ];
  }
  
  //first initialize to 0
  for (i in 1:M) {
    modeled_cases[i] = 0;
  }
  
  //now accumulate
  for (i in 1:K1) {
    modeled_cases[map_obs_loctime_obs[i]] += location_cases[map_obs_loctime_loc[i]];
  }
  //w_sum = sum(w);
  std_dev = std_dev_w * sqrt(vec_var);
}

model {
  
  // NOTE:  no prior on phi_raw, it is used to construct phi
  // the following computes the prior on phi on the unit scale with std_dev = 1
  for(i in 1:smooth_grid_N){
    // w[i] ~ normal(t_rowsum[i],std_dev[i]);
    target += normal_lpdf(w[i] | t_rowsum[i], std_dev[i]);
  }
  
  // prior on regression coefficients
  betas ~ normal(0, beta_sigma_scale);
  
  if (M_full > 0) {
    //data model for estimated rates for full time slice observations
    target += poisson_lpmf(y[ind_full]| modeled_cases[ind_full]);
  }
  
  
  if (M_right > 0) {
    //data model for estimated rates for right-censored time slice observations
    //note that according to Stan the complementary CDF, or CCDF(Y|modeled_cases))
    // is defined as Pr(Y > y | modeled_cases),
    // we therefore add the probability Pr(Y = y|modeled_cases) to CCDF(y|modeled_casees)
    // to get Pr(Y >= y|modeled_cases)
    //https://mc-stan.org/docs/2_25/functions-reference/cumulative-distribution-functions.html
    
    vector[M_right] lp_censored;
    
    for(i in 1:M_right){
      real lpmf;
      lpmf = poisson_lpmf(y[ind_right[i]] | modeled_cases[ind_right[i]]);
      // heuristic condition to only use the PMF if Prob(Y>y| modeled_cases) ~ 0
      if ((y[ind_right[i]] < modeled_cases[ind_right[i]]) || ((y[ind_right[i]] > modeled_cases[ind_right[i]]) && (lpmf > -35))) {
        real lls[2];
        lls[1] = poisson_lccdf(y[ind_right[i]] | modeled_cases[ind_right[i]]);
        lls[2] = lpmf;
        lp_censored[i] = log_sum_exp(lls);
      } else {
        lp_censored[i] = lpmf;
      }
    }
    target += sum(lp_censored);
  }
}
generated quantities {
  real<lower=0> tfrac_modeled_cases[M]; //expected number of cases for each observation
  //first initialize to 0
  for (i in 1:M) {
    tfrac_modeled_cases[i] = 0;
  }
  //now accumulate
  for (i in 1:K1) {
    tfrac_modeled_cases[map_obs_loctime_obs[i]] += tfrac[i] * location_cases[map_obs_loctime_loc[i]];
  }
}
