// Model for presence-only data

data {
  int<lower=1> N;    // number of units
  int<lower=1> M;    // number of covariates
  int<lower=1> N_obs;    // number of observations (outbreak presence)
  int<lower=1> L;        // number of upper level units for hierarchical model
  array[N_obs] int<lower=1, upper=N> ind_obs;   // indices of observations
  array[N-N_obs] int<lower=1, upper=N> ind_non_obs;   // indices of observations
  array[N] int<lower=1, upper=L> map_unit_upper;    // map from unit to upper-level
  matrix[N, M] X;    // covariates
}

transformed data {
  int N_non_obs = N-N_obs;
}

parameters {
  vector[M] beta;    // regression prob
  vector<lower=0, upper=1>[L] phi;    // probability of reporting
}

transformed parameters {
  vector[N] log_prob = log(inv_logit(X * beta));  
  vector[L] log_phi = log(phi);    // probability of reporting
}

model {
  vector[N] log_phi_vec = log_phi[map_unit_upper];
  
  // LL of observations
  target += sum(log_prob[ind_obs] + log_phi_vec[ind_obs]);
  
  // Of missing obs
  for (i in 1:N_non_obs) {
    vector[2] ll;
    int j = ind_non_obs[i];
    ll[1] = log_prob[j] + log1m_exp(log_phi_vec[j]);
    ll[2] = log1m_exp(log_prob[j]);
    target += log_sum_exp(ll);
  }
  
  // Priors
  beta ~ normal(0, 1);
  phi ~ beta(5, 1);
}

generated quantities {
  vector[N] pred_prob;
  
  pred_prob = inv_logit(X * beta) .* phi[map_unit_upper];
}
