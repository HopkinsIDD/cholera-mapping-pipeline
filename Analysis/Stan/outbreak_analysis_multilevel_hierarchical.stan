// Model for presence-only data accounting for upper-level admin units

data {
  int<lower=1> N;             // number of unique adm2 units
  int<lower=1> M;             // number of covariates
  int<lower=1> N_obs;         // number of observations (outbreak presence)
  int<lower=1> N_obs_adm2;        // number of observations at adm2 level (level of generative model)
  int<lower=1> N_non_obs_adm2;        // number of adm2 units where no outbreak was observed
  int<lower=1> N_obs_other_adm;   // number of observations at admlevels < 2
  int<lower=1> K;             // number of elements of map between adm2 and upper admin levels
  int<lower=1> U;             // number of upper units for hierarchical model (AFRO regions)
  int<lower=1> C;             // number of unique countries
  int<lower=0> L;             // number of unique admin levels < 1
  array[N_obs_adm2] int<lower=1, upper=N> ind_obs_adm2;    // indices of observations at adm2 level
  array[N_non_obs_adm2] int<lower=1, upper=N> ind_non_obs_adm2;    // indices of non-obsevations (either true 0 of false negative) at adm2 level
  array[K] int<lower=1, upper=N> map_adm2_other;                      // map from adm2 unit to upper admin units for computation of probabilities at upper levels
  array[N_obs_other_adm] int<lower=1, upper=K> map_adm_starts;    // start indices for upper admin levels
  array[N_obs_other_adm] int<lower=1, upper=K> map_adm_ends;      // end indices for upper admin levels
  array[N] int<lower=1, upper=C> map_adm2_countries;              // map from adm2 units to countries
  array[N] int<lower=1, upper=U> map_adm2_upper;                  // map from adm2 to upper level for hierarchical model (AFRO regions)
  array[C] int<lower=1, upper=U> map_country_upper;               // map from countries to upper level for hierarchical model (AFRO regions)
  matrix[N, M] X;    // covariates
}

transformed data {
  array[N_obs_adm2] int y_presence;    // fake data vector for presence in adm2 level units
  array[N_non_obs_adm2] int y_absence;    // fake data vector for presence in adm2 level units
  
  for (i in 1:N_obs_adm2) {
    y_presence[i] = 1;
  }
  for (i in 1:N_non_obs_adm2) {
    y_absence[i] = 0;
  }
}

parameters {
  // regression coefficients
  vector[M] mu_beta;    // regression prob
  vector<lower=0>[M] sd_beta;
  matrix[U, M] r_mu_beta_tilde;    
  matrix<lower=0>[U, M] r_sd_beta;
  matrix[C, M] c_beta_tilde;    
  
  // probability of reporting at country level
  real mu_logit_phi;    
  real<lower=0> sd_logit_phi;    
  vector[U] r_logit_phi_tilde;    // regression prob
  vector<lower=0>[U] r_sd_logit_phi;    // regression prob
  vector[C] c_logit_phi_tilde;    // regression prob
  
}

transformed parameters {
  vector[N] log_prob;  
  vector[C] log_phi;    // probability of reporting
  matrix[U, M] r_beta;  
  
  {
    // temp variables
    matrix[M, N] beta;
    vector[U] r_logit_phi;
    vector[C] c_logit_phi;
    matrix[C, M] c_beta;  
    
    // Regression coef
    for (m in 1:M) {
      for (u in 1:U) {
        r_beta[u, m] = mu_beta[m] + r_mu_beta_tilde[u, m] * sd_beta[m];
      }
      for (c in 1:C) {
        int u = map_country_upper[c];
        c_beta[c, m] = r_beta[u, m] + c_beta_tilde[c, m] * r_sd_beta[u, m];
      }
    }
    
    // Fill matrix beta
    for (i in 1:N) {
      int u = map_adm2_countries[i];
      beta[, i] = to_vector(c_beta[u, ]);
    }
    
    // Compute log prob
    for (i in 1:N) {
      log_prob[i] = -log1p_exp(-1 * X[i, ] * beta[, i]);
    }
    
    // Prob occurence
    for (u in 1:U) {
      r_logit_phi[u] = mu_logit_phi + r_logit_phi_tilde[u] * sd_logit_phi;
    } 
    
    for (c in 1:C) {
      int u = map_country_upper[c];
      c_logit_phi[c] = r_logit_phi[u] + c_logit_phi_tilde[c] * r_sd_logit_phi[u];
    }
    
    // Fill matrix beta
    for (c in 1:C) {
      log_phi[c] = -log1p_exp(-c_logit_phi[c]);
    }
  }
}

model {
  
  // temp vectors
  vector[N] log_eta = log_prob + log_phi[map_adm2_countries];     // total observation probabilities at adm2
  vector[N] logit_eta = log_eta - log1m_exp(log_eta);     // total observation probabilities at adm2
  
  
  // Likelihood for ADM2 and lower observations is a Bernoulli accounting for detection probability
  y_presence ~ bernoulli_logit(logit_eta[ind_obs_adm2]);
  y_absence ~ bernoulli_logit(logit_eta[ind_non_obs_adm2]);
  
  // Likelihood for upper level ADM units
  for (i in 1:N_obs_other_adm) {
    real x = 0;
    int y = 1;    // fake presence
    // Loop over composing adm2 units and sum log probabilities
    for (j in map_adm_starts[i]:map_adm_ends[i]) {
      x += log1m_exp(log_eta[map_adm2_other[j]]);
    }
    y ~ bernoulli_logit(log1m_exp(x) - x);
  }
  
  // Priors
  // regression coefficients
  mu_beta[1] ~ normal(0, 2);
  mu_beta[2:M] ~ normal(0, 1);
  
  sd_beta ~ normal(0, 2.5);
  for (u in 1:U) {
    r_mu_beta_tilde[u, ] ~ std_normal();
  }
  for (u in 1:U) {
    r_sd_beta[u, ] ~ normal(0, 2.5);
  }
  for (c in 1:C) {
    c_beta_tilde[c, ] ~ std_normal();
  }
  
  // probability of reporting at country level
  mu_logit_phi ~ normal(1.5, .5);    
  sd_logit_phi ~ normal(0, 1);    
  r_logit_phi_tilde ~ std_normal();    // regression prob
  r_sd_logit_phi ~ normal(0, 1);    // regression prob
  c_logit_phi_tilde ~ std_normal();    // regression prob
}

generated quantities {
  vector[N] pred_prob;         // predicted probability of outbreak observations (accounting for reporting)
  vector[N] pred_prob_true;    // predicted underlying outbreak probability
  vector[N_obs_adm2] pred_obs_prob_adm2;
  vector[N_obs_other_adm] pred_obs_prob_other_adm;
  vector[U] r_baseline_prob;      // regional mean baseline probability (intercept of the regression)
  matrix[U, M-1] r_odd_ratios;    // natural scale odds ratios
  real baseline_prob;             // overall mean baseline probability
  vector[M-1] odd_ratios;         // overall mean odds ratio
  matrix[U, M] ICC;    // intra-class clustering cofficients  
  matrix[C, M] c_beta;    // country-level parameters
  
  r_baseline_prob = inv_logit(r_beta[, 1]);
  r_odd_ratios = exp(r_beta[, 2:M]);
  baseline_prob = inv_logit(mu_beta[1]);
  odd_ratios = exp(mu_beta[2:M]);
  
  for (m in 1:M) {
    ICC[, m] = sd_beta[m]^2./(sd_beta[m]^2 + r_sd_beta[, m].^2);
  }
  
  // Regression coef
  for (m in 1:M) {
    for (c in 1:C) {
      int u = map_country_upper[c];
      c_beta[c, m] = r_beta[u, m] + c_beta_tilde[c, m] * r_sd_beta[u, m];
    }
  }
  
  {
    // temp vectors
    vector[N] log_eta = log_prob + log_phi[map_adm2_countries];     // total observation probabilities at adm2
    
    pred_prob = exp(log_eta);
    pred_obs_prob_adm2 = pred_prob[ind_obs_adm2];
    pred_prob_true = exp(log_prob);
    
    for (i in 1:N_obs_other_adm) {
      real x = 0;
      // Loop over composing adm2 units and sum log probabilities
      for (j in map_adm_starts[i]:map_adm_ends[i]) {
        x += log1m_exp(log_eta[map_adm2_other[j]]);
      }
      pred_obs_prob_other_adm[i] = inv_logit(log1m_exp(x) - x);
    }
  }
}
