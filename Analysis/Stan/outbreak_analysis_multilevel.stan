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
  vector[M] beta;    // regression prob
  vector<lower=0, upper=1>[C] phi;    // probability of reporting at country level
}

transformed parameters {
  vector[N] log_prob = -log1p_exp(-1 * X * beta);  
  vector[C] log_phi = log(phi);    // probability of reporting
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
  beta ~ normal(0, 1);
  phi ~ beta(5, 1);
}

generated quantities {
  vector[N] pred_prob;
  vector[N_obs_adm2] pred_obs_prob_adm2;
  vector[N_obs_other_adm] pred_obs_prob_other_adm;
  
  {
    // temp vectors
    vector[N] log_eta = log_prob + log_phi[map_adm2_countries];     // total observation probabilities at adm2
    
    pred_prob = exp(log_eta);
    pred_obs_prob_adm2 = pred_prob[ind_obs_adm2];
    
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
