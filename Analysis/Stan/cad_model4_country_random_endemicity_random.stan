// model4_country_random_endemicity_random.stan
//
// "Country (random) + Endemicity (random)" variant: replaces the fixed slope
// beta * endemic_ci (model2) with a random intercept w_{e(c)} over the two
// endemicity classes, partially pooled toward gamma0 via an estimated
// tau_endemic -- on the same footing as u_r / v_c elsewhere, rather than a
// single confidently-estimated slope. No region term (matches the
// equation as written in the report; see "Region (random) + Country
// (random) + Endemicity (random)", the region-retained variant, in
// model6_region_random_country_random_endemicity_random.stan).
//
//   logit(pbar_ci) = gamma0 + w_{e(ci)} + v_c
//   w_e ~ N(0, tau_endemic^2),  e in {epidemic, endemic}
//   v_c ~ N(0, tau_country^2)
//
// w is a length-2 vector: w[1] = epidemic, w[2] = endemic. `endemic[i]` is
// 0/1 as in model2/model3, used here as an index (endemic[i] + 1) instead
// of a slope.

data {
  int<lower=1> N;
  int<lower=1> C;
  array[N] int<lower=1, upper=C> country;
  array[N] int<lower=0> n;
  array[N] int<lower=0> y;
  array[N] int<lower=0, upper=1> endemic;    // 0 = epidemic, 1 = endemic; used as (endemic[i] + 1) below
}

parameters {
  real gamma0;
  vector[2] w_raw;                 // non-centered endemicity-class random effect: [1]=epidemic, [2]=endemic
  vector[C] v_raw;
  real<lower=0> tau_endemic;
  real<lower=0> tau_country;
  real<lower=0> phi;
}

transformed parameters {
  vector[2] w = w_raw * tau_endemic;
  vector[C] v = v_raw * tau_country;
  vector[N] pbar;
  for (i in 1:N) {
    pbar[i] = inv_logit(gamma0 + w[endemic[i] + 1] + v[country[i]]);
  }
}

model {
  gamma0 ~ normal(0, 5);
  w_raw ~ std_normal();
  v_raw ~ std_normal();
  tau_endemic ~ normal(0, 1);
  tau_country ~ normal(0, 1);
  phi ~ gamma(2, 0.1);

  for (i in 1:N) {
    y[i] ~ beta_binomial(n[i], pbar[i] * phi, (1 - pbar[i]) * phi);
  }
}

generated quantities {
  vector[N] log_lik;
  array[N] int y_rep;
  // ADDED: fallback prediction for a country with no age-split data at
  // all -- uses w[1] (the epidemic-class random intercept) and drops the
  // country effect, per "Predicting for countries with no data" in
  // cholera_age_distribution.qmd.
  real p_epidemic_posterior = inv_logit(gamma0 + w[1]);
  for (i in 1:N) {
    log_lik[i] = beta_binomial_lpmf(y[i] | n[i], pbar[i] * phi, (1 - pbar[i]) * phi);
    y_rep[i] = beta_binomial_rng(n[i], pbar[i] * phi, (1 - pbar[i]) * phi);
  }
}
