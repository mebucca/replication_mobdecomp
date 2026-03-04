// ==============================================================================
// GLM Mobility Model with Counterfactual Simulation
// Estimates group-specific log-linear income models with heteroskedasticity,
// then computes counterfactual mobility probabilities under parameter swaps.
//
// Model: log(y) = alpha[g] + beta[g] * log(x) + error
//        error ~ N(0, sigma[g](x))
//        sigma depends on x via: log(sigma) = gamma[g] + lambda[g] * log(x)
//
// Counterfactuals: Swap alpha, beta, or sigma between groups
// Priors: Weakly informative, centered on typical US mobility patterns
//         (IGE ~ 0.3-0.6, modest heteroskedasticity)
// ==============================================================================

data {
  int<lower=1> N;                              // Number of observations
  vector[N] x;                                  // Parent income (level scale)
  vector[N] y;                                  // Child income (level scale)
  array[N] int<lower=0, upper=1> group;        // Group indicator (0 or 1)

  real epsilon;                                 // Mobility threshold (log scale, ~15%)
  real x_ref_log;                               // Reference point for log-centering
  
  int<lower=1> N_new;                          // Number of prediction points
  vector[N_new] x_new;                         // Parent income for predictions
  array[N_new] int<lower=0, upper=1> group_new; // Group for predictions
}

parameters {
  // Group-level raw deviations (non-centered parameterization)
  vector[2] alpha_raw;   // Intercept deviations
  vector[2] beta_raw;    // Slope deviations (IGE)
  vector[2] gamma_raw;   // Baseline log-sd deviations
  vector[2] lambda_raw;  // Heteroskedasticity slope deviations

  // Population means
  real alpha_mean;
  real beta_mean;
  real gamma_mean;
  real lambda_mean;

  // Group-level standard deviations
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
  real<lower=0> gamma_sd;
  real<lower=0> lambda_sd;

  // Additional noise in sigma (beyond systematic heteroskedasticity)
  vector[N] sigma_raw;
  real<lower=0> sigma_dispersion;
}

transformed parameters {
  // Group-specific parameters (non-centered transformation)
  vector[2] alpha  = alpha_mean + alpha_sd * alpha_raw;
  vector[2] beta   = beta_mean + beta_sd * beta_raw;
  vector[2] gamma  = gamma_mean + gamma_sd * gamma_raw;
  vector[2] lambda = lambda_mean + lambda_sd * lambda_raw;

  // Expected log(y) and standard deviation for each observation
  vector[N] mu;
  vector<lower=0>[N] sigma;

  for (i in 1:N) {
    int g = group[i] + 1;                      // Convert 0/1 to 1/2 for indexing
    real logx_ctr = log(x[i]) - x_ref_log;     // Log-centered parent income
    
    mu[i] = alpha[g] + beta[g] * logx_ctr;     // Mean of log(y)
    sigma[i] = exp(gamma[g] + lambda[g] * logx_ctr + sigma_dispersion * sigma_raw[i]);
  }
}

model {
  // Population-level priors (weakly informative for US context)
  alpha_mean  ~ normal(0, 0.7);      // Log income intercept
  beta_mean   ~ normal(0.5, 0.2);    // IGE centered at 0.5, allows 0.3-0.7
  gamma_mean  ~ normal(0, 0.7);      // Baseline log-sd
  lambda_mean ~ normal(0, 0.2);      // Heteroskedasticity slope

  // Group-level variation (half-normal via truncation)
  alpha_sd  ~ normal(0, 0.5);
  beta_sd   ~ normal(0, 0.2);
  gamma_sd  ~ normal(0, 0.3);
  lambda_sd ~ normal(0, 0.1);

  // Raw deviations (standard normal)
  alpha_raw  ~ normal(0, 1);
  beta_raw   ~ normal(0, 1);
  gamma_raw  ~ normal(0, 1);
  lambda_raw ~ normal(0, 1);

  // Additional sigma noise
  sigma_dispersion ~ normal(0, 0.15);
  sigma_raw ~ normal(0, 1);

  // Likelihood
  y ~ lognormal(mu, sigma);
}

generated quantities {
  // Mobility probabilities at each prediction point
  // Format: [mobility_type]_[scenario]
  // Scenarios: obs (observed), alpha/beta/sigma (swapped parameter)
  
  vector[N_new] up_obs;     // Upward mobility, observed parameters
  vector[N_new] down_obs;   // Downward mobility, observed
  vector[N_new] imm_obs;    // Immobile, observed

  vector[N_new] up_alpha;   // Upward, swapped alpha (intercept)
  vector[N_new] down_alpha;
  vector[N_new] imm_alpha;

  vector[N_new] up_beta;    // Upward, swapped beta (slope/IGE)
  vector[N_new] down_beta;
  vector[N_new] imm_beta;

  vector[N_new] up_sigma;   // Upward, swapped sigma (variance)
  vector[N_new] down_sigma;
  vector[N_new] imm_sigma;

  // Posterior predictive checks
  vector[N] y_pred;         // Predicted child income
  vector[N] residual;       // Log-scale residuals
  vector[N] log_lik;        // Log-likelihood for LOO/WAIC

  // Generate predictions for observed data
  for (i in 1:N) {
    y_pred[i] = lognormal_rng(mu[i], sigma[i]);
    residual[i] = log(y[i]) - mu[i];
    log_lik[i] = lognormal_lpdf(y[i] | mu[i], sigma[i]);
  }

  // Compute mobility probabilities for counterfactual scenarios
  for (j in 1:N_new) {
    int g     = group_new[j] + 1;  // Own group
    int other = 3 - g;              // Other group (for swaps)

    real logx_ctr_new = log(x_new[j]) - x_ref_log;

    // Observed parameters (own group)
    real mu_obs     = alpha[g] + beta[g] * logx_ctr_new;
    real sigma_obs  = exp(gamma[g] + lambda[g] * logx_ctr_new);

    // Counterfactual parameters
    real mu_alpha   = alpha[other] + beta[g] * logx_ctr_new;       // Swap intercept
    real mu_beta    = alpha[g] + beta[other] * logx_ctr_new;       // Swap slope
    real sigma_shared = exp(gamma[other] + lambda[other] * logx_ctr_new); // Swap variance

    // Mobility thresholds (log scale)
    real up_bound  = log(x_new[j]) + epsilon;   // Must exceed this for upward
    real low_bound = log(x_new[j]) - epsilon;   // Must fall below for downward
    
    // Observed scenario
    up_obs[j]   = 1 - normal_cdf(up_bound | mu_obs, sigma_obs);
    down_obs[j] = normal_cdf(low_bound | mu_obs, sigma_obs);
    imm_obs[j]  = normal_cdf(up_bound | mu_obs, sigma_obs) - normal_cdf(low_bound | mu_obs, sigma_obs);

    // Swapped alpha (intercept) scenario
    up_alpha[j]   = 1 - normal_cdf(up_bound | mu_alpha, sigma_obs);
    down_alpha[j] = normal_cdf(low_bound | mu_alpha, sigma_obs);
    imm_alpha[j]  = normal_cdf(up_bound | mu_alpha, sigma_obs) - normal_cdf(low_bound | mu_alpha, sigma_obs);

    // Swapped beta (slope) scenario
    up_beta[j]   = 1 - normal_cdf(up_bound | mu_beta, sigma_obs);
    down_beta[j] = normal_cdf(low_bound | mu_beta, sigma_obs);
    imm_beta[j]  = normal_cdf(up_bound | mu_beta, sigma_obs) - normal_cdf(low_bound | mu_beta, sigma_obs);

    // Swapped sigma (variance) scenario
    up_sigma[j]   = 1 - normal_cdf(up_bound | mu_obs, sigma_shared);
    down_sigma[j] = normal_cdf(low_bound | mu_obs, sigma_shared);
    imm_sigma[j]  = normal_cdf(up_bound | mu_obs, sigma_shared) - normal_cdf(low_bound | mu_obs, sigma_shared);
  }
}
