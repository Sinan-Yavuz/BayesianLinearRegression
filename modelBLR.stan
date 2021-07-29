
data {
  int<lower = 1> N;                    // number of students
  int<lower = 1> M;                    // number of covariates
  matrix[N, M] x;                      // covariates
  vector[N] y;                         // outcomes
}

transformed data {
  real mu_y = mean(y);
}
parameters {
  vector[M] beta;                 // M number of beta_std values, priors for beta - standardized prior
  real<lower = 0> sigma_y;        // sigma lowest value is 0, priros for the sd of the prior
}
model {
  beta[1] ~ normal(mu_y, 25);           // the average score is around 500
  for (m in 2:M) {
  beta[m] ~ normal(0, 25);
  }
  sigma_y ~ cauchy(0, 15);        // prior of sd
  y ~ normal(x * beta, sigma_y); // the model for standardized y values
}

