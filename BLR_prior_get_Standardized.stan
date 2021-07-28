
#This BLR model standardizes the data, calculates coefficients and turn it back to original

modelstring <- '
data {
  int<lower = 1> N;                    // number of students
  int<lower = 1> M;                    // number of covariates
  matrix[N, M] x;                      // covariates
  vector[N] y;                         // outcomes
}

transformed data {
  vector[M] mu_x;                      // mean of the each covariate
  vector[M] sd_x;                      // sd of each covariate 
  matrix[N, M] x_std;                  // 
  
  real mu_y = mean(y);
  real sd_y = sd(y);
  vector[N] y_std = (y - mu_y) / sd_y;  //standardized dependent variable
  
  // x[, 1] is the intercept
  x_std[, 1] = x[, 1];                // set intercept to 1
  for (m in 2:M) {
    mu_x[m] = mean(x[, m]);           // mean of each covariate
    sd_x[m] = sd(x[, m]);             // sd of each covariate
    x_std[, m] = (x[, m] - mu_x[m]) / sd_x[m];  // standardized full set of predictors
  }
}

parameters {
  vector[M] beta_std;                 // M number of beta_std values, priors for beta - standardized prior
  real<lower = 0> sigma_y_std;        // sigma lowest value is 0, priros for the sd of the prior
}

model {
  beta_std ~ normal(0, 25);           // prior
  sigma_y_std ~ cauchy(0, 15);        // prior of sd
  
  y_std ~ normal(x_std * beta_std, sigma_y_std); // the model for standardized y values
}

generated quantities {
  vector[M] beta;                     // M number of beta
  real<lower = 0> sigma_y = sigma_y_std * sd_y;     // estimated sd 
  
  beta[1] = sd_y * beta_std[1] + mu_y;     // non standardized beta
  for (m in 2:M) {
    beta[m] = sd_y / sd_x[m] * beta_std[m];   // non standardized beta
    beta[1] -= beta[m] * mu_x[m];
  }
  
}
'
