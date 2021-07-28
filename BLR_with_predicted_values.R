#Author: Sinan Yavuz
#Date: July 28, 2021

#BLR with predicted values - calculate y_test based on x_test
#I will get the regression coefficient of PISA 2009 via noninformative BLR. It has 6 independent variables. 

#call packages
library(dplyr)
library(rstan)
library(rstanarm)
library(loo)
options(mc.cores = 4)

#read the dataset
pisa.2009.stu <- read.csv("PISA2009_forInf.csv")

#missing data needs to be modeled in stan, I will listwise delete. My goal is just to have sense of the priors 
dt <- na.omit(pisa.2009.stu)
y <- dt$PV1READ
x <- dt[,-7]

modelstring <- '
data {
  int<lower = 1> N;                    // number of students
  int<lower = 1> M;                    // number of covariates
  matrix[N, M] x;                      // covariates
  vector[N] y;                         // outcomes
  int<lower=0> N_new;				   // number of students in the prediction data
  matrix[N_new, M] x_new;			   // test data set
}

transformed data {
  real mu_y = mean(y);
}
parameters {
  vector[M] beta;                 // M number of beta_std values, priors for beta - standardized prior
  real<lower = 0> sigma_y;        // sigma lowest value is 0, priros for the sd of the prior
  vector[N] y_new;                // predictions
}
model {
  beta[1] ~ normal(mu_y, 25);           // the average score is around 500 for PISA
  for (m in 2:M) {
  beta[m] ~ normal(0, 25);
  }
  sigma_y ~ cauchy(0, 15);        // prior of sd
  y ~ normal(x * beta, sigma_y); // the model for standardized y values
  y_new ~ normal(x_new * beta, sigma_y);  // prediction model
}
'

#now the data generation function requires the test set
data.stan <- function(y.train, x.train, x.test) {
  x <- cbind(Intercept = 1, x.train) #get x: intercept and all predictors, -1 stands for the dependent variable. this need to be changed if the 
  y <- y.train
  N <- nrow(x)
  M <- ncol(x)
  x_new <- cbind(Intercept = 1, x.test)
  N_new <- nrow(x_new)
  list(N = N, M = M, x = x, y = y, x_new = x_new, N_new = N_new)
}

fit <- stan('modelBLR.stan', data = data, iter = 2000, chains = 4)
est <- summary(fit)$summary
beta <- est[grep('^beta\\[', rownames(est)), ][1:ncol(x), ]
y_pred <- est[grep('^y_new\\[', rownames(est)), ][1:nrow(x), "mean"]
