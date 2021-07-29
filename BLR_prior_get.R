#Author: Sinan Yavuz
#Date: July 28, 2021
#I will get the regression coefficient of PISA 2009 via noninformative BLR. It has 6 independent variables. 
#rm(list = ls())
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#call packages
library(dplyr)
library(rstan)
library(rstanarm)
library(loo)
options(mc.cores = 4)

#read the dataset
pisa.2009.stu <- readRDS("pisa.2009.stu.RDS")

#missing data needs to be modeled in stan, I will listwise delete. My goal is just to have sense of the priors 
dt <- na.omit(pisa.2009.stu)

y <- dt$PV1READ
x <- dt[,-7]

#data prep function - it is useful and keeps the environment clean
data.stan <- function(y.train, x.train) {
  x <- cbind(Intercept = 1, x.train) #get x: intercept and all predictors, -1 stands for the dependent variable. this need to be changed if the 
  y <- y.train
  N <- nrow(x)
  M <- ncol(x)
  list(N = N, M = M, x = x, y = y)
}

#also helpful to get the estimates directly
my.blr <- function(y.train, x.train, n.chains = 4, n_iter = 00000) {
  writeLines(modelstring, con = 'modelBLR.stan')
  data <- data.stan(y.train, x.train)
  fit <- stan('modelBLR.stan', data = data, iter = n_iter, chains = n.chains)
  est <- summary(fit)$summary
  beta <- est[grep('^beta\\[', rownames(est)), ][1:data$M,c("mean","sd")]
  rownames(beta) <- colnames(x)
  return(beta)
}

modelstring <- '
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
'

priorBLR <- my.blr(y, x)
saveRDS(priorBLR, "priors.RDS")
