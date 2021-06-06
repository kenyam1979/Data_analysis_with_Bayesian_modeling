library(rstan)
library(bayesplot)

# Stanでの予測(単回帰モデル) ----
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

file_beer_sales_2 <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/3-2-1-beer-sales-2.csv')

sample_size <- nrow(file_beer_sales_2)

temperature_pred <- 11:30

data_list_pred <- list(
  N=sample_size,
  sales=file_beer_sales_2$sales,
  temperature=file_beer_sales_2$temperature,
  N_pred=length(temperature_pred),
  temperature_pred=temperature_pred)

mcmc_result_pred <- stan(
  file='Chapter3/3-3-1-simple-lm-pred.stan',
  data=data_list_pred,
  seed=1)

mcmc_sample_pred <- rstan::extract(mcmc_result_pred, permuted=FALSE)

mcmc_intervals(mcmc_sample_pred, regex_pars=c('sales_pred.'))
