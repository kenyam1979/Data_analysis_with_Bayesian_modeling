library(rstan)

# Stanの基本 ----
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## データ準備
file_beer_sales_1 <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/2-4-1-beer-sales-1.csv')

sample_size <- nrow(file_beer_sales_1)
data_list <- list(
  sales=file_beer_sales_1$sales,
  N=sample_size)


## パラメタ推定
mcmc_result <- stan(
  file='Chapter2/2-4-1-calc-mean-variance.stan',
  data=data_list,
  seed=1,
  chains=4,
  iter=2000,
  warmup=1000,
  thin=1)

print(mcmc_result)

## 収束の確認
traceplot(mcmc_result)
traceplot(mcmc_result, inc_warmup=T)


# 結果評価 ----
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)

## Bayesplotでの結果描画
library(bayesplot)

mcmc_hist(mcmc_sample, pars=c('mu', 'sigma'))
mcmc_dens(mcmc_sample, pars=c('mu', 'sigma'))
mcmc_trace(mcmc_sample, pars=c('mu', 'sigma'))
mcmc_combo(mcmc_sample, pars=c('mu', 'sigma'))

mcmc_intervals(mcmc_sample, pars=c('mu', 'sigma'))
mcmc_areas(mcmc_sample, pars=c('mu', 'sigma'))

mcmc_acf_bar(mcmc_sample, pars=c('mu', 'sigma'))



