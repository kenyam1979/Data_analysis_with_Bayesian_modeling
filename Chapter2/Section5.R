library(rstan)

animul_num <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/2-5-1-animal-num.csv')

sample_size <- nrow(animul_num)

data_list <- list(
  animal_num=animul_num$animal_num,
  N=sample_size)

mcmc_normal <- stan(
  file='Chapter2/2-5-1-normal-dist.stan',
  data=data_list,
  seed=1)


mcmc_poisson <- stan(
  file='Chapter2/2-5-2-poisson-dist.stan',
  data=data_list,
  seed=1)


y_rep_normal <- rstan::extract(mcmc_normal)$pred
y_rep_poisson <- rstan::extract(mcmc_poisson)$pred

ppc_hist(y=animul_num$animal_num,
         yrep=y_rep_normal[1:5, ])
ppc_hist(y=animul_num$animal_num,
         yrep=y_rep_poisson[1:5, ])
