library(rstan)
library(bayesplot)
library(tidyverse)

# Stanでの推定 ----
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

file_beer_sales_2 <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/3-2-1-beer-sales-2.csv')

sample_size <- nrow(file_beer_sales_2)

file_beer_sales_2 %>%
  ggplot(aes(x=temperature, y=sales)) +
  geom_point()

data_list <- list(
  sales=file_beer_sales_2$sales,
  temperature=file_beer_sales_2$temperature,
  N=sample_size)

mcmc_result <- stan(
  file='Chapter3/3-2-1-simple-lm.stan',
  data=data_list,
  seed=1)

print(mcmc_result)


mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)

mcmc_combo(mcmc_sample, pars=c('Intercept', 'beta', 'sigma'))


# LMでの推定 ----
lm_result <- lm(sales~temperature, file_beer_sales_2)
summary(lm_result)
