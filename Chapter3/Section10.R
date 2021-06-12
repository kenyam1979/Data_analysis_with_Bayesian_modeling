library(rstan)
library(brms)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

interaction_1 <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/3-10-1-interaction-1.csv')

interaction_brms_1 <- brm(
  formula=sales ~ publicity * bargen,
  family=gaussian(link='identity'),
  data=interaction_1,
  seed=1,
  prior=c(set_prior('', class='Intercept'),
          set_prior('', class='sigma')))

interaction_brms_1

eff_1 <- marginal_effects(interaction_brms_1, effects='publicity:bargen')
plot(eff_1, points=TRUE)
