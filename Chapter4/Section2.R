library(rstan)
library(bayesplot)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fish_num_climate_3 <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/4-2-1-fish-num-3.csv')


glmm_pois_brms_human <- brm(
  formula=fish_num ~ weather + temperature + (1|human),
  family=poisson(),
  data=fish_num_climate_3,
  seed=1,
  prior=c(set_prior('', 'Intercept'),
          set_prior('', class='sd')))

glmm_pois_brms_human

ranef(glmm_pois_brms_human)

conditions <- data.frame(
  human=c('A','B','C','D','E','F','G','H','I','J'))

eff_glmm_human <- marginal_effects(
  glmm_pois_brms_human,
  effects='temperature:weather',
  re_formula=NULL,
  conditions=conditions)
plot(eff_glmm_human, points=T)
