library(rstan)
library(brms)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fish_num_climate <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/3-8-1-fish-num-1.csv')

fish_num_climate %>%
  ggplot(aes(x=temperature, y=fish_num, color=weather)) +
  geom_point()

glm_pois_brms <- brm(
  formula=fish_num ~ temperature + weather,
  family=poisson(),
  data=fish_num_climate,
  seed=1,
  prior=c(set_prior('', class='Intercept')))

glm_pois_brms

eff <- marginal_effects(glm_pois_brms, effects='temperature:weather')
plot(eff, points=TRUE)


set.seed(1)
eff_pre <- conditional_effects(glm_pois_brms,
                            method='predict',
                            effects='temperature:weather',
                            prob=0.99)
plot(eff_pre, points=TRUE)


