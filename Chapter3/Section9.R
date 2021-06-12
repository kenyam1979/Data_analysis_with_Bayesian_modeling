library(rstan)
library(brms)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

germination_dat <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/3-9-1-germination.csv')

germination_dat %>%
  ggplot(aes(x=nutrition, y=germination, color=solar)) +
  geom_point()

glm_binom_brms <- brm(
  germination | trials(size) ~ solar + nutrition,
  family=binomial(),
  data=germination_dat,
  seed=1,
  prior=c(set_prior('', class='Intercept')))

glm_binom_brms

eff <- conditional_effects(glm_binom_brms, effects='nutrition:solar')
plot(eff, points=TRUE)
