library(rstan)
library(brms)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sales_climate <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/3-7-1-beer-sales-4.csv')


sales_climate %>%
  ggplot(aes(x=temperature, y=sales, colour=weather)) +
  geom_point()


lm_brms <- brm(
  formula=sales ~ temperature + weather,
  family=gaussian(),
  data=sales_climate,
  seed=1,
  prior=c(set_prior('', class='Intercept'),
          set_prior('', class='sigma')))


eff <- marginal_effects(lm_brms, effects='temperature:weather')
plot(eff, points=TRUE)


