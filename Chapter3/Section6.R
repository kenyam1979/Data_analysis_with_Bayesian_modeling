library(rstan)
library(brms)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sales_weather <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/3-6-1-beer-sales-3.csv')

sales_weather %>%
  ggplot(aes(x=weather, y=sales)) +
  geom_violin() + 
  geom_point(aes(color=weather))


anova_brms <- brm(
  formula=sales ~ weather,
  family=gaussian(),
  data=sales_weather,
  seed=1,
  prior=c(set_prior('', class='Intercept'),
          prior=c(set_prior('', class='sigma'))))

anova_brms

eff <- marginal_effects(anova_brms)
plot(eff, points=FALSE)
