library(rstan)
library(bayesplot)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fish_num_climate_2 <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/4-1-1-fish-num-2.csv')
fish_num_climate_2$id <- as.factor(fish_num_climate_2$id)

# ポワソン回帰でのモデリング ----

glm_pois_brms <- brm(
  formula=fish_num ~ weather + temperature,
  family=poisson(),
  data=fish_num_climate_2,
  seed=1,
  prior=c(set_prior('', class='Intercept')))

set.seed(1)
eff_glm_pre <- marginal_effects(
  glm_pois_brms,
  method='predict',
  effects='temperature:weather',
  probs=c(0.005, 0.995))
plot(eff_glm_pre, points=T)


# GLMMでのモデリング ----
## stan
formula_pois <- formula(fish_num ~ weather + temperature)
design_mat <- model.matrix(formula_pois, fish_num_climate_2)
sunny_dummy <- as.numeric(design_mat[, 'weathersunny'])

data_list_1 <- list(
  N=nrow(fish_num_climate_2),
  fish_num=fish_num_climate_2$fish_num,
  temp=fish_num_climate_2$temperature,
  sunny=sunny_dummy)

glmm_pois_stan <- stan(
  file='Chapter4/4-1-1-glmm-pois.stan',
  data=data_list_1,
  seed=1)

mcmc_rhat(rhat(glmm_pois_stan))

print(glmm_pois_stan, 
      pars=c('Intercept', 'b_sunny', 'b_temp', 'sigma_r'),
      probs=c(0.025, 0.5, 0.975))

## brms
glmm_pois_brms <- brm(
  formula=fish_num ~ weather + temperature + (1|id),
  family=poisson(),
  data=fish_num_climate_2,
  seed=1,
  prior=c(set_prior('', class='Intercept'),
          set_prior('', class='sd')))

glm_pois_brms
