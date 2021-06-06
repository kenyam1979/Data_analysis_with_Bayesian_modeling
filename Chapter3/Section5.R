library(rstan)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

file_beer_sales_2 <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-r-stan-bayesian-model-intro/master/book-data/3-2-1-beer-sales-2.csv')

## brms基本
simple_lm_brms <-brm(
  formula= sales ~ temperature,
  family=gaussian(link='identity'),
  data=file_beer_sales_2,
  seed=1)

plot(simple_lm_brms)


## フォーミュラ定義
simple_lm_formula <- bf(sales ~ temperature)
simple_lm_brms2 <- brm(
  formula=simple_lm_formula,
  family=gaussian(),
  data=file_beer_sales_2,
  seed=1,
  chains=4,
  iter=2000,
  warmup=1000,
  thin=1)

plot(simple_lm_brms2)

## 事前分布指定
simple_lm_brms3 <-brm(
  formula= sales ~ temperature,
  family=gaussian(link='identity'),
  data=file_beer_sales_2,
  seed=1,
  prior=c(set_prior('', class='Intercept'),
          set_prior('', class='sigma')))

plot(simple_lm_brms3)


## 事後分布プロット
mcmc_plot(simple_lm_brms,
         type='intervals',
         pars='^b_')

## 予測
new_data <- data.frame(temperature=20)
fitted(simple_lm_brms, new_data)
predict(simple_lm_brms, new_data)


## 回帰曲線図示
### 95%ベイズ信頼区間
eff <- marginal_effects(simple_lm_brms)
plot(eff, points=TRUE)

### 95%予測区間
eff_pre <- marginal_effects(simple_lm_brms, method='predict')
plot(eff_pre, points=TRUE)
