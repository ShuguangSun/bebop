

# Check save-load consistency ----
# apply(sapply(sims$sims, function(x) x$results$ProbAccEff) > 0.7, 1, mean)
# # 0.90 0.89 0.92 0.93 0.87 0.93 under 441
# 
#sims2 <- jsonlite::fromJSON(readLines(paste0(out_dir, json_file)),
#simplifyVector = TRUE,
#simplifyDataFrame = FALSE,
#simplifyMatrix = FALSE)
#apply(sapply(sims2$sims, function(x) x$results$ProbAccEff) > 0.7, 1, mean)
## 0.90 0.89 0.92 0.93 0.87 0.93
## OK

# Making changes to refine ----
source('CommonObjects.R')
# Model
model_dir = '611-params/'
sysname <- unname(Sys.info()['sysname'])
model_file <- paste0(model_dir, 'model-', sysname, '.RData')
load(model_file); model
# Priors
priors_dir = paste0(model_dir, 'uninf_priors/')
source(paste0(priors_dir, 'SpecificObjects.R'))
priors <- get_priors()
# Data
dat <- get_data_in_cohorts(num_patients = 60, prob_eff = sc4_prob_eff, 
                           prob_tox = sc4_prob_tox, eff_tox_or = sc4_eff_tox_or,
                           cohort_rho = cohort_rho)
dat <- append(dat, priors)
# Sample
library(rstan)
samp <- sampling(model, data = dat)
refine(samp, dat, min_eff = 0.1, max_tox = 0.3) # Add post CI

# Refine
fit=samp
acc_eff <- as.matrix(rstan::extract(fit, par = 'prob_eff')[[1]]) > 0.1
acc_tox <- as.matrix(rstan::extract(fit, par = 'prob_tox')[[1]]) < 0.3
prob_eff <- colMeans(as.matrix(rstan::extract(fit, 'prob_eff')[[1]]))
prob_tox <- colMeans(as.matrix(rstan::extract(fit, 'prob_tox')[[1]]))

l <- list(
  ProbEff = prob_eff,
  ProbAccEff = apply(acc_eff, 2, mean),
  ProbTox = prob_tox,
  ProbAccTox = apply(acc_tox, 2, mean)
)

params_to_summarise <- names(fit)[!sapply(names(fit), function(x) grepl('log_lik', x) | grepl('prob_', x))]
summ_samp <- summary(samp, params_to_summarise, probs = c(0.05, 0.95))$summary
l <- append(l, summ_samp[, 'mean'])

prob_eff_labs <- names(fit)[sapply(names(fit), function(x) grepl('prob_eff', x))]
summ_samp <- summary(samp, prob_eff_labs, probs = c(0.05, 0.95))$summary
l[['ProbEff_5%']] <- round(summ_samp[, '5%'], 3)
l[['ProbEff_95%']] <- round(summ_samp[, '95%'], 3)
prob_tox_labs <- names(fit)[sapply(names(fit), function(x) grepl('prob_tox', x))]
summ_samp <- summary(samp, prob_tox_labs, probs = c(0.05, 0.95))$summary
l[['ProbTox_5%']] <- round(summ_samp[, '5%'], 3)
l[['ProbTox_95%']] <- round(summ_samp[, '95%'], 3)

jsonlite::toJSON(l, auto_unbox = T)
# <updates CommonObjects.R>
refine(samp, dat, min_eff = 0.1, max_tox = 0.3)
# There it is

# Calculating % of post estimates in CI in a set of sims:
library(magrittr)
t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) %>% head()
sc4_prob_eff
in_ci <- t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) < 
  matrix(sc4_prob_eff, ncol = length(sc4_prob_eff), nrow = 100, byrow = TRUE) &
  t(sapply(sims$sims, function(x) x$results[['ProbEff_95%']])) >
  matrix(sc4_prob_eff, ncol = length(sc4_prob_eff), nrow = 100, byrow = TRUE) 
colMeans(in_ci) # 84 - 92



# Adding log-likelihood for LOOIC support ----
source('CommonObjects.R')
dat <- get_data_in_cohorts(num_patients = 600, prob_eff = sc4_prob_eff, 
                           prob_tox = sc4_prob_tox, eff_tox_or = sc4_eff_tox_or,
                           cohort_rho = cohort_rho)


library(rstan)
library(loo)

model1 <- stan_model(file = '411-params/PePS2.stan')
model2 <- stan_model(file = '441-params/PePS2_loglik.stan')

source('411-params/scept_priors/SpecificObjects.R')
priors1 <- get_priors()
dat1 <- append(dat, priors1)
samp1 <- rstan::sampling(object = model1, data = dat1, chains = 4, cores = 4)
log_lik_1 <- extract_log_lik(samp1)
loo_1 <- loo(log_lik_1)

source('441-params/scept_priors/SpecificObjects.R')
priors2 <- get_priors()
dat2 <- append(dat, priors2)
samp2 <- rstan::sampling(object = model2, data = dat2, chains = 4, cores = 4)
log_lik_2 <- extract_log_lik(samp2)
loo_2 <- loo(log_lik_2)

loo_1
loo_2
compare(loo_1, loo_2)



# Stan working ----
eight_schools <- stan_demo("eight_schools")
eight_schools
