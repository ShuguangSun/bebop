
require(trialr)
require(jsonlite)

seed <- 123
out_dir <- '5params/paper_priors/sims/'
json_file <- 'scenarioRed.json'
num_sims <- 10000
prob_eff = c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439)
rho <- c(15.7, 21.8, 12.4, 20.7, 18.0, 11.4)

# The 5-parameter model must be compiled before it can be used. 
# See 5params/BuildStanModel.R
# Load the model that you compiled:
load("5params/model-mac.RData")
# load("5params/model-win.RData")
# load("5params/model-ubu.RData")

# The object 'model' should now be defined:
print(model)
# You should see the Stan definition in the console.

# Functions ----
peps2_run_sims <- function(num_sims, sample_data_func, summarise_func, 
                           stan_model = model, ...) {
  
  .run.sims <- function(model, num_sims, sample_data_func, summarise_func, ...) {
    sims <- list()
    for(i in 1:num_sims) {
      print(i)
      dat <- sample_data_func()
      fit <- rstan::sampling(model, data = dat, ...)
      sim <- summarise_func(dat, fit)
      sims[[i]] <- sim
    }
    return(sims)
  }
  
  dat <- sample_data_func()
  return(.run.sims(stan_model, num_sims, sample_data_func, summarise_func, 
                   ...))
}

sc_1_get_data <- function() trialr::peps2_get_data(num_patients = 60,
                                                   prob_eff = prob_eff,
                                                   prob_tox = 0.1,
                                                   eff_tox_or = 1.0,
                                                   cohort_rho = rho
)
process <- function(dat, fit, min_eff = 0.1, max_tox = 0.3,
                    eff_cert = 0.7, tox_cert = 0.9) {
  acc_eff <- as.matrix(rstan::extract(fit, par = 'prob_eff')[[1]]) > min_eff
  acc_tox <- as.matrix(rstan::extract(fit, par = 'prob_tox')[[1]]) < max_tox
  accept <- (apply(acc_eff, 2, mean) > eff_cert) &
    (apply(acc_tox, 2, mean) > tox_cert)
  prob_eff <- colMeans(as.matrix(rstan::extract(fit, 'prob_eff')[[1]]))
  prob_tox <- colMeans(as.matrix(rstan::extract(fit, 'prob_tox')[[1]]))
  l <- list(ProbEff = prob_eff, ProbAccEff = apply(acc_eff, 2, mean),
            ProbTox = prob_tox, ProbAccTox = apply(acc_tox, 2, mean),
            Accept = accept)
  # Append posterior parameter means
  l <- append(l, lapply(rstan::extract(fit, pars=c('alpha', 'beta', 'gamma',
                                                   'zeta', 'lambda')), mean))
  # Add psi posterior mean
  if('psi' %in% names(fit))
    l <- append(l, lapply(rstan::extract(fit, pars=c('psi')), mean))
  x <- l
  x$cohort_n = dat$cohort_n
  x$cohort_eff = dat$cohort_eff
  x$cohort_tox = dat$cohort_tox
  
  return(x)
}

# Run scenario ----

set.seed(seed)
sims <- peps2_run_sims(num_sims = num_sims, sample_data_func = sc_1_get_data,
                       summarise_func = process,
                       iter = 1000, chains = 2, verbose = FALSE)
# Expected Prob(Approve)
apply(sapply(sims, function(x) x$Accept), 1, mean)
# ? on Mac

# Write as JSON file
write(jsonlite::toJSON(sims, auto_unbox = T, pretty = T), file(paste0(out_dir, json_file)))

