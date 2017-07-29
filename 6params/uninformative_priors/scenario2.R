
require(trialr)
require(jsonlite)

seed <- 123
out_dir <- '6params/uninformative_priors/sims/'
json_file <- 'scenario2.json'
num_sims <- 10000

# Scenario 1
rho <- c(15.7, 21.8, 12.4, 20.7, 18.0, 11.4)
sc_1_get_data <- function() trialr::peps2_get_data(num_patients = 60,
                                                   prob_eff = rep(0.1, 6),
                                                   prob_tox = 0.3,
                                                   eff_tox_or = 1.0,
                                                   cohort_rho = rho,
                                                   alpha_mean = 0, alpha_sd = 10,
                                                   beta_mean = 0, beta_sd = 10,
                                                   gamma_mean = 0, gamma_sd = 10,
                                                   zeta_mean = 0, zeta_sd = 10,
                                                   lambda_mean = 0, lambda_sd = 10,
                                                   psi_mean = 0, psi_sd = 10
                                                  
)
process <- function(dat, fit, min_eff = 0.1, max_tox = 0.3,
                    eff_cert = 0.7, tox_cert = 0.9) {
  x <- trialr::peps2_process(dat = dat, fit = fit,
                             min_eff = min_eff, max_tox = max_tox,
                             eff_cert = eff_cert, tox_cert = tox_cert)
  # Additionally record the sampled cohort sizes and event frequencies
  x$cohort_n = dat$cohort_n
  x$cohort_eff = dat$cohort_eff
  x$cohort_tox = dat$cohort_tox
  
  return(x)
}

# Run simulations
set.seed(seed)
sims <- trialr::peps2_run_sims(num_sims = num_sims, sample_data_func = sc_1_get_data,
                               summarise_func = process,
                               iter = 1000, chains = 2, verbose = FALSE)
# Expected Prob(Approve)
apply(sapply(sims, function(x) x$Accept), 1, mean)
# 0.0230 0.0223 0.0185 0.0208 0.0228 0.0204 on Win

# Expected cohort sizes
round(apply(sapply(sims, function(x) x$cohort_n), 1, mean), 1)
# 9.4 13.1  7.5 12.4 10.8  6.9 on Win

# Write as JSON file
write(jsonlite::toJSON(sims, auto_unbox = T, pretty = T), file(paste0(out_dir, json_file)))


