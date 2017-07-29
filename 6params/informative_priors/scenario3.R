
require(trialr)
require(jsonlite)

seed <- 123
out_dir <- '6params/informative_priors/sims/'
json_file <- 'scenario3.json'
num_sims <- 10000

# Scenario 1
rho <- c(15.7, 21.8, 12.4, 20.7, 18.0, 11.4)
sc_1_get_data <- function() trialr::peps2_get_data(num_patients = 60,
                                                   prob_eff = rep(0.3, 6),
                                                   prob_tox = 0.1,
                                                   eff_tox_or = 0.2,
                                                   cohort_rho = rho,
                                                   alpha_mean = 0.0, alpha_sd = 1.3,
                                                   beta_mean = -1.0, beta_sd = 1.3,
                                                   gamma_mean = -2.75, gamma_sd = 1.3,
                                                   zeta_mean = -2.2, zeta_sd = 1.3,
                                                   lambda_mean = -2.2, lambda_sd = 2,
                                                   psi_mean = 0, psi_sd = 1
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
# 0.8709 0.9145 0.9989 0.8070 0.8423 0.9952 on Mac

# Expected cohort sizes
round(apply(sapply(sims, function(x) x$cohort_n), 1, mean), 1)
# 9.4 13.1  7.5 12.4 10.8  6.9 on Mac

# Write as JSON file
write(jsonlite::toJSON(sims, auto_unbox = T, pretty = T), file(paste0(out_dir, json_file)))
