
require(trialr)
require(jsonlite)

seed <- 123
out_dir <- '6params/paper_priors/sims/'
json_file <- 'scenario1.json'
num_sims <- 10000

# Scenario 1
rho <- c(15.7, 21.8, 12.4, 20.7, 18.0, 11.4)
sc_1_get_data <- function() trialr::peps2_get_data(num_patients = 60,
                                                   prob_eff = rep(0.3, 6),
                                                   prob_tox = 0.1,
                                                   eff_tox_or = 1.0,
                                                   cohort_rho = rho
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

set.seed(seed)
sims <- trialr::peps2_run_sims(num_sims = num_sims, sample_data_func = sc_1_get_data,
                               summarise_func = process,
                               iter = 1000, chains = 2, verbose = FALSE)
# Expected Prob(Approve)
apply(sapply(sims, function(x) x$Accept), 1, mean)
# 0.8965 0.9150 0.9043 0.9082 0.9012 0.8782 on Mac

round(apply(sapply(sims, function(x) x$cohort_n), 1, mean), 1)
# 9.4 13.1  7.5 12.4 10.8  6.9 on Mac

# Write as JSON file
write(jsonlite::toJSON(sims, auto_unbox = T, pretty = T), file(paste0(out_dir, json_file)))


#in_dir <- 'C:/Users/krist/Dropbox/Research/bebop/6params/paper_priors/sims/'
#sims <- jsonlite::fromJSON(readLines(file(paste0(in_dir, 'scenario1.json'))),
#    simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
#apply(sapply(sims, function(x) x$Accept), 1, mean)
#apply(sapply(sims, function(x) x$cohort_n), 1, mean)

# in_dir <- 'C:/Users/krist/Dropbox/Projects/Learning/RStan/package/phase2/bebop/peps2/categorical-pdl1/6params/sims/'
# sims <- jsonlite::fromJSON(readLines(file(paste0(in_dir, 'scenario1.json'))))
# apply(do.call(rbind, sims$Decision), 2, mean)
# 0.9012 0.9204 0.9101 0.9127 0.9053 0.8855
