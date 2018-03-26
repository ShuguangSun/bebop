# Development ----
# Scenario 7 
sims <- jsonlite::fromJSON(readLines('411-params/scept_priors/sims/scenario7.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)

prob_approve(sims)
# 0.27 0.72 0.73 0.18 0.70 0.73
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.25 0.66 0.67 0.17 0.65 0.67

# Proposed priors
# TODO

sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario7.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)

prob_approve(sims)
# 0.319 0.856 0.787 0.048 0.171 0.093
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.273 0.762 0.639 0.031 0.105 0.052

sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario7-equal-cohorts.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)

prob_approve(sims)
# 0.309 0.817 0.866 0.050 0.159 0.112  # Why lower at 6 than 5? Makes no sense.
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.274 0.710 0.734 0.025 0.095 0.055
# TODO: WEIRD. LEARN WHY
ls(sims)
ls(sims$params)
sims$sims[[1]]$results$cohort_n
cohort_n <- do.call(rbind, lapply(sims$sims, function(x) x$results$cohort_n))
colMeans(cohort_n)


sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario7-n=100.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)

prob_approve(sims)
# 0.359 0.966 0.906 0.051 0.161 0.106
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.334 0.917 0.831 0.023 0.087 0.052

sims$params$get_data_args