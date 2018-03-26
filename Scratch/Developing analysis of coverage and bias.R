

sims_611_4_120 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario4-n=120.json'),
                                     simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                     simplifyMatrix = FALSE)
prob_approve(sims_611_4_120) 
# 0.5423 0.7313 0.9893 0.1881 0.5248 0.9648 (n=10,000)
sims_611_4_120$params$executation_info$exec_s / (60^2) # 14.1 hours walltime



sims <- sims_611_4_120
prob_eff <- sims$params$get_data_args$prob_eff

ls(sims$params)

library(magrittr)

# Mean 5% percentile
t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) %>% colMeans()
# 0.0600460 0.0883966 0.2961222 0.0263940 0.0579221 0.2345409

# Mean 95% percentile
t(sapply(sims$sims, function(x) x$results[['ProbEff_95%']])) %>% colMeans()
# 0.3167693 0.3272994 0.7011182 0.1923003 0.2937736 0.6511326

in_ci <- 
  t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) < 
  matrix(prob_eff, ncol = length(prob_eff), nrow = length(sims$sims), byrow = TRUE) &
  t(sapply(sims$sims, function(x) x$results[['ProbEff_95%']])) >
  matrix(prob_eff, ncol = length(prob_eff), nrow = length(sims$sims), byrow = TRUE) 
colMeans(in_ci) 
# 0.8547 0.8676 0.8782 0.8359 0.8592 0.8728
# Not quite 90% coverage from the 90% CIs, but not far off


# Try lower sample size
sims <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario4-n=60.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_eff <- sims$params$get_data_args$prob_eff

# Mean 5% percentile
t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) %>% colMeans()
# 0.0410934 0.0613198 0.2380588 0.0176489 0.0393234 0.1803950
# vs, with n=120:
# 0.0600460 0.0883966 0.2961222 0.0263940 0.0579221 0.2345409

# Mean 95% percentile
t(sapply(sims$sims, function(x) x$results[['ProbEff_95%']])) %>% colMeans()
# 0.3611193 0.3736263 0.7577932 0.2205052 0.3359275 0.7097407
# vs, with n=120:
# 0.3167693 0.3272994 0.7011182 0.1923003 0.2937736 0.6511326

# In every cohort, the CI is narrower with larger sample size, as expected.

in_ci <- 
  t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) < 
  matrix(prob_eff, ncol = length(prob_eff), nrow = length(sims$sims), byrow = TRUE) &
  t(sapply(sims$sims, function(x) x$results[['ProbEff_95%']])) >
  matrix(prob_eff, ncol = length(prob_eff), nrow = length(sims$sims), byrow = TRUE) 
colMeans(in_ci) 
# 0.7511 0.8467 0.8538 0.6459 0.7731 0.8404
# vs, with n=120:
# 0.8547 0.8676 0.8782 0.8359 0.8592 0.8728
# The material bias in cohorts 1, 4 and 5 with n=60 is largely ameliorated with n=120



# Now compare the CI coverage from a model that should have bias
sims <- jsonlite::fromJSON(readLines('411-params/uninf_priors/sims/scenario4.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_eff <- sims$params$get_data_args$prob_eff

# Mean 5% percentile
t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) %>% colMeans()
# 0.0421157 0.0732702 0.2726390 0.0268242 0.0466812 0.1980156
# vs, 6-param model with n=60:
# 0.0410934 0.0613198 0.2380588 0.0176489 0.0393234 0.1803950
# vs, 6-param model with n=120:
# 0.0600460 0.0883966 0.2961222 0.0263940 0.0579221 0.2345409

# Mean 95% percentile
t(sapply(sims$sims, function(x) x$results[['ProbEff_95%']])) %>% colMeans()
# 0.3122079 0.3646307 0.7427246 0.2297014 0.3059023 0.6676153
# vs, 6-param model with n=60:
# 0.3611193 0.3736263 0.7577932 0.2205052 0.3359275 0.7097407
# vs, 6-param model with n=120:
# 0.3167693 0.3272994 0.7011182 0.1923003 0.2937736 0.6511326

in_ci <- 
  t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) < 
  matrix(prob_eff, ncol = length(prob_eff), nrow = length(sims$sims), byrow = TRUE) &
  t(sapply(sims$sims, function(x) x$results[['ProbEff_95%']])) >
  matrix(prob_eff, ncol = length(prob_eff), nrow = length(sims$sims), byrow = TRUE) 
colMeans(in_ci) 
# 0.8027 0.8625 0.8668 0.8412 0.8337 0.8601
# vs, 6-param model with n=60:
# 0.7511 0.8467 0.8538 0.6459 0.7731 0.8404
# vs, 6-param model with n=120:
# 0.8547 0.8676 0.8782 0.8359 0.8592 0.8728
# Well, that is surprising. Excluding cohort 1, the 4-param efficacy model is
# not bad at all.

