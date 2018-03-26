


# Using cohorts prevalences used in paper and Table 1 in supplement ----
sim_file <- '411-params/scept_priors/sims/scenario1.json'
sims <- jsonlite::fromJSON(readLines(file(sim_file)), 
                           simplifyVector = TRUE, 
                           simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
cohort_n <- t(sapply(sims, function(x) x$cohort_n))


# Mean cohort sizes
colMeans(cohort_n)
# Median cohort sizes
apply(cohort_n, 2, median)
# 2.5% quantile
apply(cohort_n, 2, quantile, 0.025)
# 97.5% quantile
apply(cohort_n, 2, quantile, 0.975)

# Cohort prevalences
library(gtools)
set.seed(123)
x <- rdirichlet(n = 100000, alpha = c(15.7, 21.8, 12.4, 20.7, 18.0, 11.4))
# 2.5% quantile
round(apply(x, 2, quantile, 0.025), 3)
# 97.5% quantile
round(apply(x, 2, quantile, 0.975), 3)
