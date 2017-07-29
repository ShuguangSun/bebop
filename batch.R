
library(rstan)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

# Main paper ----
# 6-param model
source('6params/paper_priors/scenario1.R')
source('6params/paper_priors/scenario2.R')
source('6params/paper_priors/scenario3.R')
source('6params/paper_priors/scenario4.R')
source('6params/paper_priors/scenario5.R')
source('6params/paper_priors/scenario6.R')
source('6params/paper_priors/scenario7.R')
# 5-param model
source('5params/paper_priors/scenario4.R')
source('5params/paper_priors/scenario6.R')


# Supplement ----
# Uninformative priors
source('6params/uninformative_priors/scenario1.R')
source('6params/uninformative_priors/scenario2.R')
source('6params/uninformative_priors/scenario3.R')
source('6params/uninformative_priors/scenario4.R')
source('6params/uninformative_priors/scenario5.R')
source('6params/uninformative_priors/scenario6.R')
source('6params/uninformative_priors/scenario7.R')

# Informative priors
source('6params/informative_priors/scenario1.R')
source('6params/informative_priors/scenario2.R')
source('6params/informative_priors/scenario3.R')
source('6params/informative_priors/scenario4.R')
source('6params/informative_priors/scenario5.R')
source('6params/informative_priors/scenario6.R')
source('6params/informative_priors/scenario7.R')


# Extras ----
# Flat cohort prevalences
source('6params/paper_priors/flat_prevs/scenario4.R')

