
library(rstan)
rstan_options(auto_write = TRUE)
source('CommonObjects.R')

# Common ----
seed <- 836837029
sampling_args <- list(chains = 2, iter = 2000, seed = seed)
refine_args <- list(min_eff = 0.1, max_tox = 0.3)
num_sims <- 10000

run_batch <- function(seed, num_sims, get_data_args, priors, model, 
                      sampling_args, refine_args, label, out_file = NULL) {
  set.seed(seed)
  start_time <- lubridate::now()
  sims <- simn(n = num_sims, get_data_args = get_data_args, priors = priors,
               sampling_args = append(sampling_args, list(object = model)),
               refine_args = refine_args)
  end_time <- lubridate::now()
  sims <- add_params(sims, n = num_sims, label = label, get_data_args, 
                     sampling_args, refine_args, priors, start_time, end_time)
  if(!is.null(out_file)) {
    out_conn = file(out_file)
    write(jsonlite::toJSON(sims, auto_unbox = TRUE, pretty = TRUE, 
                           force = TRUE), file = out_conn)
    close(out_conn)
  }
  return(sims)
}

# Select model ----
# 441-Model
model_dir = '441-params/'
sysname <- unname(Sys.info()['sysname'])
model_file <- paste0(model_dir, 'model-', sysname, '.RData')
load(model_file)
model


# Select priors ----
# Sceptical priors
# priors_dir = paste0(model_dir, 'scept_priors/')
# out_dir <- paste0(priors_dir, 'sims/')
# source(paste0(priors_dir, 'SpecificObjects.R'))
# priors <- get_priors()
# jsonlite::toJSON(priors, auto_unbox = TRUE, pretty = TRUE)

# Informative priors
# priors_dir = paste0(model_dir, 'inf_priors/')
# out_dir <- paste0(priors_dir, 'sims/')
# source(paste0(priors_dir, 'SpecificObjects.R'))
# priors <- get_priors()
# jsonlite::toJSON(priors, auto_unbox = TRUE, pretty = TRUE)

# Uninformative priors
priors_dir = paste0(model_dir, 'uninf_priors/')
out_dir <- paste0(priors_dir, 'sims/')
source(paste0(priors_dir, 'SpecificObjects.R'))
priors <- get_priors()
jsonlite::toJSON(priors, auto_unbox = TRUE, pretty = TRUE)

label <- 'no label'


# n = <other> ----
n_pats <- 60

# Scenario 1 ----
scenario <- 1
json_file <- paste0('scenario', scenario, '-n=', n_pats, '.json')
out_file <- paste0(out_dir, json_file)

get_data_args <- list(
  num_patients = n_pats,
  prob_eff = sc1_prob_eff,
  prob_tox = sc1_prob_tox,
  eff_tox_or = sc1_eff_tox_or,
  cohort_rho = cohort_rho
)
sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                  sampling_args, refine_args, label, out_file)


# Scenario 2 ----
scenario <- 2
json_file <- paste0('scenario', scenario, '-n=', n_pats, '.json')
out_file <- paste0(out_dir, json_file)

get_data_args <- list(
  num_patients = n_pats,
  prob_eff = sc2_prob_eff,
  prob_tox = sc2_prob_tox,
  eff_tox_or = sc2_eff_tox_or,
  cohort_rho = cohort_rho
)
sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                  sampling_args, refine_args, label, out_file)


# Scenario 3 ----
scenario <- 3
json_file <- paste0('scenario', scenario, '-n=', n_pats, '.json')
out_file <- paste0(out_dir, json_file)

get_data_args <- list(
  num_patients = n_pats,
  prob_eff = sc3_prob_eff,
  prob_tox = sc3_prob_tox,
  eff_tox_or = sc3_eff_tox_or,
  cohort_rho = cohort_rho
)
sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                  sampling_args, refine_args, label, out_file)


# Scenario 4 ----
scenario <- 4
json_file <- paste0('scenario', scenario, '-n=', n_pats, '.json')
out_file <- paste0(out_dir, json_file)

get_data_args <- list(
  num_patients = n_pats,
  prob_eff = sc4_prob_eff,
  prob_tox = sc4_prob_tox,
  eff_tox_or = sc4_eff_tox_or,
  cohort_rho = cohort_rho
)
sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                  sampling_args, refine_args, label, out_file)


# Scenario 5 ----
scenario <- 5
json_file <- paste0('scenario', scenario, '-n=', n_pats, '.json')
out_file <- paste0(out_dir, json_file)

get_data_args <- list(
  num_patients = n_pats,
  prob_eff = sc5_prob_eff,
  prob_tox = sc5_prob_tox,
  eff_tox_or = sc5_eff_tox_or,
  cohort_rho = cohort_rho
)
sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                  sampling_args, refine_args, label, out_file)


# Scenario 6 ----
scenario <- 6
json_file <- paste0('scenario', scenario, '-n=', n_pats, '.json')
out_file <- paste0(out_dir, json_file)

get_data_args <- list(
  num_patients = n_pats,
  prob_eff = sc6_prob_eff,
  prob_tox = sc6_prob_tox,
  eff_tox_or = sc6_eff_tox_or,
  cohort_rho = cohort_rho
)
sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                  sampling_args, refine_args, label, out_file)


# Scenario 7----
scenario <- 7
json_file <- paste0('scenario', scenario, '-n=', n_pats, '.json')
out_file <- paste0(out_dir, json_file)

get_data_args <- list(
  num_patients = n_pats,
  prob_eff = sc7_prob_eff,
  prob_tox = sc7_prob_tox,
  eff_tox_or = sc7_eff_tox_or,
  cohort_rho = cohort_rho
)
sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                  sampling_args, refine_args, label, out_file)
