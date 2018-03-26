
# Investigating run speed ----
library(rstan)
source('CommonObjects.R')
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



# Investigating 411 model ----
model_dir = '411-params/'
sysname <- unname(Sys.info()['sysname'])
model_file <- paste0(model_dir, 'model-', sysname, '.RData')
load(model_file)
model

# Priors
priors_dir = paste0(model_dir, 'scept_priors/')
source(paste0(priors_dir, 'SpecificObjects.R'))
priors <- get_priors()

label <- 'Scenario 1'
get_data_args <- list(
  num_patients = 60,
  prob_eff = sc1_prob_eff,
  prob_tox = sc1_prob_tox,
  eff_tox_or = sc1_eff_tox_or,
  cohort_rho = cohort_rho
)

seed <- 836837029
refine_args <- list(min_eff = 0.1, max_tox = 0.3)

sampling_args <- list(chains = 2, iter = 2000, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# With log_lik calculation. 100s. i.e. 10s per sim. This used to be 1.5s. Why the change?

sampling_args <- list(chains = 4, iter = 1000, cores = 4, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# With log_lik calculation. 71s. Parallel is a bit quicker.

# Is the log_lik calc to blame?
# <edit and re-compile PePS2.stan>
sampling_args <- list(chains = 2, iter = 2000, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# Without log_lik calculation. 100s.

sampling_args <- list(chains = 4, iter = 1000, cores = 4, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# Without log_lik calculation. 71s. 
# So, the log_lik calc is not to blame.

# Am I inadvertently simulating LOTS of data?
length(sims$sims)
sims$sims[[1]]
sims$params$n
sims$params$sampling_args
sims$params$executation_info$exec_s
# No. Mysterious.

# The above are all on my MBP. Is that it?
sampling_args <- list(chains = 2, iter = 2000, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# With log_lik calculation. 161s. MP is slower still.

# <Reinstalls rstan on MP, rebuilds stan model>
sampling_args <- list(chains = 2, iter = 2000, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# With log_lik calculation. 31s. WTF?!?!?

# Now try parallel
sampling_args <- list(chains = 4, iter = 1000, cores = 4, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# With log_lik calculation. 61s. Parallel slower here.

# Back to MBP, just to check:
sampling_args <- list(chains = 2, iter = 2000, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# With log_lik calculation. 16s. So, recompiling the model changed the run time
# dramatically.

# MBP parallel:
sampling_args <- list(chains = 4, iter = 1000, cores = 4, seed = seed, refresh = 0)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# With log_lik calculation. 43s. Stay serial!

sampling_args <- list(chains = 2, iter = 2000, cores = 2, seed = seed, refresh = 0)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# With log_lik calculation. 31s. Stay serial!!


# Investigating the 611 model ----
model_dir = '611-params/'
sysname <- unname(Sys.info()['sysname'])
model_file <- paste0(model_dir, 'model-', sysname, '.RData')
load(model_file)
model

# Priors
priors_dir = paste0(model_dir, 'uninf_priors/')
source(paste0(priors_dir, 'SpecificObjects.R'))
priors <- get_priors()

label <- 'Scenario 1'
get_data_args <- list(
  num_patients = 60,
  prob_eff = sc1_prob_eff,
  prob_tox = sc1_prob_tox,
  eff_tox_or = sc1_eff_tox_or,
  cohort_rho = cohort_rho
)

seed <- 836837029
refine_args <- list(min_eff = 0.1, max_tox = 0.3)

sampling_args <- list(chains = 2, iter = 2000, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# Slow as shit. I am not even going to wait for it.
# <recompiles on MBP>
load(model_file)

sampling_args <- list(chains = 2, iter = 2000, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# Still slow.

# <recompiles on MP>
load(model_file)
sampling_args <- list(chains = 2, iter = 2000, seed = seed)
num_sims <- 10
system.time(sims <- run_batch(seed, num_sims, get_data_args, priors, model, 
                              sampling_args, refine_args, label, out_file = NULL))
# Much better. Uninf priors and/or 8-param model are def slower. 
# But it looks categorially like MBP compiles slow models.