
library(knitr)
library(xtable)

# Functions and common objects ----
beta_bin_decision <- function(sim,
                              eff_prior_alpha, eff_prior_beta,
                              tox_prior_alpha, tox_prior_beta,
                              eff_hurdle, eff_certainty,
                              tox_hurdle, tox_certainty) {
    prob_acc_eff = pbeta(q = eff_hurdle,
                       shape1 = eff_prior_alpha + sim$cohort_eff,
                       shape2 = eff_prior_beta + sim$cohort_n - sim$cohort_eff,
                       lower.tail = FALSE)
    prob_acc_tox = pbeta(q = tox_hurdle,
                       shape1 = tox_prior_alpha + sim$cohort_tox,
                       shape2 = tox_prior_beta + sim$cohort_n - sim$cohort_tox,
                       lower.tail = TRUE)
    accept = (prob_acc_eff > eff_certainty) & (prob_acc_tox > tox_certainty)
    return(accept)
}

beta_bin_perf <- function(sims,
                          eff_prior_alpha = 0.4, eff_prior_beta = 1.6,
                          tox_prior_alpha = 0.4, tox_prior_beta = 1.6,
                          eff_hurdle = 0.1, eff_certainty = 0.7,
                          tox_hurdle = 0.3, tox_certainty = 0.9) {
    return(apply(sapply(sims, beta_bin_decision,
                        eff_prior_alpha = eff_prior_alpha,
                        eff_prior_beta = eff_prior_beta,
                        tox_prior_alpha = tox_prior_alpha,
                        tox_prior_beta = tox_prior_beta,
                        eff_hurdle = eff_hurdle,
                        eff_certainty = eff_certainty,
                        tox_hurdle = tox_hurdle,
                        tox_certainty = tox_certainty)
                      , 1, mean))
}

scenario_params = list(
  list(
    ProbEff = rep(0.3, 6),
    ProbTox = rep(0.1, 6),
    Odds = rep(1, 6)
  ),
  list(
    ProbEff = rep(0.1, 6),
    ProbTox = rep(0.3, 6),
    Odds = rep(1, 6)
  ),
  list(
    ProbEff = rep(0.3, 6),
    ProbTox = rep(0.1, 6),
    Odds = rep(0.2, 6)
  ),
  list(
    ProbEff = c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439),
    ProbTox = rep(0.1, 6),
    Odds = rep(1, 6)
  ),
  list(
    ProbEff = c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439),
    ProbTox = rep(0.3, 6),
    Odds = rep(1, 6)
  ),
  list(
    ProbEff = c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439),
    ProbTox = rep(0.1, 6),
    Odds = rep(0.2, 6)
  ),
  list(
    ProbEff = c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439),
    ProbTox = rep(0.1, 6),
    Odds = rep(5, 6)
  )
)


# Table 5 in manuscript using modestly informative (aka "paper") priors ----
sim_files <- c(
  '6params/paper_priors/sims/scenario1.json',
  '6params/paper_priors/sims/scenario2.json',
  '6params/paper_priors/sims/scenario3.json',
  '6params/paper_priors/sims/scenario4.json',
  '6params/paper_priors/sims/scenario5.json',
  '6params/paper_priors/sims/scenario6.json'
  )

sims_list <- lapply(sim_files, function(sim_file) jsonlite::fromJSON(readLines(file(sim_file)),
    simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE))

BEBOP <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$Accept), 1, mean))
N <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_n), 1, mean))
Eff <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_eff), 1, mean))
Tox <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_tox), 1, mean))
BetaBin <- lapply(sims_list, beta_bin_perf)  # Default beta-bin priors

dfs <- lapply(1:length(sim_files), function(i) 
  data.frame(Scenario = i, 
             Cohort = 1:6,
             ProbEff = scenario_params[[i]]$ProbEff,
             ProbTox = scenario_params[[i]]$ProbTox,
             Odd = scenario_params[[i]]$Odds,
             N = N[[i]],
             Eff = Eff[[i]],
             Tox = Tox[[i]],
             BEBOP = BEBOP[[i]],
             BetaBin = BetaBin[[i]]
  )
)
tabs <- lapply(dfs, function(df) kable(df, digits = c(0,0,3,1,1,1,1,1,3,3)))
tabs
# Vertically stack the scenario data.frames
all_dfs = do.call(rbind, dfs)
kable(all_dfs, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3))
caption <- "A summary of simulated trials..."
print(xtable(all_dfs, digits = c(0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3),
             caption = caption, label = "tab:ocs"),
      hline.after = c(-1,0,6, 12, 18, 24, 30, 36), comment=F,
      include.rownames = FALSE)


# Table 6 in manuscript using 5-parameter model and paper priors ----
sim_files <- c(
  '5params/paper_priors/sims/scenario4.json',
  '5params/paper_priors/sims/scenario6.json'
)

sims_list <- lapply(sim_files, function(sim_file) jsonlite::fromJSON(readLines(file(sim_file)),
                                                                     simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE))

BEBOP <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$Accept), 1, mean))
N <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_n), 1, mean))
Eff <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_eff), 1, mean))
Tox <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_tox), 1, mean))
Scenarios = c(4, 6)
dfs <- lapply(1:length(sim_files), function(i) 
  data.frame(Scenario = Scenarios[i], 
             Cohort = 1:6,
             ProbEff = scenario_params[[Scenarios[i]]]$ProbEff,
             ProbTox = scenario_params[[Scenarios[i]]]$ProbTox,
             Odd = scenario_params[[Scenarios[i]]]$Odds,
             N = N[[i]],
             Eff = Eff[[i]],
             Tox = Tox[[i]],
             BEBOP = BEBOP[[i]]
  )
)
tabs <- lapply(dfs, function(df) kable(df, digits = c(0,0,3,1,1,1,1,1,3,3)))
tabs
# Vertically stack the scenario data.frames
all_dfs = do.call(rbind, dfs)
kable(all_dfs, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3))
caption <- "A subset of Table TODO showing performance of the 5-parameter BEBOP..."
print(xtable(all_dfs, digits = c(0, 0, 0, 3, 1, 1, 1, 1, 1, 3),
             caption = caption, label = "tab:5param_ocs"),
      hline.after = c(-1,0,6, 12), comment=F,
      include.rownames = FALSE)


# Table 4 in supplement using uninformative priors ----
sim_files <- c(
  '6params/uninformative_priors/sims/scenario1.json',
  '6params/uninformative_priors/sims/scenario2.json',
  '6params/uninformative_priors/sims/scenario3.json',
  '6params/uninformative_priors/sims/scenario4.json',
  '6params/uninformative_priors/sims/scenario5.json',
  '6params/uninformative_priors/sims/scenario6.json'
)

sims_list <- lapply(sim_files, function(sim_file) jsonlite::fromJSON(readLines(file(sim_file)),
                                                                     simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE))

BEBOP <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$Accept), 1, mean))
N <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_n), 1, mean))
Eff <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_eff), 1, mean))
Tox <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_tox), 1, mean))
BetaBin <- lapply(sims_list, beta_bin_perf, 
                  eff_prior_alpha = 0.001, eff_prior_beta = 0.001,
                  tox_prior_alpha = 0.001, tox_prior_beta = 0.001)

dfs <- lapply(1:length(sim_files), function(i) 
  data.frame(Scenario = i, 
             Cohort = 1:6,
             ProbEff = scenario_params[[i]]$ProbEff,
             ProbTox = scenario_params[[i]]$ProbTox,
             Odd = scenario_params[[i]]$Odds,
             N = N[[i]],
             Eff = Eff[[i]],
             Tox = Tox[[i]],
             BEBOP = BEBOP[[i]],
             BetaBin = BetaBin[[i]]
  )
)
tabs <- lapply(dfs, function(df) kable(df, digits = c(0,0,3,1,1,1,1,1,3,3)))
tabs
# Vertically stack the scenario data.frames
all_dfs = do.call(rbind, dfs)
kable(all_dfs, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3))
caption <- "Uninformative priors..."
print(xtable(all_dfs, digits = c(0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3),
             caption = caption, label = "tab:uninformative_ocs"),
      hline.after = c(-1,0,6, 12, 18, 24, 30, 36), comment=F,
      include.rownames = FALSE)


# Table 8 in supplement using informative priors ----
sim_files <- c(
  '6params/informative_priors/sims/scenario1.json',
  '6params/informative_priors/sims/scenario2.json',
  '6params/informative_priors/sims/scenario3.json',
  '6params/informative_priors/sims/scenario4.json',
  '6params/informative_priors/sims/scenario5.json',
  '6params/informative_priors/sims/scenario6.json'
)

sims_list <- lapply(sim_files, function(sim_file) jsonlite::fromJSON(readLines(file(sim_file)),
                                                                     simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE))

BEBOP <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$Accept), 1, mean))
N <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_n), 1, mean))
Eff <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_eff), 1, mean))
Tox <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_tox), 1, mean))

dfs <- lapply(1:length(sim_files), function(i) 
  data.frame(Scenario = i, 
             Cohort = 1:6,
             ProbEff = scenario_params[[i]]$ProbEff,
             ProbTox = scenario_params[[i]]$ProbTox,
             Odd = scenario_params[[i]]$Odds,
             N = N[[i]],
             Eff = Eff[[i]],
             Tox = Tox[[i]],
             BEBOP = BEBOP[[i]]
             #BetaBin = BetaBin[[i]]
  )
)
tabs <- lapply(dfs, function(df) kable(df, digits = c(0,0,3,1,1,1,1,1,3)))
tabs
# Vertically stack the scenario data.frames
all_dfs = do.call(rbind, dfs)
kable(all_dfs, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3))
caption <- "Informative priors..."
print(xtable(all_dfs, digits = c(0, 0, 0, 3, 1, 1, 1, 1, 1, 3),
             caption = caption, label = "tab:uninformative_ocs"),
      hline.after = c(-1,0,6, 12, 18, 24, 30, 36), comment=F,
      include.rownames = FALSE)


# Table 9 in supplement using flat cohort prevalences ----
sim_files <- c(
  '6params/paper_priors/sims/scenario4.json',
  '6params/paper_priors/flat_prevs/sims/scenario4.json'
)

sims_list <- lapply(sim_files, function(sim_file) jsonlite::fromJSON(readLines(file(sim_file)),
                                                                     simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE))

BEBOP <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$Accept), 1, mean))
N <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_n), 1, mean))
Eff <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_eff), 1, mean))
Tox <- lapply(sims_list, function(sims) apply(sapply(sims, function(x) 
  x$cohort_tox), 1, mean))

scenario = 4
df <- data.frame(
  Cohort = 1:6,
  ProbEff = scenario_params[[scenario]]$ProbEff,
  ProbTox = scenario_params[[scenario]]$ProbTox,
  N1 = N[[1]],
  ProbApprove1 = BEBOP[[1]],
  N2 = N[[2]],
  ProbApprove2 = BEBOP[[2]]
)
df
kable(df, digits = c(0, 3, 1, 1, 3, 1, 3))
print(xtable(df, digits = c(0, 0, 3, 1, 1, 3, 1, 3),
             caption = '', label = "tab:oc.prevalence.comparison"),
      hline.after = c(-1,0,6), comment=F,
      include.rownames = FALSE)


