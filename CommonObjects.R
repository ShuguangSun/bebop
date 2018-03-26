
# Scenario parameters ----
cohort_rho <- c(15.7, 21.8, 12.4, 20.7, 18.0, 11.4)
# cohort_probs <-  NULL

sc1_prob_eff <- rep(0.3, 6)
sc1_prob_tox <- rep(0.1, 6)
sc1_eff_tox_or <- rep(1, 6)

sc2_prob_eff <- rep(0.1, 6)
sc2_prob_tox <- rep(0.3, 6)
sc2_eff_tox_or <- rep(1, 6)

sc3_prob_eff <- rep(0.3, 6)
sc3_prob_tox <- rep(0.1, 6)
sc3_eff_tox_or <- rep(0.2, 6)

sc4_prob_eff <- c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439)
sc4_prob_tox <- rep(0.1, 6)
sc4_eff_tox_or <- rep(1, 6)

sc5_prob_eff <- c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439)
sc5_prob_tox <- rep(0.3, 6)
sc5_eff_tox_or <- rep(1, 6)

sc6_prob_eff <- c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439)
sc6_prob_tox <- rep(0.1, 6)
sc6_eff_tox_or <- rep(0.2, 6)

# sc7_prob_eff <- c(0.12, 0.43, 0.55, 0.10, 0.39, 0.48)
# sc7_prob_tox <- c(rep(0.1, 3), rep(0.3, 3))
# sc7_eff_tox_or <- rep(1, 6)
sc7_prob_eff <- c(0.167, 0.192, 0.500, 0.091, 0.156, 0.439)
sc7_prob_tox <- c(rep(0.1, 3), rep(0.3, 3))
sc7_eff_tox_or <- rep(1, 6)

# Simulation functions ----
get_data_in_cohorts <- function(num_patients, cohort_probs = NULL,
                                prob_eff, prob_tox, eff_tox_or,
                                cohort_rho = cohort_rho)
{
  if (is.null(cohort_probs))
    cohort_probs <- gtools::rdirichlet(1, cohort_rho)
  cohort_sizes <- c(stats::rmultinom(1, size = num_patients,
                                     prob = cohort_probs))
  cohorts <- rep(1:length(cohort_sizes), times = cohort_sizes)
  cohorts = factor(cohorts, levels = 1:length(cohort_probs))

  cohort.params = cbind(cohort_sizes, prob_eff, prob_tox, eff_tox_or)
  cohort.params = split(cohort.params, 1:length(cohort_sizes))
  outcomes <- lapply(cohort.params, function(x) ranBin2(x[1], x[2:3], psi = x[4]))
  outcomes <- do.call(rbind, outcomes)

  eff <- outcomes[, 1]
  tox <- outcomes[, 2]
  x1 <- as.integer(cohorts %in% 4:6)
  x2 <- as.integer(cohorts == 1 | cohorts == 4)
  x3 <- as.integer(cohorts == 2 | cohorts == 5)
  cohort_eff = unname(tapply(eff, cohorts, sum))
  cohort_eff[is.na(cohort_eff)] = 0
  cohort_tox = unname(tapply(tox, cohorts, sum))
  cohort_tox[is.na(cohort_tox)] = 0

  dat <- list(
    # Data
    num_patients = num_patients,
    # Outcomes
    eff = eff,
    tox = tox,
    # Covariates
    x1 = x1,
    x2 = x2,
    x3 = x3,
    # Cohort counts
    cohort_n = cohort_sizes,
    cohort_eff = cohort_eff,
    cohort_tox = cohort_tox
  )

  return(dat)
}

refine <- function(fit, dat, min_eff, max_tox)
{
  acc_eff <- as.matrix(rstan::extract(fit, par = 'prob_eff')[[1]]) > min_eff
  acc_tox <- as.matrix(rstan::extract(fit, par = 'prob_tox')[[1]]) < max_tox
  prob_eff <- colMeans(as.matrix(rstan::extract(fit, 'prob_eff')[[1]]))
  prob_tox <- colMeans(as.matrix(rstan::extract(fit, 'prob_tox')[[1]]))

  l <- list(
    ProbEff = prob_eff,
    ProbAccEff = apply(acc_eff, 2, mean),
    ProbTox = prob_tox,
    ProbAccTox = apply(acc_tox, 2, mean)
  )

  # Posterior parameter means 
  params_to_summarise <- names(fit)[!sapply(names(fit), function(x) grepl('log_lik', x) | grepl('prob_', x))]
  summ_samp <- summary(fit, params_to_summarise, probs = c(0.05, 0.95))$summary
  l <- append(l, summ_samp[, 'mean'])
  
  # Posterior CIs of prob_eff
  prob_eff_labs <- names(fit)[sapply(names(fit), function(x) grepl('prob_eff', x))]
  summ_samp <- summary(fit, prob_eff_labs, probs = c(0.05, 0.95))$summary
  l[['ProbEff_5%']] <- round(summ_samp[, '5%'], 3)
  l[['ProbEff_95%']] <- round(summ_samp[, '95%'], 3)
  
  # Posterior CIs of prob_tox
  prob_tox_labs <- names(fit)[sapply(names(fit), function(x) grepl('prob_tox', x))]
  summ_samp <- summary(fit, prob_tox_labs, probs = c(0.05, 0.95))$summary
  l[['ProbTox_5%']] <- round(summ_samp[, '5%'], 3)
  l[['ProbTox_95%']] <- round(summ_samp[, '95%'], 3)
  
  # Sample size and number of events by cohort
  l$cohort_n = dat$cohort_n
  l$cohort_eff = dat$cohort_eff
  l$cohort_tox = dat$cohort_tox

  return(l)
}

sim1 <- function(get_data_args, priors, sampling_args = list(), refine_args)
{
  dat <- do.call(get_data_in_cohorts, get_data_args)
  dat <- append(dat, priors)

  sampling_args[['data']] <- dat
  fit <- do.call(rstan::sampling, sampling_args)

  refine_args[['fit']] <- fit
  refine_args[['dat']] <- dat
  results = do.call(refine, refine_args)

  return(list(results = results))
}

simn <- function(n, ...)
{
  sims <- list()
  for (i in 1:n)
  {
    print(i)
    sim <- sim1(...)
    sims[[i]] <- sim
  }
  return(list(sims = sims))
}

add_params <- function(sims, n, label, get_data_args, sampling_args, refine_args, 
                       priors, start_time, end_time)
{
  executation_info <- list(
    start_time = start_time,
    end_time = end_time,
    exec_s = as.numeric(difftime(end_time, start_time, units = 'secs')),
    sessionInfo = sessionInfo()
  )
  sims$params <- list(n = n, label = label, get_data_args = get_data_args,
    sampling_args = sampling_args, refine_args = refine_args, priors = priors,
    executation_info = executation_info)
  return(sims)
}

# Functions reproduced from binarySimCLF for sampling correlated binary events ----
# https://github.com/cran/binarySimCLF/blob/master/R/ranBin2.R

# This fails:
# install.packages('binarySimCLF')
# So I have taken the functions from archive tarball

.mardia <-
  function(a, b, c)
  {
    if (a == 0)
    {
      return(-c / b);

    }
    k <- ifelse(b > 0, 1, ifelse(b < 0, -1, 0));
    p <- -0.5 * (b + k * sqrt(b ^ 2 - 4 * a * c));
    r1 <- p / a;
    r2 <- c / p;

    r <- ifelse(r2 > 0, r2, r1);
    return(r);
  }

.solve2 <-
  function(mui, muj, psi)
  {
    if (psi == 1)
    {
      return(mui * muj);
    }
    else if (psi != 1)
    {
      a <- 1 - psi;
      b <- 1 - a * (mui + muj);
      c <- -psi * (mui * muj);
      muij <- .mardia(a, b, c);
    }
    return(muij);
  }

#' @title Sample pairs of correlated binary events
#'
#' @description This function is reproduced from the \code{binarySimCLF} package
#' on CRAN. The original package appears no longer to be maintained.
#' View the original source at:
#'  https://github.com/cran/binarySimCLF/blob/master/R/ranBin2.R
#'
#' @param nRep Number of simulated event pairs, positive integer.
#' @param u Mean event probabilities, expressed as a vector of length 2. E.g.
#' to simulate associated bivariate events with probabilities 80% and 30%, use
#' \code{u = c(0.8, 0.3)}.
#' @param psi Odds ratio, number. This parameter controls the strength of
#' association. Use \code{psi = 1} for no association. Values greater than 1
#' correspond to increasingly positive association between the two events,
#' and vice-versa.
#'
#' @return Matrix of events represented as 0s and 1s, with \code{nRep} rows
#' and 2 columns. The first column is the incidence of event 1.
#' @export
#'
#' @examples
#' probs <- c(0.8, 0.3)
#' s <- ranBin2(1000, probs, psi=0.2)  # 1000 pairs of outcomes
#' cor(s)  # Negatively correlated because psi < 1
#' colMeans(s)  # Event rates as expected
ranBin2 <-
  function(nRep, u, psi)
  {
    if (nRep > 0)
    {
      u12 <- .solve2(u[1], u[2], psi);
      y <- matrix(rep(-1, 2 * nRep), nrow = nRep);
      y[, 1] <- ifelse(stats::runif(nRep) <= u[1], 1, 0);
      y[, 2] <- y[, 1] * (stats::runif(nRep) <= u12 / u[1]) + (1 - y[, 1]) *
        (stats::runif(nRep) <= (u[2] - u12) / (1 - u[1]));
      return(y)
    } else
    {
      return(matrix(ncol = 2, nrow = 0))
    }
  }

# Results ----
# Arrange and present simulation results using functions ----
prob_approve <- function(sims, eff_certainty = 0.7, tox_certainty = 0.9)
{
  prob <- apply(
    sapply(sims$sims, function(x) x$results$ProbAccEff) > eff_certainty &
      sapply(sims$sims, function(x) x$results$ProbAccTox) > tox_certainty,
    1,
    mean)
  return(prob)
}

# stack_sims_old <- function(files, scen_str = NULL, num_cohorts = 6, ...) {
#   # E.g.
#   # files <- c('411-params/scept_priors/sims/scenario1.json',
#   #            '411-params/scept_priors/sims/scenario2.json')
#   # ... args ar passed to prob_approve
#   sims_batches <- lapply(
#     files, function(x) jsonlite::fromJSON(readLines(x),
#                                           simplifyVector = TRUE, 
#                                           simplifyDataFrame = FALSE, 
#                                           simplifyMatrix = FALSE)
#   )
#   
#   prob_eff <- sapply(sims_batches, function(x) x$params$get_data_args$prob_eff)
#   prob_tox <- sapply(sims_batches, function(x) x$params$get_data_args$prob_tox)
#   odds <- sapply(sims_batches, function(x) x$params$get_data_args$eff_tox_or)
#   N <- sapply(sims_batches, function(x) apply(sapply(
#     x$sims, function(y) y$results$cohort_n), 1, mean))
#   Eff <- sapply(sims_batches, function(x) apply(sapply(
#     x$sims, function(y) y$results$cohort_eff), 1, mean))
#   Tox <- sapply(sims_batches, function(x) apply(sapply(
#     x$sims, function(y) y$results$cohort_tox), 1, mean))
#   Perf <- sapply(sims_batches, function(x) prob_approve(x, ...))
#   
#   if(is.null(scen_str)) {
#     scenarios <- as.character(1:length(files))
#   } else{ 
#     scenarios <- scen_str 
#   }
#   scenarios <- as.vector(
#     sapply(scenarios, function(x) c(x, rep('', num_cohorts - 1))))
#   cohorts <- rep(1:num_cohorts, length(files))
#   
#   df <- data.frame(
#     Scenario = scenarios,
#     Cohort = cohorts,
#     ProbEff = as.vector(prob_eff),
#     ProbTox = as.vector(prob_tox),
#     Odds = as.vector(odds),
#     N = as.vector(N),
#     Eff = as.vector(Eff),
#     Tox = as.vector(Tox),
#     ProbApprove = as.vector(Perf)
#   )
#   df
# }

stack_sims <- function(files, scenario_labels = NULL, num_cohorts = 6, 
                       include_num_sims = FALSE,
                       include_total_num_patients = FALSE, 
                       include_odds = TRUE,
                       include_eff_coverage = FALSE,
                       include_tox_coverage = FALSE,
                       ...) {
  # E.g.
  # files <- c('411-params/scept_priors/sims/scenario1.json',
  #            '411-params/scept_priors/sims/scenario2.json')
  # ... args ar passed to prob_approve
  
  # # Scratch
  # files = c('441-params/uninf_priors/sims/scenario1-n=60.json')
  # length(sims_batches)
  # ls(sims_batches[[1]]$params)
  # sims_batches[[1]]$params$get_data_args$num_patients
  
  # Scenario
  if(is.null(scenario_labels)) {
    scenarios <- as.character(1:length(files))
  } else{ 
    scenarios <- scenario_labels 
  }
  scenarios <- as.vector(
    sapply(scenarios, function(x) c(x, rep('', num_cohorts - 1))))
  df <- data.frame(
    Scenario = scenarios
  )
  
  
  # Simulation results
  sims_batches <- lapply(
    files, function(x) jsonlite::fromJSON(readLines(x),
                                          simplifyVector = TRUE, 
                                          simplifyDataFrame = FALSE, 
                                          simplifyMatrix = FALSE)
  )
  
  # Number of simulations
  if(include_num_sims) {
    num_sims <- sapply(sims_batches, function(x) x$params$n)
    num_sims <- as.vector(sapply(num_sims, function(x) c(x, rep('', num_cohorts - 1))))
    df$NumSims <- num_sims
  }
  
  # Total number of patients
  if(include_total_num_patients) {
    total_n <- sapply(sims_batches, 
                      function(x) x$params$get_data_args$num_patients)
    total_n <- as.vector(
      sapply(total_n, function(x) c(x, rep('', num_cohorts - 1))))
    df$TotalN <- total_n
  }
  
  # Cohort
  cohorts <- rep(1:num_cohorts, length(files))
  df$Cohort = cohorts
  
  # ProbEff and ProbTox
  prob_eff <- sapply(sims_batches, function(x) x$params$get_data_args$prob_eff)
  df$ProbEff <- as.vector(prob_eff)
  prob_tox <- sapply(sims_batches, function(x) x$params$get_data_args$prob_tox)
  df$ProbTox <- as.vector(prob_tox)
  
  # Odds
  if(include_odds) {
    odds <- sapply(sims_batches, function(x) x$params$get_data_args$eff_tox_or)
    df$Odds <- as.vector(odds)
  }
  
  # N, Eff, Tox, ProbApprove
  N <- sapply(sims_batches, function(x) apply(sapply(
    x$sims, function(y) y$results$cohort_n), 1, mean))
  df$N <- as.vector(N)
  Eff <- sapply(sims_batches, function(x) apply(sapply(
    x$sims, function(y) y$results$cohort_eff), 1, mean))
  df$Eff <- as.vector(Eff)
  Tox <- sapply(sims_batches, function(x) apply(sapply(
    x$sims, function(y) y$results$cohort_tox), 1, mean))
  df$Tox <- as.vector(Tox)
  Perf <- sapply(sims_batches, function(x) prob_approve(x, ...))
  df$ProbApprove <- as.vector(Perf)
  
  # EffCov
  if(include_eff_coverage) {
    calc_eff_coverage <- function(sims) {
      prob_eff <- sims$params$get_data_args$prob_eff
      in_ci <- 
        t(sapply(sims$sims, function(x) x$results[['ProbEff_5%']])) < 
        matrix(prob_eff, ncol = length(prob_eff), nrow = length(sims$sims), 
               byrow = TRUE) &
        t(sapply(sims$sims, function(x) x$results[['ProbEff_95%']])) >
        matrix(prob_eff, ncol = length(prob_eff), nrow = length(sims$sims), 
               byrow = TRUE) 
      colMeans(in_ci) 
    }
    df$EffCov <- as.vector(sapply(sims_batches, function(x) calc_eff_coverage(x)))
  }
  
  # ToxCov
  if(include_tox_coverage) {
    calc_tox_coverage <- function(sims) {
      prob_tox <- sims$params$get_data_args$prob_tox
      in_ci <- 
        t(sapply(sims$sims, function(x) x$results[['ProbTox_5%']])) < 
        matrix(prob_tox, ncol = length(prob_tox), nrow = length(sims$sims), 
               byrow = TRUE) &
        t(sapply(sims$sims, function(x) x$results[['ProbTox_95%']])) >
        matrix(prob_tox, ncol = length(prob_tox), nrow = length(sims$sims), 
               byrow = TRUE) 
      colMeans(in_ci) 
    }
    df$ToxCov <- as.vector(sapply(sims_batches, function(x) calc_tox_coverage(x)))
  }
  
  df
}

beta_bin_decision <- function(sim,
                              eff_prior_alpha, eff_prior_beta,
                              tox_prior_alpha, tox_prior_beta,
                              eff_hurdle, eff_certainty,
                              tox_hurdle, tox_certainty) {
  prob_acc_eff = pbeta(q = eff_hurdle,
                       shape1 = eff_prior_alpha + sim$results$cohort_eff,
                       shape2 = eff_prior_beta + sim$results$cohort_n - sim$results$cohort_eff,
                       lower.tail = FALSE)
  prob_acc_tox = pbeta(q = tox_hurdle,
                       shape1 = tox_prior_alpha + sim$results$cohort_tox,
                       shape2 = tox_prior_beta + sim$results$cohort_n - sim$results$cohort_tox,
                       lower.tail = TRUE)
  accept = (prob_acc_eff > eff_certainty) & (prob_acc_tox > tox_certainty)
  return(accept)
}

beta_bin_perf <- function(sims,
                          eff_prior_alpha = 0.4, eff_prior_beta = 1.6,
                          tox_prior_alpha = 0.4, tox_prior_beta = 1.6,
                          eff_hurdle = 0.1, eff_certainty = 0.7,
                          tox_hurdle = 0.3, tox_certainty = 0.9) {
  return(apply(sapply(sims$sims, beta_bin_decision,
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

stack_sims_beta_binom <- function(files, scen_str = NULL, num_cohorts = 6, ...) {
  # E.g.
  # files <- c('411-params/scept_priors/sims/scenario1.json',
  #            '411-params/scept_priors/sims/scenario2.json')
  # ... args ar passed to prob_approve
  sims_batches <- lapply(
    files, function(x) jsonlite::fromJSON(readLines(x),
                                          simplifyVector = TRUE, 
                                          simplifyDataFrame = FALSE, 
                                          simplifyMatrix = FALSE)
  )
  
  prob_eff <- sapply(sims_batches, function(x) x$params$get_data_args$prob_eff)
  prob_tox <- sapply(sims_batches, function(x) x$params$get_data_args$prob_tox)
  odds <- sapply(sims_batches, function(x) x$params$get_data_args$eff_tox_or)
  N <- sapply(sims_batches, function(x) apply(sapply(
    x$sims, function(y) y$results$cohort_n), 1, mean))
  Eff <- sapply(sims_batches, function(x) apply(sapply(
    x$sims, function(y) y$results$cohort_eff), 1, mean))
  Tox <- sapply(sims_batches, function(x) apply(sapply(
    x$sims, function(y) y$results$cohort_tox), 1, mean))
  Perf <- sapply(sims_batches, function(x) beta_bin_perf(x, ...))
  
  if(is.null(scen_str)) {
    scenarios <- as.character(1:length(files))
  } else{ 
    scenarios <- scen_str 
  }
  scenarios <- as.vector(
    sapply(scenarios, function(x) c(x, rep('', num_cohorts - 1))))
  cohorts <- rep(1:num_cohorts, length(files))
  
  df <- data.frame(
    Scenario = scenarios,
    Cohort = cohorts,
    ProbEff = as.vector(prob_eff),
    ProbTox = as.vector(prob_tox),
    Odds = as.vector(odds),
    N = as.vector(N),
    Eff = as.vector(Eff),
    Tox = as.vector(Tox),
    ProbApprove = as.vector(Perf)
  )
  df
}