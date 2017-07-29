
seed <- 123

# Common ----
library(gtools)
library(knitr)
library(xtable)

pi_e <- function(x1, x2, x3, alpha, beta, gamma, zeta) {
    z <- alpha + beta * x1 + gamma * x2 + zeta * x3
    return(inv.logit(z))
}

pi_t <- function(lambda) {
    return(inv.logit(lambda))
}

x = list(
  c(0, 1, 0),
  c(0, 0, 1),
  c(0, 0, 0),
  c(1, 1, 0),
  c(1, 0, 1),
  c(1, 0, 0)
)

n <- 10000


# Modestly informative (aka "paper") priors and Table 4 in main paper ----
set.seed(seed)
alpha_samp <- rnorm(n = n, mean = -2.2, sd = 2)
beta_samp <- rnorm(n = n, mean = -0.5, sd = 2)
gamma_samp <- rnorm(n = n, mean = -0.5, sd = 2)
zeta_samp <- rnorm(n = n, mean = -0.5, sd = 2)
lambda_samp <- rnorm(n = n, mean = -2.2, sd = 2)
psi_samp <- rnorm(n = n, mean = 0, sd = 1)

prob_eff <- lapply(x, function(y) pi_e(y[1], y[2], y[3], alpha_samp, beta_samp, 
                                       gamma_samp, zeta_samp))
prob_tox <- pi_t(lambda_samp)

plot(density(prob_eff[[1]]), main = 'Prior Prob(Eff) in Cohort 1', xlab = 'Prob(Eff)')
plot(density(prob_eff[[2]]), main = 'Prior Prob(Eff) in Cohort 2', xlab = 'Prob(Eff)')
plot(density(prob_eff[[3]]), main = 'Prior Prob(Eff) in Cohort 3', xlab = 'Prob(Eff)')
plot(density(prob_eff[[4]]), main = 'Prior Prob(Eff) in Cohort 4', xlab = 'Prob(Eff)')
plot(density(prob_eff[[5]]), main = 'Prior Prob(Eff) in Cohort 5', xlab = 'Prob(Eff)')
plot(density(prob_eff[[6]]), main = 'Prior Prob(Eff) in Cohort 6', xlab = 'Prob(Eff)')

paper_prior_beliefs = data.frame(
    Cohort = 1:6,
    
    ProbEffL = sapply(prob_eff, quantile, probs = 0.025),
    ProbEff = sapply(prob_eff, mean),
    ProbEffU = sapply(prob_eff, quantile, probs = 0.975),

    ProbToxL = quantile(prob_tox, probs = c(0.025)),
    ProbTox = mean(prob_tox),
    ProbToxU = quantile(prob_tox, probs = c(0.975))
)
round(paper_prior_beliefs, 2)
kable(paper_prior_beliefs, digits = c(0, 2, 2, 2, 2, 2, 2))
print(xtable(paper_prior_beliefs, digits = c(0, 0, 2, 2, 2, 2, 2, 2),
             caption = "", label = "tab:paper_priors"),
      hline.after = c(0), comment=F,
      include.rownames = FALSE)


# Uninformative priors and Table 3 in supplement ----
set.seed(seed)
alpha_samp <- rnorm(n = n, mean = 0, sd = 10)
beta_samp <- rnorm(n = n, mean = 0, sd = 10)
gamma_samp <- rnorm(n = n, mean = 0, sd = 10)
zeta_samp <- rnorm(n = n, mean = 0, sd = 10)
lambda_samp <- rnorm(n = n, mean = 0, sd = 10)
psi_samp <- rnorm(n = n, mean = 0, sd = 10)

prob_eff <- lapply(x, function(y) pi_e(y[1], y[2], y[3], alpha_samp, beta_samp, 
                                       gamma_samp, zeta_samp))
prob_tox <- pi_t(lambda_samp)

plot(density(prob_eff[[1]]), main = 'Prior Prob(Eff) in Cohort 1', xlab = 'Prob(Eff)')
plot(density(prob_eff[[2]]), main = 'Prior Prob(Eff) in Cohort 2', xlab = 'Prob(Eff)')
plot(density(prob_eff[[3]]), main = 'Prior Prob(Eff) in Cohort 3', xlab = 'Prob(Eff)')
plot(density(prob_eff[[4]]), main = 'Prior Prob(Eff) in Cohort 4', xlab = 'Prob(Eff)')
plot(density(prob_eff[[5]]), main = 'Prior Prob(Eff) in Cohort 5', xlab = 'Prob(Eff)')
plot(density(prob_eff[[6]]), main = 'Prior Prob(Eff) in Cohort 6', xlab = 'Prob(Eff)')

uninf_prior_beliefs = data.frame(
  Cohort = 1:6,
  
  ProbEffL = sapply(prob_eff, quantile, probs = 0.025),
  ProbEff = sapply(prob_eff, mean),
  ProbEffU = sapply(prob_eff, quantile, probs = 0.975),
  
  ProbToxL = quantile(prob_tox, probs = c(0.025)),
  ProbTox = mean(prob_tox),
  ProbToxU = quantile(prob_tox, probs = c(0.975))
)
round(uninf_prior_beliefs, 2)
kable(uninf_prior_beliefs, digits = c(0, 2, 2, 2, 2, 2, 2))
print(xtable(uninf_prior_beliefs, digits = c(0, 0, 2, 2, 2, 2, 2, 2),
             caption = "", label = "tab:uninf_priors"),
      hline.after = c(0), comment=F,
      include.rownames = FALSE)


# Informative priors and Table 7 in supplement ----
set.seed(seed)
alpha_samp <- rnorm(n = n, mean = 0, sd = 1.3)
beta_samp <- rnorm(n = n, mean = -1, sd = 1.3)
gamma_samp <- rnorm(n = n, mean = -2.75, sd = 1.3)
zeta_samp <- rnorm(n = n, mean = -2.2, sd = 1.3)
lambda_samp <- rnorm(n = n, mean = -2.2, sd = 2)
psi_samp <- rnorm(n = n, mean = 0, sd = 1)

prob_eff <- lapply(x, function(y) pi_e(y[1], y[2], y[3], alpha_samp, beta_samp, 
                                       gamma_samp, zeta_samp))
prob_tox <- pi_t(lambda_samp)

plot(density(prob_eff[[1]]), main = 'Prior Prob(Eff) in Cohort 1', xlab = 'Prob(Eff)')
plot(density(prob_eff[[2]]), main = 'Prior Prob(Eff) in Cohort 2', xlab = 'Prob(Eff)')
plot(density(prob_eff[[3]]), main = 'Prior Prob(Eff) in Cohort 3', xlab = 'Prob(Eff)')
plot(density(prob_eff[[4]]), main = 'Prior Prob(Eff) in Cohort 4', xlab = 'Prob(Eff)')
plot(density(prob_eff[[5]]), main = 'Prior Prob(Eff) in Cohort 5', xlab = 'Prob(Eff)')
plot(density(prob_eff[[6]]), main = 'Prior Prob(Eff) in Cohort 6', xlab = 'Prob(Eff)')

inf_prior_beliefs = data.frame(
  Cohort = 1:6,
  
  ProbEffL = sapply(prob_eff, quantile, probs = 0.025),
  ProbEff = sapply(prob_eff, mean),
  ProbEffU = sapply(prob_eff, quantile, probs = 0.975),
  
  ProbToxL = quantile(prob_tox, probs = c(0.025)),
  ProbTox = mean(prob_tox),
  ProbToxU = quantile(prob_tox, probs = c(0.975))
)
round(inf_prior_beliefs, 2)
kable(inf_prior_beliefs, digits = c(0, 2, 2, 2, 2, 2, 2))
print(xtable(inf_prior_beliefs, digits = c(0, 0, 2, 2, 2, 2, 2, 2),
             caption = "", label = "tab:inf_priors"),
      hline.after = c(0), comment=F,
      include.rownames = FALSE)
