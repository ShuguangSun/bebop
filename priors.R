
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

n = 10 ^ 5


# Sceptical priors ----
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

# plot(density(prob_eff[[1]]), main = 'Prior Prob(Eff) in Cohort 1', xlab = 'Prob(Eff)')

sceptical_prior_beliefs = data.frame(
    Cohort = 1:6,
    ProbEffL = sapply(prob_eff, quantile, probs = 0.05),
    ProbEffl = sapply(prob_eff, quantile, probs = 0.25),
    ProbEff = sapply(prob_eff, mean),
    ProbEffu = sapply(prob_eff, quantile, probs = 0.75),
    ProbEffU = sapply(prob_eff, quantile, probs = 0.95),

    ProbToxL = quantile(prob_tox, probs = c(0.05)),
    ProbToxl = quantile(prob_tox, probs = c(0.25)),
    ProbTox = mean(prob_tox),
    ProbToxu = quantile(prob_tox, probs = c(0.75)),
    ProbToxU = quantile(prob_tox, probs = c(0.95))
)

round(sceptical_prior_beliefs, 2)
kable(sceptical_prior_beliefs, digits = c(0, 2, 2, 2, 2, 2, 2))


sceptical_prior_beliefs_e = data.frame(
  Cohort = 1:6,
  ProbEffL = sapply(prob_eff, quantile, probs = 0.05),
  ProbEffl = sapply(prob_eff, quantile, probs = 0.25),
  ProbEff = sapply(prob_eff, mean),
  ProbEffu = sapply(prob_eff, quantile, probs = 0.75),
  ProbEffU = sapply(prob_eff, quantile, probs = 0.95)
)

sceptical_prior_beliefs_t = data.frame(
  Cohort = 1:6,
  ProbToxL = quantile(prob_tox, probs = c(0.05)),
  ProbToxl = quantile(prob_tox, probs = c(0.25)),
  ProbTox = mean(prob_tox),
  ProbToxu = quantile(prob_tox, probs = c(0.75)),
  ProbToxU = quantile(prob_tox, probs = c(0.95))
)
# print(xtable(paper_prior_beliefs, digits = c(0, 0, rep(2, 10)),
#              caption = "", label = "tab:paper_priors"),
#       hline.after = c(0), comment=F,
#       include.rownames = FALSE)
print(xtable(sceptical_prior_beliefs_e, digits = c(0, 0, rep(2, 5)),
             caption = "", label = "tab:bebop:paper_priors_ci_e"),
      hline.after = c(0), comment=F,
      include.rownames = FALSE)
print(xtable(sceptical_prior_beliefs_t, digits = c(0, 0, rep(2, 5)),
             caption = "", label = "tab:bebop:paper_priors_ci_t"),
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

uninf_prior_beliefs = data.frame(
  Cohort = 1:6,
  ProbEffL = sapply(prob_eff, quantile, probs = 0.05),
  ProbEffl = sapply(prob_eff, quantile, probs = 0.25),
  ProbEff = sapply(prob_eff, mean),
  ProbEffu = sapply(prob_eff, quantile, probs = 0.75),
  ProbEffU = sapply(prob_eff, quantile, probs = 0.95),
  
  ProbToxL = quantile(prob_tox, probs = c(0.05)),
  ProbToxl = quantile(prob_tox, probs = c(0.25)),
  ProbTox = mean(prob_tox),
  ProbToxu = quantile(prob_tox, probs = c(0.75)),
  ProbToxU = quantile(prob_tox, probs = c(0.95))
)

kable(uninf_prior_beliefs, digits = c(0, rep(2, ncol(uninf_prior_beliefs))))
kable(uninf_prior_beliefs, digits = c(0, rep(3, ncol(uninf_prior_beliefs))))
print(xtable(uninf_prior_beliefs, digits = c(0, rep(3, ncol(uninf_prior_beliefs))),
             caption = "", label = "tab:bebop:uninf_beliefs"),
      hline.after = c(0), comment = F,
      include.rownames = FALSE)

# Split by eff and tox
uninf_prior_beliefs_e = data.frame(
  Cohort = 1:6,
  ProbEffL = sapply(prob_eff, quantile, probs = 0.05),
  ProbEffl = sapply(prob_eff, quantile, probs = 0.25),
  ProbEff = sapply(prob_eff, mean),
  ProbEffu = sapply(prob_eff, quantile, probs = 0.75),
  ProbEffU = sapply(prob_eff, quantile, probs = 0.95)
)

uninf_prior_beliefs_t = data.frame(
  Cohort = 1:6,
  ProbToxL = quantile(prob_tox, probs = c(0.05)),
  ProbToxl = quantile(prob_tox, probs = c(0.25)),
  ProbTox = mean(prob_tox),
  ProbToxu = quantile(prob_tox, probs = c(0.75)),
  ProbToxU = quantile(prob_tox, probs = c(0.95))
)
print(xtable(uninf_prior_beliefs_e, digits = c(0, 0, rep(3, 5)),
             caption = "", label = "tab:bebop:uninf_priors_ci_e"),
      hline.after = c(0), comment=F,
      include.rownames = FALSE)
print(xtable(uninf_prior_beliefs_t, digits = c(0, 0, rep(3, 5)),
             caption = "", label = "tab:bebop:uninf_priors_ci_t"),
      hline.after = c(0), comment=F,
      include.rownames = FALSE)




# Proposed informative priors ----
# PFS in Garon et al. is (0.30, 0.51, 0.75) in TN and (0.36, 0.36, 0.55) in PT
set.seed(seed)
alpha_samp <- rnorm(n = n, mean = -0.3, sd = 2)
beta_samp <- rnorm(n = n, mean = -0.7, sd = 2)
gamma_samp <- rnorm(n = n, mean = -2.0, sd = 2)
zeta_samp <- rnorm(n = n, mean = -2.0, sd = 2)
lambda_samp <- rnorm(n = n, mean = -2.2, sd = 1.7)
psi_samp <- rnorm(n = n, mean = 0, sd = 1)
prob_eff <- lapply(x, function(y) pi_e(y[1], y[2], y[3], alpha_samp, beta_samp,
                                       gamma_samp, zeta_samp))
prob_tox <- pi_t(lambda_samp)
prop_prior_beliefs = data.frame(
    Cohort = 1:6,
    ProbEffL = sapply(prob_eff, quantile, probs = 0.05),
    ProbEffl = sapply(prob_eff, quantile, probs = 0.25),
    ProbEff = sapply(prob_eff, mean),
    ProbEffu = sapply(prob_eff, quantile, probs = 0.75),
    ProbEffU = sapply(prob_eff, quantile, probs = 0.95),

    ProbToxL = quantile(prob_tox, probs = c(0.05)),
    ProbToxl = quantile(prob_tox, probs = c(0.25)),
    ProbTox = mean(prob_tox),
    ProbToxu = quantile(prob_tox, probs = c(0.75)),
    ProbToxU = quantile(prob_tox, probs = c(0.95))
)
kable(prop_prior_beliefs, digits = c(0, rep(2, ncol(prop_prior_beliefs))))

# All together
print(xtable(prop_prior_beliefs, digits = c(0, rep(2, ncol(prop_prior_beliefs))),
             caption = "", label = "tab:bebop:proposed_beliefs"),
      hline.after = c(0), comment = F,
      include.rownames = FALSE)

# Efficacy and toxicity separate
library(dplyr)
print(xtable(prop_prior_beliefs %>% select(Cohort, ProbEffL, ProbEffl, ProbEff, ProbEffu, ProbEffU), 
             digits = c(0, rep(2, 6)),
             caption = "", label = "tab:bebop:proposed_beliefs"),
      hline.after = c(0), comment = F,
      include.rownames = FALSE)
print(xtable(prop_prior_beliefs %>% select(Cohort, ProbToxL, ProbToxl, ProbTox, ProbToxu, ProbToxU), 
             digits = c(0, rep(2, 6))),
      hline.after = c(0), comment = F,
      include.rownames = FALSE)



# # Scratch - Informative priors and Table 7 in supplement ----
# set.seed(seed)
# alpha_samp <- rnorm(n = n, mean = 0, sd = 1.3)
# beta_samp <- rnorm(n = n, mean = -1, sd = 1.3)
# gamma_samp <- rnorm(n = n, mean = -2.75, sd = 1.3)
# zeta_samp <- rnorm(n = n, mean = -2.2, sd = 1.3)
# lambda_samp <- rnorm(n = n, mean = -2.2, sd = 2)
# psi_samp <- rnorm(n = n, mean = 0, sd = 1)
# 
# prob_eff <- lapply(x, function(y) pi_e(y[1], y[2], y[3], alpha_samp, beta_samp, 
#                                        gamma_samp, zeta_samp))
# prob_tox <- pi_t(lambda_samp)
# 
# # plot(density(prob_eff[[1]]), main = 'Prior Prob(Eff) in Cohort 1', xlab = 'Prob(Eff)')
# # plot(density(prob_eff[[2]]), main = 'Prior Prob(Eff) in Cohort 2', xlab = 'Prob(Eff)')
# # plot(density(prob_eff[[3]]), main = 'Prior Prob(Eff) in Cohort 3', xlab = 'Prob(Eff)')
# # plot(density(prob_eff[[4]]), main = 'Prior Prob(Eff) in Cohort 4', xlab = 'Prob(Eff)')
# # plot(density(prob_eff[[5]]), main = 'Prior Prob(Eff) in Cohort 5', xlab = 'Prob(Eff)')
# # plot(density(prob_eff[[6]]), main = 'Prior Prob(Eff) in Cohort 6', xlab = 'Prob(Eff)')
# 
# inf_prior_beliefs = data.frame(
#   Cohort = 1:6,
#   
#   ProbEffL = sapply(prob_eff, quantile, probs = 0.025),
#   ProbEff = sapply(prob_eff, mean),
#   ProbEffU = sapply(prob_eff, quantile, probs = 0.975),
#   
#   ProbToxL = quantile(prob_tox, probs = c(0.025)),
#   ProbTox = mean(prob_tox),
#   ProbToxU = quantile(prob_tox, probs = c(0.975))
# )
# round(inf_prior_beliefs, 2)
# kable(inf_prior_beliefs, digits = c(0, 2, 2, 2, 2, 2, 2))
# print(xtable(inf_prior_beliefs, digits = c(0, 0, 2, 2, 2, 2, 2, 2),
#              caption = "", label = "tab:inf_priors"),
#       hline.after = c(0), comment=F,
#       include.rownames = FALSE)
# 
