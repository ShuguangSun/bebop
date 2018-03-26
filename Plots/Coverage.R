
library(ggplot2)
library(dplyr)
library(jsonlite)

true_tox <- 0.1



# 411 model ----
sims1 <- jsonlite::fromJSON(
  readLines('411-params/uninf_priors/sims/scenario1.json'), 
  simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
coh_1 <- 6
prob_tox_l_1 <- sapply(sims1$sims, function(x) x$results[['ProbTox_5%']][coh_1])
prob_tox_m_1 <- sapply(sims1$sims, function(x) x$results[['ProbTox']][coh_1])
prob_tox_u_1 <- sapply(sims1$sims, function(x) x$results[['ProbTox_95%']][coh_1])

df1 <- data.frame(i = as.factor(1:length(prob_tox_l_1)),
                  ProbToxL = prob_tox_l_1,
                  ProbTox = prob_tox_m_1,
                  ProbToxU = prob_tox_u_1) %>% 
  mutate(InCI = ProbToxL < true_tox & ProbToxU > true_tox)

df1 %>% head(10)

ggplot(df1 %>% head(100), aes(y = i, col = InCI)) + 
  geom_point(aes(x = ProbTox)) + 
  geom_errorbarh(aes(xmin = ProbToxL, xmax = ProbToxU), height = 0) +
  geom_vline(xintercept = mean(prob_tox_m_1), col = 'orange', linetype = 'dashed') + 
  geom_vline(xintercept = true_tox, col = 'purple', linetype = 'dotted') + 
  xlim(0, 0.8) + 
  theme(legend.position = 'none', 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(title = 'Posterior Prob(Tox) under model 411 in Scenario 1', 
       y = 'Iteration') 



# 441 model ----
sims2 <- jsonlite::fromJSON(
  readLines('441-params/uninf_priors/sims/scenario1-n=60.json'), 
  simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
coh_2 <- 6
prob_tox_l_2 <- sapply(sims1$sims, function(x) x$results[['ProbTox_5%']][coh_2])
prob_tox_m_2 <- sapply(sims1$sims, function(x) x$results[['ProbTox']][coh_2])
prob_tox_u_2 <- sapply(sims1$sims, function(x) x$results[['ProbTox_95%']][coh_2])

df2 <- data.frame(i = as.factor(1:length(prob_tox_l_2)),
                  ProbToxL = prob_tox_l_2,
                  ProbTox = prob_tox_m_2,
                  ProbToxU = prob_tox_u_2) %>% 
  mutate(InCI = ProbToxL < true_tox & ProbToxU > true_tox)

df2 %>% head(10)

ggplot(df2 %>% head(100), aes(y = i, col = InCI)) + 
  geom_point(aes(x = ProbTox)) + 
  geom_errorbarh(aes(xmin = ProbToxL, xmax = ProbToxU), height = 0) +
  geom_vline(xintercept = mean(prob_tox_m_2), col = 'orange', linetype = 'dashed') + 
  geom_vline(xintercept = true_tox, col = 'purple', linetype = 'dotted') + 
  xlim(0, 0.8) + 
  theme(legend.position = 'none', 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(title = 'Posterior Prob(Tox) under model 441 in Scenario 1', 
       y = 'Iteration') 
