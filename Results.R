
library(tidyverse)
library(knitr)
library(xtable)


source('CommonObjects.R')



# Parametising pE and pT ----
tab1 <- stack_sims(files = c('411-params/scept_priors/sims/scenario1.json',
                             '411-params/scept_priors/sims/scenario2.json'), 
                   eff_certainty = 0.7, tox_certainty = 0.7)
tab2 <- stack_sims(files = c('411-params/scept_priors/sims/scenario1.json',
                             '411-params/scept_priors/sims/scenario2.json'), 
                   eff_certainty = 0.7, tox_certainty = 0.8)
tab3 <- stack_sims(files = c('411-params/scept_priors/sims/scenario1.json',
                             '411-params/scept_priors/sims/scenario2.json'), 
                   eff_certainty = 0.7, tox_certainty = 0.9)

sc_labs <- c('Favourable', 'Adverse')
df1 <- data.frame(Parameters = c('$p_E=0.7, p_T=0.7$', ''), 
                  Scenario = sc_labs, 
                  matrix(tab1$ProbApprove, nrow = 2, byrow = T))
colnames(df1)[3:8] <- 1:6

df2 <- data.frame(Parameters = c('$p_E=0.7, p_T=0.8$', ''), 
                  Scenario = sc_labs, 
                  matrix(tab2$ProbApprove, nrow = 2, byrow = T))
colnames(df2)[3:8] <- 1:6

df3 <- data.frame(Parameters = c('$p_E=0.7, p_T=0.9$', ''), 
                  Scenario = sc_labs, 
                  matrix(tab3$ProbApprove, nrow = 2, byrow = T))
colnames(df3)[3:8] <- 1:6

print(xtable(rbind(df1, df2, df3)), include.rownames = FALSE)


# P2TNE OCs with 411 model ----

# Sceptical priors
ocs_411_scept <- stack_sims(
  files = c('411-params/scept_priors/sims/scenario1.json',
            '411-params/scept_priors/sims/scenario2.json',
            '411-params/scept_priors/sims/scenario3.json',
            '411-params/scept_priors/sims/scenario4.json',
            '411-params/scept_priors/sims/scenario5.json',
            '411-params/scept_priors/sims/scenario6.json'
            )
)
kable(ocs_411_scept, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3))

ocs_411_scept2 <- stack_sims(
  files = c('411-params/scept_priors/sims/scenario1.json',
            '411-params/scept_priors/sims/scenario2.json',
            '411-params/scept_priors/sims/scenario3.json',
            '411-params/scept_priors/sims/scenario4.json',
            '411-params/scept_priors/sims/scenario5.json',
            '411-params/scept_priors/sims/scenario6.json'
  ), include_eff_coverage = T, include_tox_coverage = T
)
kable(ocs_411_scept2, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))


# Informative priors
ocs_411_inf <- stack_sims(
  files = c('411-params/inf_priors/sims/scenario1.json',
            '411-params/inf_priors/sims/scenario2.json',
            '411-params/inf_priors/sims/scenario3.json',
            '411-params/inf_priors/sims/scenario4.json',
            '411-params/inf_priors/sims/scenario5.json',
            '411-params/inf_priors/sims/scenario6.json'
  ), eff_certainty = 0.7, tox_certainty = 0.95
)
kable(ocs_411_inf, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3))

ocs_411_inf2 <- stack_sims(
  files = c('411-params/inf_priors/sims/scenario1.json',
            '411-params/inf_priors/sims/scenario2.json',
            '411-params/inf_priors/sims/scenario3.json',
            '411-params/inf_priors/sims/scenario4.json',
            '411-params/inf_priors/sims/scenario5.json',
            '411-params/inf_priors/sims/scenario6.json'
  ), eff_certainty = 0.7, tox_certainty = 0.95,
  include_eff_coverage = T, include_tox_coverage = T
)
kable(ocs_411_inf2, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))


# Uninformative priors
ocs_411_uninf <- stack_sims(
  files = c('411-params/uninf_priors/sims/scenario1.json',
            '411-params/uninf_priors/sims/scenario2.json',
            '411-params/uninf_priors/sims/scenario3.json',
            '411-params/uninf_priors/sims/scenario4.json',
            '411-params/uninf_priors/sims/scenario5.json',
            '411-params/uninf_priors/sims/scenario6.json'
  )
)
kable(ocs_411_uninf, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3))

ocs_411_uninf2 <- stack_sims(
  files = c(
    '411-params/uninf_priors/sims/scenario1.json',
    '411-params/uninf_priors/sims/scenario2.json',
    '411-params/uninf_priors/sims/scenario3.json',
    '411-params/uninf_priors/sims/scenario4.json',
    '411-params/uninf_priors/sims/scenario5.json',
    '411-params/uninf_priors/sims/scenario6.json'
  ), include_eff_coverage = T, include_tox_coverage = T
)
kable(ocs_411_uninf2, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))


# Bias appears to be less with inf priors...
mean(abs(ocs_411_scept2$EffCov - 0.9))  # 2.5%
mean(abs(ocs_411_uninf2$EffCov - 0.9))  # 6.0%
mean(abs(ocs_411_inf2$EffCov - 0.9))    # 3.2%
# Uninformative priors lead to most bias.


ocs_table <- ocs_411_inf %>% 
  select(Scenario, Cohort, ProbEff, ProbTox, Odds, N, Eff, Tox, ProbApprove) %>% 
  rename(Sc = Scenario, PrEff = ProbEff, PrTox = ProbTox, 'Inf' = ProbApprove) %>% 
  cbind(ocs_411_scept %>% select(Scept = ProbApprove)) %>% 
  cbind(ocs_411_uninf %>% select(Uninf = ProbApprove)) 
ocs_table
print(xtable(ocs_table, 
             digits = c(0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3),
             caption = "", label = "tab:bebop:ocs"),
      hline.after = c(0, 6, 12, 18, 24, 30), comment = F,
      include.rownames = FALSE)


# OCs of beta-binomial models in individual cohorts, c.f. 411 model ----
# With 'modestly informative' beta priors
ocs_beta_binom <- stack_sims_beta_binom(
  files = c('411-params/scept_priors/sims/scenario1.json',
            '411-params/scept_priors/sims/scenario2.json',
            '411-params/scept_priors/sims/scenario3.json',
            '411-params/scept_priors/sims/scenario4.json',
            '411-params/scept_priors/sims/scenario5.json',
            '411-params/scept_priors/sims/scenario6.json'
  ),
  eff_prior_alpha = 0.4, eff_prior_beta = 1.6,
  tox_prior_alpha = 0.4, tox_prior_beta = 1.6
)
kable(ocs_beta_binom, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3))

# With uninformative beta priors
ocs_beta_binom_uninf <- stack_sims_beta_binom(
  files = c('411-params/scept_priors/sims/scenario1.json',
            '411-params/scept_priors/sims/scenario2.json',
            '411-params/scept_priors/sims/scenario3.json',
            '411-params/scept_priors/sims/scenario4.json',
            '411-params/scept_priors/sims/scenario5.json',
            '411-params/scept_priors/sims/scenario6.json'
  ), 
  eff_prior_alpha = 0.001, eff_prior_beta = 0.001, 
  tox_prior_alpha = 0.001, tox_prior_beta = 0.001
)
kable(ocs_beta_binom_uninf, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3))

ocs_beta_binom %>% 
  rename(Sc = Scenario, PrEff = ProbEff, PrTox = ProbTox, 'Inf' = ProbApprove) %>% 
  cbind(ocs_beta_binom_uninf %>% select(Uninf = ProbApprove))

ocs_table <- ocs_411_proposed %>% 
  rename(Sc = Scenario, Coh = Cohort, PrEff = ProbEff, PrTox = ProbTox, 'Inf' = ProbApprove) %>% 
  cbind(ocs_411_paper %>% select(Scept = ProbApprove)) %>% 
  cbind(ocs_411_uninf %>% select(Uninf = ProbApprove)) %>% 
  cbind(ocs_beta_binom_uninf %>% select(BetaBin = ProbApprove)) 
kable(ocs_table, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3, 3))
print(xtable(ocs_table, 
             digits = c(0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3, 3),
             caption = "", label = "tab:bebop:ocs"),
      hline.after = c(0, 6, 12, 18, 24, 30), comment = F,
      include.rownames = FALSE)






# P2TNE OCs with 611 model ----

ocs_611_uninf <- stack_sims(
  files = c('611-params/uninf_priors/sims/scenario1-n=60.json',
            '611-params/uninf_priors/sims/scenario1-n=70.json',
            '611-params/uninf_priors/sims/scenario1-n=80.json',
            '611-params/uninf_priors/sims/scenario1-n=90.json',
            '611-params/uninf_priors/sims/scenario1-n=100.json',
            '611-params/uninf_priors/sims/scenario4-n=60.json',
            '611-params/uninf_priors/sims/scenario4-n=70.json',
            '611-params/uninf_priors/sims/scenario4-n=80.json',
            '611-params/uninf_priors/sims/scenario4-n=120.json',
            '611-params/uninf_priors/sims/scenario5-n=60.json',
            '611-params/uninf_priors/sims/scenario5-n=70.json',
            '611-params/uninf_priors/sims/scenario5-n=80.json',
            '611-params/uninf_priors/sims/scenario5-n=120.json'
  ), 
  scenario_labels = c(1, 1, 1, 1, 1, 4, 4, 4, 4, 5, 5, 5, 5),
  include_num_sims = TRUE, include_total_num_patients = TRUE, 
  include_eff_coverage = TRUE, include_tox_coverage = TRUE
)
kable(ocs_611_uninf, digits = c(0, 0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))



ocs_611_uninf_present <- stack_sims(
  files = c('611-params/uninf_priors/sims/scenario1-n=60.json',
            '611-params/uninf_priors/sims/scenario1-n=80.json',
            '611-params/uninf_priors/sims/scenario1-n=100.json',
            '611-params/uninf_priors/sims/scenario4-n=60.json',
            '611-params/uninf_priors/sims/scenario4-n=80.json',
            '611-params/uninf_priors/sims/scenario4-n=180.json',
            '611-params/uninf_priors/sims/scenario5-n=60.json',
            '611-params/uninf_priors/sims/scenario5-n=80.json',
            '611-params/uninf_priors/sims/scenario5-n=120.json'
            #'611-params/uninf_priors/sims/scenario5-n=180.json'
  ), 
  scenario_labels = c(1, 1, 1, 4, 4, 4, 5, 5, 5),
  include_num_sims = TRUE, 
  include_total_num_patients = TRUE, 
  include_eff_coverage = TRUE, include_tox_coverage = TRUE
)

kable(ocs_611_uninf_present %>% select(-NumSims), 
      digits = c(0, 0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))

ocs_611_uninf_present %>% select(-NumSims, -Odds) %>% 
  rename(Sc = Scenario, TotN = TotalN, Coh = Cohort, 
         PrEff = ProbEff, PrTox = ProbTox, Uninf = ProbApprove) %>% 
  xtable(digits = c(0, 0, 0, 0, 3, 1, 1, 1, 1, 3, 3, 3),
         caption = "", label = "tab:bebopextra:611:ocs") %>% 
  print(hline.after = c(0, 6, 12, 18, 18, 24, 30, 36, 36, 42, 48), comment = F,
        include.rownames = FALSE)







# P2TNE OCs with 441 model ----
ocs_441_scept <- stack_sims(
  files = c('441-params/scept_priors/sims/scenario1-n=60.json',
            '441-params/scept_priors/sims/scenario1-n=80.json',
            '441-params/scept_priors/sims/scenario1-n=100.json',
            '441-params/scept_priors/sims/scenario1-n=110.json',
            
            '441-params/scept_priors/sims/scenario2-n=60.json',
            '441-params/scept_priors/sims/scenario2-n=80.json',
            '441-params/scept_priors/sims/scenario2-n=100.json',
            '441-params/scept_priors/sims/scenario2-n=110.json',
            
            '441-params/scept_priors/sims/scenario3-n=60.json',
            '441-params/scept_priors/sims/scenario3-n=80.json',
            '441-params/scept_priors/sims/scenario3-n=100.json',
            '441-params/scept_priors/sims/scenario3-n=110.json',
            
            '441-params/scept_priors/sims/scenario4-n=60.json',
            '441-params/scept_priors/sims/scenario4-n=80.json',
            '441-params/scept_priors/sims/scenario4-n=100.json',
            '441-params/scept_priors/sims/scenario4-n=110.json',
            
            '441-params/scept_priors/sims/scenario5-n=60.json',
            '441-params/scept_priors/sims/scenario5-n=80.json',
            '441-params/scept_priors/sims/scenario5-n=100.json',
            '441-params/scept_priors/sims/scenario5-n=110.json',
            
            '441-params/scept_priors/sims/scenario6-n=60.json',
            '441-params/scept_priors/sims/scenario6-n=80.json',
            '441-params/scept_priors/sims/scenario6-n=100.json',
            '441-params/scept_priors/sims/scenario6-n=110.json',
            
            '441-params/scept_priors/sims/scenario7-n=60.json',
            '441-params/scept_priors/sims/scenario7-n=80.json',
            '441-params/scept_priors/sims/scenario7-n=100.json',
            '441-params/scept_priors/sims/scenario7-n=110.json'
  ), 
  scenario_labels = rep(1:7, each = 4), 
  include_num_sims = TRUE, include_total_num_patients = TRUE,
  include_eff_coverage = TRUE, include_tox_coverage = TRUE
)
kable(ocs_441_scept, digits = c(0, 0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))


ocs_441_uninf <- stack_sims(
  files = c('441-params/uninf_priors/sims/scenario1-n=60.json',
            # '441-params/uninf_priors/sims/scenario1-n=90.json',
            '441-params/uninf_priors/sims/scenario1-n=100.json',
            # '441-params/uninf_priors/sims/scenario1-n=110.json',
            
            # '441-params/uninf_priors/sims/scenario2-n=60.json',
            # '441-params/uninf_priors/sims/scenario2-n=90.json',
            '441-params/uninf_priors/sims/scenario2-n=100.json',
            # '441-params/scept_priors/sims/scenario2-n=110.json',
            # 
            # '441-params/uninf_priors/sims/scenario3-n=60.json',
            # '441-params/uninf_priors/sims/scenario3-n=90.json',
            # '441-params/uninf_priors/sims/scenario3-n=100.json',
            # '441-params/uninf_priors/sims/scenario3-n=110.json',
            
            '441-params/uninf_priors/sims/scenario4-n=60.json',
            # '441-params/uninf_priors/sims/scenario4-n=90.json',
            # '441-params/uninf_priors/sims/scenario4-n=100.json',
            '441-params/uninf_priors/sims/scenario4-n=110.json',
            
            '441-params/uninf_priors/sims/scenario5-n=60.json',
            # '441-params/uninf_priors/sims/scenario5-n=90.json',
            # '441-params/uninf_priors/sims/scenario5-n=100.json',
            '441-params/uninf_priors/sims/scenario5-n=110.json',
            
            # '441-params/uninf_priors/sims/scenario6-n=60.json',
            # '441-params/uninf_priors/sims/scenario6-n=90.json',
            # '441-params/uninf_priors/sims/scenario6-n=100.json',
            # '441-params/uninf_priors/sims/scenario6-n=110.json',
            
            '441-params/uninf_priors/sims/scenario7-n=60.json',
            # '441-params/uninf_priors/sims/scenario7-n=90.json',
            # '441-params/uninf_priors/sims/scenario7-n=100.json',
            '441-params/uninf_priors/sims/scenario7-n=110.json'
  ), 
  scenario_labels = c(1, 1, 2, 4, 4, 5, 5, 7, 7), 
  include_num_sims = TRUE, include_total_num_patients = TRUE,
  include_eff_coverage = TRUE, include_tox_coverage = TRUE
)
kable(ocs_441_uninf, digits = c(0, 0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))


ocs_441_uninf %>% select(-NumSims, -Odds) %>% 
  rename(Sc = Scenario, TotN = TotalN, Coh = Cohort,
         PrEff = ProbEff, PrTox = ProbTox, Uninf = ProbApprove) %>%
  xtable(digits = c(0, 0, 0, 0, 3, 1, 1, 1, 1, 3, 3, 3),
         caption = "", label = "tab:bebopextra:441:ocs") %>%
  print(hline.after = c(0, 6, 12, 12, 18, 18, 24, 30, 30, 36, 42, 42, 48), 
        comment = F, include.rownames = FALSE)




# 
# ocs_441_uninf_n60 <- stack_sims(
#   files = c('441-params/uninf_priors/sims/scenario1-n=60.json',
#             '441-params/uninf_priors/sims/scenario2-n=60.json',
#             '441-params/uninf_priors/sims/scenario3-n=60.json',
#             '441-params/uninf_priors/sims/scenario4-n=60.json',
#             '441-params/uninf_priors/sims/scenario5-n=60.json',
#             '441-params/uninf_priors/sims/scenario6-n=60.json'#,
#             # '441-params/uninf_priors/sims/scenario7-n=60.json'
#   ), 
#   include_num_sims = TRUE, include_total_num_patients = TRUE,
#   include_eff_coverage = TRUE, include_tox_coverage = TRUE
# )
# kable(ocs_441_uninf_n60, digits = c(0, 0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))
# 
# ocs_441_uninf_n90 <- stack_sims(
#   files = c('441-params/uninf_priors/sims/scenario1-n=90.json',
#             '441-params/uninf_priors/sims/scenario2-n=90.json',
#             '441-params/uninf_priors/sims/scenario3-n=90.json',
#             '441-params/uninf_priors/sims/scenario4-n=90.json',
#             '441-params/uninf_priors/sims/scenario5-n=90.json',
#             '441-params/uninf_priors/sims/scenario6-n=90.json'#,
#             # '441-params/uninf_priors/sims/scenario7-n=90.json'
#   ), include_eff_coverage = TRUE, include_tox_coverage = TRUE
# )
# kable(ocs_441_uninf_n90, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))
# 
# ocs_441_uninf_n100 <- stack_sims(
#   files = c('441-params/uninf_priors/sims/scenario1-n=100.json',
#             '441-params/uninf_priors/sims/scenario2-n=100.json',
#             '441-params/uninf_priors/sims/scenario3-n=100.json',
#             '441-params/uninf_priors/sims/scenario4-n=100.json',
#             '441-params/uninf_priors/sims/scenario5-n=100.json',
#             '441-params/uninf_priors/sims/scenario6-n=100.json'#,
#             # '441-params/uninf_priors/sims/scenario7-n=100.json'
#   ), include_eff_coverage = TRUE, include_tox_coverage = TRUE
# )
# kable(ocs_441_uninf_n100, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))
# 
# ocs_441_uninf_n110 <- stack_sims(
#   files = c('441-params/uninf_priors/sims/scenario1-n=110.json',
#             '441-params/uninf_priors/sims/scenario2-n=110.json',
#             '441-params/uninf_priors/sims/scenario3-n=110.json',
#             '441-params/uninf_priors/sims/scenario4-n=110.json',
#             '441-params/uninf_priors/sims/scenario5-n=110.json',
#             '441-params/uninf_priors/sims/scenario6-n=110.json'#,
#             # '441-params/uninf_priors/sims/scenario7-n=110.json'
#   ), include_eff_coverage = TRUE, include_tox_coverage = TRUE
# )
# kable(ocs_441_uninf_n110, digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))
# 
# mean(abs(ocs_441_uninf_n60$EffCov - 0.9))  # 5.7%
# mean(abs(ocs_441_uninf_n90$EffCov - 0.9))  # 4.6%
# mean(abs(ocs_441_uninf_n100$EffCov - 0.9)) # 3.6%
# mean(abs(ocs_441_uninf_n110$EffCov - 0.9)) # 3.2%
# # Bias reduces with n, as expected


# P2TNE OCs with 410 model ----
# Uninformative priors
ocs_410_uninf <- stack_sims(
  files = c('410-params/uninf_priors/sims/scenario1.json',
            '410-params/uninf_priors/sims/scenario2.json',
            '410-params/uninf_priors/sims/scenario3.json',
            '410-params/uninf_priors/sims/scenario4.json',
            '410-params/uninf_priors/sims/scenario5.json',
            '410-params/uninf_priors/sims/scenario6.json'
  ), include_num_sims = T, include_eff_coverage = T, include_tox_coverage = T
)
kable(ocs_410_uninf %>% select(-NumSims), 
      digits = c(0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3))
ocs_410_uninf %>% select(-NumSims) %>% 
  rename(Sc = Scenario, Coh = Cohort,
         PrEff = ProbEff, PrTox = ProbTox, Uninf = ProbApprove) %>%
  xtable(digits = c(0, 0, 0, 3, 1, 1, 1, 1, 1, 3, 3, 3),
         caption = "", label = "tab:bebopextra:410:ocs") %>% 
  print(hline.after = c(0, 6, 12, 18), comment = F, include.rownames = FALSE)
