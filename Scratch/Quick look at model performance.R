source('CommonObjects.R')

# Comparing performance (and certainty parameters) across different models & priors ----



# 411 vs 441 models ----

# Scenario 1 
sims <- jsonlite::fromJSON(readLines('411-params/scept_priors/sims/scenario1.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.8962 0.9200 0.9093 0.9123 0.9086 0.8929
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.8920 0.9160 0.9051 0.9081 0.9047 0.8889


sims <- jsonlite::fromJSON(readLines('411-params/inf_priors/sims/scenario1.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.8866 0.9096 0.9831 0.8784 0.8766 0.9618
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.8833 0.9065 0.9796 0.8754 0.8734 0.9585


sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario1.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.749 0.797 0.744 0.813 0.787 0.704  # This the cost of three extra params in tox model
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.641 0.705 0.614 0.732 0.696 0.578  # Furthermore, extra tox_cert is even more damaging cf 411 model



# Scenario 2 
sims <- jsonlite::fromJSON(readLines('411-params/scept_priors/sims/scenario2.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.0248 0.0275 0.0295 0.0240 0.0235 0.0246
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.0118 0.0138 0.0145 0.0116 0.0111 0.0120


sims <- jsonlite::fromJSON(readLines('411-params/inf_priors/sims/scenario2.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.0262 0.0284 0.0804 0.0193 0.0180 0.0577 # That 8% is undesirable. Look at greater tox certainty
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.0118 0.0134 0.0381 0.0091 0.0092 0.0274


sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario2.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.029 0.029 0.041 0.014 0.020 0.030



# Scenario 3 
sims <- jsonlite::fromJSON(readLines('411-params/scept_priors/sims/scenario3.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.8973 0.9200 0.9095 0.9131 0.9084 0.8933
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.8933 0.9160 0.9053 0.9087 0.9042 0.8891


sims <- jsonlite::fromJSON(readLines('411-params/inf_priors/sims/scenario3.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.8870 0.9092 0.9844 0.8798 0.8765 0.9633
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.8844 0.9063 0.9810 0.8770 0.8736 0.9600


sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario3.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.769 0.802 0.759 0.831 0.802 0.711



# Scenario 4 
sims <- jsonlite::fromJSON(readLines('411-params/scept_priors/sims/scenario4.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.4507 0.6901 0.9814 0.2772 0.4934 0.9301
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.4483 0.6868 0.9767 0.2754 0.4910 0.9254


sims <- jsonlite::fromJSON(readLines('411-params/inf_priors/sims/scenario4.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.4099 0.6533 0.9963 0.2092 0.4065 0.9651
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.4083 0.6509 0.9925 0.2081 0.4050 0.9614


sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario4.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.413 0.587 0.806 0.255 0.406 0.747  # Again, the cost of the 441 model apparent
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.352 0.514 0.660 0.237 0.366 0.632  # and the secondary effect of high tox_cert




# Scenario 5 
sims <- jsonlite::fromJSON(readLines('411-params/scept_priors/sims/scenario5.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.0629 0.0988 0.1407 0.0372 0.0712 0.1354
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.0281 0.0439 0.0631 0.0163 0.0323 0.0614


sims <- jsonlite::fromJSON(readLines('411-params/inf_priors/sims/scenario5.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.0628 0.1008 0.1555 0.0320 0.0642 0.1516
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.0273 0.0457 0.0713 0.0142 0.0303 0.0703


sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario5.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.079 0.108 0.119 0.045 0.066 0.133
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.034 0.067 0.056 0.027 0.034 0.064


# Scenario 6 
sims <- jsonlite::fromJSON(readLines('411-params/scept_priors/sims/scenario6.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.4505 0.6892 0.9814 0.2781 0.4931 0.9294
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.4490 0.6864 0.9770 0.2771 0.4911 0.9251

sims <- jsonlite::fromJSON(readLines('411-params/inf_priors/sims/scenario6.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.4096 0.6535 0.9965 0.2087 0.4030 0.9659
prob_approve(sims, eff_certainty = 0.7, tox_certainty = 0.95)
# 0.4081 0.6515 0.9929 0.2080 0.4022 0.9623


sims <- jsonlite::fromJSON(readLines('441-params/scept_priors/sims/scenario6.json'),
                           simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                           simplifyMatrix = FALSE)
prob_approve(sims)
# 0.420 0.603 0.808 0.257 0.421 0.747



# 611 vs 411 models ----


# Scenario 1
sims_411_1_60 <- jsonlite::fromJSON(readLines('411-params/uninf_priors/sims/scenario1.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_411_1_60)
# 0.8778 0.9047 0.8163 0.8959 0.8900 0.8193
sims_411_1_60$params$executation_info$exec_s / (60^2) # 5.3 hours walltime

sims_611_1_60 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario1-n=60.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_1_60)
# 0.7543 0.8385 0.7161 0.8262 0.7972 0.6832 (n=10,000)
sims_611_1_60$params$executation_info$exec_s / (60^2) # 10.7 hours walltime

sims_611_1_70 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario1-n=70.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_1_70) 
# 0.7985 0.8728 0.7500 0.8646 0.8328 0.7147 (n=10,000)
sims_611_1_70$params$executation_info$exec_s / (60^2) # 12.7 hours walltime

sims_611_1_80 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario1-n=80.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_1_80) 
# 0.8294 0.9036 0.7883 0.8857 0.8588 0.7529 (n=10,000)
sims_611_1_80$params$executation_info$exec_s / (60^2) # 13.6 hours walltime

# sims_611_1_100 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario1-n=100.json'),
#                                     simplifyVector = TRUE, simplifyDataFrame = FALSE, 
#                                     simplifyMatrix = FALSE)
# prob_approve(sims_611_1_100) 
# #  (n=10,000)
# sims_611_1_100$params$executation_info$exec_s / (60^2) # . hours walltime



# Scenario 4
sims_411_4_60 <- jsonlite::fromJSON(readLines('411-params/uninf_priors/sims/scenario4.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_411_4_60)
# 0.3980 0.6333 0.9738 0.2146 0.4186 0.9307
sims_411_4_60$params$executation_info$exec_s / (60^2) # 5.1 hours walltime

sims_611_4_60 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario4-n=60.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_4_60)
# .4056 0.5605 0.9291 0.1747 0.4076 0.8632 (n=10,000)
sims_611_4_60$params$executation_info$exec_s / (60^2) # 9.3 hours walltime

sims_611_4_70 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario4-n=70.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_4_70)
# 0.4432 0.6001 0.9505 0.1821 0.4244 0.8903 (n=10,000)
sims_611_4_70$params$executation_info$exec_s / (60^2) # 18.9 hours walltime

sims_611_4_80 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario4-n=80.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_4_80)
# 0.4696 0.6388 0.9625 0.1756 0.4434 0.9182 (n=10,000)
sims_611_4_80$params$executation_info$exec_s / (60^2) # 20.2 hours walltime

sims_611_4_120 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario4-n=120.json'),
                                     simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                     simplifyMatrix = FALSE)
prob_approve(sims_611_4_120) 
# 0.5423 0.7313 0.9893 0.1881 0.5248 0.9648 (n=10,000)
sims_611_4_120$params$executation_info$exec_s / (60^2) # 14.1 hours walltime



# Scenario 5
sims_411_5_60 <- jsonlite::fromJSON(readLines('411-params/uninf_priors/sims/scenario5.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_411_5_60)
# 0.0392 0.0663 0.1019 0.0208 0.0446 0.0991
sims_411_5_60$params$executation_info$exec_s / (60^2) # 5.3 hours walltime

sims_611_5_60 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario5-n=60.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_5_60)
# 0.0410 0.0591 0.0986 0.0185 0.0436 0.0933
sims_611_5_60$params$executation_info$exec_s / (60^2) # 9.8 hours walltime

sims_611_5_70 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario5-n=70.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_5_70)
# 0.0509 0.0706 0.1114 0.0220 0.0484 0.1041
sims_611_5_70$params$executation_info$exec_s / (60^2) # 10.8 hours walltime

sims_611_5_80 <- jsonlite::fromJSON(readLines('611-params/uninf_priors/sims/scenario5-n=80.json'),
                                    simplifyVector = TRUE, simplifyDataFrame = FALSE, 
                                    simplifyMatrix = FALSE)
prob_approve(sims_611_5_80)
# 0.0547 0.0757 0.1165 0.0205 0.0533 0.1114
sims_611_5_80$params$executation_info$exec_s / (60^2) # 11.7 hours walltime

