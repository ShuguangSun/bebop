


library(rstan)

# Compile model ----
model <- stan_model(file = '5params/PePS2.stan')

# Choose the save option for your system:

# Mac
save('model', file = '5params/model-mac.RData')

# Windows
save('model', file = '5params/model-win.RData')

# Ubuntu
save('model', file = '5params/model-ubu.RData')

# Or, of course, add your own...