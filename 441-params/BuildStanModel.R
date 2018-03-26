
library(rstan)
sysname <- unname(Sys.info()['sysname'])

model_dir = '441-params/'


# Compile model ----
model <- stan_model(file = paste0(model_dir, 'PePS2.stan'))
# Save:
save('model', file = paste0(model_dir, 'model-', sysname, '.RData'))


# Or, of course, add your own... e.g.
# save('model', file = paste0(model_dir, 'model-ubu.RData'))
