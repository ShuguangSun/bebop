
get_priors <- function()
{
  priors <- list(
    alpha_mean = -0.3,
    alpha_sd = 2,
    beta_mean = -0.7,
    beta_sd = 2,
    gamma_mean = -2.0,
    gamma_sd = 2,
    zeta_mean = -2.0,
    zeta_sd = 2,
    lambda_mean = -2.2,
    lambda_sd = 1.7,
    psi_mean = 0,
    psi_sd = 1
  )
  return(priors)
}
