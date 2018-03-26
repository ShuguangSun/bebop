
get_priors <- function()
{
  priors <- list(
    alpha_mean = -2.2,
    alpha_sd = 2,
    beta_mean = -0.5,
    beta_sd = 2,
    gamma_mean = -0.5,
    gamma_sd = 2,
    zeta_mean = -0.5,
    zeta_sd = 2,
    lambda_mean = -2.2,
    lambda_sd = 2,
    psi_mean = 0,
    psi_sd = 1
  )
  return(priors)
}
