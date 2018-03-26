
get_priors <- function()
{
  priors <- list(
    alpha_mean = 0,
    alpha_sd = 10,
    beta_mean = 0,
    beta_sd = 10,
    gamma_mean = 0,
    gamma_sd = 10,
    zeta_mean = 0,
    zeta_sd = 10,
    lambda_mean = 0,
    lambda_sd = 10,
    mu_mean = 0,
    mu_sd = 10,
    nu_mean = 0,
    nu_sd = 10,
    xi_mean = 0,
    xi_sd = 10,
    psi_mean = 0,
    psi_sd = 10
  )
  return(priors)
}
