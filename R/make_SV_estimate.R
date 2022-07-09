#' Get SV estimate
#'
#' Wrapper for preprocessing ISVM data and getting a Stochastic Volatility estimate.
#'
#' @param DATA A file generated from the "make_ISVM_data" function
#'
#' @return v: The spot volatilities
#' @return sigma1: The data of the function gamma (see equation (20))
#' @return Sigma1: The nonparametric estimator of the function gamma
#' @return sigma22: The data of the function eta^2 (see equation (25))
#' @return Sigma22: The nonparametric estimator of the function eta^2
#' @return Sigma2: The nonparametric estimator of eta
#' @return mu: The data of the function mu (see equation (26))
#' @return Mu: The nonparametric estimator of the function mu
#' @return Rho: The nonparametric estimator of the leverage effect function rho (see equation (35))
#' @return dsigma1: The local linear estimator of the first order derivative of gamma (see equation (24))

#' @export
get_SV_estimate <- function(DATA){
  data <- data_crypto(DATA) # Data preprocessing

  n = data$n
  beta01 = data$beta01; beta02 = data$beta02; beta10 = data$beta10
  v = data$v; x_all = data$x_all; t_all = data$t_all; k_all = data$k_all
  r = data$r; d = data$d; numofIV = data$numofIV

  # Combine the proprocessed data
  dat = matrix(c(v, beta01, beta02, beta10, r, d), nrow = n)

  # Nonparametric estimation
  b = SVestimate(dat) # This plot corresponds to Figure 1 of the code guide file without the red dashed curves for bootstrap CI
  return(b)
}
