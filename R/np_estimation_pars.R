#' Nonparametric ISVM estimation
#'
#' Getting a nonparametric Stochastic Volatility estimate.
#'
#' @import data.table
#' @import rmatio
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import np
#' @import latex2exp
#' @import parallel
#'
#' @param DT_ISVM A file generated from the "make_ISVM_data" function.
#' @param select_bandwidth Boolean value. Should the optimal bandwidth for the local polynomial kernel regression be selected automatically? If TRUE, bandwidth will be selected according to least-squares cross-validation. Defaults to FALSE.
#' @param make_plot Boolean value. Should the results be plotted? Defaults to TRUE.
#'
#' @return beta01: Vector of regression coefficient beta^{(0,1)} (see equation (36))
#' @return beta02: Vector of regression coefficient beta^{(0,2)} (see equation (36))
#' @return beta10: Vector of regression coefficient beta^{(1,0)} (see equation (36))
#' @return v: Vector of spot volatilities, i.e., beta^{(0,0)} in equation (36)
#' @return sigma1: The data of the function gamma (see equation (20))
#' @return Sigma1: The nonparametric estimator of the function gamma
#' @return sigma22: The data of the function eta^2 (see equation (25))
#' @return Sigma22: The nonparametric estimator of the function eta^2
#' @return Sigma2: The nonparametric estimator of eta
#' @return mu: The data of the function mu (see equation (26))
#' @return Mu: The nonparametric estimator of the function mu
#' @return Rho: The nonparametric estimator of the leverage effect function rho (see equation (35))
#' @return dsigma1: The local linear estimator of the first order derivative of gamma (see equation (24))
#' @return n: Sample size, i.e., the number of days
#' @return x_all: IV after data filteration
#' @return t_all: Time-to-maturities after data filteration
#' @return k_all: Log-moneyness after data filteration
#' @return r: Risk-free interest rates after data filteration
#' @return d: Dividend yields after data filteration
#' @return numofIV: Vector for recording the number of IV observations on each day

#' @export
np_estimation_pars <- function(DT_ISVM, select_bandwidth = F, make_plot = T){
  data <- data_crypto(DT_ISVM) # Data preprocessing
  # Nonparametric estimation
  if (make_plot) {
    DT_res <- SVestimate(data$v, data$beta01, data$beta02, data$beta10, data$r, data$d, select_bandwidth = select_bandwidth) # This plot corresponds to Figure 1 of the code guide file without the red dashed curves for bootstrap CI
  } else {
    DT_res <- SVestimate(data$v, data$beta01, data$beta02, data$beta10, data$r, data$d, select_bandwidth = select_bandwidth, make_plot = F)
  }
  return(list("DT_res" = DT_res,
              "data" = data,
              "n" = data$n,
              "beta01" = data$beta01,
              "beta02" = data$beta02,
              "beta10" = data$beta10,
              "x_all" = data$x_all,
              "t_all" = data$t_all,
              "k_all" = data$k_all,
              "r" = data$r,
              "d" = data$d,
              "model_fit" = data$model_fit,
              "numofIV" = data$numofIV))
}
