#' SV estimate
#'
#' Getting a Stochastic Volatility estimate.
#'
#' @import data.table
#' @import rmatio
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import np
#'
#' @param v Vector of spot volatilities, i.e., beta^{(0,0)} in equation (36)
#' @param beta01 Vector of regression coefficient beta^{(0,1)} (see equation (36))
#' @param beta02 Vector of regression coefficient beta^{(0,2)} (see equation (36))
#' @param beta10 Vector of regression coefficient beta^{(1,0)} (see equation (36))
#' @param r Risk-free interest rates.
#' @param d Dividend yields.
#' @param date Character value. Estimation date for plot title. Optional.
#' @param bw1 Numeric value for bandwidth of Gamma function estimate in case "select_bandwith" = FALSE. Defaults to 0.15.
#' @param bw2 Numeric value for bandwidth of Eta^2 function estimate in case "select_bandwith" = FALSE. Defaults to 0.16.
#' @param bw3 Numeric value for bandwidth of Mu function estimate in case "select_bandwith" = FALSE. Defaults to 0.15.
#' @param select_bandwidth Boolean value. Should the optimal bandwidth for the local polynomial kernel regression be selected automatically? If TRUE, bandwidth will be selected according to least-squares cross-validation. Defaults to FALSE.
#' @param nmulti How many iterations should be performed for bandwidth selection? Defaults to 10.
#' @param make_plot Boolean value. Should the results be plotted? Defaults to TRUE.
#'
#' @return v: The spot volatilities
#' @return sigma1: The data of the function gamma (see equation (20))
#' @return Sigma1: The nonparametric estimator of the function gamma
#' @return sigma22: The data of the function eta^2 (see equation (25))
#' @return Sigma22: The nonparametric estimator of the function eta^2
#' @return Sigma2: The nonparametric estimator of eta
#' @return fit_gamma: The fit of the nonparametric regression of gamma
#' @return fit_eta2: The fit of the nonparametric regression of eta^2
#' @return fit_mu: The fit of the nonparametric regression of mu
#' @return mu: The data of the function mu (see equation (26))
#' @return Mu: The nonparametric estimator of the function mu
#' @return Rho: The nonparametric estimator of the leverage effect function rho (see equation (35))
#' @return dsigma1: The local linear estimator of the first order derivative of gamma (see equation (24))

#' @export
SVestimate <- function(v,beta01,beta02,beta10,r,d, date = "", bw1 = 0.15, bw2 = 0.16, bw3 = 0.15, select_bandwidth = F, nmulti = 10, make_plot = TRUE){

  # get plot theme
  plot_theme_ISVM <- make_plot_theme_ISVM()

  # Calculate the data of gamma
  sigma1 <- 2*v*beta01 # Equation (20)

  # Set the bandwidth in the nonparametric estimation of gamma
  if (select_bandwidth) {
    bw1 <- npregbw(xdat = v, ydat = sigma1, ckertype = 'epanechnikov', nmulti = nmulti,
                   regtype = 'll', bwtype = 'fixed', gradients = TRUE, bwmethod = "cv.ls")$bw
  }

  # Local linear nonparametric estimation of gamma
  fit1 <- npreg(bws = bw1, txdat = v, tydat = sigma1, ckertype = 'epanechnikov',
                regtype = 'll', bwtype = 'fixed', gradients = TRUE)
  dsigma1 <- fit1$grad[,] # Equation (24)
  Sigma1 <- fit1$mean # Equation (23)

  # Calculate the data of eta^2
  sigma22 <- 3*v^3*beta02*2 - v*Sigma1*dsigma1 + 3/2*Sigma1^2 # Equation (25)

  # Set the bandwidth in the nonparametric estimation of eta^2
  if (select_bandwidth) {
    bw2 <- npregbw(xdat = v, ydat = sigma22, ckertype = 'epanechnikov', nmulti = nmulti,
                   regtype = 'll', bwtype = 'fixed', gradients = TRUE, bwmethod = "cv.ls")$bw
  }
  # Local linear nonparametric regression of eta^2
  fit2 <- npreg(bws = bw2, txdat = v, tydat = sigma22, ckertype = 'epanechnikov',
                regtype = 'll', bwtype = 'fixed', gradients = TRUE)
  Sigma22 <- fit2$mean
  
  if(any(is.nan(sqrt(Sigma22)))) {
    message("Error: Sigma22 could not be estimated. Returning empty data.table")
    return(data.table())
  }
  
  Sigma2 <- sqrt(Sigma22)
  
  # Calculate the data of mu
  mu <- 1/12/v*(24*v*beta10 - 2*Sigma1*(6*(d - r) - 2*v*dsigma1 + 3*v^2) - 3*Sigma1^2 - 2*Sigma22) # Equation (26)

  # Set the bandwidth in the nonparametric estimation of mu
  if (select_bandwidth) {
    bw3 <-  npregbw(xdat = v, ydat = mu, ckertype = 'epanechnikov', nmulti = nmulti,
                    regtype = 'll', bwtype = 'fixed', bwmethod = "cv.ls")$bw
  }
  # Local linear nonparametric estimation of mu
  fit3 <- npreg(bws = bw3, txdat = v, tydat = mu, ckertype = 'epanechnikov',
                regtype = 'll', bwtype = 'fixed')
  Mu <- fit3$mean

  Rho <- Sigma1/sqrt(Sigma1^2 + Sigma22)

  # Set the range of plots
  plot_index <- (v <= 1.2 & v >= 0.01)
  xlimit <- c(min(v[plot_index])*0.975,max(v[plot_index])*1.025)
  if (make_plot) {
    # Plot the estimated functions
    p1 <- qplot(v[plot_index], sigma1[plot_index], xlab = "v", ylab = "gamma(v)") + geom_line(aes(v[plot_index], Sigma1[plot_index]), col = 'red') +  xlim(xlimit) + plot_theme_ISVM
    p2 <- qplot(v[plot_index], sigma22[plot_index], xlab = "v", ylab = "eta(v)^2") + geom_line(aes(v[plot_index], Sigma22[plot_index]), col = 'red') +  xlim(xlimit) + plot_theme_ISVM
    p3 <- qplot(v[plot_index], Sigma2[plot_index], geom = 'line', xlab = "v", ylab = "eta(v)") +  xlim(xlimit) + plot_theme_ISVM
    p4 <- qplot(v[plot_index], mu[plot_index], xlab = "v", ylab = "mu(v)") + geom_line(aes(v[plot_index], Mu[plot_index]), col = 'red') +  xlim(xlimit) + plot_theme_ISVM
    p5 <- qplot(v[plot_index], Rho[plot_index], geom = 'line', xlab = "v", ylab = "rho(v)") +  xlim(xlimit) + plot_theme_ISVM
    grid.arrange(p1, p2, p3, p4, p5, nrow = 2, top = textGrob(date,gp = gpar(fontsize = 20, font = 2)))
  }
  # Combine and return the results
  DT_res <- data.table("v" = v,
                       "sigma1" = sigma1,
                       "Sigma1" = Sigma1,
                       "sigma22" = sigma22,
                       "Sigma22" = Sigma22,
                       "Sigma2" = Sigma2,
                       "model_fits" =  list(list("fit_gamma" = fit1,"fit_eta2" = fit2,"fit_mu" = fit3)),
                       "mu" = mu,
                       "Mu" = Mu,
                       "Rho" = Rho,
                       "dsigma1" = dsigma1,
                       "bw1" = bw1,
                       "bw2" = bw2,
                       "bw3" = bw3)
  return(DT_res)
}
