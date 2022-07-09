#' Bootstrap Trials
#'
#' Generates bootstrap trials for estimating the confidence intervals of an Implied Stochastic Volatility Model
#'
#' @import data.table
#' @import rmatio
#' @import np
#' @import latex2exp
#' @import parallel
#'
#' @param numoftrial Integer value. Number of Bootstrap iterations. Defaults to 500.
#' @param n Integer value. Sample size, i.e., the number of days.
#' @param v0 The spot volatilities.
#' @param k_all Log-moneyness.
#' @param t_all Time-to-maturities.
#' @param x_all IV.
#' @param numofIV Vector for recording the number of IV observations on each point in time t.
#' @param r Risk-free interest rates.
#' @param d Dividend yields.
#' @param bw1 Numeric value for bandwidth of Gamma function estimate in case "select_bandwith" = FALSE. Defaults to 0.15.
#' @param bw2 Numeric value for bandwidth of Eta^2 function estimate in case "select_bandwith" = FALSE. Defaults to 0.16.
#' @param bw3 Numeric value for bandwidth of Mu function estimate in case "select_bandwith" = FALSE. Defaults to 0.15.
#' @param select_bandwidth Boolean value. Should the optimal bandwidth for the local polynomial kernel regression be selected automatically? If TRUE, bandwidth will be selected according to least-squares cross-validation. Defaults to FALSE.
#'
#' @return v: The spot volatilities
#' @return Sigma1: The nonparametric bootstrap estimator of the function gamma
#' @return Sigma22: The nonparametric bootstrap estimator of the function eta^2
#' @return Sigma2: The nonparametric bootstrap estimator of eta
#' @return mu_data: The original data of the function mu.
#' @return Mu: The nonparametric bootstrap estimator of the function mu.
#' @return Rho: The nonparametric bootstrap estimator of the leverage effect function rho.

#' @export
bootstrap_trials <- function(numoftrial = 500, n, v0, k_all, t_all, x_all, numofIV, r, d, bw1 = 0.15, bw2 = 0.16, bw3 = 0.15, select_bandwidth = FALSE){

  # Initialization
  vboot <- NULL
  rboot <- NULL
  dboot <- NULL
  beta01boot <- NULL
  beta02boot <- NULL
  beta10boot <- NULL
  v_all <- NULL
  sigma1_all <- NULL
  sigma2_all <- NULL
  sigma22_all <- NULL
  mu_all <- NULL
  rho_all <- NULL
  
  output <- mclapply(seq(numoftrial), function(x) {
    
    model_fit <- vector(mode = "list", length = n)
    for (i in 1:n) {

      vt = v0[i]
      k_trial0 <- k_all[[i]]
      t_trial0 <- t_all[[i]]
      x_trial0 <- x_all[[i]] # The original data on the ith day

      # Randomly select IV from the original IV surface
      repeat {
        trial_ind <- sample(1:length(x_trial0), numofIV[i], replace = T)
        t_trial <- t_trial0[trial_ind]
        k_trial <- k_trial0[trial_ind]
        x_trial <- x_trial0[trial_ind]
        if (length((t_trial)) >= 4) break
      }

      # Construct the regressors
      k2 <- k_trial^2
      tk <- t_trial*k_trial
      t2 <- t_trial^2
      t2k <- t_trial^2*k_trial

      # Bivariate regression
      obj <- lm(x_trial ~ k_trial + k2 + t_trial + tk + t2 + t2k)
      model_fit[[i]] <- obj
      vboot[i] <- obj$coefficients[1]
      beta01boot[i] <- obj$coefficients[2]
      beta02boot[i] <- obj$coefficients[3]
      beta10boot[i] <- obj$coefficients[4]
      rboot[i] <- r[i]
      dboot[i] <- d[i]
    }
    ind <- which(is.nan(vboot))
    if (length(ind) > 0){
      beta01boot <- beta01boot[-ind]
      beta02boot <- beta02boot[-ind]
      beta10boot <- beta20boot[-ind]
      rboot <- rboot[-ind]
      dboot <- dboot[-ind]
      vboot <- vboot[-ind]
      model_fit <- model_fit[-ind]
    }
    # Nonparametric estimation based on the bootstrap data
    DT_boot <- SVestimate(vboot, beta01boot, beta02boot, beta10boot, rboot, dboot, bw1, bw2, bw3, select_bandwidth = select_bandwidth, make_plot = F)
    v <- DT_boot[,v]
    sigma1 <- DT_boot[,sigma1]
    Sigma1 <- DT_boot[,Sigma1]
    sigma22 <- DT_boot[,sigma22]
    Sigma22 <- DT_boot[,Sigma22]
    Sigma2 <- DT_boot[,Sigma2]
    mu <- DT_boot[,mu]
    Mu <- DT_boot[,Mu]
    Rho <- DT_boot[,Rho]
    dsigma1 <- DT_boot[,dsigma1]
    # Save the nonparametric estimators
    return(list("v_all" = v,
                "sigma1_all" = Sigma1,
                "sigma2_all" = Sigma2,
                "sigma22_all" = Sigma22,
                "mu_data" = mu,
                "mu_all" = Mu,
                "rho_all" = Rho,
                "model_fit" = model_fit))
  }, mc.cores = detectCores()*0.9)
  output_list <- list("v_all" = lapply(seq_along(output), function(x) output[[x]]$v_all),
                      "sigma1_all" = lapply(seq_along(output), function(x) output[[x]]$sigma1_all),
                      "sigma2_all" = lapply(seq_along(output), function(x) output[[x]]$sigma2_all),
                      "sigma22_all" = lapply(seq_along(output), function(x) output[[x]]$sigma22_all),
                      "mu_data" = lapply(seq_along(output), function(x) output[[x]]$mu_data),
                      "mu_all" = lapply(seq_along(output), function(x) output[[x]]$mu_all),
                      "rho_all" = lapply(seq_along(output), function(x) output[[x]]$rho_all),
                      "model_fit" = lapply(seq_along(output), function(x) output[[x]]$model_fit))
  return(output_list)
}
