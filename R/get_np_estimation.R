#' Get nonparametric boostrap ISVM estimate.
#'
#' Getting a Stochastic Volatility estimate with bootstrapping confidence intervals.
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
#' @param DT_ISVM A file generated from the "make_ISVM_data" function
#' @param boottrials Number of Bootstrap iterations. Defaults to 500.
#' @param select_bandwidth Boolean value. Should the optimal bandwidth for the local polynomial kernel regression be selected automatically? If TRUE, bandwidth will be selected according to least-squares cross-validation. Defaults to FALSE.
#' @param return_np_estimation_pars Should the fitted estimation parameters be returned? Defaults to `FALSE`.
#' @param return_lm Should the fitted lm object be returned? Defaults to `FALSE`.
#' @param return_lm_boot Should the bootstrap fitted lm object be returned? Only works if `return_lm = FALSE`. Defaults to `FALSE`.
#' @param save Should the plots be saved? Defaults to FALSE.
#' @param filename If "save" = TRUE, what is the filename of the file that should be saved?
#'
#' @return Returns a list with the plots of the nonparametric function estimates.

#' @export
get_np_estimation <- function(DT_ISVM, boottrials = 500, select_bandwidth = F, return_np_estimation_pars = FALSE, return_lm = FALSE, return_lm_boot = FALSE, save = FALSE, filename = NULL){

  plot_theme_ISVM <- make_plot_theme_ISVM()
  par_list <- np_estimation_pars(DT_ISVM, select_bandwidth = select_bandwidth, make_plot = F)
  
  if(return_np_estimation_pars) {
    return(par_list)
  }
  
  if(return_lm) {
    return(par_list$model_fit)
  }
  
  n <- par_list$n
  x_all <- par_list$x_all
  t_all <- par_list$t_all
  k_all <- par_list$k_all
  numofIV <- par_list$numofIV
  r <- par_list$r
  d <- par_list$d

  # Save the estimation results
  v0 <- par_list$DT_res[,v]
  sigma10 <- par_list$DT_res[,sigma1]
  Sigma10 <- par_list$DT_res[,Sigma1]
  sigma220 <- par_list$DT_res[,sigma22]
  Sigma220 <- par_list$DT_res[,Sigma22]
  Sigma20 <- par_list$DT_res[,Sigma2]
  mu0 <- par_list$DT_res[,mu]
  Mu0 <- par_list$DT_res[,Mu]
  Rho0 <- par_list$DT_res[,Rho]
  dsigma10 <- par_list$DT_res[,dsigma1]
  bw1 <- par_list$DT_res[1,bw1]
  bw2 <- par_list$DT_res[1,bw2]
  bw3 <- par_list$DT_res[1,bw3]

  #### Construct the confidence intervals by bootstrap ####
  # Bootstrap
  trial_list <- bootstrap_trials(boottrials, n, v0, k_all, t_all, x_all, numofIV, r, d, bw1, bw2, bw3, select_bandwidth)
  numoftrial_final <- which(!is.nan(sapply(trial_list$sigma2_all, sum)))
  v_all <- trial_list$v_all[numoftrial_final]
  sigma1_all <- trial_list$sigma1_all[numoftrial_final]
  sigma2_all <- trial_list$sigma2_all[numoftrial_final]
  sigma22_all <- trial_list$sigma22_all[numoftrial_final]
  mu_data <- trial_list$mu_data[numoftrial_final]
  mu_all <- trial_list$mu_all[numoftrial_final]
  rho_all <- trial_list$rho_all[numoftrial_final]
  len_numoftrial_final <- length(numoftrial_final)
  
  ## if we want the lm object as output ##
  
  if (return_lm_boot) {
    return( trial_list$model_fit[numoftrial_final])  
  }

  #### Plot the estimation result with CI ####
  # x includes 50 points in the range of v0. We use this range for the plot
  x <- max(min(v0), 0.01) + (0:49)/49*(min(max(v0), 1.2) - max(min(v0), 0.01))

  # Initialization
  Sigma1_pre <- matrix(nrow = length(x), ncol = len_numoftrial_final)
  Sigma2_pre <- matrix(nrow = length(x), ncol = len_numoftrial_final)
  Sigma22_pre <- matrix(nrow = length(x), ncol = len_numoftrial_final)
  Mu_pre <- matrix(nrow = length(x), ncol = len_numoftrial_final)
  Rho_pre <- matrix(nrow = length(x), ncol = len_numoftrial_final)

  # Use smoothing spline to interpolate the unknown functions at x
  n_iter <- 1:len_numoftrial_final
  for (i in n_iter) {
    tryCatch(expr = {
      plot_index = (v_all[[i]] <= 1.2 & v_all[[i]] >= 0.01)
    },
    error = function(e){
      message('Error: v_all seems to be')
      print(e)
      return(list())
    },
    warning = function(w){
      message('Caught a warning!')
      print(w)
      plot_index = (v_all[[i]] <= 1.2 & v_all[[i]] >= 0.01)
    })
    Sigma1_pre[1:length(x), i] <- predict(smooth.spline(v_all[[i]][plot_index], sigma1_all[[i]][plot_index],
                                                          df = 5, spar = 0.5), x)$y
    Sigma2_pre[1:length(x), i] <- predict(smooth.spline(v_all[[i]][plot_index], sigma2_all[[i]][plot_index],
                                                          df = 5, spar = 0.5), x)$y
    Sigma22_pre[1:length(x), i] <- predict(smooth.spline(v_all[[i]][plot_index], sigma22_all[[i]][plot_index],
                                                           df = 5, spar = 0.5), x)$y
    Mu_pre[1:length(x), i] <- predict(smooth.spline(v_all[[i]][plot_index], mu_all[[i]][plot_index],
                                                      df = 5, spar = 0.5), x)$y
    Rho_pre[1:length(x), i] <- predict(smooth.spline(v_all[[i]][plot_index], rho_all[[i]][plot_index],
                                                       df = 5, spar = 0.5), x)$y
  }

  # Calculate the standard deviations of the bootstrap estimators
  Sigma1_sd <- rep(0,length(x))
  Sigma2_sd <- rep(0,length(x))
  Sigma22_sd <- rep(0,length(x))
  Mu_sd <- rep(0,length(x))
  Rho_sd <- rep(0,length(x))
  for (i in 1:length(x)) {
    Sigma1_sd[i] <- sd(Sigma1_pre[i, n_iter][which(!is.nan(Sigma1_pre[i, n_iter]))])
    Sigma2_sd[i] <- sd(Sigma2_pre[i, n_iter][which(!is.nan(Sigma2_pre[i, n_iter]))])
    Sigma22_sd[i] <- sd(Sigma22_pre[i, n_iter][which(!is.nan(Sigma22_pre[i, n_iter]))])
    Mu_sd[i] <- sd(Mu_pre[i, n_iter][which(!is.nan(Mu_pre[i, n_iter]))])
    Rho_sd[i] <- sd(Rho_pre[i, n_iter][which(!is.nan(Rho_pre[i, n_iter]))])
  }

  # Calculate the unknown functions at x based on the original (not bootstrap) nonparametric estimators
  plot_index <- (v0 <= 1.2 & v0 >= 0.01)
  Sigma10_pre <- predict(smooth.spline(v0[plot_index], Sigma10[plot_index], df = 5, spar = 0.5), x)$y

  # get Sigma20_pre (this fails sometimes)
  tryCatch(expr = {
    Sigma20_pre <- (predict(smooth.spline(v0[plot_index], Sigma20[plot_index], df = 5, spar = 0.5), x)$y)
  },
  error = function(e){
    message('Caught an error!')
    print(e)
  },
  warning = function(w){
    message('Caught a warning!')
    print(w)
    Sigma20_pre <- (predict(smooth.spline(v0[plot_index], Sigma20[plot_index], df = 5, spar = 0.5), x)$y)
  })
  Sigma220_pre <- predict(smooth.spline(v0[plot_index], Sigma220[plot_index], df = 5, spar = 0.5), x)$y
  Mu0_pre <- predict(smooth.spline(v0[plot_index], Mu0[plot_index], df = 5, spar = 0.5), x)$y

  # get Rho20_pre (this fails sometimes)
  tryCatch(expr = {
    Rho0_pre <- predict(smooth.spline(v0[plot_index], Rho0[plot_index], df = 5, spar = 0.5), x)$y
  },
  error = function(e){
    message('Error: ')
    print(e)
  },
  warning = function(w){
    message('Warning: ')
    print(w)
    Rho0_pre <- predict(smooth.spline(v0[plot_index], Rho0[plot_index], df = 5, spar = 0.5), x)$y
  })


  # Calculate CI (estimator +/- 2*standard error)
  tryCatch(expr = {
    Sigma1_CI <- make_CI(Sigma10_pre, Sigma1_sd)
    Sigma2_CI <- make_CI(Sigma20_pre, Sigma2_sd)
    Sigma22_CI <- make_CI(Sigma220_pre, Sigma22_sd)
    Mu_CI <- make_CI(Mu0_pre, Mu_sd)
    Rho_CI <- make_CI(Rho0_pre, Rho_sd)
  },
  error = function(e){
    message('Error: ')
    print(e)
  },
  warning = function(w){
    message('Warning: ')
    print(w)
    Sigma1_CI <- make_CI(Sigma10_pre, Sigma1_sd)
    Sigma2_CI <- make_CI(Sigma20_pre, Sigma2_sd)
    Sigma22_CI <- make_CI(Sigma220_pre, Sigma22_sd)
    Mu_CI <- make_CI(Mu0_pre, Mu_sd)
    Rho_CI <- make_CI(Rho0_pre, Rho_sd)
  })

  # Final plot with CI TeX("Function: $\\gamma$"), TeX(("$v$"))
  xlimit <- c(min(v0[plot_index])*0.975,max(v0[plot_index])*1.025)

  # p1 = ggplot(data.table(v0[plot_index], sigma10[plot_index]), aes(x = V1, y = V2)) + geom_point(size = 2) + labs(xlab = TeX("$v$"), ylab = TeX(r"(Function: $\gamma (v)$)")) + geom_line(aes(x, Sigma10_pre), col = 'red') + geom_line(aes(x, Sigma1_CI$u), col = 'red', linetype = 'dashed') + geom_line(aes(x, Sigma1_CI$d), col = 'red', linetype = 'dashed') +  xlim(xlimit) + plot_theme_ISVM
  p1 = qplot(v0[plot_index], sigma10[plot_index], xlab = TeX("$v$"), ylab = TeX(r"(Function: $\gamma (v)$)"), size = 2) + geom_line(aes(x, Sigma10_pre), col = 'red', size = 2) + geom_line(aes(x, Sigma1_CI$u), col = 'red', linetype = 'dashed', size = 2) + geom_line(aes(x, Sigma1_CI$d), col = 'red', linetype = 'dashed', size = 2) +  xlim(xlimit) + plot_theme_ISVM
  p2 = qplot(v0[plot_index], sigma220[plot_index], xlab = TeX("$v$"), ylab = TeX(r"(Function: $\eta(v)^2$)"), size = 2) + geom_line(aes(x, Sigma220_pre), col = 'red', size = 2) + geom_line(aes(x, Sigma22_CI$u), col = 'red', linetype = 'dashed', size = 2) + geom_line(aes(x, Sigma22_CI$d), col = 'red',linetype = 'dashed', size = 2) +  xlim(xlimit) + plot_theme_ISVM
  p3 = qplot(xlab = TeX("$v$"), ylab = TeX(r"(Function: $\eta(v)$)"), size = 2) + geom_line(aes(x, Sigma20_pre), col = "red") + geom_line(aes(x, Sigma2_CI$u), col = 'red', linetype = 'dashed', size = 2) + geom_line(aes(x, Sigma2_CI$d), col = 'red', linetype = 'dashed', size = 2) +  xlim(xlimit) + plot_theme_ISVM
  p4 = qplot(v0[plot_index], mu_data[[length(mu_data)]][plot_index], xlab = TeX("$v$"), ylab = TeX(r"(Function: $\mu(v)$)"), size = 2) + geom_line(aes(x, Mu0_pre), col = 'red', size = 2) + geom_line(aes(x, Mu_CI$u), col = 'red', linetype = 'dashed', size = 2) + geom_line(aes(x, Mu_CI$d), col = 'red', linetype = 'dashed', size = 2) +  coord_cartesian(ylim=quantile(mu_data[[length(mu_data)]][plot_index], c(0.01, 0.99))) + xlim(xlimit) + plot_theme_ISVM
  p5 = qplot(xlab = TeX("$v$"), ylab = TeX(r"(Function: $\rho (v)$)"), size = 2) + geom_line(aes(x, Rho0_pre), col = "red") + geom_line(aes(x, Rho_CI$u), col = 'red',linetype = 'dashed', size = 2) + geom_line(aes(x, Rho_CI$d), col = 'red', linetype = 'dashed', size = 2) + xlim(xlimit) + plot_theme_ISVM
  plot_with_ci <- grid.arrange(p1, p2, p3, p4, p5, nrow = 2) # This plot corresponds to Figure 1 of the code guide file with CI

  if (save){
    if (is.null(filename)){
      message("Error: no filename provided")
    } else {
      ggsave(plot = plot_with_ci, file = paste(filename, ".png", sep = ""), bg = "transparent",
             width = 11.69, height = 8.27, dpi = 300)
    }
  }
  plot_list <- list("gamma" = p1, "eta2" = p2, "eta" = p3, "mu" = p4, "rho" = p5)
  return(plot_list)
}
