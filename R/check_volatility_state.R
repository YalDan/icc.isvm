#' Check Volatility State
#'
#' For a time series clustered by ICC when K=2, this returns to which state an unclustered observation would belong based on the likelihood.
#'
#' @param mean_impl_vola the 'mean_impl_vola' output returned by ICC clustering.
#' @param target_value an IV value.
#' @param distr the distribution based on which the likelihood should be computed. Can be either "norm" for normal or "lognorm" for lognormal.
#'
#' @return 0 or 1. Reports to which state the observation belongs.

#' @export
check_volatility_state <- function(mean_impl_vola, target_value, distr = "norm"){

  if (distr == "lognorm") {
    loglik_distr_0 <- log(dlnorm(target_value,mean_impl_vola[state == 0,mean],mean_impl_vola[state == 0,sd]))
    loglik_distr_1 <- log(dlnorm(target_value,mean_impl_vola[state == 1,mean],mean_impl_vola[state == 1,sd]))
  } else {
    loglik_distr_0 <- log(dnorm(target_value,mean_impl_vola[state == 0,mean],mean_impl_vola[state == 0,sd]))
    loglik_distr_1 <- log(dnorm(target_value,mean_impl_vola[state == 1,mean],mean_impl_vola[state == 1,sd]))
  }
  state <- which.max(c(loglik_distr_0, loglik_distr_1))
  if (state == 1) {
    return(0)
  } else
  {
    return(1)
  }
}
