#' Make confidence interval
#'
#' Generates confidence intervals during ISVM calculation
#'
#' @param par_pre A vector with values.
#' @param par_sd The standard deviation of the vector.
#'
#' @return List with upper ("u") and lower ("d") confidence interval for provided value.

#' @export
make_CI <- function(par_pre, par_sd) {
  par_u <- par_pre + 2*par_sd
  par_d <- par_pre - 2*par_sd
  return(list("u" = par_u, "d" = par_d))
}
