#' ICC in R
#'
#' R wrapper for the Python ICC function.
#'
#' @import reticulate
#'
#' @param DATA A NxN correlation matrix.
#' @param K Integer value. Number of clusters. Defaults to 2.
#' @param gamma Numeric value for the switching penalty. Defaults to 0.
#' @param w_max Integer value. The number of maximum iterations for ICC. Defaults to 3.
#' @param DEBUG Integer value. Parameter for verbosity of ICC algorithm that can be either 0 or 1. Defaults to 0.
#' @param distance_function Character value. Which distance should be used? Choose either "euclidean","loglik" (loglikelihood), or "mahalanobis". Defaults to "loglik".
#' @param V Parameter V in case "loglik" distance function has been chosen. Defaults to 2.3.
#' @param random_seed Numeric value for reproducibility of ICC results. Defaults to 123.

#'
#' @return Vector of ICC state per input observation.
#' @return w. Number of successful iterations.

#' @export
ICC_R <- function(DATA, K = 2, gamma = 0, delta = 0, w_max = 3, DEBUG = 0, distance_function = "mahalanobis", V = 2.3, random_seed = 123){
  if (!exists("ICC")) {
    ## initiation file ##
    libraries_py <- list("pandas", "numpy", "scipy")
    ## ##

    ## create conda env ##
    if (!("r-conda-ICC" %in% reticulate::conda_list()$name)) {
      reticulate::conda_create("r-conda-ICC")
      reticulate::conda_install(envname = "r-conda-ICC", packages = libraries_py)
    }
    ## use env ##
    reticulate::use_condaenv("r-conda-ICC")

    ## load python functions ##
    reticulate::source_python("./python/MFCF_new/ICC.py")
    np <- reticulate::import("numpy", convert = FALSE)
    ## ##
  }
  return(ICC(DATA, K = K, gamma = gamma, w_max = w_max, DEBUG = DEBUG, distance_function = distance_function, V = V, random_seed = random_seed))
}
