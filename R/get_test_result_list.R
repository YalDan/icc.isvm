#' Get test result list
#'
#' ICC clustering for multiple datasets. List wrapper for "get_test_result" function.
#'
#' @import data.table
#'
#' @param DATA Output list generated from "make_subset" function.
#' @param K Integer value. Number of clusters. Defaults to 2.
#' @param gamma Numeric value for the switching penalty. Defaults to 0.
#' @param w_max Integer value. The number of maximum iterations for ICC. Defaults to 3.
#' @param money_range Vector with lower and upper limit of moneyness values to consider. The natural boundary would be c(0,Inf).
#' @param impute Boolean value. Should missing observations be imputed?
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered?
#' @param save_plot Boolean. Should the plots be saved?
#' @param data_only Boolean. If set to TRUE, only the data is returned and plotting is skipped. Defaults to FALSE.
#' @param parallel Boolean. Should the results be computed in parallel? Won't work on Windows machines. Defaults to TRUE.
#' @param n_cores Number of cores. This only applies if parallel = TRUE. Defaults to 50% of detected cores.
#'
#' @return DATA: The input data.
#' @return date: The date of the last observation.
#' @return symbols: List of all symbols that were used for clustering.
#' @return price_plot: Plot of the clustered price of the underlying index.
#' @return vola_plot: Plot of the clustered input data.
#' @return states: Time series with the states at each point in time.
#' @return mean_impl_vola: Table with mean and sd of the detected clusters.
#' @return w_max: Number of maximum iterations specified.
#' @return w: Number of actual iterations.
#' @return K: The input number of clusters .
#' @return scale_gamma: Indicates if the penalty gamma got scaled down.
#' @return gamma_init: Initial value provided for gamma.
#' @return gamma_value: Actual value for gamma. In case gamma hasn't been scaled down this will be identical to 'gamma_init'.

#' @export
get_test_result_list <- function(DATA,
                                 K = 2,
                                 gamma = 0,
                                 w_max = 3,
                                 money_range,
                                 impute,
                                 subset_maturities,
                                 save_plot = F,
                                 data_only = F,
                                 parallel = T,
                                 n_cores = detectCores()*0.5) {

  if (parallel) {
    split_list_DT_subset <- split(DATA, ceiling(seq_along(DATA)/(length(seq_along(DATA))/n_cores)))
    cat("\n")
    print("Calculating in parallel ... ")
    cat("\n")
    test_result <- mclapply(seq_along(split_list_DT_subset), function(l) {
      lapply(split_list_DT_subset[[l]], function(x) {
        get_test_result(x,
                        data_only = data_only,
                        save_plot = save_plot,
                        K = K,
                        gamma = gamma,
                        w_max = w_max,
                        money_range = money_range,
                        impute = impute,
                        subset_maturities = subset_maturities)
      })
    }, mc.cores = n_cores)
    return(test_result)
  } else {
  cat("\n")
  pb <- txtProgressBar(min = 0, max = length(DATA), initial = 0, style = 3)
  stepi <- 0
  test_result <- lapply(DATA, function(x) {
    res <- get_test_result(x,
                           data_only = data_only,
                           save_plot = save_plot,
                           K = K,
                           gamma = gamma,
                           w_max = w_max,
                           money_range = money_range,
                           impute = impute,
                           subset_maturities = subset_maturities)
    stepi <<- stepi + 1
    setTxtProgressBar(pb,stepi)
    return(res)
    })
  }
  return(test_result)
}
