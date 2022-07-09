#' Run ICC.
#'
#' Wrapper function of run_experiment that also loads, preprocesses and saves test data.
#' @import data.table
#' @import parallel
#'
#' @param PATH Output 'train_data' generated from `make_subset` function.
#' @param K Integer value. Number of clusters. Defaults to `2`.
#' @param subs_date_list A vector of dates to generate a subset for clustering. Defaults to ` c(1)`.
#' @param gamma_list A vector of gamma values for clustering the data. Defaults to ` c(1)`.
#' @param money_range Vector with lower and upper limit of moneyness values to consider. The natural boundary would be `c(0,Inf)`.
#' @param impute Boolean value. Should missing observations be imputed?
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered?
#' @param N_subs_maturities Integer value. If `subset_maturities = TRUE` this will select how many of the closest maturities should be considered. Defaults to `3`.
#' @param parallel Boolean value. Should the clustering be computed in parallel? Won't work on Windows machines. Defaults to `FALSE`.
#' @param parallel_preprocessing Boolean value. Should the clustering be run in parallel? Defaults to `FALSE.`
#'
#' @return Returns a list with the results from "get_clusters":
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
run_ICC <- function(PATH,
                    subs_date_list = c(1),
                    gamma_list = c(1),
                    money_range = c(0.8,1.2),
                    K = 2,
                    parallel = TRUE,
                    parallel_preprocessing = TRUE,
                    subset_maturities = TRUE,
                    N_subs_maturities = 3,
                    impute = FALSE) {

  #### specify parameters and load data ####
  DT_test <- make_and_save_test_data(PATH = PATH, filename  = "DT_test")

  #### run the loop ####
  if (parallel) {
    cluster_result <- mclapply(subs_date_list, function(subs_date) {
        tmp_result <- lapply(gamma_list, function(gamma) {
          run_experiment(DATA = DT_test,
                         K = K,
                         N_subs_date = subs_date,
                         subset_maturities = subset_maturities,
                         N_subs_maturities = N_subs_maturities,
                         money_range = money_range,
                         gamma = gamma,
                         impute = impute,
                         parallel = FALSE)
        })
        names(tmp_result) <- paste("gamma", gamma_list, sep = "_")
        return(tmp_result)
    }, mc.cores = detectCores()*0.5)
    names(cluster_result) <- paste("subs_date", subs_date_list, sep = "_")
    return(cluster_result)
  } else {
    #### run the loop ####
    cluster_result <- lapply(subs_date_list, function(subs_date) {
      tmp_result <- lapply(gamma_list, function(gamma) {
          run_experiment(DATA = DT_test,
                         K = K,
                         N_subs_date = subs_date,
                         subset_maturities = subset_maturities,
                         N_subs_maturities = N_subs_maturities,
                         money_range = money_range,
                         gamma = gamma,
                         impute = impute,
                         parallel = parallel_preprocessing)
        })
      names(tmp_result) <- paste("gamma", gamma_list, sep = "_")
      return(tmp_result)
    })
    names(cluster_result) <- paste("subs_date", subs_date_list, sep = "_")
    return(cluster_result)
  }
  return(cluster_result)
}
