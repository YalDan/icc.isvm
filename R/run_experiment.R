#' Run experiment
#'
#' Wrapper function to generate ICC estimate for provided input data.
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param N_subs_date Date value. Number of dates to subset the input data from. Defaults to "as.Date("1970-01-01")".
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered? Defaults to TRUE.
#' @param K Integer value. Number of clusters. Defaults to 2.
#' @param N_subs_maturities Integer value. If `subset_maturities = TRUE` this will select how many of the closest maturities should be considered. Defaults to `3`.
#' @param money_range Vector with lower and upper limit of moneyness values to consider. The natural boundary would be c(0,Inf). Defaults to c(0.8,1.2).
#' @param threshold How many percent of observations are allowed to be missing for each individual instrument over the observed timeframe? E.g. if set to 0.9, we allow 10% of observations to be missing. Defaults to 0.66.
#' @param parallel Boolean value. Should the clustering be computed in parallel? Won't work on Windows machines. Defaults to `FALSE`.
#' @param n_cores Number of cores. This only applies if parallel = TRUE. Defaults to 50% of detected cores.
#' @param impute Boolean value. Should missing observations be imputed? Defaults to TRUE.
#' @param verbose Boolean value. Should the function be verbose? Defaults to FALSE.
#' @param gamma Numeric value for the switching penalty. Defaults to 0.
#' @param w_max Integer value. The number of maximum iterations for ICC. Defaults to 3.
#' @param save_plot Boolean. Should the plots be saved? Defaults to TRUE.
#' @param last_only Should only the last days of the dataset be returned? If set to TRUE this will give exactly as many days as specified in "N_subs_date". Defaults to FALSE.
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
run_experiment <- function(DATA,
                           N_subs_date = 5,
                           subset_maturities = TRUE,
                           K = 2,
                           N_subs_maturities = 3,
                           money_range = c(0.8,1.2),
                           threshold = 0.66,
                           parallel = FALSE,
                           n_cores = detectCores()*0.8,
                           impute = FALSE,
                           verbose = FALSE,
                           gamma = 0,
                           w_max = 3,
                           save_plot = TRUE,
                           last_only = FALSE){
  cat("\n")
  writeLines(paste("===========================================================",
                   Sys.time(),paste(
                     "Running experiment with gamma = ", gamma,
                     ", K = ", K,
                     ", subset days = ", N_subs_date, sep = ""),
                   "===========================================================",
                   sep = "\n"))
  cat("\n")

  #### load data ####
  list_DT_subset <- make_subset_list(DATA,
                                     N_subs_date = N_subs_date,
                                     subset_maturities = subset_maturities,
                                     N_subs_maturities = N_subs_maturities,
                                     money_range = money_range,
                                     threshold = threshold,
                                     verbose = verbose,
                                     impute = impute,
                                     parallel = parallel,
                                     n_cores = n_cores,
                                     last_only = last_only)
  ## ##
  cat("\n")
  cat("\n")
  writeLines(paste("===========================================================",
                   Sys.time(),
                     "calculating test results now",
                   "===========================================================",
                   sep = "\n"))
  cat("\n")

  #### get clusters ####
  #### ## split data in chunks
  cluster_result <- get_test_result_list(list_DT_subset,
                                         gamma = gamma,
                                         w_max = w_max,
                                         K = K,
                                         money_range = money_range,
                                         impute = impute,
                                         subset_maturities = subset_maturities,
                                         save_plot = save_plot,
                                         parallel = parallel,
                                         n_cores = n_cores)
  return(cluster_result)
}
