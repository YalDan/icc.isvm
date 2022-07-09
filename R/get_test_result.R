#' Get test result
#'
#' ICC clustering for a subset of datapoints. Wrapper for "get_clusters" function.
#'
#' @import data.table
#'
#' @param DATA Output list generated from "make_subset" function.
#' @param K Integer value. Number of clusters. Defaults to 2.
#' @param gamma Numeric value for the switching penalty. Defaults to 0.
#' @param w_max Integer value. The number of maximum iterations for ICC. Defaults to 3.
#' @param money_range Vector with lower and upper limit of moneyness values to consider. The natural boundary would be c(0,Inf). Defaults to c(0.8,1.2).
#' @param impute Boolean value. Should missing observations be imputed? Defaults to TRUE.
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered? Defaults to FALSE.
#' @param save_plot Boolean. Should the plots be saved? Defaults to FALSE.
#' @param data_only Boolean. If set to TRUE, only the data is returned and plotting is skipped. Defaults to FALSE.
#' @param distance_function Character value. Which distance should be used? Choose either "euclidean","loglik" (loglikelihood), or "mahalanobis". Defaults to "loglik".
#' @param print_plot Boolean. Should the plots be saved? Defaults to TRUE.
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
get_test_result <- function(DATA,
                            K = 2,
                            gamma = 0,
                            w_max = 3,
                            money_range = c(0.8,1.2),
                            impute = TRUE,
                            subset_maturities = FALSE,
                            save_plot = F,
                            data_only = F,
                            distance_function = "loglik",
                            print_plot = T){

  train_data <- DATA$train_data
  plot_data <- DATA$plot_data

  if ("date" %in% names(train_data) & "t" %in% names(train_data)) {
    tryCatch(expr = {
      cor_test <- cor(train_data[,!c("date", "t")], use = "complete.obs")
      },
      error = function(e){
        message('Caught an error!')
        print(e)
        return(list())
      },
      warning = function(w){
        message('Caught a warning!')
        print(w)
        cor_test <- cor(train_data[,!c("date", "t")], use = "complete.obs")
      })
  } else if ("date" %in% names(train_data)) {
    tryCatch(expr = {
      cor_test <- cor(train_data[,!"date"], use = "complete.obs")
    },
    error = function(e){
      message('Caught an error!')
      print(e)
      return(list())
    },
    warning = function(w){
      message('Caught a warning!')
      print(w)
      cor_test <- cor(train_data[,!"date"], use = "complete.obs")
    })
  } else if ("date" %in% names(train_data) & "t" %in% names(train_data)) {
    tryCatch(expr = {
      cor_test <- cor(train_data[,!c("date", "t")], use = "complete.obs")
    },
    error = function(e){
      message('Caught an error!')
      print(e)
      return(list())
    },
    warning = function(w){
      message('Caught a warning!')
      print(w)
      cor_test <- cor(train_data[,!c("date", "t")], use = "complete.obs")
    })
  } else if ("t" %in% names(train_data)) {
    tryCatch(expr = {
      cor_test <- cor(train_data[,!"t"], use = "complete.obs")
    },
    error = function(e){
      message('Caught an error!')
      print(e)
      return(list())
    },
    warning = function(w){
      message('Caught a warning!')
      print(w)
      cor_test <- cor(train_data[,!"t"], use = "complete.obs")
    })
  } else {
    tryCatch(expr = {
      cor_test <- cor(train_data, use = "complete.obs")
    },
    error = function(e){
      message('Caught an error!')
      print(e)
      return(list())
    },
    warning = function(w){
      message('Caught a warning!')
      print(w)
      cor_test <- cor(train_data, use = "complete.obs")
    })
  }


if (check_invertible(cor_test)) {
  cluster_result <- get_clusters(train_data,
                                 plot_data,
                                 N_subs_date = DATA$N_subs_date,
                                 data_only = data_only,
                                 save_plot = save_plot,
                                 K = K,
                                 gamma = gamma,
                                 w_max = w_max,
                                 money_range = money_range,
                                 impute = impute,
                                 subset_maturities = subset_maturities,
                                 random_seed = 123,
                                 DEBUG = 0,
                                 distance_function = distance_function,
                                 print_plot = print_plot)
  return(cluster_result)
  } else {
  message("Error: correlation matrix is not invertible. Maybe you forgot to remove any date/time columns?")
  return(list())
  }
}
