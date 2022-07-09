#' Make ISVM matrix
#'
#' Helper function for wrapping output of "make_subset" function for ISVM as matrix.
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param start_time When does the trading day start? Defaults to "08:20:00" (Deribit options expire at "08:00:00").
#' @param end_time When does the trading day end? Defaults to "07:59:59" (Deribit options expire at "08:00:00").
#' @param start_date When does the sampling period start? Defaults to NA.
#' @param end_date When does the sampling period end? Defaults to NA.
#' @param N_subs_date Number of dates to subset the input data from. Defaults to 5.
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered? Defaults to TRUE.
#' @param N_subs_maturities Integer value. If 'subset_maturities' = TRUE this will select how many of the closest maturities should be considered. Defaults to 3.
#' @param money_range Vector with lower and upper limit of moneyness values to consider. The natural boundary would be c(0,Inf). Defaults to c(0.8,1.2).
#' @param verbose Boolean value. Should the function be verbose?
#' @param impute Boolean value. Should missing observations be imputed? Defaults to TRUE.
#' @param threshold How many percent of observations are allowed to be missing for each individual instrument over the observed timeframe? E.g. if set to 0.9, we allow 10% of observations to be missing. Defaults to 0.66.
#' @param value_var The variable for which we want to generate a clustering data set.
#'
#' @return Returns a matrix from "make_subset" function without timestamp.

#' @export
make_ISVM_matrix <- function(DATA,
                             start_time = "08:20:00",
                             end_time = "07:59:59",
                             start_date = NA,
                             end_date = NA,
                             N_subs_date = 5,
                             subset_maturities = TRUE,
                             N_subs_maturities = 3,
                             money_range = c(0.8,1.2),
                             verbose = FALSE,
                             impute = TRUE,
                             threshold = 0.66,
                             value_var) {
  subs_data <- make_subset2(DATA = DATA,
                            start_time = start_time,
                            end_time = end_time,
                            start_date = start_date,
                            end_date = end_date,
                            N_subs_date = N_subs_date,
                            subset_maturities = subset_maturities,
                            N_subs_maturities = N_subs_maturities,
                            money_range = money_range,
                            verbose = verbose,
                            impute = impute,
                            threshold = threshold,
                            value_var = value_var)
  return(as.matrix(subs_data$train_data[,!"t"]))
}
