#' Fit ICC & ISVM
#'
#' Fit an ISV model for each state detected by ICC
#'
#' @import xts
#' @import zoo
#' @import data.table
#' @import lubridate
#'
#' @param DT_full A data.table with structure as provided in the example.
#' @param last_date When does the sampling period end?
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered? Defaults to TRUE.
#' @param last_only Should only the last days of the dataset be returned? If set to TRUE this will give exactly as many days as specified in "N_subs_date". Defaults to FALSE.
#' @param N_subs_date Number of dates to subset the input data from. Defaults to 5.
#' @param K Integer value. Number of clusters. Defaults to 2.
#' @param gamma Numeric value for the switching penalty. Defaults to 10.
#' @param n_boottrial How many bootstrap iterations should be run?
#' @param parallel_ICC Should the ICC algorithm be run in parallel?
#'
#' @return Returns a list with both the ICC and the ISVM result.

#' @export
fit_ICC_ISVM <- function(DT_full,
                         last_date,
                         subset_maturities = TRUE,
                         last_only = T,
                         N_subs_date = 5,
                         K = 2,
                         gamma = 10,
                         n_boottrial = 500,
                         parallel_ICC = FALSE){

  if (last_only) {
    subs_dates <- seq(last_date - N_subs_date, last_date, by  = 1)
    DT_last_only <- copy(DT_full[date %in% subs_dates])
    N_subs_date <- DT_BTC_deribit[,length(unique(date)) - 1]

    ICC_DT_full <- run_experiment(DATA = DT_last_only,
                                  K = K,
                                  N_subs_date = N_subs_date,
                                  subset_maturities = subset_maturities,
                                  gamma = gamma,
                                  impute = T,
                                  save_plot = F,
                                  last_only = last_only,
                                  parallel = parallel_ICC)
  } else {
    ICC_DT_full <- run_experiment(DATA = DT_full,
                                  K = K,
                                  N_subs_date = N_subs_date,
                                  subset_maturities = subset_maturities,
                                  gamma = gamma,
                                  impute = T,
                                  save_plot = F,
                                  last_only = last_only,
                                  parallel = parallel_ICC)
  }
  ###

  ICC_ISVM_list <- run_ISVM(ICC_DT_full,
                            DT_full,
                            subset_maturities = subset_maturities,
                            N_subs_date = N_subs_date,
                            n_boottrial = n_boottrial)
  return(ICC_ISVM_list)
}
