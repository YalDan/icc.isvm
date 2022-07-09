#' Make subset data
#'
#' Generates a subset data list that can be used for ISVM calculation.
#'
#' @import data.table
#'
#' @param ICC_DATA A list with an object returned from the function "get_clusters".
#' @param VOLA_DATA A data.table with structure as provided in the example.
#' @param start_time When does the trading day start? Defaults to "08:20:00" (Deribit options expire at "08:00:00").
#' @param end_time When does the trading day end? Defaults to "07:59:59" (Deribit options expire at "08:00:00").
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered? Defaults to FALSE
#' @param N_subs_maturities Integer value. If 'subset_maturities' = TRUE this will select how many of the closest maturities should be considered. Defaults to 3.
#' @param money_range Vector with lower and upper limit of moneyness values to consider. The natural boundary would be c(0,Inf). Defaults to c(0.8,1.2).
#' @param subs Number of dates to subset the input data from. Defaults to 5.
#' @param impute Boolean value. Should missing observations be imputed? Defaults to TRUE.
#' @param verbose Boolean value. Should the function be verbose?
#' @param threshold How many percent of observations are allowed to be missing for each individual instrument over the observed timeframe? E.g. if set to 0.9, we allow 10% of observations to be missing. Defaults to 0.66.
#' @param state Vector with unique states. Defaults to c(0,1).
#'
#' @return cp_flag: 0 for call options and 1 for put options.
#' @return d: Daily dividend yields in percentage.
#' @return date: Numeric dates. One can use the Matlab function datestr to interpret these dates in traditional form.
#' @return IVobs: IV observations.
#' @return Money: Log-moneyness.
#' @return Mtr: Days-to-expiration.
#' @return price: Option prices.
#' @return r: Risk-free interest rates in percentage.
#' @return Sobs: Number of observations of per point in time.

#' @export
make_ISVM_data <- function(ICC_DATA,
                           VOLA_DATA,
                           start_time = "08:20:00",
                           end_time = "07:59:59",
                           subset_maturities = FALSE,
                           N_subs_maturities = 3,
                           money_range = c(0.8,1.2),
                           subs = 5,
                           impute = TRUE,
                           verbose = FALSE,
                           threshold = 0.66,
                           state = c(0,1)) {

  ### subset data
  final_list_DT_subset <- ICC_DATA
  plot_data <- final_list_DT_subset$price_plot$data
  DT_states <- VOLA_DATA[timestamp %between% c(plot_data[, min(t)], plot_data[, (max(t) - 1)]) & instrument_name %in% final_list_DT_subset$vola_plot$data$variable] # t - 1  to make sure the last batch is included (as this would give 0 / negative maturity)
  start_date <- DT_states[,min(date)]
  end_date <- DT_states[,max(date)]
  DT_states <- DT_states[moneyness %between% money_range]
  DT_states <- DT_states[!is.na(last_price)]
  separate_batches(DT_states)
  ###
  if ((length(DT_states[,unique(batch_no)]) + 1) != nrow(final_list_DT_subset$train_data)) { # +1 as a quick hack. This should be handled better by proper subsetting
    message("Error: There are not as many batches with observations as there are states. You might want to adjust the parameter 't_diff_lim' in the function 'separate_batches'. If your data is in secondly frequency, that is currently not yet supported. ")
  } else if (any(state %in% final_list_DT_subset$states[,unique(state)])) {
    print(paste("Generating ISVM data for state", paste(state, collapse = ",")))
    DT_sub_states <- DT_states[batch_no %in% (which(final_list_DT_subset$states[,state] %in% state) - 1)]
    subs_mark_iv <- make_subset2(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "mark_iv")
    unique_timestamps <- unique(subs_mark_iv$plot_data$t)
    N_complete <- length(unique_timestamps)
    Sobs <- length(subs_mark_iv$train_data)
    d <- rep(0, N_complete)
    Sobs_list <- rep(Sobs, N_complete)

    DT_ISVM <- list("IV" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "mark_iv"),
                    "money" =  make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "log_moneyness"),
                    "mtr" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "date_to_maturity"),
                    "Sobs" = Sobs_list,
                    "cp" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "put_option"),
                    "d" = d,
                    "date" = unique_timestamps,
                    "price" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "last_price"),
                    "r" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "interest_rate")
    )
    DT_ISVM <- make_columns_equal(DT_ISVM)
    return(DT_ISVM)
  } else {
    message("Error: incorrect state")
  }
}

# for dealing with custom dataset

#' @export
make_ISVM_data2 <- function(ICC_DATA,
                           VOLA_DATA,
                           start_time = "08:20:00",
                           end_time = "07:59:59",
                           subset_maturities = FALSE,
                           N_subs_maturities = 3,
                           money_range = c(0.8,1.2),
                           subs = 5,
                           impute = TRUE,
                           verbose = FALSE,
                           threshold = 0.66,
                           state = c(0,1)) {

  ### subset data
  final_list_DT_subset <- ICC_DATA
  plot_data <- final_list_DT_subset$plot_data
  DT_states <- VOLA_DATA[timestamp %between% c(plot_data[, min(t)], plot_data[, (max(t))])]
  start_date <- DT_states[,min(date)]
  end_date <- DT_states[,max(date)]
  DT_states <- DT_states[moneyness %between% money_range]
  separate_batches(DT_states)
  if ((length(DT_states[,unique(batch_no)]) + 1) != nrow(final_list_DT_subset$train_data)) { # +1 as a quick hack. This should be handled better by proper subsetting
    message("Error: There are not as many batches with observations as there are states. You might want to adjust the parameter 't_diff_lim' in the function 'separate_batches'. If your data is in secondly frequency, that is currently not yet supported. ")
  } else if (any(unlist(state) %in% final_list_DT_subset$train_data[,unique(state)])) {
    print(paste("Generating ISVM data for state", paste(state, collapse = ",")))
    DT_sub_states <- DT_states[batch_no %in% (which(final_list_DT_subset$train_data[,state] %in% unlist(state)) - 1)]
    subs_mark_iv <- make_subset2(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "mark_iv")
    unique_timestamps <- unique(subs_mark_iv$plot_data$t)
    N_complete <- length(unique_timestamps)
    Sobs <- length(subs_mark_iv$train_data)
    d <- rep(0, N_complete)
    Sobs_list <- rep(Sobs, N_complete)

    DT_ISVM <- list("IV" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "mark_iv"),
                    "money" =  make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "log_moneyness"),
                    "mtr" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "date_to_maturity"),
                    "Sobs" = Sobs_list,
                    "cp" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "put_option"),
                    "d" = d,
                    "date" = unique_timestamps,
                    "price" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "last_price"),
                    "r" = make_ISVM_matrix(DT_sub_states, start_time = start_time, end_time = end_time, threshold = threshold, subset_maturities = subset_maturities, start_date = start_date, end_date = end_date, N_subs_date = subs, money_range = money_range, verbose = verbose, impute = impute, value_var = "interest_rate")
    )
    DT_ISVM <- make_columns_equal(DT_ISVM)
    return(DT_ISVM)
  } else {
    message("Error: incorrect state")
  }
}

