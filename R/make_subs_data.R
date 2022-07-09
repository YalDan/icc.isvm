#' Make subset data
#'
#' Generates a subset data list that can be used for ICC and ISVM calculation.
#'
#' @import data.table
#' @import lubridate
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param DATE A vector of dates to subset from.
#' @param start_time When does the trading day start? Defaults to "08:20:00" (Deribit options expire at "08:00:00").
#' @param end_time When does the trading day end? Defaults to "07:59:59" (Deribit options expire at "08:00:00").
#' @param N_subs_date Number of dates to subset the input data from.
#' @param threshold How many percent of observations are allowed to be missing for each individual instrument over the observed timeframe? E.g. if set to 0.9, we allow 10% of observations to be missing. Defaults to 0.66.
#' @param value_var The variable for which we want to generate a clustering data set.
#' @param money_range Vector with lower and upper limit of moneyness values to consider. The natural boundary would be c(0,Inf). Defaults to c(0.8,1.2).
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered? Defaults to TRUE.
#' @param N_subs_maturities Integer value. If 'subset_maturities' = TRUE this will select how many of the closest maturities should be considered. Defaults to 3.
#' @param min_maturity Integer value. Optional and only applies if subset_maturities = FALSE. The lower boundary of maturities to be considered in days. Defaults to 15 days.
#' @param max_maturity Integer value. Optional and only applies if subset_maturities = FALSE. The upper boundary of maturities to be considered in days. Defaults to 60 days.
#' @param verbose Boolean value. Should the function be verbose?
#' @param impute Boolean value. Should missing observations be imputed? Defaults to TRUE.
#'
#' @return plot_data: A dataset for plotting the price of the underyling index.
#' @return train_data. A dataset for calculating ICC or ISVM values.

#' @export
make_subset <- function(DATA,
                        DATE = NA,
                        start_time = "08:20:00",
                        end_time = "07:59:59",
                        N_subs_date = 2,
                        threshold = 0.66,
                        value_var = "mark_iv",
                        money_range = c(0.8,1.2),
                        subset_maturities = TRUE,
                        N_subs_maturities = 3,
                        min_maturity = 15,
                        max_maturity = 60,
                        verbose = TRUE,
                        impute = FALSE){

  ## specify subset data ##
  if (is.na(DATE)) {
    message("Warning: DATE parameter is NA. Data was not subsetted. Please make sure this is how you wanted to use this function.")
    start_date <- DATA[, first(date)]
    end_date <- DATA[, last(date)]
  } else {
    start_date <- DATA[date %in% DATE, last(date)] - N_subs_date + 1
    end_date <- DATA[date %in% DATE, last(date)] + 1
  }
  start <- lubridate::ymd_hms(paste(start_date,  " ", start_time, sep = ""))
  end <- lubridate::ymd_hms(paste(end_date,  " ", end_time, sep = ""))
  ## ##

  ## get reduced dataset ##
  instrument_candidates <- DATA[timestamp %between% c(start,end)]
  all_maturities <- DATA[timestamp %between% c(start,end),sort(unique(maturity))]
  subs_maturity <- all_maturities[all_maturities > end_date - 1][1:N_subs_maturities] # end_date - 1 because we want to include maturities expiring on the same day

  if (subset_maturities) {
    maturity_candidates <- subs_maturity
  } else if (!is.na(min_maturity) & !is.na(max_maturity)) {
    maturity_candidates <- DATA[timestamp %between% c(start,end) & date_to_maturity %between% c(min_maturity, max_maturity),sort(unique(maturity))]
  } else {
    maturity_candidates <- all_maturities
  }

  instrument_candidate_names <- instrument_candidates[moneyness %between% money_range &
                                                   maturity %in% maturity_candidates,
                                                 sort(unique(instrument_name))]

  TS_price <- make_plot_data(DATA[instrument_name %in% instrument_candidate_names &
                                     timestamp %between% c(start,end) &
                                     maturity %in% maturity_candidates,
                                   c("timestamp", "index_price")]
  )
  ## ##

  #### prepare ICC ####

  TS_options <- make_ICC_data(DATA[instrument_name %in% instrument_candidate_names & timestamp %between% c(start,end)],
                              N_complete = nrow(TS_price),
                              value_var = value_var,
                              impute = impute,
                              verbose = verbose,
                              threshold = threshold)

  list_DT_subset <- list("plot_data" = TS_price,"train_data" = TS_options)
  return(list_DT_subset)
}

#' @export
make_subset2 <- function(DATA,
                         start_time = "08:20:00",
                         end_time = "07:59:59",
                         start_date = NA,
                         end_date = NA,
                         N_subs_date = 2,
                         threshold = 0.66,
                         value_var = "mark_iv",
                         money_range = c(0.8,1.2),
                         subset_maturities = TRUE,
                         N_subs_maturities = 3,
                         min_maturity = 15,
                         max_maturity = 60,
                         verbose = TRUE,
                         impute = FALSE){

  start <- lubridate::ymd_hms(paste(start_date,  " ", start_time, sep = ""))
  end <- lubridate::ymd_hms(paste(end_date,  " ", end_time, sep = ""))
  ## ##

  ## get reduced dataset ##
  instrument_candidates <- DATA[timestamp %between% c(start,end)]
  all_maturities <- DATA[timestamp %between% c(start,end),sort(unique(maturity))]
  subs_maturity <- all_maturities[all_maturities > end_date - 1][1:N_subs_maturities] # end_date - 1 because we want to include maturities expiring on the same day

  if (subset_maturities) {
    maturity_candidates <- subs_maturity
  } else if (!is.na(min_maturity) & !is.na(max_maturity)) {
    maturity_candidates <- DATA[timestamp %between% c(start,end) & date_to_maturity %between% c(min_maturity, max_maturity),sort(unique(maturity))]
  } else {
    maturity_candidates <- all_maturities
  }

  instrument_candidate_names <- instrument_candidates[moneyness %between% money_range &
                                                        maturity %in% maturity_candidates,
                                                      sort(unique(instrument_name))]

  TS_price <- make_plot_data(DATA[instrument_name %in% instrument_candidate_names &
                                    timestamp %between% c(start,end) &
                                    maturity %in% maturity_candidates,
                                  c("timestamp", "index_price")]
  )
  ## ##

  #### prepare ICC ####

  TS_options <- make_ICC_data(DATA[instrument_name %in% instrument_candidate_names & timestamp %between% c(start,end)],
                              N_complete = nrow(TS_price),
                              value_var = value_var,
                              impute = impute,
                              verbose = verbose,
                              threshold = threshold)

  list_DT_subset <- list("plot_data" = TS_price, "train_data" = TS_options)
  return(list_DT_subset)
}
