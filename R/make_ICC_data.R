#' Make ICC data
#'
#' Generates a dataset for ICC computation.
#'
#' @import xts
#' @import imputeTS
#' @import data.table
#' @import lubridate
#' @import zoo
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param N_complete Maximum possible number of observations during the observed timeframe. This value is used for determining which instruments are above the specified threshold.
#' @param value_var The variable for which we want to generate a clustering data set.
#' @param verbose Boolean value. Should the function be verbose?
#' @param impute Boolean value. Should missing observations be imputed? Defaults to TRUE.
#' @param threshold How many percent of observations are allowed to be missing for each individual instrument over the observed timeframe? E.g. if set to 0.9, we allow 10% of observations to be missing. Defaults to 0.66.
#'
#' @return Output: a data.table that can be used for ICC.

#' @export
make_ICC_data <- function(DATA, N_complete, value_var = "mark_iv", verbose = FALSE, impute = FALSE, threshold = 0.66){

  cols <- c("timestamp", "instrument_name", value_var)
  TS_deribit <- DATA[, ..cols]
  TS_deribit <- TS_deribit[complete.cases(TS_deribit)]
  instruments_final <- TS_deribit[,unique(instrument_name)]

  list_XTS_deribit <- lapply(seq_along(instruments_final), function(x){
    tmp_data <- TS_deribit[instrument_name %in% instruments_final[x]]
    tmp_XTS_deribit <- dcast(data = TS_deribit[instrument_name %in% instruments_final[x]],
                             formula = timestamp ~ instrument_name,
                             value.var = value_var,
                             fun.aggregate = sum,
                             fill = NA_real_)

    if (nrow(tmp_XTS_deribit[complete.cases(tmp_XTS_deribit)]) == 0) {
      if (verbose) {
        print(paste("Data omitted: instrument", instruments_final[x], "has too many missing values."))
      }
      return(data.table())
    }

    tmp_XTS_deribit <- xts(tmp_XTS_deribit[,!c("timestamp")], order.by = tmp_XTS_deribit[,timestamp])

    if (nrow(tmp_XTS_deribit) < N_complete*threshold) {
      if (verbose) {
        print(paste("Data omitted: instrument",
                    instruments_final[x],
                    "has less than",
                    threshold*100,
                    "% complete observations:",
                    nrow(tmp_XTS_deribit), "of", N_complete,
                    "(", round(nrow(tmp_XTS_deribit)/N_complete, 2)*100, "% )"))
      }
      return(data.table())
    }

    tmp_XTS_deribit <- xts::to.period(tmp_XTS_deribit, period = "minutes", k = 20)
    tmp_XTS_deribit <- xts::align.time(tmp_XTS_deribit, timespan = 20)
    index(tmp_XTS_deribit) <- lubridate::round_date(index(tmp_XTS_deribit), "20 mins")
    tmp_XTS_deribit <- data.table(index(tmp_XTS_deribit), instruments_final[x], tmp_XTS_deribit$tmp_XTS_deribit.Close)

    names(tmp_XTS_deribit) <- c("t", "instrument_name", value_var)

    if (!value_var %in% c("put_option", "interest_rate")) {
      vec_var <- round(var(tmp_XTS_deribit[,..value_var], na.rm = T), digits = 7)
      if (is.na(vec_var)) {vec_var <- 0}

      if (vec_var == 0) {
        if (verbose) {
          print(paste("Data omitted: instrument", instruments_final[x], "has 0 variance."))
        }
        return(data.table())
      }
    }
    return(tmp_XTS_deribit)
  })
  XTS_deribit <- rbindlist(list_XTS_deribit)

  ## get different batches ##
  train_l <- dcast(XTS_deribit, t ~ instrument_name, value.var = value_var, fun = mean)

  if (impute) {
    train_l <- data.table(train_l[,"t"], do.call(cbind, lapply(train_l[,!"t"], imputeTS::na_locf)))
  }

  return(train_l)
}
