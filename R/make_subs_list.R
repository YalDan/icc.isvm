#' Make subset data list
#'
#' List wrapper for "make_subset_data" function that can be used for ICC and ISVM calculation.
#'
#' @import data.table
#' @import lubridate
#' @import utils
#' @import parallel
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param N_subs_date Date value. Number of dates to subset the input data from. Defaults to "as.Date("1970-01-01")".
#' @param start_date From which day on should a list of subsets be generated?
#' @param subset_maturities Boolean value. Should only a subset of the closest maturities be considered? Defaults to TRUE.
#' @param N_subs_maturities Integer value. If 'subset_maturities' = TRUE this will select how many of the closest maturities should be considered. Defaults to 3.
#' @param threshold How many percent of observations are allowed to be missing for each individual instrument over the observed timeframe? E.g. if set to 0.9, we allow 10% of observations to be missing. Defaults to 0.66.
#' @param money_range Vector with lower and upper limit of moneyness values to consider. The natural boundary would be c(0,Inf). Defaults to c(0.8,1.2).
#' @param verbose Boolean value. Should the function be verbose?
#' @param impute Boolean value. Should missing observations be imputed? Defaults to TRUE.
#' @param parallel Boolean. Should the results be computed in parallel? Won't work on Windows machines. Defaults to TRUE.
#' @param n_cores Number of cores. This only applies if parallel = TRUE. Defaults to 50% of detected cores.
#' @param last_only Should only the last days of the dataset be returned? If set to TRUE this will give exactly as many days as specified in "N_subs_date". Defaults to FALSE.
#'#'
#' @return plot_data: A dataset for plotting the price of the underyling index.
#' @return train_data. A dataset for calculating ICC or ISVM values.

#' @export
make_subset_list <- function(DATA,
                             N_subs_date = 5,
                             start_date = as.Date("1970-01-01"),
                             subset_maturities = TRUE,
                             N_subs_maturities = 3,
                             threshold = 0.66,
                             money_range = c(0.8,1.2),
                             verbose = TRUE,
                             impute = FALSE,
                             parallel = TRUE,
                             n_cores = detectCores()*0.5,
                             last_only = F){

  unique_dates <- DATA[date >= start_date + N_subs_date,sort(unique(date))]
  print("Making subset list...")

  if (last_only) {
    print("'Last only' option was set to 'True'. Making only a subset from the last date on ")
    list_DT_subset <- make_subset(DATA,
                                  N_subs_date = N_subs_date,
                                  subset_maturities = subset_maturities,
                                  N_subs_maturities = N_subs_maturities,
                                  money_range = money_range,
                                  verbose = verbose,
                                  impute = impute,
                                  threshold = threshold)
    train_data <- list_DT_subset$train_data
    plot_data <- list_DT_subset$plot_data
    subs_date <- N_subs_date

    final_list_DT_subset <- list("plot_data" = plot_data,"train_data" = train_data, "N_subs_date" = subs_date)
    final_list_DT_subset <- list(final_list_DT_subset)
    names(final_list_DT_subset) <- DATA[,last(date)]
    return(final_list_DT_subset)
  }

  if (parallel) {
    subset_list <- parallel::mclapply(unique_dates, function(x){
      list_DT_subset <- make_subset(DATA,
                                    N_subs_date = N_subs_date,
                                    subset_maturities = subset_maturities,
                                    N_subs_maturities = N_subs_maturities,
                                    money_range = money_range,
                                    DATE = x,
                                    verbose = verbose,
                                    impute = impute,
                                    threshold = threshold)
      train_data <- list_DT_subset$train_data
      plot_data <- list_DT_subset$plot_data
      subs_date <- N_subs_date
      final_list_DT_subset <- list("plot_data" = plot_data,"train_data" = train_data, "N_subs_date" = subs_date)

      return(final_list_DT_subset)
    }, mc.cores = n_cores)
  } else {
    pb <- txtProgressBar(min = 0, max = length(unique_dates), initial = 0, style = 3)
    stepi <- 0

    subset_list <- lapply(unique_dates, function(x){
      list_DT_subset <- make_subset(DATA,
                               N_subs_date = N_subs_date,
                               subset_maturities = subset_maturities,
                               N_subs_maturities = N_subs_maturities,
                               money_range = money_range,
                               DATE = x,
                               verbose = verbose,
                               impute = impute,
                               threshold = threshold)
      train_l <- list_DT_subset$train_data
      plot_data <- list_DT_subset$plot_data
      subs_date <- N_subs_date
      final_list_DT_subset <- list("plot_data" = plot_data,"train_data" = train_l, "N_subs_date" = subs_date)

      stepi <<- stepi + 1
      setTxtProgressBar(pb,stepi)

      return(final_list_DT_subset)
    })
  }
  names(subset_list) <- unique_dates

  return(subset_list)
}
