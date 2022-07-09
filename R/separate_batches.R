#' Separate batches
#'
#' Helper function for separating batches of observations that are sampled approximately at the same time.
#'
#' @import data.table
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param t_diff_lim Numeric value. Limit after which a new batch is started. Defaults to 250.
#'
#' @return Separates data by batches based on time difference

#' @export
separate_batches <- function(DATA, t_diff_lim = 250) {
  setkey(DATA, timestamp)
  DATA[,t_diff := c(0,round(diff(timestamp)))]
  DATA[,new_batch := 0]
  DATA[t_diff > t_diff_lim, new_batch := 1]
  DATA[,"batch_no" := cumsum(new_batch)]
}
