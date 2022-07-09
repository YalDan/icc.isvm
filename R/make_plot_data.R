#' Make plot data
#'
#' Helper function for generating plot data for the underlying index.
#'
#' @import data.table
#' @import xts
#' @import lubridate
#'
#' @param DATA A data.table with structure as provided in the example.
#'
#' @return Returns a data.table with the price of the underlying index.

#' @export
make_plot_data <- function(DATA) {
  tmp_data <- DATA[,c("timestamp", "index_price")]
  tmp_data <- xts::to.period(xts::xts(tmp_data[,index_price], order.by = tmp_data[,(timestamp + 1)]), period = "minutes", k = 20) # timestamp + 1 to avoid issues with e.g. xx:39:59 where it should actually be xx:40:00
  tmp_data <- xts::align.time(tmp_data, timespan = 20)
  index(tmp_data) <- lubridate::round_date(index(tmp_data), "20 mins")
  names(tmp_data) <- c("open","high", "low", "close")
  tmp_data <- data.table("t" =  index(tmp_data), tmp_data)
  return(tmp_data)
}
