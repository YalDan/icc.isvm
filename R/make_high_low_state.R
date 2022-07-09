#' Make High and Low State
#'
#' For a time series clustered by ICC when K=2, this appends to the input data which state is the low and which state is the high mean volatility state.
#'
#' @import data.table
#'
#' @param DATA The input data. Must be a data.table
#' @param low_state The state with the lower mean. Either 0 or 1.
#' @param high_state The state with the higher mean. Either 0 or 1.
#'
#' @return Boolean. Outputs TRUE if the matrix is invertible.

#' @export
make_high_low_state <- function(DATA, low_state, high_state){
  DATA[state == low_state, `:=`(state = 2, which.state = "low")]
  DATA[state == high_state, `:=`(state = 3, which.state = "high")]
  DATA[state == 2, state := 0]
  DATA[state == 3, state := 1]
  DATA[, state := as.factor(state)]
  return(DATA)
}
