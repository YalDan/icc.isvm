#' Check Invertibility
#'
#' Verifies whether the input matrix is invertible.
#'
#' @import data.table
#'
#' @param m A NxN matrix.
#'
#' @return Boolean. Outputs TRUE if the matrix is invertible.

#' @export
check_invertible <- function(m) {
  "matrix" %in% class(try(solve(m),silent = TRUE))
}
