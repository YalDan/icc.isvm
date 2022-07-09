#' Make columns equal
#'
#' Makes sure all provided input columns are of same length based on common entries.
#'
#' @import data.table
#'
#' @param DATA A data.table.
#' @param mod_cols The columns that shpuld be checked for common values.
#'
#' @return Returns the data.table with equal length columns.

#' @export
make_columns_equal <- function(DATA, mod_cols = c("IV", "money", "mtr", "cp", "r")) {
  colnames <- sapply(DATA, colnames)
  mod_list <- lapply(mod_cols, function(x) colnames[[x]])
  intersect_names <- Reduce(intersect, mod_list)
  new_matrix_list <- lapply(mod_cols, function(x) {
    as.matrix(data.table(DATA[[x]])[, ..intersect_names])
  })
  names(new_matrix_list) <- mod_cols
  for (i in mod_cols) {
    DATA[i] = new_matrix_list[i]
  }
  return(DATA)
}
