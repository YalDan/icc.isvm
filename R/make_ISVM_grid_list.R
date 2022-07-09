#' Make ISVM grid list
#'
#' Helper function for generating ISVM plots
#'
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import scales
#'
#' @param ISVM_list List of ISVM plots
#' @param variable The function for which the plots should be generated
#'
#' @return Returns a list of plots

#' @export
make_ISVM_grid_list <- function(ISVM_list, variable) {

  if (variable == "gamma") {
    grid_list <- lapply(ISVM_list$ISVM[length(ISVM_list$ISVM):2], function(x) x[[variable]] + geom_point(size = 4)  + ylab("") + theme(axis.title.x = element_text(size = 42), axis.text = element_text(size = 26)))
    grid_list[[length(grid_list) + 1]] <- ISVM_list$ISVM[[1]][[variable]] + geom_point(size = 4)  + theme(axis.title.x = element_text(size = 42), axis.title.y = element_text(size = 36), axis.text = element_text(size = 26))
  } else {
    grid_list <- lapply(ISVM_list$ISVM[length(ISVM_list$ISVM):2], function(x) x[[variable]] + ylab(""))
    grid_list[[length(grid_list) + 1]] <- ISVM_list$ISVM[[1]][[variable]] + theme(axis.title.y = element_text(size = 18))
  }
  grid_list <- rev(grid_list)
  return(grid_list)
}
