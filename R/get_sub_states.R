#' Make subset state list
#'
#' Generates a sorted list with all states in provided ICC data.
#'
#' @param DATA The output "states" from the "get_clusters" function.
#'
#' @return A list with all states.

#' @export

get_sub_states <- function(DATA) {
  sub_states <- DATA[,sort(unique(state))]
  sub_states_list <- as.list(rev(sub_states))
  sub_states_list[[length(sub_states_list) + 1]] <- sub_states
  return(rev(sub_states_list))
}
