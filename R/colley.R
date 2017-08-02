#' Compute ratings from Colley matrix and net win differential
#'
#' This function computes the Colley ratings for a league from a Colley matrix
#' and the net number of wins by each team
#'
#' @param colley_mtx a Colley matrix containing the games played data
#' @param net_rec a vector containing the net record of each team
#' @return \code{colley} returns a vector with the Colley rating for
#' each team.
#' @export
colley <- function(colley_mtx, net_rec) {
  solve(colley_mtx, 1 + 0.5*net_rec)
}
