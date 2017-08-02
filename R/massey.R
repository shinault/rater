#' Compute ratings from Massey matrix and net point differentials
#'
#' This function computes the Massey ratings for a league from a Massey matrix
#' and the point differentials for each team.
#'
#' @param massey_mtx a matrix containing the data on number of games played by
#' each team, against other teams
#' @param point_diffs a vector containing the net point differentials for each
#' team in the games played
#' @return \code{massey_ratings} returns a vector containing the Massey ratings
#' for each team.
#' @export
massey <- function(massey_mtx, point_diffs) {
  n <- nrow(massey_mtx)
  solve(rbind(massey_mtx[1:(n-1),], 1), c(point_diffs[1:(n-1)], 0))
}


#' Compute the offensive, defensive, and total ratings for each team
#'
#' This function computes the Massey offensive and defensive ratings for each
#' team.  The total rating, which is just the sum of the offensive and
#' defensive rating, is also included.
#'
#' @param massey_mtx a matrix containing the data on number of games played
#' by each team, against other teams
#' @param points_for a vector containing the points scored by each team in the
#' games played
#' @param points_against a vector containing the points scored against each team
#' in the games played
#' @return \code{massey_adv} returns a vector containing the offensive,
#' defensive, and total Massey ratings for each team.
#' @export
massey_adv <- function(massey_mtx, points_for, points_against) {
  point_diffs <- points_for - points_against
  ratings <- massey(massey_mtx, point_diffs)
  def_ratings <- solve(abs(massey_mtx),
                       diag(massey_mtx) * ratings - points_for)
  cbind(ratings,
        defensive = def_ratings,
        offensive = ratings-def_ratings)
}
