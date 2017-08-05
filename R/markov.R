fill <- function(x) {
  if (all(x == 0)) x + 1
  else x
}

#' Compute ratings from a voting matrix
#'
#' This function computes ratings from a voting matrix.  The process is the same
#' as the PageRank algorithm.
#'
#' @param vote_mtx a matrix containing the voting information.  The matrix
#' entry \eqn{A[i,j]} encodes the votes cast by candidate \eqn{i} or candidate
#' \eqn{j}
#' @param beta a numeric parameter to guarantee an irreducible Markov matrix
#' @return A vector containing the Markov ratings for the candidates
#' @export
markov <- function(vote_mtx, beta) {
  n <- nrow(vote_mtx)
  # Matrix transformations.
  # apply does not really allow for replacing rows in a matrix, which we need.
  vote_adj <- t(apply(vote_mtx, 1, fill))
  rowvotes <- rowSums(vote_adj)
  stc_mtx <- vote_adj * (1/rowvotes)
  irr_mtx <- stc_mtx*beta + matrix(1, n, n)*(1-beta)/n
  # The method to find the stationary distribution is in Resnick's book.
  ratings <- solve(t(diag(1, n) - irr_mtx + matrix(1, n, n)), matrix(1, n, 1))
  colnames(ratings) <- "MarkovRating"
  ratings
}
