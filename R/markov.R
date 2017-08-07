#' Compute ratings from a stochastic matrix
#'
#' This function computes ratings from a stochastic matrix.
#' The stochastic matrix can be made irreducible or slightly perturbed by
#' choosing \eqn{0 < beta < 1}.
#'
#' @param stoc_mtx a matrix containing the transition information.  The matrix
#' entry \eqn{A[i,j]} encodes the transistion probability from candidate \eqn{i}
#' to candidate \eqn{j}
#' @param beta a numeric parameter to guarantee an irreducible Markov matrix
#' @return A vector containing the Markov ratings for the candidates
#' @export
markov <- function(stoc_mtx, beta = 1) {
  n <- nrow(stoc_mtx)
  irr_mtx <- stoc_mtx*beta + matrix((1-beta)/n , n, n)
  # The method to find the stationary distribution is in Resnick's book.
  ratings <- solve(t(diag(1, n) - irr_mtx + matrix(1, n, n)), matrix(1, n, 1))
  ratings
}
