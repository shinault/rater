#' Compute the Offense-Defense ratings from a score matrix
#'
#' This function computes ratings from a score matrix.
#'
#' @param score_mtx a matrix containing score information. The matrix
#' entry \eqn{A[i,j]} encodes the score for opponent \eqn{i} against \eqn{j}.
#' @param tol the stopping parameter for convergence.  The iterative algorithm
#' stops running once the improvement in an iteration is below the threshold for
#' all ratings.
#' @return a data frame with columns \code{Off}, \code{Def}, and \{Rtg}
#' @export
od <- function(score_mtx, tol=1e-6) {
  off_prev <- rep(1, times=nrow(score_mtx))
  def_prev <- rep(1, times=nrow(score_mtx))
  repeat {
    def <- score_mtx %*% (1 / (t(score_mtx) %*% (1 / def_prev)))
    off <- t(score_mtx) %*% (1 / def)
    if (max(abs(c(off - off_prev, def - def_prev))) < tol) {
      break
    } else {
      off_prev <- off
      def_prev <- def
    }
  }
  data.frame(Off=off, Def=def, Rtg=off/def)
}
