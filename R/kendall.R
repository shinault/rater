#' Return a variant of the Kendall tau-a correlation for two rankings
#'
#' The Kendall τ-a correlation is a measure of similarity between two rankings
#' of the same items.  This function is consistent with this definition, as well
#' as a modification for rankings with some distinct items.
#'
#' @param x,y ranking vectors of comparable items
#' @param unranked_items vector of items that might not be in `x` or
#' @return Numeric value the is the modified Kendall tau rank correlation
#' @export
kendall <- function(x, y, unranked_items=NULL) {
  assertthat::assert_that(!anyDuplicated(x), !anyDuplicated(y),
                          msg = "duplicate entry in ranking")
  all_items <- unique(c(x, y, unranked_items))
  n_items <- length(all_items)
  x_unranked <- setdiff(all_items, x)
  y_unranked <- setdiff(all_items, y)
  x_pairs <- combn(x,2)
  x_pairs <- union(lapply(seq_len(ncol(x_pairs)), function(j) x_pairs[,j]),
                   zipit(x, x_unranked))
  y_pairs <- combn(y,2)
  y_pairs <- union(lapply(seq_len(ncol(y_pairs)), function(j) y_pairs[,j]),
                   zipit(y, y_unranked))
  n_conc <- length(intersect(x_pairs, y_pairs))
  n_disc <- length(intersect(x_pairs, Map(rev, y_pairs)))
  n_unlabelled <- choose(length(x_unranked), 2) + choose(length(y_unranked), 2)
  (2*n_conc - choose(n_items, 2)) / (n_conc + n_disc)
}

#' Auxiliary function.
zipit <- function(x, y) {
  allpairs <- purrr::map(x, function(xi) purrr::map(y, function(yi) c(xi, yi)))
  unlist(allpairs, recursive = FALSE)
}
