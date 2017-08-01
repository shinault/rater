#' Return a variant of the Kendall tau-a correlation for two rankings
#'
#' The Kendall τ-a correlation is a measure of similarity between two rankings
#' of the same items.  This function is consistent with this definition, as well
#' as a modification for rankings with some distinct items or having different
#' length.
#'
#' @param x,y ranking vectors of comparable items
#' @param unranked_items vector of items that might not be in `x` or `y`
#' @return Numeric value the is the modified Kendall tau rank correlation
#' @export
kendall <- function(x, y, unranked_items=NULL) {
  assertthat::assert_that(!anyDuplicated(x), !anyDuplicated(y),
                          msg = "duplicate entry in ranking")
  # Relevant item sets
  all_items <- unique(c(x, y, unranked_items))
  x_unranked <- setdiff(all_items, x)
  y_unranked <- setdiff(all_items, y)
  # Ranked pairs from each ranking vector
  x_pairs <- get_pairs(x, x_unranked)
  y_pairs <- get_pairs(y, y_unranked)
  # Numeric values and return value
  n_items <- length(all_items)
  n_conc <- length(intersect(x_pairs, y_pairs))
  n_disc <- length(intersect(x_pairs, Map(rev, y_pairs)))
  n_unlabelled <- choose(length(x_unranked), 2) + choose(length(y_unranked), 2)
  (2*n_conc - choose(n_items, 2)) / (n_conc + n_disc)
}

#' Auxiliary function, not for export
#' Returns a list of 2-vectors, [(x1,y1),...,(xn,yn)]
zipit <- function(x, y) {
  allpairs <- purrr::map(x, function(xi) purrr::map(y, function(yi) c(xi, yi)))
  unlist(allpairs, recursive = FALSE)
}

#' Auxiliary function, not for export
#' Returns a list of 2-vectors for vectors x, y:
#' (x_i, x_j) for i < j and (x_k, y_l) for all k, l
#' In the ranking interpretation x is the ranking list and y is the list of
#' items that is not ranked by x.
get_pairs <- function(x, y) {
  in_x_pairs <- combn(x,2)
  all_pairs <- union(lapply(seq_len(ncol(in_x_pairs)), function(j) in_x_pairs[,j]),
                   zipit(x, y))
  all_pairs
}
