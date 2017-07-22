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

unranked <- function(x, y, other_rankings = NULL) {
  all_items <- unique(c(x, y, other_rankings))
  list(setdiff(all_items, x), setdiff(all_items,y))
}

node_labels <- function(x, y, other_rankings = NULL) {
  labs <- x
  n_unranked <- purrr::map(unranked(x, y, other_rankings), length)
  if (n_unranked[1] > 0) {
    labs <- append(labs, paste(n_unranked[1], "unranked"))
  }
  labs <- append(labs, y)
  if (n_unranked[2] > 0) {
    labs <- append(labs, paste(n_unranked[2], "unranked"))
  }
  labs
}

node_pos <- function(x, y, other_rankings = NULL) {
  gap <- 0.5
  height <- 2*max(length(x), length(y))
  xpos <- rep(0, times = length(x))
  ypos <- seq(from = height, by = -gap, length.out = length(x))
  n_unranked <- purrr::map(unranked(x, y, other_rankings), length)
  if (n_unranked[1] > 0) {
    xpos <- append(xpos, 0)
    ypos <- append(ypos, tail(ypos, 1)-gap)
  }
  xpos <- append(xpos, rep(2, times = length(y)))
  ypos <- append(ypos, seq(from = height, by = -gap, length.out = length(y)))
  if (n_unranked[2] > 0) {
    xpos <- append(xpos, 2)
    ypos <- append(ypos, tail(ypos, 1)-gap)
  }
  list(xpos, ypos)
}

fighter_nodes <- function(x, y, other_rankings = NULL) {
  labs <- node_labels(x, y, other_rankings)
  pos <- node_pos(x, y, other_rankings)
  n_nodes <- length(labs)
  DiagrammeR::create_node_df(
    n = n_nodes,
    type = "item",
    nodes = 1:n_nodes,
    label = labs,
    x = pos[[1]],
    y = pos[[2]],
    shape = "rectangle"
  )
}

#' Auxiliary function.
zipit <- function(x, y) {
  allpairs <- purrr::map(x, function(xi) purrr::map(y, function(yi) c(xi, yi)))
  unlist(allpairs, recursive = FALSE)
}
