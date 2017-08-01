#' Render the graph for two rankings
#'
#' @param x,y ranking vectors of comparable items
#' @param other_rankings vector of items that might not be in `x` or `y`
#' @export
rankplot <- function(x, y, other_rankings = NULL) {
  DiagrammeR::render_graph(
    rankgraph(x, y, other_rankings)
  )
}

#' Create the graph object for two rankings
#'
#' This returns a graph object in the sense of the \code{DiagrammeR} package.
#' This allows for some customization prior to rendering.  You can read the
#' documentation here: \url{http://rich-iannone.github.io/DiagrammeR/graph_creation.html}.
#'
#' @param x,y ranking vectors of comparable items
#' @param other_rankings vector of items that might not be in `x` or `y`
#' @export
rankgraph <- function(x, y, other_rankings = NULL) {
  DiagrammeR::create_graph(
    graph_nodes(x, y, other_rankings),
    graph_edges(x, y, other_rankings)
  )
}

#' Create graph object for two distinct graph objects
#'
#' This takes to graph objects created by \code{rankgraph} and positions them
#' side by side.  To render the graph, use \code{DiagrammeR::render_graph}.
#'
#' @param x,y ranking vectors of comparable items
#' @param other_rankings vector of items that might not be in `x` or `y`
#' @export
combine_rankgraphs <- function(left, right) {
  xshift <- 1.5 * left$nodes_df$width[1] + tail(left$nodes_df$x, n=1)
  id_shift <- left$last_node
  new_nodes <- dplyr::mutate(right$nodes_df,
                             x = x + xshift,
                             id = id + id_shift)
  new_edges <- dplyr::mutate(right$edges_df,
                             from = from + id_shift,
                             to = to + id_shift)
  new_right <- DiagrammeR::create_graph(
    nodes_df = new_nodes,
    edges_df = new_edges)
  DiagrammeR::combine_graphs(left, new_right)
}

#' Get the unranked items for each list in the comparison
unranked <- function(x, y, other_rankings = NULL) {
  all_items <- unique(c(x, y, other_rankings))
  list(setdiff(all_items, x), setdiff(all_items,y))
}

#' Get the node labels for each ranking
node_labels <- function(x, y, other_rankings = NULL) {
  leftlabs <- x
  n_unranked <- purrr::map(unranked(x, y, other_rankings), length)
  if (n_unranked[1] > 0) {
    leftlabs <- append(leftlabs, paste(n_unranked[1], "UNRANKED"))
  }
  rightlabs <- y
  if (n_unranked[2] > 0) {
    rightlabs <- append(rightlabs, paste(n_unranked[2], "UNRANKED"))
  }
  list(leftlabs, rightlabs)
}

node_pos <- function(x, y, other_rankings = NULL) {
  ygap <- 0.5
  xpos <- rep(0, times = length(x))
  ypos <- seq(from = 0, by = -ygap, length.out = length(x))
  n_unranked <- purrr::map(unranked(x, y, other_rankings), length)
  labs <- unlist(node_labels(x, y, other_rankings))
  width <- max(0.5, 0.10 * nchar(labs))
  xgap <- 0.5 * width
  if (n_unranked[1] > 0) {
    xpos <- append(xpos, 0)
    ypos <- append(ypos, tail(ypos, 1)-1.5*ygap)
  }
  xpos <- append(xpos, rep(2 + xgap, times = length(y)))
  ypos <- append(ypos, seq(from = 0, by = -ygap, length.out = length(y)))
  if (n_unranked[2] > 0) {
    xpos <- append(xpos, 2 + xgap)
    ypos <- append(ypos, tail(ypos, 1)-1.5*ygap)
  }
  list(xpos, ypos)
}

graph_nodes <- function(x, y, other_rankings = NULL) {
  labs <- unlist(node_labels(x, y, other_rankings))
  pos <- node_pos(x, y, other_rankings)
  n_nodes <- length(labs)
  width <- max(0.5, 0.10 * nchar(labs))
  DiagrammeR::create_node_df(
    n = n_nodes,
    type = "item",
    nodes = 1:n_nodes,
    label = labs,
    x = pos[[1]],
    y = pos[[2]],
    shape = "rectangle",
    width = width
  )
}

graph_edges <- function(x, y, other_rankings = NULL) {
  labs   <- node_labels(x, y, other_rankings)
  shift  <- length(labs[[1]])
  left   <- 1:length(x)
  right  <- shift + purrr::map_dbl(x, function(j) ifelse(j %in% y, which(y == j), length(y) + 1))
  notinx <- shift + which(!(y %in% x))
  right  <- append(right, notinx)
  left   <- append(left, rep(shift, times = length(notinx)))
  DiagrammeR::create_edge_df(
    from = left,
    to   = right,
    arrowhead = "none",
    tailport = "e",
    headport = "w"
  )
}
