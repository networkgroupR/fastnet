#' Eigenvalue Score
#'
#' @description Calculate the eigenvalue centrality score of a graph.
#' @param g	The input network.
#' @details \code{metric.eigen.value} calculates the eigenvalue centrality score of graph \emph{g}.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references Bonacich, Phillip, and Paulette Lloyd. "Eigenvector-like measures of centrality for asymmetric relations." Social networks 23, no. 3 (2001): 191-201.
#' @references Borgatti, Stephen P. "Centrality and network flow." Social networks 27, no. 1 (2005): 55-71.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.eigen.value(x)}
#' @export
#' @import igraph


metric.eigen.value = function (g) {
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)

  ig <- simplify(as.undirected(graph_from_adj_list(g)))

  EC <- eigen_centrality(ig, directed = FALSE, scale = TRUE)  ## same as unscaled spectrum(g)

  ecvalue <- EC$value

  ecvalue

}
