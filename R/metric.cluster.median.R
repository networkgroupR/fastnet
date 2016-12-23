#' Median Local Clustering Coefficient

#' @description Calculate the median local clustering coefficient of a graph.
#' @param g The input network.
#' @details The local clustering coefficient of a node is the ratio of the triangles connected to the node and the triples centered on the \code{node.metric.cluster.median()} calculates the (estimated) median clustering coefficient for all nodes in graph \emph{g} with a justified error.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references Wasserman, Stanley, and Katherine Faust. Social network analysis: Methods and applications. Vol. 8. Cambridge university press, 1994.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.1)
#' metric.cluster.median(x)}
#' @importFrom igraph as.undirected
#' @importFrom igraph graph_from_adj_list
#' @importFrom igraph simplify
#' @importFrom igraph transitivity
#' @export


metric.cluster.median <- function(g){

  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)

  ig <- simplify(as.undirected(graph_from_adj_list(g)))

  LCC <- transitivity(ig, type = "local", isolates = "zero")

  medianlcc <- stats::median(LCC)

  medianlcc

}
