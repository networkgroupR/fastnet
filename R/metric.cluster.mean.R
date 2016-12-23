#' Mean Local Clustering Coefficient
#'

#' @description Calculate the average local clustering coefficient of a graph.
#' @param g The input network.
#' @details The local clustering coefficient of a node is the ratio of the triangles connected to the node and the triples centered on the \code{node.metric.cluster.mean()} calculates the (estimated) average clustering coefficient for all nodes in graph \emph{g} with a justified error.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references Wasserman, Stanley, and Katherine Faust. Social network analysis: Methods and applications. Vol. 8. Cambridge university press, 1994.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(n = 1000, ncores = 3, p =  0.06)
#' metric.cluster.mean(x) }
#' @export
#' @importFrom igraph as.undirected
#' @importFrom igraph graph_from_adj_list
#' @importFrom igraph simplify
#' @importFrom igraph transitivity
#'


metric.cluster.mean <- function(g){
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)

  ig <- simplify(as.undirected(graph_from_adj_list(g)))

  LCC <- transitivity(ig, type = "local", isolates = "zero")

  meanlcc <- mean(LCC)

  meanlcc

}
