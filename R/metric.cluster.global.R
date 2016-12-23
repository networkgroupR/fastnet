#' Global Clustering Coefficient
#'

#' @description Calculate the global clustering coefficient of a graph.
#' @param g The input network.
#' @details The global clustering coefficient measures the ratio of (closed) triples versus the total number of all possible triples in network \emph{g}. \code{metric.cluster.global()} calculates the global clustering coefficient of \emph{g}.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references Wasserman, Stanley, and Katherine Faust. Social network analysis: Methods and applications. Vol. 8. Cambridge university press, 1994.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.cluster.global(x)}
#' @import igraph
#' @export
#'

metric.cluster.global <- function(g){

  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)

  ig <- simplify(as.undirected(graph_from_adj_list(g)))

  CC <- transitivity(ig)

  CC

}
