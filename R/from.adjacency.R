#' Adjacency Matrix to \code{fastnet}
#'
#' @description Transform an adjacency matrix to an ego-centric list form used in \code{fastnet}.
#' @param adj.mat	The input adjacency matrix
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Christian Llano.
#' @examples
#' adj.mat <- matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0), nrow = 4, ncol = 4)
#' g <- from.adjacency(adj.mat)
#'
#' @export

from.adjacency <- function (adj.mat){

  if (!is.matrix(adj.mat)) stop("Parameter 'adj.mat' must be a matrix", call. = FALSE)

  net <- igraph::as_adj_list( igraph::graph_from_adjacency_matrix(adj.mat), mode = "out" )

   net
}



