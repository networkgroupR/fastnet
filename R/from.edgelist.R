#' Edgelist to \code{fastnet}
#'
#' @description Transfer an edgelist to an ego-centric list form used in \code{fastnet}.
#' @param edgelist The edge list, a vector with even number of entries.
#' @details The \emph{n} nodes are placed on a circle and each node is connected to the nearest \emph{k} neighbors.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Christian Llano.
#' @examples
#' edgelist <- c(1, 3, 2, 3, 3, 4, 5, 6,5,7)
#' g <- from.edgelist(edgelist)
#' @export

from.edgelist <- function(edgelist) {

  if(!is.numeric(edgelist)) stop("Parameter 'edgelist' must be a vector", call. = FALSE)
  if(length(edgelist)%%2!=0) stop("Paramater 'edgelist must be a vector with even number of entries", call.=FALSE)

  net <- igraph::as_adj_list(igraph::make_graph(edgelist), mode = "out")

  net

}


