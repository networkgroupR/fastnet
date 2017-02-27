#' Transform Network from \code{igraph}  to \code{fastnet} Format
#'
#' @description Transform \code{igraph} object to an ego-centric list form used in \code{fastnet}.
#' @param net.igraph The input \code{igraph} object.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @details Masked from \code{igraph::as_adj_list}.
#' @author Xu Dong.
#' @examples \dontrun{
#' library("igraph")
#' net.igraph <- erdos.renyi.game(100,0.1)
#' g <- from.igraph(net.igraph)}
#' @export

from.igraph <- function(net.igraph){

  net <- igraph::as_adj_list(net.igraph)

  net

}



