#' neighbors of an agent in a network
#'
#' @description Present all neighbors of a give agent in a network.
#' @param net	The input network.
#' @param node The input node.
#' @details Neighbors of a node are nodes that directly connects to this node.
#' @return A vector.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.ring.lattice(12,4)
#' neighbors(x,2)}
#' @export

neighbors <- function(net,node) {


  net[[node]]

}
