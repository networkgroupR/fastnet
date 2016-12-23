#' Plot of a small network
#'
#' @description Plot a small network.
#' @param net	The input network.
#' @details Plot a small network.
#' @return A .gif plot.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.ring.lattice(12,4)
#' draw.net(x)}
#' @export


draw.net <- function(net) {

  net.ig <- igraph::as.undirected(igraph::graph_from_adj_list(net))

  graphics::plot(net.ig)

}
