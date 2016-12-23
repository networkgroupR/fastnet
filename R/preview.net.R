#' Preview of a network
#'
#' @description Present the first 10 ego-centric lists of a network.
#' @param net	The input network.
#' @details the connection condition of the first 10 nodes in a network.
#' @return A list.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.ring.lattice(12,4)
#' preview.net(x)}
#' @export


preview.net <- function (net) {
  utils::head(net, n = 10L)
}
