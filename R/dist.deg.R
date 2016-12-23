#' Degree Distribution
#'
#' @description Obtain the degrees for all nodes.
#' @param net	The input network.
#' @details Obtain the degrees for all nodes.
#' @return A vector.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#  x <-  net.erdos.renyi.gnp(1000, 0.01)
#' x.dist.deg <- dist.deg(x)
#' summary(x.dist.deg)}
#' @export


dist.deg <- function(net){

  lengths(net)

}
