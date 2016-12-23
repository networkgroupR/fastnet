#' Plot of the degree distribution of a network
#'
#' @description Plot the degree distribution of a network.
#' @param net The input network.
#' @details Plot the degree distribution of a network.
#' @return A .gif plot.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' draw.degdist(x)}
#' @export

draw.degdist <- function(net){

  graphics::plot(as.data.frame(table(lengths(net))),xlab="Degree")

}
