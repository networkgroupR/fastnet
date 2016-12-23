#' Plot of the degree distribution of a network, with a logarithm scale
#'
#' @description Plot the degree distribution of a network, with a logarithm scale.
#' @param net	The input network.
#' @details Plot the degree distribution of a network, with a logarithm scale.
#' @return A .gif plot.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' draw.degdist.log(x)}
#' @export

draw.degdist.log <- function(net){

  graphics::plot(cbind(names(table(lengths(net))), as.numeric(table(lengths(net)))), log="xy", xlab="Degree", ylab="Freq")

}

