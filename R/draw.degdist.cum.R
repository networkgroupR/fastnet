#' Plot of the cumulative degree distribution of a network
#'
#' @description Plot the cumulative degree distribution of a network, with a logarithm scale.
#' @param net	The input network.
#' @details Plot the cumulative degree distribution of a network, with a logarithm scale.
#' @return A .gif plot.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.barabasi.albert(1000, 20)
#' draw.degdist.cum(x)}
#' @export

draw.degdist.cum <- function(net){

 graphics::plot(stats::ecdf(lengths(net)),xlab="Degree",ylab="Freq",main="")

}
