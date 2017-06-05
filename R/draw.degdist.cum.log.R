#' Plot of the cumulative degree distribution of a network, with a logarithm scale
#'
#' @description Plot the cumulative degree distribution of a network, with a logarithm scale.
#' @param net	The input network.
#' @details Plot the cumulative degree distribution of a network, with a logarithm scale.
#' @return A .gif plot.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.barabasi.albert(1000, 20)
#' draw.degdist.cum.log(x)}
#' @export

draw.degdist.cum.log <- function(net){

  cum.freq <- cbind(as.numeric(names(table(lengths(net)))),
              rev(cumsum(rev(unname(table(lengths(net))))))/sum(unname(table(lengths(net)))))

  graphics::plot(cum.freq, log="xy", xlab="Degree", ylab="Prob")
}
