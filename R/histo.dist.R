#' Histogram of all degrees of a network
#'
#' @description Plot the histogram of all degrees of a network.
#' @param g	The input network.
#' @details Plot the histogram of all degrees of a network.
#' @return A .gif plot.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.05)
#' histo.dist(x)}
#' @export

histo.dist <- function(g) {

  graphics::hist(lengths(g),breaks = 1000, main = "Histogram of degrees")

}
