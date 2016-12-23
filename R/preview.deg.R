#' Preview of the degree distribution of a network
#'
#' @description Present the first 10 degrees of a network.
#' @param g	The input network.
#' @details Present the first 10 degrees of a network.
#' @return A vector.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.ring.lattice(12,4)
#' preview.deg(x)}
#' @export
#'
preview.deg <- function(g) {

  utils::head(lengths(g), n = 10L)


}
