#' Maximal Degree
#'
#' @description Calculate the maximal degree of a graph.
#' @param g	The input network.
#' @details The maximal degree.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#  x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.max(x)}
#' @export

metric.degree.max <- function(g) {
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)
  max(lengths(g))
}

