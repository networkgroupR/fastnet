#' Median Degree
#'
#' @description Calculate the median degree of a graph.
#' @param g	The input network.
#' @details The median degree is the median value of the degrees of all nodes in graph g.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.median(x)}
#' @export
#'

metric.degree.median <- function(g) {

  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)

  stats::median(lengths(g))

}
