#' Mean Degree
#'
#' @description Calculate the mean degree of a graph.
#' @param g	The input network.
#' @details The mean degree is the average value of the degrees of all nodes in graph g.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.mean(x)
#' }
#' @export

metric.degree.mean <- function(g) {
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)
  mean(lengths(g))
}
