#' Efficient Maximal Degree
#'
#' @description Calculate the efficient maximal degree of a graph.
#' @param g	The input network.
#' @details The efficient maximal degree is the 90% quantile of the degrees of all nodes in graph g.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples
#' \dontrun{x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.max.efficient(x)}
#' @export

metric.degree.max.efficient <- function(g){
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)
  unname(stats::quantile(lengths(g),0.9))
}
