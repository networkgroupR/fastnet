#' Effective Degree
#'
#' @description Calculate the effective degree of a network.
#' @param g	The input network.
#' @param effective_rate	The effective rate (0.9 is set by default).
#' @details The effective degree
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#  x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.effective(x)}
#' @export

metric.degree.effective <- function(g, effective_rate=0.9) {
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)
  stats::quantile(lengths(g),effective_rate)[[1]]
}

