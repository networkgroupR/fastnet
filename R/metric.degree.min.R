#' Minimal Degree
#'
#' @description Calculate the minimal degree of a network.
#' @param g	The input network.
#' @details The minimal degree.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#  x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.min(x)}
#' @export

metric.degree.min <- function(g) {
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)
  min(lengths(g))
}

