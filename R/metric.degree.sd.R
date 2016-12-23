#' Standard Deviation of Degree Distribution
#'
#' @description Calculate the standard deviation of all degrees of a network.
#' @param g	The input network.
#' @details The standard deviation of all degrees of a network.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#  x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.sd(x)}
#' @export

metric.degree.sd <- function(g) {
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)
  stats::sd(lengths(g))
}

