#' Degree Entropy

#'
#' @description Calculate the degree entropy of a graph.
#' @param g	The input network.
#' @details Calculates the degree entropy of graph g, i.e. \deqn{Entropy(g) = - \sum_{i=1}^{n} i*\log _2(i)}
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references Anand, Kartik, and Ginestra Bianconi. "Entropy measures for networks: Toward an information theory of complex topologies." Physical Review E 80, no. 4 (2009): 045102.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.degree.entropy(x)}
#' @export
#'

metric.degree.entropy <- function(g) {

  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)

  mdf <- as.numeric(table(lengths(g)))

  p <- mdf/sum(mdf) # denominator always equals to number of nodes

  H <- -sum(p*log(p,2))

  H

}
