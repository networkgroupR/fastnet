#' Median Eccentricity
#'
#' @description Calculate the (estimated) median eccentricity of a graph.
#' @param g	The input network.
#' @param p The sampling probability.
#' @details Is the median eccentricities of all nodes in graph \emph{g}.
#' \code{metric.distance.medianecc} calculates the (estimated) median eccentricity of graph \emph{g} with a justified error.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references West, Douglas Brent. Introduction to graph theory. Vol. 2. Upper Saddle River: Prentice hall, 2001.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.distance.medianecc(x, 0.01)}
#' @export
#' @import igraph
#' @import parallel
#' @import doParallel
#' @import foreach

metric.distance.medianecc <- function(g,p){

  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)
  if (p>=1 | p<=0) stop("Parameter 'probability' must be in (0,1)",call. = FALSE)

  n <- length(g)

  g <- simplify(graph_from_adj_list(g, duplicate = FALSE))

  n <- vcount(g)

  ect <- function (i,n,g) {
    p <- sample(seq(n-1),1)
    xx <- eccentricity(g,p)
    return(xx)
  }

  cl <- makeCluster(detectCores())
  registerDoParallel(cl, cores = detectCores())
  clusterExport(cl=cl, varlist=c("eccentricity"), envir = environment())
  i <- NULL
  ECT <- foreach(i = 1:(round(n*p)), .combine=c) %dopar% ect(i,n,g)

  medianecc <- mean(ECT)
  stopCluster(cl)
  medianecc

}
