#' Mean Eccentricity
#'
#' @description Calculate the mean eccentricity of a graph.
#' @param g	The input network.
#' @param p The sampling probability.
#' @details The mean eccentricities of all nodes in graph \emph{g}. Calculates the (estimated) mean eccentricity of graph \emph{g} with a justified error.
#' @return A real constant.
#' @author Xu Dong, Nazrul Shaikh.
#' @references West, Douglas Brent. Introduction to graph theory. Vol. 2. Upper Saddle River: Prentice Hall, 2001.
#' @examples \dontrun{
#' x <-  net.erdos.renyi.gnp(1000, 0.01)
#' metric.distance.meanecc(x, 0.01)}
#' @export
#' @importFrom igraph graph_from_adj_list
#' @importFrom igraph eccentricity
#' @import parallel
#' @import doParallel
#' @import foreach

metric.distance.meanecc <- function(g, p){
  if (!is.list(g)) stop("Parameter 'g' must be a list", call. = FALSE)
  if (p>=1 | p<=0) stop("Parameter 'p' must be in (0,1)", call. = FALSE)

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

  meanecc <- mean(ECT)

  stopCluster(cl)

  meanecc
}
