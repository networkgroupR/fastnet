#' Directed / Undirected Erd\"{o}s-R\'{e}nyi \eqn{G(n,p)} network
#'
#' @description Simulate a random network with \emph{n} nodes and a link connecting probability of \emph{p}, according to Ed\"{o}s and R\'{e}nyi (1959).
#' @param n Number of nodes of the network.
#' @param p Connecting probability.
#' @param ncores Number of cores, by default \code{detectCores()} from \code{parallel}.
#' @param d A logical value determining whether is a network directed (default) or indirected.
#' @details In this (simplest) random network, each edge is formed at random with a constant probability.
#' When \code{d = TRUE} is a directed network.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Luis Castro, Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <- net.erdos.renyi.gnp(1000, 0.01)}
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#' @references Erd\"{o}s, P. and R\'{e}nyi, A., On random graphs, Publicationes Mathematicae 6, 290-297 (1959).


net.erdos.renyi.gnp <- function(n, p, ncores = detectCores(), d = TRUE){

  if ( n < 0 | n%%1!=0 )
    stop("Parameter 'n' must be postive integer", call. = FALSE)
  if ( p <= 0 | p >= 1 )
    stop("Parameter 'p' must be in (0,1)", call. = FALSE)
  if ( !ncores%%1 == 0 )
    stop("Parameter 'ncores' must be integer", call. = FALSE)
  if ( ncores > detectCores() | ncores < 2 )
    stop("Parameter 'ncores' is lower than 2 or exceed number of available cores", call. = FALSE)
  if ( n < detectCores() )
    stop("Parameter 'n' should not be too small", call. = FALSE)

  cl <- makeCluster(ncores)
  on.exit(stopCluster(cl))
  registerDoParallel(cl, cores = ncores)

  if (d == TRUE){

    # create n sequential nodes
    nodes <- seq(n)
    # sample out neighboring nodes
    nei <- function(i, n, p){
      output <- seq(n)[-i][stats::runif(n-1) < p]
    }
    # parallel compute and save the neighbors information to *Network*
    Network <- parLapply(cl = cl, nodes, nei, n = n, p = p)
    Network

  }

  else {

    ## obtain the outdegree neighbor list
    # create n sequential nodes
    nodes <- seq(n)
    # sample out neighboring nodes in the Upper Triangle
    nei_to <- function(i, n, p){
      output <- seq(n-i)[stats::runif(n-i) < p] + i
    }
    # parallel compute and save the outdegree neighbors information to *neilist_to*
    neilist_to <- parLapply(cl = cl, nodes, nei_to, n = n, p = p)

    ## obtain the indegree neighbor list
    nei_from <- function(i){

      # creat an empty list
      reverse.neilist <- list()
      reverse.neilist[n] <- list(NULL)

      # revert the neighboring information
      for (j in seq(i, n, ncores)){
        for (k in neilist_to[[j]]){
          reverse.neilist[[k]] <- c(reverse.neilist[[k]], j)
        }
      }
      reverse.neilist
    }
    # Define the list combination function
    cfun <- function(a,b){
      cc <- mapply(c,a,b, SIMPLIFY=FALSE)
      cc
    }
    i<-NULL
    # parallel compute and save the indegree neighbors information to *neilist_from*
    neilist_from <- foreach(i = 1:ncores, .combine='cfun') %dopar% nei_from(i)
    ## combine the outdegree- and indegree- neighbor lists
    Network <- mapply(c, neilist_to, neilist_from, SIMPLIFY = FALSE)
    Network
  }
}
