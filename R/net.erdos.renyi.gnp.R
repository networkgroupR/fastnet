#' Directed / Undirected Erdos-Renyi \eqn{G(n,p)} network
#'
#' @description Simulate a random network with \emph{n} nodes and a link connecting probability of \emph{p}, according to Erdos and Renyi (1959).
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
#' @references Erdos, P. and Renyi, A., On random graphs, Publicationes Mathematicae 6, 290-297 (1959).


net.erdos.renyi.gnp <- function(n, p, ncores = detectCores(), d = TRUE){
  if (n<0 | n%%1!=0) stop("Parameter 'n' must be postive integer",call. = FALSE)
  if (p<=0 | p>=1) stop("Parameter 'p' must be in (0,1)",call. = FALSE)
  if (!ncores%%1==0){
    stop("Parameter 'ncores' must be integer",call. = FALSE)}
  else{

    if (ncores > detectCores() | ncores < 2)  {
      stop("Parameter 'ncores' is lower than 2 or exceed number of available cores",
           call. = FALSE)
    }else
    {
      if (d == TRUE){
        node <- rep(n, n)
        cl <- makeCluster(ncores)
        on.exit(stopCluster(cl))
        registerDoParallel(cl, cores = ncores)
        RG <- function(nodes, prob){
         n <- stats::rbinom(nodes, 1, p)
        n <- which(n == 1)
        }

        Network <- parLapply(cl = cl, node, RG, prob=p)
        Network
      }

      else {

        Cores <- ncores

        neilist <- list()
        neilist[n] <- list(NULL)

        for (i in 1:(n-1)) {

          neighbor <- ifelse(stats::runif(n-i)<= p,1,0)

          neilist[[i]] <- which(neighbor == 1) + i

        }

        cl <- makeCluster(Cores) ##Make cluster of cores
        on.exit(stopCluster(cl))
        registerDoParallel(cl, cores = Cores)

        reverse.connect <- function(i){

          reverse.neilist <- list()
          reverse.neilist[n] <- list(NULL)

          for (j in seq(i,n,Cores)  ){

            for (k in neilist[[j]]){

              reverse.neilist[[k]] <- c(reverse.neilist[[k]],j)

            }

          }

          reverse.neilist

        }

        cfun <- function(a,b){
          cc <- mapply(c,a,b, SIMPLIFY=FALSE)
          cc
        }

        i<-NULL
        reverselist <- foreach(i = 1:Cores, .combine='cfun') %dopar% reverse.connect(i)

        Network <- mapply(c,neilist,reverselist, SIMPLIFY=FALSE)
        Network

        }
    }
  }
}
