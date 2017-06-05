#' Directed / Undirected Erd\"{o}s-R\'{e}nyi \eqn{G(n,m)} network using a fix edge size.
#'
#' @description Simulate a random network with \emph{n} nodes and \emph{m} edges, according to Erd\"{o}s and R\'{e}nyi (1959).
#' @param n Number of nodes of the network.
#' @param m Number of edges of the network.
#' @param ncores Number of cores, by default \code{detectCores()} from \code{parallel}.
#' @param d A logical value determining whether is a network directed (default) or indirected.
#' @details In this (simplest) random network, \emph{m} edges are formed at random among \emph{n} nodes.
#' When \code{d = TRUE} is a directed network.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <- net.erdos.renyi.gnm(1000, 100) }
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#' @references Erd\"{o}s, P. and R\'{e}nyi, A., On random graphs, Publicationes Mathematicae 6, 290-297 (1959).


net.erdos.renyi.gnm <- function(n, m, ncores = detectCores(), d = TRUE){
  if (n<0 | n%%1!=0) stop("Parameter 'n' must be positive integer",call. = FALSE)
  if (m<=1 | m%%1!=0) stop("Parameter 'm' must be integer greater than 1",call. = FALSE)
  if (!ncores%%1==0){
    stop("Parameter 'ncores' must be integer",call. = FALSE)}
  else{

    if (ncores > detectCores() | ncores < 2)  {
      stop("Parameter 'ncores' is lower than 2 or exceed number of available cores",
           call. = FALSE)
    }
    else{
      if (d == TRUE) {

        pool <- sample.int( n*(n-1),m )

        cl <- makeCluster(ncores)
        on.exit(stopCluster(cl))
        registerDoParallel(cl, cores = ncores)

        edge.to.nei <- function(i){

          nei <- list()
          nei[n] <- list(NULL)

          for (j in seq( i, n, ncores )  ){

            raw.nei <- (intersect(pool, seq((j-1)*(n-1)+1, j*(n-1)))-1)%%(n-1)+1

            nei[[j]] <- c( raw.nei[raw.nei<j], raw.nei[raw.nei>=j]+1 )

          }

          nei

        }

        cfun <- function(a,b){
          cc <- mapply(c,a,b, SIMPLIFY=FALSE)
          cc
        }

        Network <- foreach(i = 1:ncores, .combine='cfun') %dopar% edge.to.nei(i)

        Network

      }
      else{

        pool <- sample.int( n*(n-1)/2,m )

        connect <- function(j){

          neilist.raw <- list()
          neilist.raw[n] <- list(NULL)

          for (i in seq(j,(n-1),ncores)  ){

            neilist.raw[[i]] <- intersect(pool,seq( i*n-0.5*i^2+0.5*i+1-n, i*n-0.5*i^2-0.5*i ))+i-n*i+n+0.5*i^2-0.5*i

          }

          neilist.raw

        }


        cl <- makeCluster(ncores)   ##Make cluster of cores
        on.exit(stopCluster(cl))
        registerDoParallel(cl, cores = ncores)

        cfun <- function(a,b){
          cc <- mapply(c,a,b, SIMPLIFY=FALSE)
          cc
        }

        j <- NULL

        neilist <- foreach(j = 1:ncores, .combine='cfun') %dopar% connect(j)

        reverse.connect <- function(i){

          reverse.neilist <- list()
          reverse.neilist[n] <- list(NULL)

          for (j in seq(i,n,ncores)  ){

            for (k in neilist[[j]]){

              reverse.neilist[[k]] <- c(reverse.neilist[[k]],j)

            }

          }

          reverse.neilist

        }

        i <- NULL

        reverselist <- foreach(i = 1:ncores, .combine='cfun') %dopar% reverse.connect(i)

        Network <- mapply(c,neilist,reverselist, SIMPLIFY=FALSE)

        Network

      }
    }
  }
}



