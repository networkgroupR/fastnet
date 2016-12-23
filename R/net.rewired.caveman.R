#' Rewired (Connected) Caveman Network
#'
#' @description Simulate a rewired caveman network of m cliques of size k, and with a link rewiring probability p.
#' @param nc Number of cliques (or caves) in the network.
#' @param m Number of nodes per clique.
#' @param p Link rewiring probability.
#' @details The rewired caveman network is built on the corresponding regular caveman network with m cliques of size k. Then the links in this caveman network are rewired with probability p.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh
#' @references Watts, D. J. Networks, Dynamics, and the Small-World Phenomenon. Amer. J. Soc. 105, 493-527, 1999.
#' @examples \dontrun{
#' x <- net.rewired.caveman(50, 20, 0.0005)}
#' @import doParallel
#' @export
#'
net.rewired.caveman <- function( nc, m, p){
  if (nc<=0 | nc%%1!=0) stop("Parameter 'nc' must be a non negative integer", call. = FALSE)
  if (m<=0 | m%%1!=0) stop("Parameter 'm' must be a non negative integer", call. = FALSE)
  if (p<=0 | p>=1) stop("Parameter 'p' must be in (0,1)", call. = FALSE)
  n <- nc*m

  pool <- ifelse(stats::runif(nc*(m-1)*m/2)<=p,1,0)
  pool <- which(pool==1)

  edge.to.nei <- function(i){

    nei =list()
    nei[n] <- list(NULL)

    for (j in seq(i,n,Cores)  ){

      if ( j%%m != 0) {

        cavej <- (j-1)%/%m+1

        numj <- (j-1)%%m+1

        neij <- seq( (cavej-1)*(m-1)*m/2 + (numj-1)*(m-numj/2) + 1, (cavej-1)*(m-1)*m/2 + numj*(m-(numj+1)/2)  )

        delete.nei <- which(neij %in% intersect(pool, neij ) )

        delete.edge.num <- length(delete.nei)

        if ( delete.edge.num !=0  ) {

          nei[[j]] <- seq(j+1,m*cavej)[-delete.nei]

          for ( k in seq(delete.edge.num) ) {

            from <- sample(seq(n),1)

            cave.from <- (from-1)%/%m+1

            num.from <- (from-1)%%m+1

            to <- sample(seq(n)[-seq( m*(cave.from-1)+1 , m*cave.from)],1)

            nei[[from]] <- c(nei[[from]],to)

          }

        } else {

          nei[[j]] <- seq(j+1,m*cavej)

        }

      } else { }

    }

    nei

  }

  cfun <- function(a,b){
    cc <- mapply(c,a,b, SIMPLIFY=FALSE)
    cc
  }

  Cores <- detectCores()
  cl <- makeCluster(Cores)
  registerDoParallel(cl, cores=Cores)

  i <- NULL
  neilist <- foreach(i = 1:Cores, .combine='cfun') %dopar% edge.to.nei(i)

  reverse.connect <- function(i){

    reverse.neilist =list()
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
  i <- NULL
  reverselist <- foreach(i=1:Cores, .combine='cfun') %dopar% reverse.connect(i)

  Network <- mapply(c,neilist,reverselist, SIMPLIFY=FALSE)
  stopCluster(cl)
  Network

}
