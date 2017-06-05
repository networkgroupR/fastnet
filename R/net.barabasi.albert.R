
#' Barab\'{a}si-Albert Scale-free Graph
#'
#' @description Simulate a scale-free network using a preferential attachment mechanism (Bar\'{a}basi and Albert, 1999)
#' @param n Number of nodes of the network.
#' @param m Number of nodes to which a new node connects at each iteration.
#' @param ncores Number of cores, by default \code{detectCores()} from \code{parallel}.
#' @param d A logical value determining whether the generated network is a directed or undirected (default) network.
#' @details Starting with \emph{m} nodes, the preferential attachment mechaism adds one node and \emph{m} edges in each step. The edges will be placed with one end on the newly-added node and the other end on the existing nodes, according to probabilities that associate with their current degrees.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Luis Castro, Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <- net.barabasi.albert(1000, 20) # using default ncores }
#' @import parallel
#' @import doParallel
#' @export
#' @references Barab\'{a}si, A.- L. and Albert R. 1999. Emergence of scaling in random networks. Science, 286 509-512.


net.barabasi.albert <- function(n, m, ncores = detectCores(), d = FALSE ){
  if (n<=0 | n%%1!=0) stop("Parameter 'n' must be a non negative integer", call. = FALSE)
  if (m<=1 | m%%1!=0) stop("Parameter 'm' must be a non negative integer greater than 1", call. = FALSE)

  if (!ncores%%1==0){
    stop("Parameter 'ncores' must be integer", call. = FALSE)}
  else{

    if (ncores > detectCores() | ncores < 2)  {
      stop("Parameter 'ncores' is lower than 2 or exceed number of available cores",
           call. = FALSE)
    } else{
      if (d == TRUE){
        cl <- makeCluster(ncores)  ##Make cluster of cores
        on.exit(stopCluster(cl))
        registerDoParallel(cl, cores = ncores)

        ##M0 graph
        m0 <- function(n, links){
          edge <- seq(1, links)[-n]
          edge <- sample(edge, 1)
          edge
        }
        n.init <- seq(1, m)

        Net1 <- parLapply(cl = cl, n.init, m0, links = m)

        ##From link+1 to n - preferntial attachment  + growth process

        deg <- function(node, Network){
          d <- length(unlist(Network[node]))
          d
        }

        New.edge <- function(new.edge, Network, new.node){
          n <- c(unlist(Network[new.edge]), new.node)
          n
        }


        for (i in (m+1):n){
          degree <- parLapply(cl = cl, n.init, deg, Network = Net1)
          Tdeg <- sum(unlist(degree))
          p <- unlist(degree)/Tdeg

          new.edge <- sample(n.init, m, prob = p)
          Net2 <- parLapply(cl = cl, new.edge, New.edge, Network = Net1, i)

          Net1[new.edge] <- Net2
          Net1[i] <- list(sort(new.edge))
          n.init <- c(n.init, i)
        }

        Net1
      }

      else{
        neilist <- list()
        neilist[n] <- list(NULL)

        ##Create the m0 graph
        df <- c(rep(1, m), m, rep(0, n-m-1))

        neilist[[m+1]] <- seq(m)

        for (i in (m+2):n){

          new.neighbor <- sample(seq(n),m,prob = df)

          neilist[[i]] <- new.neighbor

          df[i] <- m

          df[new.neighbor] <- df[new.neighbor] + 1  ####

        }

        if (n < ncores) {

          ncores = n

        } else { }

        cl <- makeCluster(ncores) ##Make cluster of cores
        on.exit(stopCluster(cl))
        registerDoParallel(cl, cores = ncores)

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

        cfun <- function(a,b){
          cc <- mapply(c,a,b, SIMPLIFY=FALSE)
          cc
        }

        i <- NULL
        reverselist <- foreach(i = 1:ncores, .combine='cfun') %dopar% reverse.connect(i)
        Network <- mapply(c,neilist,reverselist, SIMPLIFY=FALSE)
        Network
      }
    }
  }
}
