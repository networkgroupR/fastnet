#' Average Path Length
#'
#' @description Calculate the average path length of a graph.
#' @param Network The input network.
#' @param probability The confidence level probability.
#' @param error The sampling error.
#' @param Cores Number of cores to use in the computations. By default uses \emph{parallel} function \code{detecCores()}.
#' @param full.apl  It will calculate the sampling version by default. If it is set to true,
#' the population APL will be calculated and the rest of the parameters will be ignored.
#' @details The average path length (APL) is the average shortest path lengths of all
#' pairs of nodes in graph \emph{Network}. \code{metric.distance.apl} calculates
#' the population APL and estimated APL of graph g with a sampling error set by the user.
#'
#' The calculation uses a parallel load balancing approach,
#' distributing jobs equally among the cores defined by the user.
#' @return A real value.
#' @author Luis Castro, Nazrul Shaikh.
#' @examples \dontrun{
#' ##Default function
#' x <-  net.erdos.renyi.gnp(1000,0.01)
#' metric.distance.apl(x)
#' ##Population APL
#' metric.distance.apl(x, full.apl=TRUE)
#' ##Sampling at 99% level with an error of 10% using 5 cores
#' metric.distance.apl(Network = x, probability=0.99, error=0.1, Cores=5)
#'}
#'
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#' @references E. W. Dijkstra. 1959. A note on two problems in connexion with graphs. Numer. Math. 1, 1 (December 1959), 269-271.
#' @references Castro L, Shaikh N. Estimation of Average Path Lengths
#' of Social Networks via Random Node Pair Sampling.
#' Department of Industrial Engineering, University of Miami. 2016.



metric.distance.apl <-  function(Network,probability=0.95,error=0.03,
                              Cores=detectCores(), full.apl=FALSE){

  if (!is.list(Network)) stop("Parameter 'Network' must be a list", call. = FALSE)
  if (probability>=1 | probability<=0) stop("Parameter 'probability' must be in (0,1)", call. = FALSE)
  if (error>1 | error<0) stop("Parameter 'error' must be in [0,1]", call. = FALSE)
  if (Cores <= 0 | Cores > detectCores() | Cores%%1!=0) stop("Parameter 'Cores' must be a positive integer number greater than one and less available cores",call. = FALSE)
  if (!is.logical(full.apl)) stop("Parameter 'full.ap' must be logical", call. = FALSE)
  if (0 %in% lengths(Network)) stop("The network object contains isolated nodes", call = FALSE)

      ##//Inner function SPL by edeges


  Shortest.path.big <- function(matrix.edges,network){

    ##Parameters
    #matrix.edges <- edges by rows, first element source, second element destination - for apply use!!
    #network in list representation


    Shortest.path.int <- function(edge,Network){

      ##//Parameters
      #orig  - source node
      #dest  - final destination node
      #Network - egocentric representation of the network

      ##Computation
      sp <- 1
      r1 <- 0
      #edge <- unlist(edge)
      orig <- edge[1]
      dest <- edge[2]



      ##Correcting same origen and destination
      if (orig==dest){
        #print("Si")
        x <- setdiff(seq(length(Network)),orig)
        #print(length(x))
        dest <- sample(x,1)
        #print(dest)
      }


      ##Loop of SP
      while (r1==0){

        if (sp==1){
          neig <- unlist(Network[orig])
          r1 <- which(neig==dest)
          r1 <- length(r1)
        }

        if (r1==0){
          neig <- unlist(Network[neig])
          r1 <- which(neig==dest)
          r1 <- length(r1)
          sp <- sp+1
        }
      }
      sp
    }

    output <- apply(matrix.edges,1,Shortest.path.int,Network=network)

    output

  }


  ##//Sample nodes

  #Get 1000 of SP for st dev calculation for sampling
  N <- length(Network)
  s <- round(1000/Cores)*Cores
  x <- array(seq(N))
  S1 <- matrix(nrow=s,ncol=2)
  s1 <- sample(x,s,replace=TRUE); S1[,1] <- s1
  s1 <- sample(x,s,replace=TRUE); S1[,2] <- s1
  s1 <- c(seq(1,length(s1),length(s1)/Cores),length(s1)+1)
  S <- list()
  for (i in 1:(length(s1)-1)){
    S[[i]] <- S1[s1[i]:(s1[i+1]-1),]
  }
  cl <- makeCluster(Cores)
  on.exit(stopCluster(cl))
  registerDoParallel(cl, cores = Cores)
  v <- parLapply(cl=cl,S,Shortest.path.big,network=Network)
  v <- stats::sd(unlist(v))

  #Final sample size

  #/Sample APL
  if (full.apl==FALSE){
    d <- 1-probability
    Z <- stats::qnorm(1-d/2)
    s <- round((min(N,(Z*v/error)**2)/Cores))*Cores
    S1 <- matrix(nrow=s,ncol=2)
    s1 <- sample(x,s,replace=TRUE); S1[,1] <- s1
    s1 <- sample(x,s,replace=TRUE); S1[,2] <- s1
    s1 <- c(seq(1,length(s1),length(s1)/Cores),length(s1)+1)
    S <- list()
    for (i in 1:(length(s1)-1)){
      S[[i]] <- S1[s1[i]:(s1[i+1]-1),]
    }
  }

  #/Full APL
  if (full.apl==TRUE){
    s <- round((N/Cores))*Cores
    S1 <- matrix(nrow=s,ncol=2)
    s1 <- sample(x,s,replace=FALSE); S1[,1] <- s1
    s1 <- sample(x,s,replace=FALSE); S1[,2] <- s1
    s1 <- c(seq(1,length(s1),length(s1)/Cores),length(s1)+1)
    S <- list()
    for (i in 1:(length(s1)-1)){
      S[[i]] <- S1[s1[i]:(s1[i+1]-1),]
    }
  }

  ##/Parallel processing
  #cl <- makeCluster(Cores)
  #on.exit(stopCluster(cl))
  registerDoParallel(cl, cores = Cores)
  Paths <- parLapply(cl=cl,S,Shortest.path.big,network=Network)


  ##/Calculate APL (change here to median/max/min for Cristian!)
  Paths <- mean(unlist(Paths))

  ##/Return final output
  return(Paths)

}
