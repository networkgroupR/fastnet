#' Median Path Length
#'
#' @description Calculate the median path length (MPL) of a network.
#' @param Network The input network.
#' @param probability The confidence level probability
#' @param error The sampling error
#' @param Cores Number of cores to use in the computations. By default \code{detecCores()} from \emph{parallel}.
#' @param full  It calculates the sampling version by default. If it is set to true, the population MPL will be calculated and the rest of the parameters will be ignored.
#' @details The median path length (MPL) is the median shortest path lengths of all pairs of nodes in \emph{Network}.
#' \emph{metric.distance.mpl(g)} calculates the population MPL OR estimated MPL of network g with a sampling error set by the user. The calculation uses a parallel load balancing approach, distributing jobs equally among the cores defined by the user.
#' @return A real integer
#' @author Luis Castro, Nazrul Shaikh.
#' @examples \dontrun{
#' ##Default function
#' x <-  net.erdos.renyi.gnp(1000,0.01)
#' metric.distance.mpl(x)
#' ##Population MPL
#' metric.distance.mpl(x, full=TRUE)
#'##Sampling at 99% level with an error of 10% using 5 cores
#'metric.distance.mpl(Network = x, probability=0.99, error=0.1, Cores=5)
#'}
#'
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#' @references E. W. Dijkstra. 1959. A note on two problems in connexion with graphs. Numer. Math. 1, 1 (December 1959), 269-271.
#' @references Castro L, Shaikh N. Estimation of Average Path Lengths of Social Networks via Random Node Pair Sampling. Department of Industrial Engineering, University of Miami. 2016.



metric.distance.mpl <-  function(Network,probability=0.95,error=0.03,
                                 Cores=detectCores(), full=FALSE){

  if (!is.list(Network)) stop("Parameter 'Network' must be a list",call. = FALSE)
  if (probability>=1 | probability<=0) stop("Parameter 'probability' must be in (0,1)",call. = FALSE)
  if (error>=1 | error<=0) stop("Parameter 'error' must be in (0,1)",call. = FALSE)
  if (Cores <= 0 | Cores > detectCores() | Cores%%1!=0) stop("Parameter 'Cores' must be a positive integer greater than one and less than available cores",call. = FALSE)
  if (!is.logical(full)) stop("Parameter 'full' must be logical",call. = FALSE)

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
    #output <- lapply(list.edges,Shortest.path.int,Network=network)

    output

  }


  ##//Sample nodes

  #Get 1000 of SP for st dev calculation for sampling
  N <- length(Network)
  #s <- round(min(N*(N-1)*0.001)/Cores,N)*Cores
  s <- round(1000/Cores)*Cores
  x= array(seq(N))
  S1 <- matrix(nrow=s,ncol=2)
  s1 <- sample(x,s,replace=TRUE); S1[,1] <- s1
  s1 <- sample(x,s,replace=TRUE); S1[,2] <- s1
  s1 <- c(seq(1,length(s1),length(s1)/Cores),length(s1)+1)
  S <- list()
  for (i in 1:(length(s1)-1)){
    S[[i]] <- S1[s1[i]:(s1[i+1]-1),]
  }
  cl <- makeCluster(Cores)
  registerDoParallel(cl, cores = Cores)
  v <- parLapply(cl=cl,S,Shortest.path.big,network=Network)
  stopCluster(cl)
  v <- stats::sd(unlist(v))

  #Final sample size

  #/Sample
  if (full==FALSE){
    d <- 1-probability
    Z <- stats::qnorm(1-d/2)
    s <- round((min(N,(Z*v/error)**2)/Cores))*Cores
    #print(s)
    #print(N)
    #print(length(x))
    S1 <- matrix(nrow=s,ncol=2)
    s1 <- sample(x,s,replace=TRUE); S1[,1] <- s1
    s1 <- sample(x,s,replace=TRUE); S1[,2] <- s1
    s1 <- c(seq(1,length(s1),length(s1)/Cores),length(s1)+1)
    S <- list()
    for (i in 1:(length(s1)-1)){
      S[[i]] <- S1[s1[i]:(s1[i+1]-1),]
    }
  }

  #/Full
  if (full==TRUE){
    s <- round((N/Cores))*Cores
    #print(s)
    #print(N)
    #print(length(x))
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
  cl <- makeCluster(Cores)
  registerDoParallel(cl, cores = Cores)
  Paths <- parLapply(cl=cl,S,Shortest.path.big,network=Network)
  stopCluster(cl)


  ##/Calculate MPL
  Paths <- stats::median(unlist(Paths))

  ##/Return final output
  return(Paths)

}
