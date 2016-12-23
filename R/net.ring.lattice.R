#' \emph{k} - regular ring lattice
#'
#' @description Simulate a network with a \emph{k} -regular ring lattice structure.
#' @param n Number of nodes in the network.
#' @param k Number of edges per node.
#' @details The \emph{n} nodes are placed on a circle and each node is connected to the nearest \emph{k} neighbors.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh
#' @examples \dontrun{
#' x <- net.ring.lattice(1000, 10)}
#' @import parallel
#' @import doParallel
#' @export
#' @references Duncan J Watts and Steven H Strogatz: Collective dynamics of 'small world' networks, Nature 393, 440-442, 1998.


net.ring.lattice <- function(n, k){
  if (n<0 | n%%1!=0) stop("Parameter 'n' must be positive integer",call. = FALSE)
  if (k<=1 | k%%1!=0) stop("Parameter 'k' must be integer greater than 1",call. = FALSE)

  ## cores: number of cores to be used
  cores <- detectCores()

  HalfDeg <- k/2

  NIDList <- seq(n)     # List of all Node IDs, from 1 to n
  #node = seq(0, nodes-1)   # Node ID from 0 to n-1

  ## Generate a regular lattice ##

  ## Inner parallel function: Neighbor Connection ##
  NeiConn <- function(NID, HalfDeg, n){

    HalfNei <- seq(NID + 1, NID + HalfDeg)%%n

    HalfNei[HalfNei == 0] <- n

    HalfNei
  }

  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))
  registerDoParallel(cl, cores = cores)

  ## First Parallel loop ##
  Lattice <- parLapply(cl = cl, NIDList, NeiConn, HalfDeg, n)
  Lattice

}

