#' Watts-Strogatz Small-world Network
#'
#' @description Simulate a small-world network according to the model of Watts and Strogatz (1998).
#' @param n The number of the nodes in the network (or lattice).
#' @param k Number of edges per node.
#' @param re Rewiring probability.
#' @details The formation of Watts-Strogatz network starts with a ring lattice with \emph{n} nodes and \emph{k} edges per node, then each edge is rewired at random with probability \emph{re}.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh
#' @examples \dontrun{
#' x <- net.watts.strogatz(1000, 10, 0.05)}
#' @import parallel
#' @import doParallel
#' @export
#' @references Duncan J. Watts and Steven H. Strogatz: Collective dynamics of 'small world' networks, Nature 393, 440-442, 1998.


net.watts.strogatz <- function(n, k, re){
  if (n<0 | n%%1!=0) stop("Parameter 'n' must be positive integer",call. = FALSE)
  if (k<=1 | k%%1!=0) stop("Parameter 'k' must be integer greater than 1",call. = FALSE)
  if (re<=0 | re>=1) stop("Parameter 're' must be in (0,1)",call. = FALSE)



  ## cores: number of cores to be used
  cores <- detectCores()

  HalfDeg <- k/2

  NIDList <- seq(n)     # List of all Node IDs, from 1 to n
  #node <- seq(0, nodes-1)   # Node ID from 0 to n-1

  ## Generate a regular lattice ##

  ## Inner parallel function: Neighbor Connection ##
  NeiConn <- function(NID, HalfDeg, n){

    HalfNei <- seq(NID + 1, NID + HalfDeg) %% n

    HalfNei[HalfNei == 0] <- n

    HalfNei
  }

  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))
  registerDoParallel(cl, cores = cores)

  ## First Parallel loop ##
  Lattice <- parLapply(cl = cl, NIDList, NeiConn, HalfDeg, n)

  LinkRewire <- function(NID, n, HalfDeg, re){

    NeiRem <- Lattice[[NID]][which(stats::rbinom(HalfDeg, 1, (1-re)) == 1)]
    remove.link.size <- HalfDeg - length(NeiRem)  ## sample with probability re ##

    pool <- seq(NID + HalfDeg + 1, NID + round(n/2)) %% n
    pool[pool==0] <- n

    add.link <- sample(pool, remove.link.size)

    NewNeiList <- c(NeiRem, add.link)

    NewNeiList
  }

  RewireLattice <- parLapply(cl = cl, NIDList, LinkRewire, n, HalfDeg, re)

  #stopCluster(cl)

  #########################
  ## reverse links ############
  ############################

  LinkRow <- vector("list",n) ## Create a list with size of n

  for (i in 1:HalfDeg){

    zz <- sapply(RewireLattice,"[[",i)
    NewLinkRow <- vector("list",n)

    for (j in 1:n){

      NewLinkRow[[zz[[j]]]] <- c(NewLinkRow[[zz[[j]]]], j)

    }

    LinkRow <- mapply(c, LinkRow, NewLinkRow, SIMPLIFY = FALSE)
    ## Sort Des to all start nodes' ego-centric list,

  }

  ######################################
  ## combine links on both directions ##
  ######################################

  OUTPUT <- mapply(c, RewireLattice, LinkRow, SIMPLIFY = FALSE)
  OUTPUT

}
