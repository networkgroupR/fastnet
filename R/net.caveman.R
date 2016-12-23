#' Caveman Network
#'
#' @description Simulate a (connected) caveman network of \emph{m} cliques of size \emph{k}.
#' @param m Number of cliques (or caves) in the network.
#' @param k Number of nodes per clique.
#' @param ncores Number of cores, by default \code{detectCores()} from \code{parallel}.
#' @details The (connected) caveman network is formed by connecting a set of isolated \emph{k} - cliques
#' (or "caves"), neighbor by neighbor and head to toe,  using one edge that removed from each
#' clique such that all \emph{m} cliques form a single circle (Watts 1999). The total number of nodes, i.e. \emph{n}, in this network is given by \eqn{k*m}.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <- net.caveman(50, 20) #using ncores by default }
#' @import doParallel
#' @export
#' @references Watts, D. J. Networks, Dynamics, and the Small-World Phenomenon. Amer. J. Soc. 105, 493-527, 1999.


net.caveman <- function(m, k, ncores = detectCores() ){
  if (m<=0 | m%%1!=0) stop("Parameter 'm' must be a non negative integer", call. = FALSE)
  if (k<=0 |  k%%1!=0) stop("Parameter 'k' must be a non negative integer", call. = FALSE)

  if (!ncores%%1==0){
    stop("Parameter ncores must be integer",call. = FALSE)}
  else{

    if (ncores > detectCores() | ncores < 2)  {
    stop("Parameter ncores is lower than 2 or exceed number of available cores",
           call. = FALSE)
    }
    else{

      size <- m*k
      NODEIDS <- seq(size)

      ##Caves generation

      cave.member.connect <- function(ID, m, k, size){

        neilist <- seq((ID - 1) %/% k * k + 1,
                       (ID + k -1) %/% k * k)
        neilist <- neilist[-which(neilist==ID)]

        if (ID %% k == 1){

          neilist <- neilist[-which(neilist == (ID+1))]

        } else if (ID%%k == 2){

          neilist <- neilist[-which(neilist==(ID-1))]


          if (ID%/%k == 0){
            neilist <- c(neilist, size - k + 3)
          } else {
            neilist <- c(neilist, ID - k + 1)
          }

        } else if (ID%%k==3) {
          if (ID%/%k==(m - 1)){
            neilist <- c(neilist, 2)
          } else {
            neilist <- c(neilist, ID + k - 1)
          }

        } else

          neilist

      }

      cl <- makeCluster(ncores) ##Make cluster of cores
      on.exit(stopCluster(cl))
      registerDoParallel(cl,  cores = ncores)

      Network <- parLapply(cl = cl, NODEIDS,
                           cave.member.connect, m, k, size)
      Network
    }
  }
}

