#' Complete Network
#'
#' @description Simulate a complete (or full) network.
#' @param n Number of nodes of the network.
#' @param ncores Number of cores, by default \code{detectCores()} from \code{parallel}.
#' @details The \emph{n} nodes in the network are fully connected.
#' @details Note that the input \emph{n} should not excess 10000, for the sake of memory overflow.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh.
#' @examples \dontrun{
#' x <- net.complete(1000) #using ncores by default}
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#'


net.complete = function(n, ncores = detectCores()){
  if (n<=0 | n%%1!=0 | n>=10000) stop("Parameter 'n' must be a non negative integer. 'n' should excess 10000", call. = FALSE)
  if (!ncores%%1==0){
    stop("Parameter 'ncores' must be integer",call. = FALSE)}
  else{

    if (ncores > detectCores() | ncores < 2)  {
      stop("Parameter 'ncores' is lower than 2 or exceed number of available cores",
           call. = FALSE)
    }
    else{


      ## cores: number of cores to be used
      cl <- makeCluster(ncores)
      on.exit(stopCluster(cl))
      registerDoParallel(cl, cores = ncores)

      # NIDList = seq(n)     # List of all Node IDs, from 1 to n
      Batch <- round(n/ncores)

      ## Inner parallel function: Neighbor Connection ##
      NeiConn <- function(k){
        if ( k != ncores ) {
          NeiList <- lapply(seq(((k-1)*Batch+1),k*Batch), function(j) seq(n)[-j])
        } else {
          NeiList <- lapply(seq(((k-1)*Batch+1),n), function(j) seq(n)[-j])
        }
        NeiList
      }

      i <- NULL # To avoid Warning on "R CMD check"
      Network <- foreach(i = 1:ncores, .combine = c) %dopar% NeiConn(i)
      Network

    }
  }
}
