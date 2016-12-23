#' Holme-Kim Network
#'
#' @description Simulate a scale-free network with relatively high clustering, comparing to B-A networks (Holme and Kim, 1999).
#' @param n Number of nodes of the network.
#' @param m Number of nodes to which a new node connects at each iteration.
#' @param pt Triad formation probability after each preferential attachment mechanism.
#' @details The Holme-Kim network model is a simple extension of B-A model. It adds an additional step, called "Triad formation", with the probability \emph{pt} that compensates the low clustering in B-A networks.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh
#' @examples \dontrun{
#' x <- net.holme.kim (1000, 20, 0.1)}
#' @references Holme, Petter, and Beom Jun Kim. "Growing scale-free networks with tunable clustering."Physical review E65, no. 2 (2002): 026107.
#' @export


net.holme.kim <- function( n, m, pt ){
  if (n<0 | n%%1!=0) stop("Parameter 'n' must be positive integer", call. = FALSE)
  if (m<1 | m%%1!=0) stop("Parameter 'm' must be integer  greater than 1", call. = FALSE)
  if (pt<0 | pt>1) stop("Parameter 'pt' must be in (0,1)", call. = FALSE)

  neilist <- list()
  neilist[n] <- list(NULL)

  ##Create the m0 graph + (m+1)node
  neilist[[m+1]] <- seq(m)

  for ( k in seq(m)) {

    neilist[[k]] <- m+1

  }

  df <- c( rep(1,m),m,rep(0,n-m-1))

  for (i in (m+2):n){

    pa.neighbor <- sample(seq(n),1,prob = df)

    neilist[[i]] <- pa.neighbor

    neilist[[pa.neighbor]] <- c(neilist[[pa.neighbor]],i)

    df[pa.neighbor] <- df[pa.neighbor] + 1

    for (j in seq(2,m) ) {

      pool <- setdiff( neilist[[pa.neighbor]], c(i,neilist[[i]]) )

      if ( stats::runif(1) <= pt && length( pool ) !=0 ) {

        tf.neighbor <- sample(pool, 1)

        neilist[[i]] <- c(neilist[[i]], tf.neighbor)

        neilist[[tf.neighbor]] = c(neilist[[tf.neighbor]],i)

        df[tf.neighbor] <- df[tf.neighbor] + 1

      } else {

        pa.neighbor <- sample( seq(n)[-neilist[[i]]],1,prob = df[-neilist[[i]]] )

        neilist[[i]] <- c(neilist[[i]], pa.neighbor)

        neilist[[pa.neighbor]] <- c(neilist[[pa.neighbor]],i)

        df[pa.neighbor] <- df[pa.neighbor] + 1

      }

    }

    df[i] <- m

  }

  neilist

}

