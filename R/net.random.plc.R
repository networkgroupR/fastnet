
#' Random Network with a Power-law Degree Distribution that Has An Exponential Cutoff
#'
#' @description Simulate a random network with a power-law degree distribution that has an exponential cutoff, according to Newman et al. (2001).
#' @param n  The number of the nodes in the network.
#' @param cutoff Exponential cutoff of the degree distribution of the network.
#' @param exponent Exponent of the degree distribution of the network.
#' @details The generated random network has a power-law degree distribution with an exponential degree cutoff.
#' @return A list containing the nodes of the network and their respective neighbors.
#' @author Xu Dong, Nazrul Shaikh
#' @examples \dontrun{
#' x <- net.random.plc(1000, 10, 2)}
#' @export
#' @references Newman, Mark EJ, Steven H. Strogatz, and Duncan J. Watts. "Random graphs with arbitrary degree distributions and their applications." Physical review E 64, no. 2 (2001): 026118.

net.random.plc <- function(n, cutoff, exponent){
  if (n<0 | n%%1!=0) stop("Parameter 'n' must be positive integer", call. = FALSE)
  if (cutoff<0) stop("Parameter 'cutoff' must be positive", call. = FALSE)
  if (exponent<0 | exponent%%1!=0) stop("Parameter 'exponent' must be positive integer", call. = FALSE)
  ##//Generate Degree Distribution//##

  Li_Tau <- function(x, tau){

    sum <- 0

    for (i in 1:10000){

      sum <- sum + (x^i)/(i^tau)

    }
    sum
  }

  #Theoretical_AveDeg <- Li_Tau(exp(-1/cutoff),exponent-1)/Li_Tau(exp(-1/cutoff),exponent)

  AccPk <- 0
  Interval <- 0
  MaxDeg <- 0

  while (AccPk <= 0.999999){

    MaxDeg <- MaxDeg+1
    Pk <- (MaxDeg^(-exponent)) * exp(-MaxDeg/cutoff) /
                  Li_Tau(exp(-1/cutoff), exponent)
    AccPk <- AccPk + Pk
    Interval <- c(Interval, AccPk)

  }

  DegList <- vector("list",n)

  for (i in 1:(n-1)){

    rand <- stats::runif(1)

    for (j in 1:MaxDeg){

      if ((rand <= Interval[j+1]) && (rand > Interval[j])){

        DegList[[i]] <- j

      } else {}

    }
    if (length(DegList[[i]]) == 0){

      DegList[[i]] <- MaxDeg

    }
  }

  JO <- 1

  while (JO == 1) {

    rand <- stats::runif(1)

    for (j in 1:MaxDeg){

      if ((rand <= Interval[j+1]) && (rand>Interval[j])){

        DegList[[n]] = j

      } else {}

    }

    if (length(DegList[[i]]) == 0){

      DegList[[i]] <- MaxDeg

    }

    JO <- do.call(sum, DegList)%%2

  }

  # hist(as.numeric(DegList))   ## Show the histogram of degree

  DegList_Sort <- order(as.numeric(DegList),decreasing = TRUE)

  ###################
  ## edge sampling ##
  ###################
  NeiList <- vector("list", n) ## Create a list

  NodePool <- seq(n)

  k <- 1

  for (i in DegList_Sort){

    NodePool <- NodePool[!NodePool == i]

    if (DegList[[i]] == 0){

    } else if (length(NodePool) == 1){

      NeiList[[i]] <- c(NeiList[[i]], NodePool)
      NeiList[[NodePool]] <- c(NeiList[[NodePool]], i)
      DegList[[i]] <- 0
      DegList[[NodePool]] <- 0

    } else{

      s1 <- sample(NodePool, DegList[[i]])

      NeiList[[i]] <- c(NeiList[[i]], s1)
      DegList[[i]] <- 0

      for (j in s1){

        NeiList[[j]] <- c(NeiList[[j]], i)
        DegList[[j]] <- DegList[[j]] - 1

        if (DegList[[j]] == 0){

          NodePool <- NodePool[!NodePool == j]

        } else{ }

      }

    }

    k <- k + 1

  }

  NeiList

}

