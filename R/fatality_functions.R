#' Draw values for Xtilde given a ghat
#'
#' @param n number of random draws
#' @param ghat estimated detection probability
#' @param seed seed used to set the random number generator
#' @return Xtilde 
#' @examples NA
#' @export 
#'
rXtilde <- function(n = 1, ghat, seed = 1){
  set.seed(seed)
  Xtilde <- cbinom::rcbinom(n, 1 / ghat, ghat)
  return(Xtilde)
}

#' Draw values for Mtilde given a ghat
#'
#' @param n number of random draws
#' @param ghat estimated detection probability
#' @param seed seed used to set the random number generator
#' @return Mtilde 
#' @examples NA
#' @export 
#'
rMtilde <- function(n = 1, ghat, seed = 1){
  
  Xtilde <- rXtilde(n, ghat, seed)
  MtildeVec <- (Xtilde - (mucbinom(ghat) - 1)) / ghat
  Mtilde <- matrix(MtildeVec, ncol = ncol(ghat))
  return(Mtilde)
}

#' Calculate Mhat for a given Mtilde and DWP
#'
#' @param Mtilde Mtilde value
#' @param DWP Density weighted proportion associated with Mtilde
#' @return Mhat 
#' @examples NA
#' @export 
#'
calcMhat <- function(Mtilde, DWP = 1){
  Mhat <- Mtilde / DWP
  return(Mhat)
}


