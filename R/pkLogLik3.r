
#' Calculate the negative log-likelihood of a searcher efficiency model.
#' 
#' @param misses Number of searches when carcass was present but
#'  not found.
#' @param foundOn Search on which carcass was found.
#' @param beta Parameters to be optimized.
#' @param nbeta_p Number of parameters associated with p
#' @param cellByCarc Which cell each observation belongs to.
#' @param maxmisses Maximum possible number of misses for a carcass.
#' @param cellMM Combined pk model matrix.
#' @param kFixed Value of k if fixed. 
#' @return Negative log likelihood of the observations, given the parameters.
#' @examples
#' NA
#' @export 
#'
pkLogLik3 <- function(misses, foundOn, beta, nbeta_p, cellByCarc, maxmisses, 
              cellMM, kFixed = NULL){

  if (length(kFixed) == 1){
    beta <- c(beta, logit(kFixed))
  }

  ncell <- nrow(cellMM)
  nbeta <- length(beta)
  which_p <- 1:nbeta_p
  which_k <- (nbeta_p + 1):nbeta

  beta_p <- beta[which_p]
  beta_k <- beta[which_k]
  Beta <- matrix(0, nrow = nbeta, ncol = 2)
  Beta[which_p, 1] <- beta[which_p]
  Beta[which_k, 2] <- beta[which_k]

  pk <- alogit(cellMM %*% Beta)
  p <- pk[ , 1]
  k <- pk[ , 2]

  powk <- matrix(k, nrow = ncell, ncol = maxmisses + 1)
  powk[ , 1] <- 1
  powk <- matrixStats::rowCumprods(powk)

  pmiss <- matrix(1 - (p * powk[ , 1:(maxmisses + 1)]), nrow = ncell)
  pmiss <- matrixStats::rowCumprods(pmiss)
  pfind <- matrixStats::rowDiffs(1 - pmiss)
  pfind_si <- cbind(pk[ , 1], pfind)

  notFoundCell <- cellByCarc[foundOn == 0]
  notFoundMisses <- misses[foundOn == 0]
  notFoundCellMisses <- cbind(notFoundCell, notFoundMisses)
  foundCell <- cellByCarc[foundOn > 0]
  foundFoundOn <- foundOn[foundOn > 0]
  foundCellFoundOn <- cbind(foundCell, foundFoundOn)

  ll_miss <- sum(log(pmiss[notFoundCellMisses]))
  ll_found <- sum(log(pfind_si[foundCellFoundOn]))
  nll_total <- -(ll_miss + ll_found)
 
  return(nll_total)
}
