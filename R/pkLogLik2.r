
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
pkLogLik2 <- function(misses, foundOn, beta, nbeta_p, cellByCarc, 
    maxmisses, cellMM, kFixed = NULL, betaHi = NULL, betaLo = NULL,
    reducedCellMMbdiag = NULL){

    betaFixed = numeric(ncol(reducedCellMMbdiag))
    betaFixed[colSums(reducedCellMMbdiag) !=0] = c(beta, logit(kFixed))
    betaFixed[betaHi] = logit(0.99)
    betaFixed[betaLo] = logit(0.01)

  # if (length(kFixed) == 1){
    # betaFixed <- c(betaFixed, logit(kFixed))
  # }

  ncell <- nrow(cellMM)
  nbeta <- length(betaFixed)
  which_p <- 1:nbeta_p
  which_k <- (nbeta_p + 1):nbeta

  beta_p <- betaFixed[which_p]
  beta_k <- betaFixed[which_k]
  Beta <- matrix(0, nrow = nbeta, ncol = 2)
  Beta[which_p, 1] <- betaFixed[which_p]
  Beta[which_k, 2] <- betaFixed[which_k]

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


dmatrix = function(m1, m2){
    out = cbind(
        rbind(m1, matrix(0,  ncol = ncol(m1), nrow = nrow(m2))),
        rbind(matrix(0, ncol = ncol(m2), nrow = nrow(m1)), m2))
    rownames(out) = c(rownames(m1), rownames(m2))
    colnames(out) = c(colnames(m1), colnames(m2))
    return(out)
}
