
#' Summarize total aggregate mortality
#'
#' skeleton...This would work nicely as an S3 function, but we'd need to create
#'  a Mhat class structure for it to operate on.
#'
#' @param M numeric array (ncarc x nsim) of estimated number of fatalities
#'  represented by each carcass discovered
#' @param CL confidence level
#' @return list with elements Mhat vector and summary statistics
#'
#' @export
#'
summary.Mest <- function(object, CL = 0.9, ...){
  Mhat <- colSums(object)
  alpha <- 1 - CL
  sumry <- list()
  sumry$Mhat <- Mhat
  sumry$stats <- c(
    median = median(Mhat),
    IQR = quantile(Mhat, c(0.25, 0.75)),
    CI = quantile(Mhat, c(alpha/2, 1 - alpha/2))
  )
  return(sumry)
}