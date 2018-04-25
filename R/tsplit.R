calcRate <- function(Mtilde, Aj, days, searches){
  if (is.null(dim(Mtilde))){
    Mtilde <- matrix(Mtilde, nrow = 1)
    Aj <- matrix(Aj, nrow = 1)
    searches <- matrix(searches, nrow = 1)
  }
  calcRateC(Mtilde, Aj, days, searches)
}
calcTsplit <- function(rate, days, tsplit){
  if (is.null(dim(rate))){
    rate <- matrix(rate, nrow = 1)
  }
  calcTsplitC(rate, days, tsplit)
}