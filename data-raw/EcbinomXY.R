# This script creates the data used by the Ecbinom function
############
cbmean <- function(N, p, tlim = 0) {
  # assumes length(N) = length(p)
  if (length(tlim) == 1){
    tlim <- rep(tlim, length(N))
  }
  ans <- numeric(length(N))
  for (i in 1:length(N)){
    ans[i] <-tlim[i] + integrate(function(x)
      pbeta(p[i], x, N[i] - x + 1),
      lower = tlim[i], upper = N[i] + 1, rel.tol = 100*eps)$value/
      (cbinom::pcbinom(tlim[i], N[i], p[i], lower.tail=FALSE))
  }
  ans
}
## Split the domain of g into sections and interpolate within subsections so that
#  the interpolation never misses the true mean by more than approx 0.0001
eps <- .Machine$double.eps
tol <- 0.00001
# Ecbinom
gkey <- 0.0001205
node0 <- gkey
while(node0 < 0.9999){
  E0 <- cbmean(1/node0, node0)
  incr <- ifelse(node0 + 0.01 <= 1, 0.01, 1 - node0)
  node1 <- node0 + incr
  node2 <- node0

  E1 <- cbmean(1/node1, node1)
  gmid <- (node0 + node1)/2

  while (abs(cbmean(1/gmid, gmid) - (E0 + E1)/2) < tol){
    node2 <- node1
    node1 <- min(node2 + 0.01, 1)
    E1 <- cbmean(1/node1, node1)
    gmid <- (node0 + node1)/2
  }

  node1 <- node2 + (node1 - node2)/2 # go half-way between node2 and node1
  # keep dropping the node in half until < 0.0001
  E1 <- cbmean(1/node1, node1)
  gmid <- (node0 + node1)/2
  while (abs(cbmean(1/gmid, gmid) - (E0 + E1)/2) > tol){
    node1 <- node2 + (node1 - node2)/2 # go half-way between node2 and node1
    E1 <- cbmean(1/node1, node1)
    gmid <- (node0 + node1)/2
  }
  gkey <- c(gkey, node1)
  node0 <- gkey[length(gkey)]
}
x <- c(0, gkey, 1)
y <- c(1.48120380, cbmean(1/gkey, gkey), 2)
EcbinomXY <- list("X" = x, "Y" = y)

devtools::use_data(EcbinomXY, internal = TRUE, overwrite = TRUE)

