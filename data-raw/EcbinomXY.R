# This script creates the data used by the mucbinom function

eps <- .Machine$double.eps
g <- c(0.0001205, 1 - 2*eps)
mug <- numeric(2)
for (gi in 1:length(g)){
  mug[gi] <- integrate(
    function(x, size, prob){
      1 - exp(cbinom::pcbinom(x, size, prob, log = T))
    },
    size = 1/g[gi], prob = g[gi],
    lower = 0, upper = 1/g[gi] + 1 - 2*eps,
    rel.tol = 100*eps
  )$val
}
mug
Ecbin <- seq(min(mug), max(mug), length = 200)
gkey <- numeric(length(Ecbin))
for (yi in 1:length(Ecbin)){
  gkey[yi] <- uniroot(f = function(prob){
    integrate(function(x) 1 - exp(cbinom::pcbinom(x, 1/prob, prob, log = T)),
      rel.tol = 100*eps,
      lower = 0, upper = 1/prob + 1
    )$val - Ecbin[yi]
  },
  lower = 0,  upper = 1,
  tol = 10*eps)$root
}

gkey <- c(0, gkey, 1)
Ecbin <- c(1.481, Ecbin, 2)
EcbinomXY <- list("X" = gkey, "Y" = Ecbin)
devtools::use_data(EcbinomXY, internal = TRUE)
