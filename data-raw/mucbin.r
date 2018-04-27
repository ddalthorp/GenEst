# This script defines the mucbin function, which uses linear interpolation to
# calculate the mean of the continuous binomial distribution with parameters
# size = 1/prob.
eps <- .Machine$double.eps
g <- c(0.0001261, 0.99999)
mug <- numeric(2)
for (gi in 1:length(g)){
  mug[gi] <- integrate(function(x, size, prob) 1 - cbinom::pcbinom(x, size, prob),
  size = 1/g[gi], prob = g[gi], lower = 0, upper = 1/g[gi] + 1 - eps)$val
}
y <- seq(min(mug), max(mug), length = 200) # 0 and 1 will be appended at the end
gkey <- numeric(length(y))
for (yi in 1:length(y)){
  gkey[yi] <- uniroot(f = function(prob){
    integrate(function(x) 1 - cbinom::pcbinom(x, 1/prob, prob),
      lower = 0, upper = 1/prob + 1)$val - y[yi]
    },
    lower = 0.00012609,
    upper = 1 - eps)$root
}
gkey <- c(0, gkey, 1)
Ecbin <- c(1.48204, y, 2)
mucbin <- approxfun(x = gkey, y = Ecbin)
devtools::use_data(mucbin, internal = T)

