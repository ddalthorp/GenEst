# This script creates the data used by the mucbinom function 

cbinomF1 <- function(x, size, prob){
  1 - cbinom::pcbinom(x, size, prob)
}
cbinomF2 <- function(x, prob){
  1 - cbinom::pcbinom(x, 1/prob, prob)
}
cbinomF3 <- function(prob, y){
  integrate(cbinomF2, prob = prob, lower = 0, upper = 1 / prob + 1)$val - y
}

res <- 200

g <- c(0.0001261, 0.99999)
mug <- numeric(2)
for (gi in 1:length(g)){
  mug[gi] <- integrate(cbinomF1, size = 1 / g[gi], prob = g[gi], 
               lower = 0, upper = 1 / g[gi] + 1 - .Machine$double.eps
             )$val
}
y <- seq(min(mug), max(mug), length = res)
x <- numeric(res)
for (yi in 1:res){
  x[yi] <- uniroot(cbinomF3, y = y[yi], lower = 0.00012609, 
              upper = 1 - .Machine$double.eps
           )$root
}

mucbinomxy <- list("x" = x, "y" = y)

devtools::use_data(mucbinomxy, internal = TRUE)

