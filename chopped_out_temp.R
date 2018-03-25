obsData <- data[ , c(left, right)]
  if (any(is.infinite(t2))){
    t2[which(is.infinite(t2))] <- NA
  }
  t12 <- (t1 + pmin(t2, 2 * max(t2, na.rm = TRUE), na.rm = TRUE)) / 2


  betaInit1 <- switch(dist,
                   "loglogistic" = c(mean(log(t12)), var(log(t12)) / 3.2),
                   "exponential" = 1 / mean(t12),
                   "lognormal" = c(sqrt(var(t12)), log(mean(t12))),
                   "weibull" = weibullStart(t12))
  nparam <- length(betaInit1)
  betaInit <- matrix(0, nrow = ncol(cellMM), ncol = nparam, byrow = TRUE)
  betaInit[1, ] <- betaInit1