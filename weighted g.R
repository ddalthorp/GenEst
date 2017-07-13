# Estimation of total mortality across two independent classes (in the sense
# that SE/CP field trials in the two classes use distinct sets of carcasses)
# Estimate the posterior distribution of M | (X, g), where X is a vector of
# carcass counts (one count for each class) and g is a "vector" of distributions
# representing estimated detection probabilities for each class.
# Estimation is via sum(X) ~ binomial(M, g), where M is the sum of counts in the
# two classes and g is the weighted average of detection probabilities among the
# class, i.e., g = sum(g[i] * rho[i]), where rho is the fraction of total carcasses
# in each of the classes.
M <- c(800, 200) # a particular number (NOT multinomial or 2-d Poisson)
rho <- M/sum(M)
g <- c(0.1, 0.8) # true g for two classes
# NOTE: this pushes the limits of reality w/ [0.3, 0.7] more likely for most species
nclass <- length(g)
# g is estimated with uncertainty from field trials
N <- c(80, 50) # number of "field trial carcasses" for estimating g
alpha <- c(0.5, 0.2, 0.1, 0.05, 0.01)
nsim <- 1000 # number of "years" to simulate

# simulate field trials for estimating g
y <- array(rbinom(nsim * length(g), N, g), dim=c(length(g), nsim))
sum(y == 0) # y = 0 crashes program b/c arrays too large
y[y==0] <- 1

# simulate carcass searches
Xsim <- t(array(rbinom(nsim * length(g), M, g), dim = c(length(g), nsim)))

# utility function that returns a and b parameters for a beta distribution given the mean and variance
Babgmus2 <- function(mu, s2){
  a <- mu^2/s2*(1-mu)-mu
  list(shape1 = a, shape2 = a*(1/mu - 1))
}

Mci <- array(dim=c(nsim, 2, length(alpha))) # CI's for M
Mhat <- array(dim=c(nsim, 2)) # median and mean of posterior M
nsim0 <- 1000 # number of simulation draws w/in each year for estimating rho, g
gsim <- array(dim = c(nsim0, nclass)) # estimated g
msimr <- array(dim = c(nsim0, nclass)) # estimated rho
tst <- proc.time()
for (simi in 1:nsim){ # each simi is a simulated year
  for (cli in 1:nclass){
    gsim[, cli] <- rbeta(nsim0, y[cli, simi] + 0.5, N[cli] - y[cli, simi] + .5)
    msimr[, cli] <- Xsim[simi, cli]/gsim[, cli] # point estimate of rho
  }
  # combine the g's
  rhosim <- msimr/rowSums(msimr)
  ghat <- rowSums(gsim * rhosim)
  Bparm <- suppressWarnings(
    MASS::fitdistr(ghat, 'beta', start = Babgmus2(mean(ghat), var(ghat)))
  )
  g0 <- qbeta(0.005, Bparm$est[1], Bparm$est[2])
  x <- sum(Xsim[simi, ])
  mmax <- round(x/g0 + 8*sqrt(x*(1-g0)/g0^2))
  pm <- eoa::postM.ab(x, Bparm$est[1], Bparm$est[2], prior = "betabinRef", mmax=mmax)
  for (ai in 1:length(alpha)) Mci[simi, ,ai] <- eoa::MCI(pm, 1-alpha[ai])
  Mhat[simi,1] <- min(which(cumsum(pm) >= 0.5)) - 1
  Mhat[simi,2] <- sum((1:length(pm)-1)*pm)
  if (simi%%100 == 0){
    print(paste(simi, round((proc.time()-tst)[3],1), " seconds"))
    flush.console()
    tst<-proc.time()
  }
}
# coverages for Mci's are right on target,
# intervals have (roughly) equal probabilities of missing high or low
for (ai in 1:length(alpha)){
  print(paste0(
    "1 - alpha = ", 1 - alpha[ai], ", ",
    "coverage = ", 1 - sum(Mci[,1, ai] > sum(M) | Mci[,2,ai] < sum(M))/nsim, ",  ",
    sum(Mci[,1, ai] > sum(M)), " missed high, ",
    sum(Mci[,2,ai] < sum(M)), " missed low")
  )
}

## figs showing CIs and summary stats for 1st 100 sim reps:
ai<-1
plot(0, 0, type = 'n', xlim = c(1, 100), ylim = range(Mci[1:100, , ai]), xlab = 'simulation rep', ylab = 'estimated M')
for (i in 1:100){
  lines(rep(i,2), Mci[i, , ai], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)))
  points(i, Mhat[i, 1], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)), pch = 3)
}
abline(h = sum(M), col=4)
title("Simulated CIs for total mortality in two independent classes")
mtext(side = 3, adj = 0, text = paste0("\u03b1 = ", alpha[ai]), col = 2)
mtext(side = 3, adj = 1, text = paste0(
  "M = c(", M[1],", ", M[2],"), ",
  "g = c(", g[1], ", ", g[2],"), ",
  "N = c(", N[1], ", ", N[2],")  ")
)
text(par('usr')[1], par('usr')[4], adj = c(0, 1.5), col = 2, lab =
  paste0("   coverage = ", 100 - sum(Mci[1:100, 1, ai] > sum(M) | Mci[1:100, 2, ai] < sum(M)), "/100: ",
    sum(Mci[1:100, 1, ai] > sum(M)), " missed high", ", ",
    sum(Mci[1:100, 2, ai] < sum(M)), " missed low"
  )
)

ai<-2
plot(0, 0, type = 'n', xlim = c(1, 100), ylim = range(Mci[1:100, , ai]), xlab = 'simulation rep', ylab = 'estimated M')
for (i in 1:100){
  lines(rep(i,2), Mci[i, , ai], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)))
  points(i, Mhat[i, 1], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)), pch = 3)
}
abline(h = sum(M), col=4)
title("Simulated CIs for total mortality in two independent classes")
mtext(side = 3, adj = 0, text = paste0("\u03b1 = ", alpha[ai]), col = 2)
mtext(side = 3, adj = 1, text = paste0(
  "M = c(", M[1],", ", M[2],"), ",
  "g = c(", g[1], ", ", g[2],"), ",
  "N = c(", N[1], ", ", N[2],")  ")
)
text(par('usr')[1], par('usr')[4], adj = c(0, 1.5), col = 2, lab =
  paste0("   coverage = ", 100 - sum(Mci[1:100, 1, ai] > sum(M) | Mci[1:100, 2, ai] < sum(M)), "/100: ",
    sum(Mci[1:100, 1, ai] > sum(M)), " missed high", ", ",
    sum(Mci[1:100, 2, ai] < sum(M)), " missed low"
  )
)

ai<-3
plot(0, 0, type = 'n', xlim = c(1, 100), ylim = range(Mci[1:100, , ai]), xlab = 'simulation rep', ylab = 'estimated M')
for (i in 1:100){
  lines(rep(i,2), Mci[i, , ai], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)))
  points(i, Mhat[i, 1], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)), pch = 3)
}
abline(h = sum(M), col=4)
title("Simulated CIs for total mortality in two independent classes")
mtext(side = 3, adj = 0, text = paste0("\u03b1 = ", alpha[ai]), col = 2)
mtext(side = 3, adj = 1, text = paste0(
  "M = c(", M[1],", ", M[2],"), ",
  "g = c(", g[1], ", ", g[2],"), ",
  "N = c(", N[1], ", ", N[2],")  ")
)
text(par('usr')[1], par('usr')[4], adj = c(0, 1.5), col = 2, lab =
  paste0("   coverage = ", 100 - sum(Mci[1:100, 1, ai] > sum(M) | Mci[1:100, 2, ai] < sum(M)), "/100: ",
    sum(Mci[1:100, 1, ai] > sum(M)), " missed high", ", ",
    sum(Mci[1:100, 2, ai] < sum(M)), " missed low"
  )
)

ai<-4
plot(0, 0, type = 'n', xlim = c(1, 100), ylim = range(Mci[1:100, , ai]), xlab = 'simulation rep', ylab = 'estimated M')
for (i in 1:100){
  lines(rep(i,2), Mci[i, , ai], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)))
  points(i, Mhat[i, 1], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)), pch = 3)
}
abline(h = sum(M), col=4)
title("Simulated CIs for total mortality in two independent classes")
mtext(side = 3, adj = 0, text = paste0("\u03b1 = ", alpha[ai]), col = 2)
mtext(side = 3, adj = 1, text = paste0(
  "M = c(", M[1],", ", M[2],"), ",
  "g = c(", g[1], ", ", g[2],"), ",
  "N = c(", N[1], ", ", N[2],")  ")
)
text(par('usr')[1], par('usr')[4], adj = c(0, 1.5), col = 2, lab =
  paste0("   coverage = ", 100 - sum(Mci[1:100, 1, ai] > sum(M) | Mci[1:100, 2, ai] < sum(M)), "/100: ",
    sum(Mci[1:100, 1, ai] > sum(M)), " missed high", ", ",
    sum(Mci[1:100, 2, ai] < sum(M)), " missed low"
  )
)

ai<-5
plot(0, 0, type = 'n', xlim = c(1, 100), ylim = range(Mci[1:100, , ai]), xlab = 'simulation rep', ylab = 'estimated M')
for (i in 1:100){
  lines(rep(i,2), Mci[i, , ai], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)))
  points(i, Mhat[i, 1], col = 1 + (Mci[i, 1, ai] > sum(M) | Mci[i, 2, ai] < sum(M)), pch = 3)
}
abline(h = sum(M), col=4)
title("Simulated CIs for total mortality in two independent classes")
mtext(side = 3, adj = 0, text = paste0("\u03b1 = ", alpha[ai]), col = 2)
mtext(side = 3, adj = 1, text = paste0(
  "M = c(", M[1],", ", M[2],"), ",
  "g = c(", g[1], ", ", g[2],"), ",
  "N = c(", N[1], ", ", N[2],")  ")
)
text(par('usr')[1], par('usr')[4], adj = c(0, 1.5), col = 2, lab =
  paste0("   coverage = ", 100 - sum(Mci[1:100, 1, ai] > sum(M) | Mci[1:100, 2, ai] < sum(M)), "/100: ",
    sum(Mci[1:100, 1, ai] > sum(M)), " missed high", ", ",
    sum(Mci[1:100, 2, ai] < sum(M)), " missed low"
  )
)
