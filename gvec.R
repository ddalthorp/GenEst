# gvec -- a function for calculating detection probability in a single class given search schedule
#   and simulated columns of carcass persistence times and pk parameters
#
# ARGUMENTS
#   days = vector of search times, beginning at 0
#   CPab = simulated vector of persistence parameters (Weibull if 2-d or exponential if 1-d)
#     parameterization: for exponential? means; for Weibull? R's shape and scale (like eoa)
#   seef = searcher efficiency parameters
#     if k not derived from a pk model of field trial data, then seef is a vector of simulated
#       searcher efficiencies and k is a constant defined by the user
#     if k is derived from field trial data, then seef is an nsim x 2 array of simulated p and k
#
# VALUE
#   A vector of simulated detection probabilities

gvec <- function(days, CPab, seef, k = NULL){
# need: error-checking (including k must be scalar between 0 and 1 if seef is a vector)
  nsim <- dim(CPab)[1]
  samtype <- ifelse(length(unique(diff(days))) == 1, "Formula", "Custom")
  nsearch <- length(days) - 1
  if (is.vector(CPab)){
    persistence_distn <- "Exponential"
    pdb <- CPab
    pda <- 1/pdb
    pdb0 <- exp(mean(log(pdb)))
    pda0 <- 1/pdb0
  } else {
    persistence_distn <- "Weibull"
    pda <- CPab[, 1]
    pdb <- CPab[, 2]
    pdb0 <- exp(mean(log(pdb)))
    pda0 <- 1/mean(1/pda)
  }
  pk <- array(dim = c(nsim, 2))
  if (is.vector(seef)){
    pk[, 1] <- seef
    pk[, 2] <- k
  } else {
    pk <- seef
  }

  ###1. setting estimation control parameters
  ##  a. search limit: number of searches after arrival to include in estimate of seef
  ##   [when the number of searches is high, including them all in the estimation is
  ##    calculation intensive but does not contribute signficantly to the result]
  f0 <- mean(pk[, 1])
  k0 <- mean(pk[, 2])
  ind1 <- rep(1:nsearch, times = nsearch:1)
  ind2 <- ind1+1
  ind3 <- unlist(lapply(1:nsearch, function(x) x:nsearch)) + 1
  schedule.index <- cbind(ind1,ind2,ind3)
  schedule <- cbind(days[ind1],days[ind2],days[ind3])
  nmiss <- schedule.index[,3] - schedule.index[,2]
  maxmiss <- max(nmiss)
  powk <- cumprod(c(1, rep(k0, maxmiss))) # vector of k^i's
  notfind <- cumprod(1 - f0*powk[-length(powk)])
  nvec <- c(1, notfind) * f0
  # conditional probability of finding a carcass on the ith search (row) after arrival
  # for given (simulated) searcher efficiency (column):
  pfind.si <- nvec * powk
  # persistences:
  intxsearch <- unique(cbind(schedule[,2] - schedule[,1], schedule[,3] - schedule[,2]), MAR = 1)
  ppersu <- ppersist(persistence_distn,
    t_arrive0 = 0,
    t_arrive1 = intxsearch[,1],
    t_search = intxsearch[,1] + intxsearch[,2],
    pda = pda0, pdb = pdb0
  )
  arrvec <- (schedule[,2] - schedule[,1])/max(days)
  prob_obs <- numeric(dim(schedule)[1])
  for (i in 1:length(prob_obs)){
    prob_obs[i] <- pfind.si[nmiss[i]+1] *
      ppersu[which(
        abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
        abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001
      ),] * arrvec[i]
  }
  ggnm <- numeric(maxmiss+1)
  for (missi in 0:maxmiss){
    ggnm[missi+1] <- sum(prob_obs[nmiss==missi])
  }
  # if more than 10 searches, consider truncating search schedule because very few carcasses
  # will be found after being missed 9 or more times, but very large number of searches is
  # costly in terms of calculation efficiency
  if (nsearch > 10){
    iskip <- min(which(cumsum(ggnm)/sum(ggnm) > 0.99)) + 1
    # cutting off the search schedule introduces a slight bias.
    # Correct by multiplying the final g's by gadj = sum(ggnm)/ggnm[iskip]
    gadj <- sum(ggnm)/sum(ggnm[1:iskip])
  } else {
    iskip <- maxmiss
    gadj <- 1
  }
  ###2. estimation of g
  #a. subset the search schedule
  #   (ignoring probabilities of detection carcasses after they have been missed several times):
  schedule <- cbind(days[ind1], days[ind2], days[ind3])[ind2 >= ind3 - iskip + 1,]
  #columns for arrival interval and search number:
  schedule.index <- cbind(ind1, ind2, ind3)[ind2 >= ind3 - iskip+1,]
  nmiss <- schedule.index[,3] - schedule.index[,2]
  maxmiss <- max(nmiss)
  # searcher efficiencies
  if (maxmiss == 0) {
    pfind.si <- pk[,1]
  } else if (maxmiss == 1){
    pfind.si<-cbind(pk[,1], (1 - pk[,1]) * pk[,2]*pk[,1])
  } else {
    powk<-array(rep(pk[, 2], maxmiss + 1), dim=c(nsim, maxmiss+1))
    powk[,1] <- 1
    powk <- matrixStats::rowCumprods(powk)
    pfind.si <- pk[,1] * powk *
      cbind(rep(1, nsim), matrixStats::rowCumprods(1 - (pk[,1] * powk[, 1:maxmiss])))
  }
  intxsearch<-unique(cbind(schedule[,2] - schedule[,1], schedule[,3] - schedule[,2]), MAR=1)
  ppersu<-ppersist(persistence_distn,
    t_arrive0 = 0,
    t_arrive1 = intxsearch[,1],
    t_search = intxsearch[,1] + intxsearch[,2],
    pda = CPab[,1], pdb = CPab[,2]
  )
  # arrivals
  arrvec <- (schedule[,2]-schedule[,1])/max(days) # assumes uniform arrivals
  # add the probabilities
  prob_obs <- numeric(nsim)
  if (maxmiss > 0){
    for (i in 1:dim(schedule)[1]){
      prob_obs <- prob_obs +
        pfind.si[,nmiss[i]+1] *
        ppersu[which(
          abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
          abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001),
        ] * arrvec[i]
    }
  } else {
    for (i in 1:dim(schedule)[1]){
      prob_obs <- prob_obs +
      pfind.si[nmiss[i]+1] *
      ppersu[which(
        abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
        abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001),
      ] * arrvec[i]
    }
  }
  # g for monitored period
  prob_obs
}