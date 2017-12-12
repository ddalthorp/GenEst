# 2 visibility classes
# 10 turbines with differing ground configurations and search schedules
# Independent field trials among 3 visibility classes (E, M, D) and 2 species (B and b)

#### Parameters:
# the setup
# Total number of carcasses = 1000, dispersed among: 10 turbines, 3 vis, 2 sizes as follows:
nturb <- 10
vis <- c('E', 'M', 'D')
spec <- c('B', 'b') # Bat and small bird
covarray <- cbind(rep(vis, length(spec)), sort(rep(spec, length(vis)), decreasing = T))
# fractions at each turbine
set.seed(100) # ensures consistent initialization
aT <- array(runif(20, 0.05, 0.15), dim = c(10, 2)) # 10 turbines, 2 species
aT <- t(t(aT)/colSums(aT))
aT <- aT[order(aT[,1]),]
# relative class weights at each turbine
# assume the same weights for small birds and bats...maybe not realisitic but it does not come
# into play in the model. This is just an opening setup for analysis
avis <- array(dim = c(10, 3))
avis[, 1] <- runif(10)/2
avis[, 2] <- (1-avis[,1])/3
avis[, 3] <- 1 - rowSums(avis[, 1:2])
# What is the final distribution of carcasses?
# cross table with vis x spec x turbine
Mtot <- 1000
aBb <- c(0.7, 0.3) # 70% of carcasses are bats; 30%, birds
MBb <- rmultinom(1, Mtot, aBb) # total birds and bats killed
Bbturb <- cbind(rmultinom(1, MBb[1], aT[,1]), rmultinom(1, MBb[2], aT[,2])) # per turbine counts
Bbvis <- array(dim = c(10, length(vis), length(spec)))
for (speci in 1:length(spec)){
  for (turbi in 1:nturb){
    Bbvis[turbi, , speci] <- rmultinom(1, Bbturb[turbi, speci], avis[turbi, ])
  }
}
### field trials
# actual persistence parameters, Weibull with the following parameters

CPab <- array(dim = c(length(vis), 2, length(spec)))   # assume Weibull
CPab[1, ,1] <- c(0.51, 1.40) # patterned on default EoA, easy
CPab[2, ,1] <- c(0.58, 7.27) # patterned on default EoA, moderate
CPab[3, ,1] <- c(0.75, 9.68) # patterned on default EoA, difficult
CPab[1, ,2] <- c(0.55, 6)  # tweak of the CPab[2:3, ,2] patterns described below
CPab[2, ,2] <- c(0.49, 13) # patterned on Altamont, Coleman vignette for acmeR
CPab[3, ,2] <- c(0.55, 24) # patterned on Altamont, small birds (acmeR example in software)
# actual pk parameters for each class
k0 <- array(0.75, dim = c(length(vis), length(spec))) # suppose all have the same k
p0 <- cbind(c(0.8, 0.5, 0.2), c(0.7, 0.6, 0.4)) # initial SE for each visibility class for birds and bats

### carcass persistence trials
# field trials carcasses (number of carcasses in each class)
nCPcarc <- array(rep(15, length(vis) * length(spec)), dim = c(length(vis), length(spec)))
# 15 trial carcasses for each class for CP field trials
# fates of carcasses in field trials
# persistence times
set.seed(157) # random
CPtimes <- list()
for (i in 1:length(vis)){
  CPtimes[[i]] <- rweibull(nCPcarc[i], CPab[i, 1, 1], CPab[i, 2, 1])
  CPtimes[[i+length(vis)]] <- rweibull(nCPcarc[i], CPab[i, 1, 2], CPab[i, 2, 2])
}
# CP trial carcass checks
CPcheck <- c(1, 2, 3, 7, 10, 14, 21, 28)
ints <- lapply(CPtimes, function(x) findInterval(x, CPcheck))
# CP field trial results
cpTrial <- data.frame(array(dim = c(sum(nCPcarc), 4)))
names(cpTrial) <- c('vis', 'spec', 'CPmin', 'CPmax')
for (i in 1:length(CPtimes)){
  CPdat <- array(numeric(nCPcarc[i] * 2), dim = c(nCPcarc[i], 2))
  CPdat[ints[[i]] == length(CPcheck), 1] <- max(CPcheck)
  CPdat[ints[[i]] == length(CPcheck), 2] <- Inf
  ind <- which(ints[[i]] > 0 & ints[[i]] < length(CPcheck))
  CPdat[ind, 1] <- CPcheck[ints[[i]][ind]]
  CPdat[ind, 2] <- CPcheck[ints[[i]][ind]+1]
  CPdat[ints[[i]] == 0, 1] <- 0
  CPdat[ints[[i]] == 0, 2]<- CPcheck[1]
  beg <- min(which(is.na(cpTrial$vis)))
  cpTrial$CPmin[beg:(beg + nCPcarc[i] - 1)] <- CPdat[, 1]
  cpTrial$CPmax[beg:(beg + nCPcarc[i] - 1)] <- CPdat[, 2]
  cpTrial$vis[beg:(beg + nCPcarc[i] - 1)] <- covarray[i, 1]
  cpTrial$spec[beg:(beg + nCPcarc[i] - 1)] <- covarray[i, 2]
}

### searcher efficiency trials (assume no attrition from scavenging)
# Set up the trials
# 15 trial carcasses for each class for pk field trials
set.seed(23)
nPKcarc <- array(rep(15, length(vis) * length(spec)), dim = c(length(vis), length(spec)))
sn <- 7 # number of searches
# run the trials
Y <- array(dim = c(max(nPKcarc), sn, length(vis), length(spec)))
p <- array(dim = dim(Y)[-1])
for (speci in 1:length(spec)){
  for (visi in 1:length(vis)){
    nav <- nPKcarc[visi, speci] # number of carcasses available
    Y[, 1, visi, speci] <- rbinom(nav, size = 1, prob = p0[visi, speci])
    p[1, visi, speci] <- p0[visi, speci]
    nav <- sum(Y[, 1, visi, speci] == 0)
    for (si in 2:sn){
      p[si, visi, speci] <- p0[visi, speci] * k0[visi, speci]^(si-1)
      Y[which(Y[, si-1, visi, speci] == 0), si, visi, speci] <-
        rbinom(nav, size = 1, prob = p0[visi, speci] * k0[visi, speci]^(si-1))
      nav <- sum(na.omit(Y[, si, visi, speci]) == 0)
      if (nav == 0) break
    }
  }
}
# create pk trial data
pkTrial <- data.frame(array(dim = c(prod(dim(Y)[-2]), sn + 2)))
nms <- numeric(sn + 2)
nms[1:2] <- c('vis', 'spec')
for (i in 1:sn) nms[i + 2] <- paste0('s', i)
names(pkTrial) <- nms
junk <- NULL
for (i in 1:(length(vis)*length(spec))) junk <- c(junk, rep(covarray[i, 1], nPKcarc[i]))
pkTrial$vis <- junk
junk <- NULL
for (i in 1:(length(vis)*length(spec))) junk <- c(junk, rep(covarray[i, 2], nPKcarc[i]))
pkTrial$spec <- junk
for (i in 1:(length(vis)*length(spec))){
  pkTrial[1:15 + (i - 1) * max(nPKcarc), 3:dim(pkTrial)[2]] <- Y[, , i - 3*(i > 3), 1 + (i > 3)]
}

# arrivals
Isamnsearch <- cbind(rep(c(2, 7), 5), rep(c(91, 26), 5)) # Isam and nsearch
sched <- list()
for (ti in 1:nturb){
  sched[[ti]] <- seq(0, prod(Isamnsearch[ti, ]), by = Isamnsearch[ti, 1])
}
# unravel the ACTUAL carcass array into a dataframe with 1000 carcasses and covariates aligned
# turbines, species, visibility, arrival time, persistence time, actual probability of detection
trackM <- data.frame(array(dim = c(Mtot, 11)))
names(trackM) <- c('turb', 'spec', 'vis', 'arr', 'ptime', 'scav', 'nsi', 'p', 'k', 'g', 'x')
trackM$spec <- c(rep(spec[1], MBb[1]), rep(spec[2], MBb[2]))
cst <- cumsum(as.vector(Bbturb))
set.seed(91)
trackM$arr <- runif(1000) * max(unlist(lapply(sched, 'max'))) # arrival times of carcasses
for (speci in 1:length(spec)){
  for (ti in 1:nturb){
    beg <- min(which(is.na(trackM$turb)))
    nti <- Bbturb[ti, speci]
    trackM$turb[beg:(beg + nti -1)] <- ti
    trackM$vis[beg:(beg + nti -1)] <- c(
      rep(vis[1], Bbvis[ti, 1, speci]),
      rep(vis[2], Bbvis[ti, 2, speci]),
      rep(vis[3], Bbvis[ti, 3, speci])
    )
    trackM$ptime[beg:(beg + nti -1)] <- c(
      rweibull(Bbvis[ti, 1, speci], CPab[1, 1, speci], CPab[1, 2, speci]),
      rweibull(Bbvis[ti, 2, speci], CPab[2, 1, speci], CPab[2, 2, speci]),
      rweibull(Bbvis[ti, 3, speci], CPab[3, 1, speci], CPab[3, 2, speci])
    )
  }
}
trackM$scav <- trackM$arr + trackM$ptime
for (ci in 1:Mtot){
  trackM$nsi[ci] <- sum(
    (sched[[trackM$turb[ci]]] < trackM$scav[ci]) &
    (sched[[trackM$turb[ci]]] > trackM$arr[ci])
  )
  trackM$p[ci] <- p0[which(vis == trackM$vis[ci]), which(spec == trackM$spec[ci])]
  trackM$k[ci] <- k0[which(vis == trackM$vis[ci]), which(spec == trackM$spec[ci])]
}
powk <- t(t(array(trackM$k, dim = dim(pmiss)))^(1:max(trackM$nsi)-1))
pmiss <- matrixStats::rowCumprods(1 - trackM$p*powk)
ind <- which(trackM$nsi > 0)
trackM$g <- 0
trackM$g[ind] <- 1 - pmiss[cbind((1:Mtot)[ind], trackM$nsi[ind])]
trackM$x <- rbinom(Mtot, 1, trackM$g)

#search data
searchData <- trackM[trackM$x == 1, c('turb', 'vis', 'spec')]

set.seed(NULL)


