# an example of data flow and analysis
# load functions and data sets
# CP field trials, SE field trials, search results
source('utilities.R')
load('pkTrial.Rdata') # bats in three visibility classes
CPdatFull <- read.csv('BatCP.csv', head = T, as.is = T)
# NOTE: The format of this input data is suggestive but not required. TBD by JPS and MMH.
# NOTE: CP data are not from the same study as the pk data, so the resulting g's are fictitious

# pk analysis
# An exceptionally large and well-constructed (anonymized) searcher efficiency database
# is stored in "SE data (example).cvs";
pkTrial<-read.csv("SE data (example).csv", header=T, as.is = T)
# NOTE: The as.is = T arg loads character vectors as characters rather than factor levels
# subsetting to model with covariates (vis) and (small bird vs. bat)
pktmp <- subset(pkTrial, size == 's' & vis != 'V', select = c('vis', 'bob', 's1', 's2', 's3', 's4'))
# NOTE: If as.is = F in the read.csv, subsetting to ingore a given factor level carries a list
# of the original factor levels into the subsetted data frame, which would guarantee empty
# cells in the subset and cause the pk modeling functions to crash.
# NOTE: All data in the search columns (s1, s2, ...) with value not in c(0, 1) are ignored.
# It is assumed that there is at most one 1 per row and that there are no 0s any time after a 1.
# If 2 covariates, then to look at 25 models (5p x 5k):
covars <- c('bob', 'vis')
# NOTE: pk analysis alphabetizes both the covariates and the levels within covariates when
# reporting results as a convention to make it easier to keep track of indices etc.
pkmods <- list()
modcovs <- list(1, covars[1], covars[2], covars, covars)
for (ki in 1:length(modcovs)){
  for (pi in 1:length(modcovs)){
    pkmods[[paste0(pi, ki)]] <- pkMod$new(
      dat = pktmp,
      pvars = modcovs[[pi]], pop = ifelse(pi == 5, '*', '+'),
      kvars = modcovs[[ki]], kop = ifelse(ki == 5, '*', '+'),
      getVar = T
      # extracting the estimator variances nearly doubles the run-time, but it may be helpful
      # in model selection b/c it allows construction of CIs for comparison to cellwise model.
      # For simple AIC comparisons, getVar = F is faster.
    )
  }
}
pkmodsumry <- data.frame(
  pmodel = unlist(lapply(pkmods, function(x) x$pname)),
  kmodel = unlist(lapply(pkmods, function(x) x$kname)),
  AIC = unlist(lapply(pkmods, function(x) x$aic))
)
pkmodsumry$dAIC <- pkmodsumry$AIC - min(pkmodsumry$AIC)
pkmodsumry[order(pkmodsumry$AIC), ]
# NOTE: There are a couple ways to index into the list of models.
# First is simply by order number, e.g.,
pkmods[[17]]$aic # aic for the 17th model in the list
pkmods[[order(pkmodsumry$AIC)[1]]]$aic # aic for the best model (according to aic)

# Second is to index by cell code name
# e.g., model '43' is the 4th p model and the 3rd k model (p ~ cov1 + cov2, and k ~ cov2)
paste(pkmods[['43']]$pname, ', ', pkmods[['43']]$kname)
# NOTE: In order, models are: 1, cov1, cov2, cov1 + cov2, cov1 * cov2

# some additional diagnostics to supplement the AICs in model selection
# compare a given pk model results with full fits...
# full model: '55' (i.e., cov1 * cov2 for both p and k)
statsFull<-pkmods[['55']]$cellStats()
# compared with the best in AIC (pkmodsumry[order(pkmodsumry$AIC), ])
stats51 <-pkmods[['51']]$cellStats(covars) # covars is vector of covariate names for full cells
# $cellStats returns dataframes with cellwise summary stats for p and k for the given model
# e.g., some quickly sketched comparative graphs:
# cellwise fits in black; '51' model in blue
# box-and-whiskers with median, IQR, and 95% CI
par(mfcol = c(2,1), mar = c(4, 4, 1, .5))
egDat<-pkmods[['55']]$egDat
ncell <- dim(pkmods[['55']]$egDat)[1]
plot(0, 0, type = 'n', axes = F,
  xlim = c(1, ncell), ylim = range(statsFull$p),
  xlab = '', ylab = 'estimated p'
)
axis(1, at = 1:ncell, lab = egDat$cellNames)
axis(2)
box()
points(1:ncell - 0.05, statsFull$p$median, pch = 3)
points(1:ncell + 0.05, stats51$p$median, pch = 3, col = colors()[124])
for (i in 1:ncell) {
  lines(rep(i, 2) - 0.05, c(statsFull$p$q0.025[i], statsFull$p$q0.975[i]))
  lines(rep(i, 2) + 0.05, c(stats51$p$q0.025[i], stats51$p$q0.975[i]), col = colors()[124])
  polygon(i - 0.05 + 0.02*c(1, 1, -1, -1), c(statsFull$p$'1st Qu.'[i], statsFull$p$'3rd Qu.'[i], statsFull$p$'3rd Qu.'[i], statsFull$p$'1st Qu.'[i]))
  polygon(i + 0.05 + 0.02*c(1, 1, -1, -1), c(stats51$p$'1st Qu.'[i], stats51$p$'3rd Qu.'[i], stats51$p$'3rd Qu.'[i], stats51$p$'1st Qu.'[i]), border = colors()[124])
}
mtext(side = 3, pkmods[['51']]$pname)
plot(0, 0, type = 'n', axes = F,
  xlim = c(1, ncell), ylim = range(statsFull$k),
  xlab = '', ylab = 'estimated k'
)
axis(1, at = 1:ncell, lab = egDat$cellNames)
axis(2)
box()
points(1:ncell - 0.05, statsFull$k$median, pch = 3)
points(1:ncell + 0.05, stats51$k$median, pch = 3, col = colors()[124])
for (i in 1:ncell) {
  lines(rep(i, 2) - 0.05, c(statsFull$k$q0.025[i], statsFull$k$q0.975[i]))
  lines(rep(i, 2) + 0.05, c(stats51$k$q0.025[i], stats51$k$q0.975[i]), col = colors()[124])
  polygon(i - 0.05 + 0.02*c(1, 1, -1, -1), c(statsFull$k$'1st Qu.'[i], statsFull$k$'3rd Qu.'[i], statsFull$k$'3rd Qu.'[i], statsFull$k$'1st Qu.'[i]))
  polygon(i + 0.05 + 0.02*c(1, 1, -1, -1), c(stats51$k$'1st Qu.'[i], stats51$k$'3rd Qu.'[i], stats51$k$'3rd Qu.'[i], stats51$k$'1st Qu.'[i]), border = colors()[124])
}
mtext(side = 3, pkmods[['51']]$kname)

# or comparing the worst in AIC ('22')
stats22 <-pkmods[['22']]$cellStats(covars) # covars is vector of covariate names for full cells
# $cellStats returns dataframes with cellwise summary stats for p and k for the given model
# e.g., some quickly sketched comparative graphs:
# cellwise fits in black; '22' model in blue
# median and 95% CI
par(mfcol = c(2,1), mar = c(4, 4, 1, .5))
ncell <- dim(pkmods[['55']]$egDat)[1]
plot(0, 0, type = 'n', axes = F,
  xlim = c(1, ncell), ylim = range(statsFull$p),
  xlab = '', ylab = 'estimated p'
)
axis(1, at = 1:ncell, lab = egDat$cellNames)
axis(2)
box()
points(1:ncell - 0.05, statsFull$p$median, pch = 3)
points(1:ncell + 0.05, stats22$p$median, pch = 3, col = colors()[124])
for (i in 1:ncell) {
  lines(rep(i, 2) - 0.05, c(statsFull$p$q0.025[i], statsFull$p$q0.975[i]))
  lines(rep(i, 2) + 0.05, c(stats22$p$q0.025[i], stats22$p$q0.975[i]), col = colors()[124])
  polygon(i - 0.05 + 0.02*c(1, 1, -1, -1), c(statsFull$p$'1st Qu.'[i], statsFull$p$'3rd Qu.'[i], statsFull$p$'3rd Qu.'[i], statsFull$p$'1st Qu.'[i]))
  polygon(i + 0.05 + 0.02*c(1, 1, -1, -1), c(stats22$p$'1st Qu.'[i], stats22$p$'3rd Qu.'[i], stats22$p$'3rd Qu.'[i], stats22$p$'1st Qu.'[i]), border = colors()[124])
}
mtext(side = 3, pkmods[['22']]$pname)
plot(0, 0, type = 'n', axes = F,
  xlim = c(1, ncell), ylim = range(statsFull$k),
  xlab = '', ylab = 'estimated k'
)
axis(1, at = 1:ncell, lab = egDat$cellNames)
axis(2)
box()
points(1:ncell - 0.05, statsFull$k$median, pch = 3)
points(1:ncell + 0.05, stats22$k$median, pch = 3, col = colors()[124])
for (i in 1:ncell) {
  lines(rep(i, 2) - 0.05, c(statsFull$k$q0.025[i], statsFull$k$q0.975[i]))
  lines(rep(i, 2) + 0.05, c(stats22$k$q0.025[i], stats22$k$q0.975[i]), col = colors()[124])
  polygon(i - 0.05 + 0.02*c(1, 1, -1, -1), c(statsFull$k$'1st Qu.'[i], statsFull$k$'3rd Qu.'[i], statsFull$k$'3rd Qu.'[i], statsFull$k$'1st Qu.'[i]))
  polygon(i + 0.05 + 0.02*c(1, 1, -1, -1), c(stats22$k$'1st Qu.'[i], stats22$k$'3rd Qu.'[i], stats22$k$'3rd Qu.'[i], stats22$k$'1st Qu.'[i]), border = colors()[124])
}
mtext(side = 3, pkmods[['22']]$kname)

# simulate from the selected pk model
nsim <- 1000
# the '31' model has AIC nearly as low as the '51' model's, but doesn't use 'bob' as a covariate
# since the CP data set has no 'bob', let's use the '31' model here
cellpk <- pkmods[['31']]$pksim(nsim)
# NOTICE: because k model is k ~ 1, the k's for all cells are identical
# NOTE: These pk's are ready to be fed into gvec, cell-by-cell, but we need CPs first...
##########
# CP modeling
# preliminaries:
CP <- CPdatFull[(CPdatFull$Species %in% c("LABO", "LANO", "PESU")), c("Species", "Visibility", "Left", "Right", "Event")]
names(CP) <- c('spec', 'vis', 'left', 'right', 'event')
CP$vis[CP$vis == 'Difficult'] <- 'D'
CP$vis[CP$vis == 'Easy'] <- 'E'
CP$vis[CP$vis == 'Moderate'] <- 'M'

# names have been changed to match pk data (spec, vis) and other code
# data for 'survival' models must be in columns: left, right, event
# The CP data frame matches that format and is good to go
# An alternative format that may be easier for some users would be to have just two columns:
# one for "last seen" (CPmin) and one for "first gone" (CPmax). In this formatting, for carcasses persisting
# beyond  the study period, "first gone" is Inf. If the scavenging event was observed (e.g.,
# camera), then value in both columns would be identical.

# NOTE!!!! The following, short, commented-out section is unnecessary if CP data are entered
# with 'event' column. To translate  [CPmin, CPmax] format into [left, right, event] format:
#event<-ifelse(CP$CPmin == CP$CPmax, 1, ifelse(CP$CPmin == 0, 2, ifelse(CP$CPmax == Inf, 0, 3)))
#left<-CP$CPmin
#left[event==2]<-CP$CPmax[event==2]
#right<-CP$CPmax
#right[event==0]<-CP$CPmin[event==0]
#surv<- survival::Surv(time=left, time2=right, event=event, type=c('interval'))

# or, assuming data are already in [left, right, event] format:
surv<- survival::Surv(time=CP$left, time2=CP$right, event=CP$event, type=c('interval'))

# the desired model (after model selection by AIC or whatever):
# NOTE: In CPmod.R there is code for some earlier model selection routines I wrote before we
# formally moved to the four families.
modForm <- as.formula(paste('surv ~', 'vis'))
cpdist <- "weibull"
CPmod <- survival::survreg(modForm, dist = cpdist, data = CP)
egDat <- make_egDat(vars = 'vis', dat = CP)
# NOTE: up to two covariates may be used in CP and pk models combined
cellCP <- simCP(nsims = nsim, survOut = CPmod, egDat = egDat)
names(cellCP)
colnames(cellCP$pdaSim)

# NOTE: 'survival' package uses different parameterizations for the Weibull, lognormal, and
# exponential than does the R base package and uses a different parameterization from the
# package 'actuar' for modeling loglogistic. Translation guide (where 'mod' is a survival model
# as specified in survreg output => R parameter => pda or pdb in EoA):
# exponential: exp(mod$coef) = 1/rate = pdb
# Weibull: 1/mod$scale = shape = pda, exp(mod$coef) = scale = pdb
# lognormal: sdlog = mod$scale = sqrt(pda), mod$coef = meanlog = pdb
# loglogistic: 1/mod$scale = shape = pda, exp(mod$coef) = scale = pdb

# NOTE: base R does not include loglogistic; shape & scale here are for package 'actuar'
# NOTE: Wikipedia parameterization for loglogistic is identical to that of 'actuar' BUT with
#   the names of the parameters reversed!

# A simulated search data set is created below:
# 100 carcasses were found
ncarc <- 100
# 3 cells (vis = D, E, M)
ncell <- 3
set.seed(62)
# NOTE: seeds are set so that data set is reproducible
summarycells <- rmultinom(1, 100, c(0.2, 0.5, 0.3))
cells<-NULL
for (i in 1:ncell) cells<-c(cells, rep(i, summarycells[i]))

# suppose we want to summarize the data according to a split defined in covariate A (2 levels)
set.seed(127)
A <- rbinom(ncarc, 1, 1/3)
# WARNING: This scenario does not include the dwp's ('a')
# carcasses were found on turbines with two different search schedules
set.seed(83)
schedind <- rbinom(ncarc, 1, 1/4) + 1
searchSchedule <- list()
searchSchedule[[1]] <- 0:180 # search every day
searchSchedule[[2]] <- seq(0, 180, by = 3) # search once every three days
# NOTE: These schedules are formatted for the gvec function, which calculates ghat for the
# period of inference defined by the schedule. If season is a covariate, these schedules will
# need to be formatted to match the proper period of inference before being passed to gvec.
# In the splits, the carcasses found in a particular season are assumed to have arrived in that
# season.

searchdat <- data.frame(splits = A, schedule = schedind, cell = cells)
# NOTE: each row in the searchdat frame represents a carcass that was discovered in the searches
# This dataframe has been abstracted from the full array of search data (which may contain many
# more columns). The 'cells' column here comes from the matching of covariate levels in search
# data to the covariate levels in the pk and CP models. The names 'schedule' and 'cell' are used
# in the subsequent analysis and should be preserved.

# make sure indexing for cells from CP and pk trials match those of search trials
searchdat[searchdat$cell == 1,'cell']<-"D"
searchdat[searchdat$cell == 2,'cell']<-"E"
searchdat[searchdat$cell == 3,'cell']<-"M"
# NOTE: this will be more complication when there are two variables

# Estimate total M
# All estimation requires calculating gvec for combinations of search schedule and cell
components <- unique(searchdat[, c('schedule', 'cell')])
ncomp <- dim(components)[1]
gsim <- array(dim = c(nsim, ncomp))
tst<-proc.time()
for (compi in 1:ncomp){
  gsim[,compi]<-gvec(
    days = searchSchedule[[components$schedule[compi]]],
    CPab = cbind(cellCP$pdaSim[,components$cell[compi]], cellCP$pdbSim[,components$cell[compi]]),
    persdist = CPmod$dist,
    seef = cbind(cellpk$pSim[,components$cell[compi]], cellpk$kSim[,components$cell[compi]])
  )
  # NOTE: Subsetting would be smoother if CP and pk columns were grouped in pairs by cell rather
  # than all p's (or pda's) in one group and all k's (or pdb's) in a parallel group coming out of
  # $pksim (or simCP).
}
proc.time()-tst


X<-as.vector(table(searchdat$schedule, searchdat$cell))
Mhat <- rowSums(matrix(rbinom(nsim*ncomp, t(round(X/t(gsim))), gsim)/gsim, nrow = nsim))

# Estimate M for the splits delineated in the 'splits' column
splitlevel <- sort(unique(searchdat$splits))
Msplits <- array(dim = c(nsim, length(splitlevel)))
for (spi in 1:length(splitlevel)){
  X <- numeric(ncomp)
  for (compi in 1:ncomp){
    X[compi] <- sum(with(searchdat,
      splits == splitlevel[spi] &
      schedule == components$schedule[compi] &
      cell == components$cell[compi])
    )
  }
  # Xtilde estimator for splits and components...
  Msplits[, spi] <- rowSums(array(rbinom(nsim * ncomp, t(round(X/t(gsim))), gsim)/gsim, dim = c(nsim, ncomp)))
}