# an example of data flow and analysis
# load functions and data sets
# CP field trials, SE field trials, search results
source('utilities.R')
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

# fit all 25 pk models and store results in a list:
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
# basic indexing of models is:
#   1 => ~ 1
#   2 => ~ cov1
#   3 => ~ cov2
#   4 => ~ cov1 + cov2
#   5 => ~ cov1 * cov2
# within the list, a character string refers to what the p and k models are
#   e.g., '25' => p ~ cov1, k ~ cov1 * cov2

# to list the models by AIC:
pkmodsumry <- data.frame(
  pmodel = unlist(lapply(pkmods, function(x) x$pname)),
  kmodel = unlist(lapply(pkmods, function(x) x$kname)),
  AIC = unlist(lapply(pkmods, function(x) x$aic))
)
pkmodsumry$dAIC <- pkmodsumry$AIC - min(pkmodsumry$AIC)
pkmodsumry[order(pkmodsumry$AIC), ]
# NOTE: Two ways to index into the list of models:
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
# box-and-whisker with median, IQR, and 95% CI
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
