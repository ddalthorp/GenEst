### A look at splits: temporal and spatio-carcacal
# First, a quick data set is constructed and formatted for illustrating splits.
# We need to generate Aj and Mtilde arrays, which, in turn, requires ghat's.
# Massive shortcuts are taken in calculation of Aj, field trials, and estimation
# of g | (trials, ss), so the processes generating the data are not realistic,
# but the resulting Aj's and Mtilde's should be perfectly reasonable for example
# calculations.
#
# general scenario
# -- 2 seasons (spring and fall)
# -- 5 species (LABO LACI LANO MYLU MYSO)
# -- 3 visibility classes
# -- ghat ~ beta(10, 40) ==> mean of 0.2, estimated with uncertainty
# -- 15 turbines
# -- 7-day searches in spring; 3-day searches in fall

#####################################
##        preliminaries            ##
#####################################
require(GenEst) # v0.0.4.0 or later
# 0.0.4.0 is at GitHub branch "integrate-M-functions-and-splits" (2018-04-27)
require(cbinom)
#1. create raw data (search results)
spsearch <- 0:12*7 # search dates for spring
fasearch <- max(spsearch) + 1:26*3 # search dates for fall
days <- c(spsearch, fasearch) # combined search dates
set.seed(417) # for reproducibility
speclist <- c("LABO", "LACI", "LANO", "MYLU", "MYSO")
specnum <- c(82, 37, 23, 6, 1) # number of carcasses for each species
x <- sum(specnum)
discseason = sample(c("f","s"), size = x, replace = T, prob = c(.8, .2))
co <- data.frame(
  unit = sample(1:15, sum(specnum), replace = T),
  date = c( # dates (formatted as integers)
    sample(spsearch[-1], size = sum(discseason == "s"), replace = T),
    sample(fasearch, size = sum(discseason == "f"), replace = T)
  ),
  visibility = sample(c("D", "M", "RP"), size = x, replace = T, prob = c(0.45, 0.25, 0.3)),
  species = sample(rep(speclist, specnum)),
  stringsAsFactors = FALSE
)
co$si <- findInterval(co$date, days) - 1 # interval carcass was discovered in

#2. create Aj and Mtilde arrays (simulated)
nsim <- 100
probi <- c(0.05, 0.15, 0.8) # pAjgOi for j = i - 2, i - 1, i
Aj <- Mtilde <- array(0, dim = c(x, nsim))
for (xi in 1:x){
  si <- co$si[xi]
  if (si == 1){
    Aj[xi, ] <- 1
  } else if (si == 2){
    Aj[xi, ] <- si - rbinom(nsim, 1, 1 - probi[3]/sum(probi[2:3]))
  } else {
    ints <- as.vector(rmultinom(1, nsim, prob = probi))
    Aj[xi, ] <- si - sample(rep(2:0, times = ints))
  }
}
gsim <- rbeta(x * nsim, 10, 40)
Mtilde <- matrix((rcbinom(nsim*x, 1/gsim, gsim) - (mucbin(gsim)-1))/gsim, nrow = x)

masterSearchSchedule <- list(
  dwp = rep(1, 15),
  days = days,
  searches = array(1, dim = c(15, length(days))),
  season = rep(c("sp", "fa"), c(length(spsearch) - 1, length(fasearch))),
  unit = 1:15
)
#####################################
##        functions                ##
#####################################

# functions to calculate splits according to covariates, summarize results, plot
# input:
# Mest -- ncarc x nsim array of estimated mortalities
#   Mtilde is estimate for searched area; Mhat extrapolates to whole units
#   two other extrapolations we need to offer are: (1) extrapolation to whole
#   monitoring period, (2) extrapolation to whole site (e.g., sampling fraction
#   of turbines)
# Aj -- ncarc x nsim array of indices for arrival intervals,
#   e.g., Aj[xi, simi] = 3 means that carcass xi in simulation simi arrived
#   some time in search interval 3 (according to the search schedule at the unit
#   where carcass xi was discovered)
# split_CO -- a splitting covariate found in the carcass observation file (CO)
# COdat -- data frame with carcass discovery information (e.g., unit, date, and
#   covariates such as species, visibility). Format is not what we'd expect in
#   a .csv input file, but cleaned up after reading in)
# split_SS -- a splitting covariate found in the search schedule file (SS). This
#   will typically be covariate associated with arrival times (e.g., season)
# SSdat -- a list with information about the search schedule...days searched at
#   each unit, dwp for each unit, covariates (associated with each search day,
#   excluding the search at time t = 0...these covariates are assumed to be
#   associated with the interval ending with a search, so first value of a
#   covariate applies to the interval (0, t1], where t1 is the day of the first
#   "real" search
# split_time -- a vector of times to split the monitored period into.

splits<-calcSplits(Mest = Mtilde, Aj = Aj,
  split_CO = "species", COdat = co,
  split_SS = NULL, SSdat = masterSearchSchedule,
  split_time = NULL)
# calcSplits returns an entire simulated data set of class splitFull
# a splitFull object can be summarized using the S3 function:
summary(splits, CI_level = 0.95)
# and plotting using the S3 function:
plot(splits)
# 'summary' and 'plot' can be used on any splits

# up to two CO splits may be used
splits<-calcSplits(Mest = Mtilde, Aj = Aj,
  split_CO = c("species", "visibility"), COdat = co,
  split_SS = NULL, SSdat = masterSearchSchedule,
  split_time = NULL)
plot(splits)
summary(splits)

# SS splits (or covariates found in search schedule file) can be performed
# graph shows estimated number of fatalities rate for each SS covariate level
# it is assumed that covariate levels are "coherent", i.e., in contiguous
# blocks, like c("sp", "sp", "fa", "fa") but not c("sp", "fa", "sp", "fa")
splits<-calcSplits(Mest = Mtilde, Aj = Aj,
  split_CO = NULL, COdat = co,
  split_SS = "season", SSdat = masterSearchSchedule,
  split_time = NULL)
plot(splits, rate = F)

# alternatively, temporal splits can be displayed as a daily rate (M/day)
plot(splits, rate = T)

# temporal splits need not align perfectly with either the search dates or the
#   carcass discovery times (however, at this point, temporal split must span
#   the entire monitoring period)
# in this example, the monitoring period is split into an irregular sequence of days
splits<-calcSplits(Mest = Mtilde, Aj = Aj,
  split_CO = NULL, COdat = co,
  split_SS = NULL, SSdat = masterSearchSchedule,
  split_time = c(0, 22, 44, 66, 68, 73, 162))
plot(splits, rate = F) # estimated number of fatalities in each specified interval
plot(splits, rate = T) # estimated daily fatality rate in each specified interval

# one CO split may be combined with one SS split or time split:
splits<-calcSplits(Mest = Mtilde, Aj = Aj,
  split_CO = "species", COdat = co,
  split_SS = NULL, SSdat = masterSearchSchedule,
  split_time = c(0, 22, 44, 66, 68, 73, 162))
plot(splits, rate = F)
plot(splits, rate = T)

splits<-calcSplits(Mest = Mtilde, Aj = Aj,
  split_CO = "species", COdat = co,
  split_SS = "season", SSdat = masterSearchSchedule,
  split_time = NULL)
plot(splits, rate = T)
