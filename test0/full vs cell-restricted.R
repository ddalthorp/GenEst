# a quick test whether the full factorial pk model gives the same cellwise models as does
# the intercept-only model fit to data restricted to each cell
source('utilities.R')
pkTrial<-read.csv("SE data (example).csv", header=T, as.is = T)
# NOTE: If as.is = F in the read.csv, subsetting to ingore a given factor level carries a list
# of the original factor levels into the subsetted data frame, which would guarantee empty
# cells in the subset and cause the pk modeling functions to crash.

pktmp <- subset(pkTrial, size == 's' & vis != 'V', select = c('vis', 'bob', 's1', 's2', 's3', 's4'))
covars <- c('bob', 'vis') # bob = 'bird' or 'bat'; vis = 'E', 'M', or 'D'

# fit the full factorial model:
pkfull <- pkMod$new(dat = pktmp, pvars = covars, pop = '*', kvars = covars, kop = '*')
# cell stats calculated by collapsing the full factorial model to each cell
pkfull$cellStats()

# fit the intercept-only model on data restricted to cell batM
pkcell <- pkMod$new(dat = subset(pktmp, bob == 'bat' & vis == 'M'), pvars = 1, kvars = 1)
# cell stats calculated from the cell-only model
pkcell$cellStats()
# as expected, answers are the same as with the pkfull model for the batM cell
#  (with rounding error on the 7th digit)
