
library(GenEst)

rm(list = ls())


source('c:/users/prabie/documents/github/genest/r/pkm2.r')
source('c:/users/prabie/documents/github/genest/r/pkLogLik3.r')


##carcasses per class
nCarcs = 50

##p and k values for tastiness == 'X'
pAX = 0.8
kAX = 0.8
pBX = 0.3
kBX = 1.1


##p and k values for tastiness == 'Y'
pAY = 0.7
kAY = 0.9
pBY = 0.3
kBY = 1.2


pkmDat = expand.grid(carcID = 1:nCarcs, 
    size = c('A', 'B'), tastiness = c('X', 'Y'), s0 = 0)
pkmDat$p = paste0('p', pkmDat$size, pkmDat$tastiness)
pkmDat$k = paste0('k', pkmDat$size, pkmDat$tastiness)
pkmDat$p = sapply(pkmDat$p, get)
pkmDat$k = sapply(pkmDat$k, get)
    
for(row in 1:nrow(pkmDat)){
    for(s in 1:7){
        if(pkmDat[row, paste0('s', s-1)] %in% 0){
           pkmDat[row, paste0('s', s)] = 
            rbinom(1, 1, pkmDat[row, 'p'] * pkmDat[row, 'k']^(s-1))
        } else {
           pkmDat[row, paste0('s', s)] = NA
        }
    }
}

pkmDat$s0 = NULL

##make sure none of my detection probabilities exceed 1
unique(pkmDat[, 'p'] * pkmDat[, 'k']^6)



##for stepping through functions
obsCol = NULL
kFixed = NULL
kInit = 0.7
CL = 0.9


if(FALSE){
#########################Can p hi or p low be a problem?

##In each of these I feed the function terrible data for the
##size == 'A' & tastiness == 'B' cell to see what happens to 
##the SEEF estimates.

formula_p = formula('p ~ size * tastiness')
formula_k = formula('k ~ size * tastiness')

#SEEF = 1 on first search
data = pkmDat
data[data$size == 'A' & data$tastiness == 'X', 's1'] = 1
data[data$size == 'A' & data$tastiness == 'X', paste0('s', 2:7)] = NA
pkm(formula_p, formula_k, data)

# fit = pkm(formula_p, formula_k, data)
# mvtnorm::rmvnorm(100, c(fit$betahat_p, fit$betahat_k), fit$varbeta) %*% cellMM

#SEEF = 0
data = pkmDat
data[data$size == 'A' & data$tastiness == 'X', paste0('s', 1:7)] = 0
pkm(formula_p, formula_k, data)

#SEEF very near zero
data = pkmDat
data[data$size == 'A' & data$tastiness == 'X', paste0('s', 1:7)][-1,] = 0
data[data$size == 'A' & data$tastiness == 'X', paste0('s', 1:7)][1,] = 
    c(1, rep(NA, 6))
pkm(formula_p, formula_k, data)##SEEF is ok but k is not


##It appears that SEEF very high or very low is a problem
}


###########################additive model
##working on this one

data = pkmDat
formula_p = formula('p ~ size + tastiness')
formula_k = formula('k ~ size + tastiness')

fit = pkm2(formula_p, formula_k, data)
fit; fit$varbeta

pkm(formula_p, formula_k, data)

mvtnorm::rmvnorm(100, 
    c(fit$cellwiseTable$betahat_p, fit$cellwiseTable$betahat_k), 
    fit$varbeta)
################################mess

formula_p = formula('p ~ size')
formula_k = formula('k ~ tastiness')


###########################saturated model

data = pkmDat
formula_p = formula('p ~ size * tastiness')
formula_k = formula('k ~ size * tastiness')

fit = pkm2(formula_p, formula_k, data, kFixed = NULL)

##rmvnorm doesn't even need a wrapper!
mvtnorm::rmvnorm(100, 
    c(fit$cellwiseTable$betahat_p, fit$cellwiseTable$betahat_k), 
    fit$varbeta)
#########################################


data = pkmDat
formula_p = formula('p ~ size * tastiness')
formula_k = formula('k ~ size * tastiness')

pkm2(formula_p, formula_k = NULL, data, kFixed = .4)
formula_k = NULL
kFixed = .4
#########################################



data = pkmDat[1:50, ]

data = pkmDat[51:100, ]

formula_p = formula('p ~ 1')
formula_k = formula('k ~ 1')


