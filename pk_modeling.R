### functions and data for modeling pk as a function of 0-2 covariates
## The current implementation will fit a given pk model for a given data set.
## p and k are estimated simultaneously but they may depend on different model specifications.
## For example, p may be ~vis*species while k is ~vis or ~1 or a constant.

############# Preliminaries: load and subset data
require(R6)
load("pkTrialExample.Rdata")
# Example: Model pk as a function of vis and size
#   Restrict to vis = E, M, D (b/c too few carcasses in V)
#   Restrict to size = s, m (b/c few large carcasses)

# NOTE: with this data set, s = small birds + bats, m = medium birds

# In practice, we will recommend that users do not put size in as a covariate because the
# interaction structure is not pretty. It's is usually better to just run seperate analyses for
# different size classes. However, size is a fine covariate for exploring the models.
dat <- subset(pkTrialExample,
        subset = vis %in% c("E","M","D") & size %in% c("s","m"),
        select = c(vis, size, grep("s\\d{1}", names(pkTrialExample), ignore.case = TRUE)))
# Identify columns containing search outcome data
scols <- grep("s\\d{1}",names(dat), ignore.case=TRUE)
# Discard any rows with NA for first search.
dat <- dat[which(!is.na(dat[,scols[1]])),]

############# define utility functions
logit <- function(x) log(x/(1-x))
alogit <- function(x) 1/(1+exp(-x))

# This function creates a data set with columns given in vars. There is a row for
# every unique combination of the levels of vars found in dat. A final column
# gives a unique code which is the concatenation of the labels in the other columns.
# The rows define the unique cells given by combinations of categories in vars.
# If you want only one group, do: make_egDat(NULL,dat)
make_egDat <- function(vars, dat) {
  nvars <- length(vars) # Should be 0, 1, or 2 for now, but function will work for more.
  if (nvars == 0) {
    return(data.frame(group="all",cellNames="all"))
  } else {
      if(any(is.na(match(vars,names(dat))))) {
        stop("At least one var not found in dat.")
      }
      varNames <- sort(vars)
      varLabels <- list()
      varNlevels <- list()
      for (i in 1:nvars) {
        varLabels[[i]] <- levels(as.factor(dat[[varNames[i]]]))
        varNlevels[[i]] <- length(varLabels[[i]])
      }
      reps <- cumprod(varNlevels)[nvars:1] # Reverse cumulative product
      egDat <- data.frame(var1=gl(varNlevels[[1]],1,length=reps[1],labels=varLabels[[1]]))
      if (nvars > 1) {
        for (j in 2:nvars) {
          egDat[[paste("var",j,collapse=NULL)]] <- gl(varNlevels[[j]],reps[j],length=reps[1],labels=varLabels[[j]])
        }
      }
    }
  names(egDat) <- varNames
  egDat$cellNames <- apply(egDat,1,paste0,collapse=",")
  return(egDat)
}


############# pkMod class for data management
pkMod <- R6Class("pkMod",
  portable = FALSE, # can't access from other packages, but streamlines coding
  public = list(
    dat = NULL, pvars = NULL, kvars = NULL, pop = NULL, kop = NULL, k = NULL,
    fp = NULL, fk = NULL, scols = NULL, zeros = NULL, found = NULL, maxmiss = NULL,
    miniXp = NULL, miniXk = NULL, np = NULL, nk = NULL, facts = NULL,  groups = NULL,
    theta = NULL, vartheta = NULL, egDat = NULL,
    initialize = function(dat, pvars = 1, pop = NULL, k = NULL, kvars = NULL, kop = NULL){
      # dat is a dataframe with search columns s1, s2, ... and covariates. Searches result cols
      # are assumed to be in chronological order, with leftmost column earliest
      # Each row represents the fate of a single carcass in the field trials.
      # pvars and kvars are either character vectors with the names of the covariates,
      # in which case all the covariates must be included among the covariate columns of dat, or
      # 1, indicating that there are no covariates.
      # with kvars = 1, the model will optimize to find the best k (with no covariates)
      # with kvars = NULL, a fixed value of k in [0, 1] must be provided
      # there can be at most two covariates in unique(c(pvars, kvars)), but pvars and kvars
      # needn't be identical, e.g., kvars = 'spec' and pvars = 'vis' would be OK
      # pop and kop are the operations used in defining how the covariates are tied together in
      # the model, e.g.:
      #  pop = '+' => p covariates are included as additive terms
      #  pop = '*' => p covariate interaction
      dat <<- dat
      fp <<- as.formula(paste("~", paste(sort(pvars), collapse = pop)))
      if (!missing(kvars)) fk <<- as.formula(paste("~", paste(sort(kvars), collapse = kop)))
      scols <<- grep("s\\d{1}",names(dat), ignore.case=TRUE) # Identify columns containing search outcome data
      pvars <<- pvars; kvars <<- kvars
      Xp <<- model.matrix(fp, dat)
      if (!missing(kvars)) Xk <<- model.matrix(fk, dat) else k <<- k
      # Create the zeros vector, which counts the number of times the given carcass
      # was missed in the searcher efficiency field trials.
      zeros <<- matrixStats::rowCounts(as.matrix(dat[, scols]), value = 0, na.rm = T)

      # Create the found vector. It has length equal to NROWS(dat). Each element
      # gives the search occasion when the carcass is found and is 0 if the
      # carcass is never found.
      foundInd <- which(matrixStats::rowCounts(as.matrix(dat[, scols]), value = 1, na.rm = T) == 1)
      found <<- numeric(length(zeros))
      found[foundInd] <<- zeros[foundInd] + 1
      allcov <- sort(unique(c(pvars[is.character(pvars)], kvars[is.character(kvars)])))
      egDat <<- make_egDat(allcov, dat)
      miniXp <<- model.matrix(fp, egDat)
      miniXk <<- model.matrix(fk, egDat)
      np <<- dim(Xp)[2]
      nk <<- dim(Xk)[2]
      maxmiss <<- max(zeros)
      facts <<- cbind(miniXp, miniXk)
      nfact <- dim(facts)[1]
      tXpk <- t(cbind(Xp, Xk))
      groups <<- numeric(dim(Xp)[1])
      for (i in 1:nfact) groups[colSums(tXpk == facts[i,]) == np + nk] <<- i
    },
    pkreg = function(theta = NULL, getVar = FALSE){
      nfact <- dim(Xp)[2]
      if (missing(theta)){
        ngroups <- NCOL(Xp)
        empp <- numeric(nrow(Xp))
        if (sum(attr(terms(fk), "term.labels") %in% attr(terms(fp), "term.labels")) <
          length(attr(terms(fk), "term.labels"))) {
            tXp <- t(Xp)
            groups <- numeric(dim(Xp)[1])
            for (i in 1:dim(Xp)[2]) groups[colSums(tXp == miniXp[i,]) == np] <- i
        }
        for (i in 1:dim(miniXp)[1]) {
          empp[which(groups==i)] <- mean(dat$s1[which(groups == i & dat$s1 >= 0)], na.rm=TRUE)
        }
        empp[which(empp==0)] <- 0.1 # Cells with all 0's set to 0.1
        empp[which(empp==1)] <- 0.9 # Cells with all 1's set to 0.1
        # Then get least squares estimates for the betas. The first NCOL(Xp)
        # elements of theta are starting values for the betas in the p model. The
        # remaining NCOL(Xk) elements are the betas for the k model assuming a
        # constant k=0.7.
        theta <- c(solve(t(Xp)%*%Xp)%*%t(Xp)%*%logit(empp), logit(rep(0.7,times=NCOL(Xk))))
      }
      result <- optim(par = theta, fn = function(theta){
        Beta <- array(numeric(length(theta) * 2), dim = c(length(theta), 2))
        Beta[1:np,1] <- theta[1:np]
        Beta[(np+1):length(theta), 2]<-theta[(np+1):length(theta)]
        pk <- alogit(facts %*% Beta)
        powk<-array(rep(pk[, 2], maxmiss + 1), dim=c(dim(pk)[1], maxmiss+1))
        powk[,1] <- 1
        powk <- matrixStats::rowCumprods(powk)
        pmiss <- matrixStats::rowCumprods(1 - (pk[,1] * powk[, 1:maxmiss]))
        pfind.si <- pk[, 1] * powk *
          cbind(rep(1, nfact), matrixStats::rowCumprods(1 - (pk[,1] * powk[, 1:maxmiss])))
        -(sum(log(pmiss[cbind(groups[found == 0], zeros[found == 0])]))+sum(log(pfind.si[cbind(groups[found > 0], found[found > 0])])))
      }, method = "BFGS", hessian = getVar)
      betaphat <- result$par[1:NCOL(Xp)]
      betakhat <- result$par[(NCOL(Xp)+1):length(theta)]
      theta <<- c(betaphat, betakhat)
      if(getVar) vartheta <<- solve(result$hessian) else vartheta <<- NA
      return(list(
        betaphat = betaphat,
        betakhat = betakhat,
        vartheta = vartheta,
        aic=2*result$value + 2*length(result$par),
        convergence = result$convergence
      ))
    },
    pksim = function(nsim){
      if (is.null(vartheta)) pkreg(getVar = T)
      betaSim <- mvtnorm::rmvnorm(nsim , mean = theta, sigma = vartheta)
      pSim <- alogit(betaSim[,1:np]%*%t(miniXp))
      kSim <- alogit(betaSim[,(np + 1):(np + nk)]%*%t(miniXk))
      colnames(pSim) <- egDat$cellNames
      colnames(kSim) <- egDat$cellNames
      return(list(pSim = pSim, kSim = kSim))
    }
  )
)

############# usage
### instantiate the class with data, e.g.:
pkdata <- pkMod$new(dat = dat, pvars = 1, pop = '*', kvars = 'size')
# NOTE: option for constant, specified k is not yet implemented. Coming soon...

### fit the model
pkdata$pkreg()
# NOTE: if doing model selection via AIC, use arg getVar = F (default) because extracting
# the variance of the parameter estimates is time-consuming

### simulate pk for the various cells
pkdata$pksim(nsim = 10)
