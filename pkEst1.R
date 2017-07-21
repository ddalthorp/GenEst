################################
# Function to estimate p and k using data in dat with models given in model
# matrices Xp and Xk and search data in Data. Expecting Data to have column
# names s1, s2, etc.

pkEst1 <- function(fp,fk,dat) {
    
  Xp <- model.matrix(fp,dat)
  Xk <- model.matrix(fk,dat)
    
  scols <- grep("s\\d{1}",names(dat), ignore.case=TRUE) # Identify columns containing search outcome data
  Data <- dat[,scols]
  
  # Create the zeros vector, which counts the number of times the given carcass
  # was missed in the searcher efficiency field trials.
  zeros <- matrixStats::rowCounts(as.matrix(Data), value = 0, na.rm = T)
  
  # Create the found vector. It has length equal to NROWS(Data). Each element
  # gives the search occasion when the carcass is found and is 0 if the
  # carcass is never found.
  foundInd <- which(matrixStats::rowCounts(as.matrix(Data), value = 1, na.rm = T) == 1)
  found <- numeric(length(zeros))
  found[foundInd] <- zeros[foundInd] + 1
  
  # Find reasonable starting values. First calculate empp, a vector of length
  # NROW(Data). The ith element is the observed proportion of carcasses found
  # in search 1 for the ith carcass's group, as defined by the model matrix
  # Xp. According to the model, the probability that a carcass is found during
  # the first search is p.
  empp <- numeric(nrow(Xp))
  groups <- findGroups(Xp)
  for (i in 1:length(groups)) {
    empp[which(groups==i)] <- with(Data,mean(s1[which(groups==i & s1>=0)],na.rm=TRUE))
  }

  # Then get least squares estimates for the betas. The first NCOL(Xp)
  # elements of theta are starting values for the betas in the p model. The
  # remaining NCOL(Xk) elements are the betas for the k model, assuming
  # a constant k=0.7.  
  theta <- c(solve(t(Xp)%*%Xp)%*%t(Xp)%*%logit(empp),
             logit(rep(0.7,times=NCOL(Xk))))
  
  # Perform the optimization.
  # pre-processing
  np <- dim(Xp)[2]
  nk <- dim(Xk)[2]
  maxmiss <- max(zeros)
  facts <- unique(cbind(Xp, Xk))
  nfact <- dim(facts)[1]
  tXpk <- t(cbind(Xp, Xk))
  pkind <- numeric(dim(Xp)[1])
  for (i in 1:nfact) pkind[colSums(tXpk == facts[i,]) == np + nk] <- i
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
    -(sum(log(pmiss[cbind(pkind[found == 0], zeros[found == 0])]))+sum(log(pfind.si[cbind(pkind[found > 0], found[found > 0])])))
  }, method = "BFGS")

  # Extract the parameters estimates for the p and k models.
  betaphat <- result$par[1:NCOL(Xp)]
  betakhat <- result$par[(NCOL(Xp)+1):length(theta)]
  
  return(list(betaphat=betaphat,
              betakhat=betakhat,
              aic=2*result$value + 2*length(result$par),
              convergence=result$convergence))
}

# Utility functions for logit and its inverse:
logit <- function(x) log(x/(1-x))
alogit <- function(x) 1/(1+exp(-x))

# Find groups defined by a design matrix X.
findGroups <- function(X) {
  nparams <- NCOL(X)
  Xrows <- sapply(1:NROW(X),FUN=function(x) paste0(X[x,],collapse=""))
  Urows <- unique(Xrows) # Should be NCOL(X) values.
  match(Xrows,Urows)
}
