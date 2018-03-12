# working here to bring in the CP functions from paul and dan
# currently in progress on the cpm function. 
# component functions are up top
# 
# switched the fitting to be done with optim, advoiding all the hess nonsense
# using eweibull in EnvStats to do the MME estimation for the init vals
#
# in general this is looking good! 
#need to add documentation and then can build upwards

weibullStart <- function(x){
 
  if (length(x) == 1){
    x <- rep(x, 2)
  }
  if (sum(x) == 0){
    x[length(x)] <- x[length(x)] + 1 / length(x)
  }

  MME <- EnvStats::eweibull(x, method = "mme")
  a <- as.numeric(MME$parameters["scale"])
  b <- as.numeric(MME$parameters["shape"])
  
  return(c(log(a), 1 / b))
}


cpLogLik <- function(beta, t1, t2, dataMM, dist){
  beta <- matrix(beta, ncol = 2)
  Beta <- dataMM %*% beta
  psurv_t1 <- survival::psurvreg(t1, Beta[ , 1], Beta[ , 2], dist)
  psurv_t2 <- survival::psurvreg(t2, Beta[ , 1], Beta[ , 2], dist)
  psurv_t2[which(is.na(psurv_t2))] <- 1
  lik <- psurv_t2 - psurv_t1
  lik[lik < .Machine$double.eps] <- .Machine$double.eps
  nll <- -sum(log(lik))
  return(nll)
}


cpLogLik_exp <- function(beta, t1, t2, dataMM){
  t2[which(is.na(t2))] <- Inf
  Beta <- matrix(dataMM %*% beta)
  psurv_t1 <- survival::psurvreg(t1, Beta, scale = 1, "exponential")
  psurv_t2 <- survival::psurvreg(t2, Beta, scale = 1, "exponential")
  nll <- -sum(log(psurv_t2 - psurv_t1))
  return(nll)
}




cpm <- function(formula, data = NULL, left = NULL, right = NULL, 
                dist = "weibull", CL = 0.9){

  if (length(left) == 0){
    left <- "left"
    if (!"left" %in% colnames(data)){
      stop(paste("No column name provided for last time observed (left) ",
                 "and no column in data is named \"left\".", sep = ""
           )
      )
    }
  } else if (length(left) > 1){
    stop("Input for last time observed column can only be length 0 or 1.")
  }
  if (!left %in% colnames(data)){
    stop("Column name for last time observed (left) is not in the data.")
  }
  if (length(right) == 0){
    right <- "right"
    if (!"right" %in% colnames(data)){
      stop(paste("No column name provided for first time absent (right) ",
                 "and no column in data is named \"right\".", sep = ""
           )
      )
    }
  } else if (length(right) > 1){
    stop("Input for first time absent column can only be length 0 or 1.")
  }
  if (!right %in% colnames(data)){
    stop("Column name for first time absent (right) is not in the data.")
  }
  predCheck <- all.vars(formula[[3]])
  if (sum(predCheck %in% colnames(data)) != length(predCheck)){
    stop("Predictor(s) in formula(e) not found in data.")
  }

  formulaRHS <- formula(delete.response(terms(formula)))
  preds <- all.vars(formula[[3]])
  cells <- combinePreds(preds, data)
  ncell <- nrow(cells)
  cellNames <- cells$CellNames

  dataMM <- model.matrix(formulaRHS, data)
  cellMM <- model.matrix(formulaRHS, cells) 

  t1 <- data[ , left]
  t2 <- data[ , right]
  obsData <- data[ , c(left, right)]
  if (any(is.infinite(t2))){
    t2[which(is.infinite(t2))] <- NA
  }
  t12 <- (t1 + pmin(t2, 2 * max(t2, na.rm = TRUE), na.rm = TRUE)) / 2
  
  betaInit1 <- switch(dist,
                   "loglogistic" = c(mean(log(t12)), var(log(t12)) / 3.2),
                   "exponential" = 1 / mean(t12),
                   "lognormal" = c(sqrt(var(t12)), log(mean(t12))),
                   "weibull" = weibullStart(t12))
  nparam <- length(betaInit1)
  betaInit <- matrix(0, nrow = ncol(cellMM), ncol = nparam, byrow = TRUE)
  betaInit[1, ] <- betaInit1
  betaInit <- as.vector(betaInit)

  probs <- data.frame(c(0.5, (1 - CL) / 2, 1 - (1 - CL) / 2))

  if (dist == "exponential"){
    MLE <- tryCatch(
             optim(par = betaInit, fn = cpLogLik_exp, method = "BFGS",
               t1 = t1, t2 = t2, dataMM = dataMM, hessian = TRUE,
               control = list(maxit = 5000)
             ), error = function(x) {NA}
           )
    betahat <- MLE$par
    betahatMatrix <- matrix(betahat, nrow = ncol(cellMM))
    locations <- cellMM %*% betahatMatrix 
    cellTable_loc <- apply(probs, 1, survival::qsurvreg, mean = locations, 
                       scale = 1, distribution = "exponential")
    colnames(cellTable_loc) <- c("loc_median", "loc_lower", "loc_upper")
    cellTable_sc <- matrix(1, nrow = ncell, ncol = 1)
    colnames(cellTable_sc) <- c("scale")
    cellTable <- data.frame(cell = cellNames, cellTable_loc, cellTable_sc)
  } else{
    MLE <- tryCatch(
             optim(par = betaInit, fn = cpLogLik, method = "BFGS",
               t1 = t1, t2 = t2, dataMM = dataMM, dist = dist, hessian = TRUE,
               control = list(maxit = 5000)
             ), error = function(x) {NA}
           )
    betahat <- MLE$par
    betahatMatrix <- matrix(betahat, nrow = ncol(cellMM))

    locations <- cellMM %*% betahatMatrix[ , 1]
    scales <- cellMM %*% betahatMatrix[ , 2]
    cellTable_loc <- apply(probs, 1, survival::qsurvreg, mean = locations, 
                   scale = scales, distribution = dist)
    colnames(cellTable_loc) <- c("loc_median", "loc_lower", "loc_upper")
    cellTable_sc <- matrix(scales, ncol = 1)
    colnames(cellTable_sc) <- c("scale")
    cellTable <- data.frame(cell = cellNames, cellTable_loc, cellTable_sc)
  }

  convergence <- MLE$convergence
  betaHessian <- MLE$hessian
  llik <- MLE$value  

  nparam <- length(betahat)
  ncarc <- length(t12)
  AIC <- 2 * llik + 2 * nparam
  AICcOffset <- (2 * nparam * (nparam + 1)) / (ncarc - nparam - 1)
  AICc <- round(AIC + AICcOffset, 3)

  varbeta <- tryCatch(solve(betaHessian), error = function(x) {NA})
  if (is.na(varbeta)[1]){
    stop("Model generates unstable variance estimate.")
  }

  cellByCarc <- numeric(ncarc)
  for (celli in 1:ncell){
    groupPattern <- cellMM[celli, ]
    matchingMatrix <- t(dataMM) == groupPattern
    matchingParts <- apply(matchingMatrix, 2, sum)
    matchingTotal <- matchingParts == ncol(cellMM)
    cellByCarc[matchingTotal] <- celli
  }
  carcCells <- cellNames[cellByCarc]

  output <- list()
  output$call <- match.call()
  output$formula <- formula
  output$predictors <- preds 
  output$AIC <- AIC
  output$AICc <- AICc
  output$convergence <- convergence
  output$varbeta <- varbeta
  output$betahat <- betahat
  output$cellMM <- cellMM
  output$cells <- cells
  output$ncell <- ncell
  output$cellwiseTable <- cellTable
  output$observations <- obsData
  output$carcCells <- carcCells
  output$CL <- CL
  class(output) <- c("pkm", "list")
  attr(output, "hidden") <- c("predictors", "AIC", "convergence", "varbeta",
                              "cellMM", "cells", "ncell", "observations",
                              "carcCells", "CL"
                              )
  return(output)
}

