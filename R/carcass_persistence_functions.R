cpLogLik <- function(beta, t1, t2, dataMM, dist){
  t2[which(is.na(t2))] <- Inf
  beta <- matrix(beta, ncol = 2)
  Beta <- dataMM %*% beta
  psurv_t1 <- survival::psurvreg(t1, Beta[ , 1], Beta[ , 2], dist)
  psurv_t2 <- survival::psurvreg(t2, Beta[ , 1], Beta[ , 2], dist)
  psurv_t2[which(is.na(psurv_t2))] <- 1
  lik <- psurv_t2 - psurv_t1
  too_small <- t1 + .Machine$double.eps >= t2
  lik[too_small] <- survival::dsurvreg(t2, Beta[ , 1], Beta[ , 2], dist)
  lik <- pmax(lik, .Machine$double.eps) 
  nll <- -sum(log(lik))
  return(nll)
}

cpm <- function(formula, data = NULL, left = NULL, right = NULL, 
                dist = "weibull", CL = 0.9){

  if (length(left) == 0){
    left <- "left"
    if (!"left" %in% colnames(data)){
      msg <- paste("No column name provided for first time observed ", 
               "(left) and no column in data is named \"left\".", sep = "")
      stop(msg)
    }
  } else if (length(left) > 1){
    stop("Input for first time observed column can only be length 0 or 1.")
  }
  if (!left %in% colnames(data)){
    stop("Column name for first time observed (left) is not in the data.")
  }
  if (length(right) == 0){
    right <- "right"
    if (!"right" %in% colnames(data)){
      msg <- paste("No column name provided for last time observed ", 
               "(right) and no column in data is named \"right\".", sep = "")
      stop(msg)
    }
  } else if (length(right) > 1){
    stop("Input for last time absent column can only be length 0 or 1.")
  }
  if (!right %in% colnames(data)){
    stop("Column name for last time absent (right) is not in the data.")
  }

  formulaRHS <- formula(delete.response(terms(formula)))
  predCheck <- all.vars(formulaRHS)
  if (sum(predCheck %in% colnames(data)) != length(predCheck)){
    stop("Predictor(s) in formula(e) not found in data.")
  }

  preds <- all.vars(formulaRHS) 
  cells <- combinePreds(preds, data)
  ncell <- nrow(cells)
  cellNames <- cells$CellNames

  dataMM <- model.matrix(formulaRHS, data)
  cellMM <- model.matrix(formulaRHS, cells) 

  t1 <- data[ , left]
  t2 <- data[ , right]
  event <- rep(3, length(t1))
  event[is.na(t2) | is.infinite(t2)] <- 0
  event[round(t1, 3) == round(t2, 3)] <- 1
  t1 <- pmax(t1, 0.0001)
  tevent <- survival::Surv(time = t1, time2 = t2, event = event, 
              type = "interval")
  init_form <- reformulate(as.character(formulaRHS)[-1], response = "tevent")
  init_mod <- survival::survreg(formula = init_form, data = data, dist = dist)
  init_scales <- rep(init_mod$scale, length(init_mod$coef))
  betaInit <- matrix(c(init_mod$coef, init_scales), ncol = 2)
  probs <- data.frame(c(0.5, (1 - CL) / 2, 1 - (1 - CL) / 2))

  MLE <- tryCatch(
           optim(par = betaInit, fn = cpLogLik, method = "BFGS", 
             t1 = t1, t2 = t2, dataMM = dataMM, dist = dist, hessian = TRUE,
             control = list(maxit = 5000)
           ), error = function(x) {NA}
         )
  betahat <- MLE$par
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
  class(output) <- c("cpm", "list")
  attr(output, "hidden") <- c("predictors", "AIC", "convergence", "varbeta",
                              "cellMM", "cells", "ncell", "observations",
                              "carcCells", "CL"
                              )
  return(output)
}

