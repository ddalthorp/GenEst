# working here to bring in the CP functions from paul and dan
# currently in progress on the cpm function.
# component functions are up top
#
# switched the fitting to be done with optim
#
# in general this is looking good!
#need to add documentation and then can build upwards

cpLogLik <- function(beta, t1, t2, dataMM, dist){
  beta <- matrix(beta, ncol = 2)
  Beta <- dataMM %*% beta
  psurv_t1 <- survival::psurvreg(t1, Beta[ , 1], Beta[ , 2], dist)
  psurv_t2 <- survival::psurvreg(t2, Beta[ , 1], Beta[ , 2], dist)
  psurv_t2[which(is.na(psurv_t2))] <- 1
  lik <- psurv_t2 - psurv_t1
  lik[t1 + .Machine$double.eps >= t2] <-
    survival::dsurvreg(t2, Beta[ , 1], Beta[ , 2], dist)
  lik <- pmax(lik, .Machine$double.eps) # eliminates zeros so log(lik) works
  nll <- -sum(log(lik))
  return(nll)
}


cpLogLik_exp <- function(beta, t1, t2, dataMM){
  t2[which(is.na(t2))] <- Inf
  Beta <- matrix(dataMM %*% beta)
  psurv_t1 <- survival::psurvreg(t1, Beta, scale = 1, "exponential")
  psurv_t2 <- survival::psurvreg(t2, Beta, scale = 1, "exponential")
  lik <- psurv_t2 - psurv_t1
  lik[t1 + .Machine$double.eps >= t2] <-
    survival::dsurvreg(t2, Beta, "exponential")
  lik <- pmax(lik, .Machine$double.eps) # eliminates zeros so log(lik) works
  nll <- -sum(log(lik))
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
  predCheck <- all.vars(formulaRHS)
  if (sum(predCheck %in% colnames(data)) != length(predCheck)){
    stop("Predictor(s) in formula(e) not found in data.")
  }

  formulaRHS <- formula(delete.response(terms(formula)))
  preds <- all.vars(formulaRHS) # safer alternative to formula[[3]]
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
  cpTime <- survival::Surv(time = t1, time2 = t2, event = event, type = "interval")
  imod <- survival::survreg(
    reformulate(as.character(formulaRHS)[-1], response = 'cpTime'),
    data = data,
    dist = dist
  )

  betaInit <- c(imod$coef, rep(imod$scale, length(imod$coef)))
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
