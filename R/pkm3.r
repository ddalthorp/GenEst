#' Fit a single searcher efficiency model.
#' 
#' Searcher efficiency is modeled as a function of the number of times a 
#' carcass has been missed in previous searches and any number of covariates. 
#' Format and usage parallel that of common \code{R} functions such as 
#' \code{lm}, \code{glm}, and \code{gam}. However, the input data 
#' (\code{data}) is structured differently to accommodate the multiple-search
#' searcher efficiency trials (see 'Details'), and model formulas may be 
#' entered for both \code{p} (akin to an intercept) and \code{k} (akin to a 
#' slope).
#'
#' The probability of finding a carcass that is present at the time of search
#' is \code{p} on the first search after carcass arrival and is assumed to
#' decrease by a factor of \code{k} each time the carcass is missed in 
#' searches. Both \code{p} and \code{k} may depend on covariates such as 
#' ground cover, season, species, etc., and a separate model format 
#' (\code{formula_p} and \code{formula_k}) may be entered for each. The models 
#' are entered as they would be in the familiar \code{lm} or \code{glm} 
#' functions in R. For example, \code{p} might vary with \code{visibility}, 
#' \code{season}, and \code{site}, while \code{k} varies only with 
#' \code{visibility}. A user might then enter \code{p ~ visibility + season + 
#' site} for \code{formula_p} and \code{k ~ visibility} for \code{formula_k}. 
#' Other R conventions for defining formulas may also be used, with 
#' \code{covar1:covar2} for the interaction between covariates 1 and 2 and 
#' \code{covar1 * covar2} as short-hand for \code{covar1 + covar2 + 
#' covar1:covar2}.
#'
#' Search trial \code{data} must be entered in a data frame with data in 
#' each row giving the fate of a single carcass in the field trials. There
#' must be a column for each search occassion, with 0, 1, or NA depending on 
#' whether the carcass was missed, found, or not available (typically because 
#' it was found and removed on a previous search, had been earlier removed by 
#' scavengers, or was not searched for) on the given search occasion. 
#' Additional columns with values for categorical covariates (e.g., 
#' visibility = E, M, or D) may also be included.
#'
#' @param formula_p Formula for p; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 
#' 'Details'.
#'
#' @param data Dataframe with results from searcher efficiency trials and any
#' covariates included in \code{formula_p} or {formula_k} (required).
#'
#' @param formula_k Formula for k; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 
#; 'Details'.
#'
#' @param obsCol Vector of names of columns in \code{data} where results 
#' for each search occasion are stored (optional). If no \code{obs_cols} are 
#' provided, \code{pkm} uses as \code{obs_cols} all columns with names that 
#' begin with an \code{'s'} or \code{'S'} and end with a number, e.g., 's1',
#' 's2', 's3', etc. This option is included as a convenience for the user, 
#' but care must be taken that other data are not stored in columns with 
#' names matching that pattern. Alternatively, \code{obs_cols} may be 
#' entered as a vector of names, like \code{c('s1', 's2', 's3')}, 
#' \code{paste0('s', 1:3)}, or \code{c('initialSearch', 'anotherSearch', 
#' 'lastSearch')}.
#'
#' @param kFixed Parameter for user-specified \code{k} value (optional). If a
#' value is provided, \code{formula_k} is ignored and the model is fit under 
#' the assumption that the \code{k} parameter is fixed and known to be
#' \code{fix_k}.
#'
#' @param kInit Initial value used for \code{k} in the optimization.
#'
#' @param CL confidence level
#'
#' @return \code{pkm} returns an object of class "\code{pkm}", which is a list
#' whose components characterize the fit of the model. Due to the large number
#' and complexity of components, only a subset of them is printed 
#' automatically; the rest can be viewed/accessed directly via the \code{$}
#' operator if desired.
#'
#' The following components are displayed automatically:
#'
#' \describe{
#'  \item{\code{call}}{the function call to fit the model}
#'  \item{\code{predictors}}{list of covariates of \code{p} and/or \code{k}}
#'  \item{\code{cellwiseTable}}{summary statistics for estimated cellwise 
#'    \code{p} and \code{k}, including the medians and upper & lower bounds
#'    on CIs for each parameter, indexed by cell (or combination of
#'    covariate levels).}
#'  \item{\code{AIC}}{the 
#'    \href{https://en.wikipedia.org/wiki/Akaike_information_criterion}{AIC}
#'    value for the fitted model}
#'  \item{\code{AICc}}{the AIC value as corrected for small sample size}
#'  \item{\code{convergence}}{convergence status of the numerical optimization 
#'    to find the maximum likelihood estimates of \code{p} and \code{k}. A 
#'    value of \code{0} indicates that the model was fit successfully. For 
#'    help in deciphering other values, see \code{\link{optim}}.}
#' }
#'
#' The following components are not printed automatically but can be accessed
#' via the \code{$} operator:
#' \describe{
#'   \item{\code{formula_p}}{the model formula for the \code{p} parameter}
#'   \item{\code{formula_k}}{the model formula for the \code{k} parameter}
#'   \item{\code{betahat_p}}{parameter estimates for the terms in the 
#'     regression model for for \code{p} (logit scale)}
#'   \item{\code{betahat_k}}{parameter estimates for the terms in the 
#'     regression model for for \code{k} (logit scale). If \code{k} is fixed 
#'     and known, \code{betahat_k} is not calculated.}
#'   \item{\code{betavar}}{the variance-covariance matrix of the estimators
#'     for \code{c(betahat_p, betahat_k}.}
#'   \item{\code{cellMM_p}}{a cellwise model (design) matrix for covariate 
#'     structure of \code{p_formula}}
#'   \item{\code{cellMM_k}}{a cellwise model(design) matrix for covariate 
#'     structure of \code{k_formula}}
#'   \item{\code{levels_p}}{all levels of each covariate of \code{p}}
#'   \item{\code{levels_k}}{all levels of each covariate of \code{k}}
#'   \item{\code{nbeta_p, nbeta_k}}{number of parameters to fit the \code{p}
#'     and \code{k} models}
#'   \item{\code{cells}}{cell structure of the pk-model, i.e., combinations of
#'     all levels for each covariate of \code{p} and \code{k}. For example, if
#'     \code{covar1} has levels \code{"a"}, \code{"b"}, and \code{"c"}, and
#'     \code{covar2} has levels \code{"X"} and \code{"Y"}, then the cells 
#'     would consist of \code{a.X}, \code{a.Y}, \code{b.X}, \code{b.Y}, 
#'     \code{c.X}, and \code{c.Y}.}
#'   \item{\code{ncell}}{total number of cells}
#'  \item{\code{predictors_k}}{list of covariates of \code{p}}
#'  \item{\code{predictors_p}}{list of covariates of \code{k}}
#'  \item{\code{observations}}{observations used to fit the model}
#'  \item{\code{kFixed}}{the input \code{kFixed}}
#'  \item{\code{CL}}{the input \code{CL}}
#'}
#'
#' @examples
#' data(pkmdat)
#' pkm(p ~ visibility, k ~ 1, data = pkmdat)
#' pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' pkm(p ~ visibility, k = 0.7, data = pkmdat)
#' @export
#'

# pkm <- function(formula_p, formula_k = NULL, data, obsCol = NULL, 
         # kFixed = NULL, kInit = 0.7, CL = 0.9){

##Observation columns

  if (length(obsCol) == 0){
    obsCol <- grep("^[sS].*[0-9]$", names(data), value = TRUE)
    nobsCol <- length(obsCol)
    if (nobsCol == 0){
      stop("No observation columns provided and no appropriate column names.")
    }
  }

##Error checking
  if (length(kFixed) == 1){
    if (kFixed < 0){
      message("Provided k is negative. Using k = 0")
      kFixed <- 0.0
   }
    if (kFixed > 1){
      message("Provided k too large. Using k = 1.0")
      kFixed <- 1.0
    }
  }
  if (length(formula_k) > 0 & length(kFixed) == 1){
    message("Both formula and fixed value provided for k, fixed value used.")
  }
  if (length(formula_k) == 0 & length(kFixed) == 0){
    message("No formula or fixed value provided for k, fixed at 1.")
    kFixed <- 1
  }
  if (length(obsCol) == 1 & length(kFixed) == 0){
    message("Only one observation, k cannot be estimated, fixed at 1")
    kFixed <- 1
  }


##Setup
  nsearch <- length(obsCol)
  ncarc <- nrow(data)

  obsData <- data[ , obsCol]
  obsData <- as.matrix(obsData, ncol = nsearch)
  firstObs <- obsData[ , 1]
  missData <- apply(obsData, 2, match, 0)

  misses <- apply(missData, 1, sum, na.rm = TRUE)
  maxmisses <- max(misses)
  found <- apply(obsData, 1, sum, na.rm = TRUE)
  carcassesFound <- which(found == 1)
  foundOn <- numeric(ncarc)
  foundOn[carcassesFound] <- misses[carcassesFound] + 1


########Formulae and matrices
  preds_p <- all.vars(formula_p[[3]])
  formulaRHS_p <- formula(delete.response(terms(formula_p)))
  levels_p <- .getXlevels(terms(formulaRHS_p), data)

  preds_k <- character(0)
  if (length(formula_k) > 0){
    preds_k <- all.vars(formula_k[[3]])
    formulaRHS_k <- formula(delete.response(terms(formula_k)))
    levels_k <- .getXlevels(terms(formulaRHS_k), data)
  }
  if (length(kFixed) == 1){
    preds_k <- character(0)
    formulaRHS_k <- formula(~1)  
    formula_k <- c(fixedk = kFixed)
    levels_k <- .getXlevels(terms(formulaRHS_k), data)
  }

  preds <- unique(c(preds_p, preds_k))
  cells <- combinePreds(preds, data)
  ncell <- nrow(cells)
  cellNames <- cells$CellNames

  dataMM_p <- model.matrix(formulaRHS_p, data)
  dataMM_k <- model.matrix(formulaRHS_k, data)
  dataMM <- t(cbind(dataMM_p, dataMM_k))
   

  cellMM_p <- model.matrix(formulaRHS_p, cells)
  cellMM_k <- model.matrix(formulaRHS_k, cells)
  cellMM <- cbind(cellMM_p, cellMM_k)
 
  nbeta_k <- ncol(dataMM_k)
  nbeta_p <- ncol(dataMM_p)
  nbeta <- nbeta_p + nbeta_k

  cellByCarc <- numeric(ncarc)
  for (celli in 1:ncell){
    groupPattern <- cellMM[celli, ]
    matchingMatrix <- dataMM == groupPattern
    matchingParts <- apply(matchingMatrix, 2, sum)
    matchingTotal <- matchingParts == ncol(cellMM)
    cellByCarc[matchingTotal] <- celli
  }
  carcCells <- cellNames[cellByCarc]

  pInit <- numeric(ncarc)
  for (celli in 1:ncell){
    cellMatch <- which(cellByCarc == celli)
    pInitCellMean <- mean(firstObs[cellMatch])
    pInit[cellMatch] <- pInitCellMean
  }
  pInit[which(pInit < 0.1)] <- 0.1
  pInit[which(pInit > 0.9)] <- 0.9 

  cellMatrix_p <- solve(t(dataMM_p) %*% dataMM_p)
  cellImpact_p <- t(dataMM_p) %*% logit(pInit)
  betaInit_p <- cellMatrix_p %*% cellImpact_p
  betaInit_k <- logit(rep(kInit, nbeta_k))
  betaInit <- c(betaInit_p, betaInit_k)

  if (length(kFixed) == 1){
    betaInit <- betaInit[-length(betaInit)]
  }

  MLEflag = TRUE

  # while(MLEflag){

  MLE <- tryCatch(
           optim(par = betaInit, fn = pkLogLik3, method = "BFGS", hessian = T, 
             cellByCarc = cellByCarc, misses = misses, maxmisses = maxmisses,
             foundOn = foundOn, cellMM = cellMM, nbeta_p = nbeta_p, 
             kFixed = kFixed
           ), 
           error = function(x) {NA}
         )
  
   pHat = alogit(cellMM[, 1:nbeta_p] %*% MLE$par[1:nbeta_p])
   pHatHi = which(pHat > 0.99)
   pHatLo = which(pHat < 0.01)
   
   kHat = alogit(cellMM[, -(1:nbeta_p)] %*% MLE$par[-(1:nbeta_p)])
   kHatHi = which(kHat > 0.99)
   kHatLo = which(kHat < 0.01)

   if(length(c(pHatHi, pHatLo, kHatHi, kHatLo)) == 0){
        MLEflag = FALSE
        break()
    }

    predsCheat_k = paste0(preds_k, 'Cheat_k')
    predsCheat_p = paste0(preds_p, 'Cheat_p')
    # for(i in preds_k){
        # levels(data[, i]) <- c(levels(data[, i]), 'kHi', 'kLo')
    # }
    # for(i in preds_p){
        # levels(data[, i]) <-c(levels(data[, i]), 'pHi', 'pLo')
    # }
    

   if(all(!predsCheat_k %in% names(data))){
        data = cbind(data, data[, preds_k])
        names(data)[(ncol(data) - length(preds_k) + 1):ncol(data)] <- 
            predsCheat_k
   }

   for(badCelli in kHatHi){
        kHiI <- which(matrixStats::rowProds(
            data[ , preds_k] == 
            cells[rep(badCelli, nrow(data)), preds_k]) == 1)
            data[ kHiI, predsCheat_k ] <- NA
            data[ kHiI, 'kCheat'] = 'hi'
    }

   for(badCelli in kHatLo){
        kLoI <- which(matrixStats::rowProds(
            data[ , preds_k] == 
            cells[rep(badCelli, nrow(data)), preds_k]) == 1)
            data[ kLoI, predsCheat_k ] <- NA
            data[ kLoI, 'kCheat'] = 'lo'
    }

   if(all(!predsCheat_p %in% names(data))){
        data = cbind(data, data[, preds_p])
        names(data)[(ncol(data)-length(preds_p) + 1): ncol(data)] <- 
            predsCheat_p
   }

   for(badCelli in pHatHi){
        pHiI <- which(matrixStats::rowProds(
            data[ , paste0(preds_p)] == 
            cells[rep(badCelli, nrow(data)), preds_p]) == 1)
            data[ pHiI, predsCheat_p ] <- NA
            data[ pHiI, 'pCheat'] = 'hi'
        }
    

   for(badCelli in pHatLo){
        pLoI <- which(matrixStats::rowProds(
            data[ , paste0(preds_p, 'Cheat_p')] == 
            cells[rep(badCelli, nrow(data)), preds_p]) == 1)
            data[ pHiI, predsCheat_p ] <- NA
            data[ pHiI, 'pCheat'] = 'lo'
    }

terms_k <- (formulaRHS_k)
attr(terms_k, 'term.labels') <- c(predsCheat_k, 'kCheat')
model.matrix(terms_k, cells)
    
#################################
  preds <- unique(c(preds_p, preds_k))
  cells <- combinePreds(preds, data)
  ncell <- nrow(cells)
  cellNames <- cells$CellNames

  dataMM_p <- model.matrix(formulaRHS_p, data)
  dataMM_k <- model.matrix(formulaRHS_k, data)
  dataMM <- t(cbind(dataMM_p, dataMM_k))
   

  cellMM_p <- model.matrix(formulaRHS_p, cells)
  cellMMreduced_k <- model.matrix(formulaRHS_k, kCellsReduced)
  cellMM <- cbind(cellMM_p, cellMM_k)
 
  nbeta_k <- ncol(dataMM_k)
  nbeta_p <- ncol(dataMM_p)
  nbeta <- nbeta_p + nbeta_k
####################################

 


   # cellMMreduced[c(kHatHi, kHatLo), -(1:nbeta_p)] = 0

   # cellMMreduced = cbind(cellMMreduced[, 1:nbeta_p], 'pHi' = 0, pLo = 0, 
                    # cellMMreduced[, -(1:nbeta_p)], 'kHi' = 0, 'kLo' = 0)

   # cellMMreduced[pHatHi, 'pHi'] = 1
   # cellMMreduced[pHatLo, 'pLo'] = 1
   # cellMMreduced[kHatHi, 'kHi'] = 1
   # cellMMreduced[kHatLo, 'kLo'] = 1


  
alogit(cellMMreduced[, -(1:nbeta_p)] %*% MLE$par[-(1:nbeta_p)])






  convergence <- MLE$convergence
  betahat <- MLE$par
  betaHessian <- MLE$hessian
  llik <- MLE$value

  nparam <- length(betahat)  
  AIC <- round(2 * llik + 2 * nparam, 3)
  AICcOffset <- (2 * nparam * (nparam + 1)) / (ncarc - nparam - 1)
  AICc <- AIC + round(AICcOffset, 3)

  betahat_p <- betahat[1:nbeta_p]
  names(betahat_p) <- colnames(dataMM_p)
  betahat_k <- NULL
  if (length(kFixed) == 0){
    betahat_k <- betahat[(nbeta_p + 1):(nbeta)]
    names(betahat_k) <- colnames(dataMM_k)
  }

  varbeta <- tryCatch(solve(betaHessian), error = function(x) {NA})
  if (is.na(varbeta)[1]){
    stop("Model generates unstable variance estimate.")
  }
  varbeta_p <- varbeta[1:nbeta_p, 1:nbeta_p]
  cellMean_p <- cellMM_p %*% betahat_p
  cellVar_p <- cellMM_p %*% varbeta_p %*% t(cellMM_p)
  cellSD_p <- sqrt(diag(cellVar_p))

  if (length(kFixed) == 0){
    which_k <- (nbeta_p + 1):(nbeta)
    varbeta_k <- varbeta[which_k, which_k]
    cellMean_k <- cellMM_k %*% betahat_k
    cellVar_k <- cellMM_k %*% varbeta_k %*% t(cellMM_k)
    cellSD_k <- sqrt(diag(cellVar_k))
  }else{
    cellMean_k <- rep(kFixed, ncell)
    cellSD_k <- rep(0, ncell)
  }

  probs <- data.frame(c(0.5, (1 - CL) / 2, 1 - (1 - CL) / 2))
  cellTable_p <- apply(probs, 1, qnorm, mean = cellMean_p, sd = cellSD_p)
  cellTable_p <- matrix(cellTable_p, nrow = ncell, ncol = 3)
  cellTable_p <- round(alogit(cellTable_p), 5)
  colnames(cellTable_p) <- c("p_median", "p_lower", "p_upper")
  cellTable_k <- apply(probs, 1, qnorm, mean = cellMean_k, sd = cellSD_k)
  cellTable_k <- matrix(cellTable_k, nrow = ncell, ncol = 3)
  cellTable_k <- round(alogit(cellTable_k), 5)
  colnames(cellTable_k) <- c("k_median", "k_lower", "k_upper")
  cellTable <- data.frame(cell = cellNames, cellTable_p, cellTable_k)

  output <- list()
  output$call <- match.call()
  output$formula_p <- formula_p
  output$formula_k <- formula_k
  output$predictors <- preds
  output$predictors_p <- preds_p
  output$predictors_k <- preds_k
  output$AIC <- AIC
  output$AICc <- AICc
  output$convergence <- convergence
  output$varbeta <- varbeta
  output$cellMM_p <- cellMM_p
  output$cellMM_k <- cellMM_k
  output$nbeta_p <- nbeta_p  
  output$nbeta_k <- nbeta_k
  output$betahat_p <- betahat_p
  output$betahat_k <- betahat_k
  output$levels_p <- levels_p
  output$levels_k <- levels_k
  output$cells <- cells
  output$ncell <- ncell
  output$cellwiseTable <- cellTable
  output$observations <- obsData
  output$kFixed <- kFixed
  output$carcCells <- carcCells
  output$CL <- CL
  class(output) <- c("pkm", "list")
  attr(output, "hidden") <- c("predictors_p", "predictors_k", "kFixed",
                              "betahat_p", "betahat_k", "cellMM_p", 
                              "cellMM_k", "nbeta_p", "nbeta_k", "varbeta",
                              "levels_p", "levels_k", "convergence",  
                              "AIC", "cells", "ncell", "observations",
                              "carcCells", "CL"
                            )

  return(output)
}


#' @export
#'
print.pkm <- function(model){
  hid <- attr(model, "hidden")
  notHid <- !names(model) %in% hid
  print(model[notHid])
}
 