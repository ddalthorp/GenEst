#' Fit a single searcher efficiency model.
#' 
#' Searcher efficiency is modeled as a function of the number of times a 
#'   carcass has been missed in previous searches and any number of 
#'   such as covariates. Format and usage parallel that of common \code{R} 
#'   functions \code{lm}, \code{glm}, and \code{gam}. However, the input data 
#'   (\code{data}) is structured differently to accommodate the 
#'   multiple-search searcher efficiency trials (see 'Details'), and model 
#'   formulas may be entered for both \code{p} (akin to an intercept) and  
#'   \code{k} (akin to aslope).
#'
#' The probability of finding a carcass that is present at the time of search
#'   is \code{p} on the first search after carcass arrival and is assumed to
#'   decrease by a factor of \code{k} each time the carcass is missed in 
#'   searches. Both \code{p} and \code{k} may depend on covariates such as 
#'   ground cover, season, species, etc., and a separate model format 
#'   (\code{formula_p} and \code{formula_k}) may be entered for each. The 
#'   models are entered as they would be in the familiar \code{lm} or 
#'   \code{glm} functions in R. For example, \code{p} might vary with
#'   \code{visibility}, \code{season}, and \code{site}, while \code{k} varies
#'   only with \code{visibility}. A user might then enter \code{p ~ visibility
#'   + season + site} for \code{formula_p} and \code{k ~ visibility} for 
#'   \code{formula_k}. Other R conventions for defining formulas may also be 
#'   used, with \code{covar1:covar2} for the interaction between covariates 
#'   1 and 2 and \code{covar1 * covar2} as short-hand for \code{covar1 +
#'   covar2 + covar1:covar2}.
#'
#' Search trial \code{data} must be entered in a data frame with data in 
#'   each row giving the fate of a single carcass in the field trials. There
#'   must be a column for each search occassion, with 0, 1, or NA depending on 
#'   whether the carcass was missed, found, or not available (typically 
#'   because it was found and removed on a previous search, had been earlier
#'   removed by  scavengers, or was not searched for) on the given search  
#'   occasion. Additional columns with values for categorical covariates  
#'   (e.g., visibility = E, M, or D) may also be included.
#'
#' @param formula_p Formula for p; an object of class "\code{\link{formula}}"
#'   (or one that can be coerced to that class): a symbolic description of 
#'   the model to be fitted. Details of model specification are given under 
#'   "Details".
#'
#' @param data Dataframe with results from searcher efficiency trials and any
#' covariates included in \code{formula_p} or {formula_k} (required).
#'
#' @param formula_k Formula for k; an object of class "\code{\link{formula}}"
#'   (or one that can be coerced to that class): a symbolic description of the
#'   model to be fitted. Details of model specification are given under 
#'   "Details".
#'
#' @param obsCol Vector of names of columns in \code{data} where results 
#'   for each search occasion are stored (optional). If no \code{obsCol} are 
#'   provided, \code{pkm} uses as \code{obsCol} all columns with names that 
#'   begin with an \code{"s"} or \code{"S"} and end with a number, e.g., "s1",
#'   "s2", "s3", etc. This option is included as a convenience for the user, 
#'   but care must be taken that other data are not stored in columns with 
#'   names matching that pattern. Alternatively, \code{obsCol} may be 
#'   entered as a vector of names, like \code{c("s1", "s2", "s3")}, 
#'   \code{paste0("s", 1:3)}, or \code{c("initialSearch", "anotherSearch", 
#'   "lastSearch")}.
#'
#' @param kFixed Parameter for user-specified \code{k} value (optional). If a
#'   value is provided, \code{formula_k} is ignored and the model is fit under 
#'   the assumption that the \code{k} parameter is fixed and known to be
#'   \code{fix_k}.
#'
#' @param kInit Initial value used for \code{k} in the optimization.
#'
#' @param CL confidence level
#'
#' @return \code{pkm} returns an object of class "\code{pkm}", which is a list
#'   whose components characterize the fit of the model. Due to the large
#'   number and complexity of components, only a subset of them is printed 
#'   automatically; the rest can be viewed/accessed directly via the \code{$}
#'  operator if desired.
#'
#' The following components are displayed automatically:
#'
#' \describe{
#'  \item{\code{call}}{the function call to fit the model}
#'   \item{\code{formula_p}}{the model formula for the \code{p} parameter}
#'   \item{\code{formula_k}}{the model formula for the \code{k} parameter}
#'  \item{\code{predictors}}{list of covariates of \code{p} and/or \code{k}}
#'  \item{\code{cellwiseTable}}{summary statistics for estimated cellwise 
#'    \code{p} and \code{k}, including the medians and upper & lower bounds
#'    on CIs for each parameter, indexed by cell (or combination of
#'    covariate levels).}
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
#'   \item{\code{betahat_p}}{parameter estimates for the terms in the 
#'     regression model for for \code{p} (logit scale)}
#'   \item{\code{betahat_k}}{parameter estimates for the terms in the 
#'     regression model for for \code{k} (logit scale). If \code{k} is fixed 
#'     and known, \code{betahat_k} is not calculated.}
#'   \item{\code{varbeta}}{the variance-covariance matrix of the estimators
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
#'  \item{\code{AIC}}{the 
#'    \href{https://en.wikipedia.org/wiki/Akaike_information_criterion}{AIC}
#'    value for the fitted model}
#'  \item{\code{carcCells}}{the cell to which each carcass belongs}
#'  \item{\code{CL}}{the input \code{CL}}
#'}
#'
#' @examples
#' data(pkmdat)
#' pkm(p ~ visibility, k ~ 1, data = pkmdat)
#' pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' pkm(p ~ visibility, kFixed = 0.7, data = pkmdat)
#'
#' @export
#'
pkm <- function(formula_p, formula_k = NULL, data, obsCol = NULL, 
                kFixed = NULL, kInit = 0.7, CL = 0.9){

  if(sum(obsCol %in% colnames(data)) != length(obsCol)){
    stop("Observation column provided not in data.")
  }
  if (length(obsCol) == 0){
    obsCol <- grep("^[sS].*[0-9]$", names(data), value = TRUE)
    nobsCol <- length(obsCol)
    if (nobsCol == 0){
      stop("No observation columns provided and no appropriate column names.")
    }
  }
  predCheck <- c(all.vars(formula_p[[3]]), all.vars(formula_k[[3]]))
  if (sum(predCheck %in% colnames(data)) != length(predCheck)){
    stop("Predictor(s) in formula(e) not found in data.")
  }
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

  nsearch <- length(obsCol)
  obsData <- data[ , obsCol]
  obsData <- as.matrix(obsData, ncol = nsearch)
  obsNA <- is.na(obsData)
  onlyNA <- which(apply(obsNA, 1, sum) == apply(obsNA, 1, length))
  obsData <- as.matrix(obsData[-onlyNA, ], ncol = nsearch) 
  data <- data[-onlyNA, ]
  ncarc <- nrow(obsData)

  if (any(rowSums(obsData, na.rm = TRUE) > 1)){
    stop("Carcasses observed more than once. Check data.")
  }
  tots <- apply(obsData, 2, GenEst::trueLength)
  hits <- apply(obsData, 2, sum, na.rm = TRUE) 
  misses <- tots - hits 
  if (isNeverIncreasing(tots) == FALSE){
    message("Observations appear to not be in order, attempting to sort.")

    tots_misses <- data.frame(tots, misses)
    ordering <- do.call(order, c(tots_misses, decreasing = TRUE))
    tots_misses <- tots_misses[ordering, ]
    obsCol <- rownames(tots_misses)
    obsData <- data[ , obsCol]
    obsData <- as.matrix(obsData, ncol = nsearch)
    tots <- apply(obsData, 2, GenEst::trueLength)

    if (isNeverIncreasing(tots) == FALSE){
      stop("Observations are out of order and can't be sorted. Check data.")
    }
  }

  firstObs <- obsData[ , 1]
  missData <- apply(obsData, 2, match, 0)

  misses <- apply(missData, 1, sum, na.rm = TRUE)
  maxmisses <- max(misses)
  found <- apply(obsData, 1, sum, na.rm = TRUE)
  carcassesFound <- which(found == 1)
  foundOn <- numeric(ncarc)
  foundOn[carcassesFound] <- misses[carcassesFound] + 1

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

  MLE <- tryCatch(
           optim(par = betaInit, fn = pkLogLik, method = "BFGS", 
             hessian = TRUE, cellByCarc = cellByCarc, misses = misses, 
             maxmisses = maxmisses, foundOn = foundOn, cellMM = cellMM, 
             nbeta_p = nbeta_p, kFixed = kFixed
           ), error = function(x) {NA}
         )

  convergence <- MLE$convergence
  betahat <- MLE$par
  betaHessian <- MLE$hessian
  llik <- MLE$value

  nparam <- length(betahat)  
  AIC <- 2 * llik + 2 * nparam
  AICcOffset <- (2 * nparam * (nparam + 1)) / (ncarc - nparam - 1)
  AICc <- round(AIC + AICcOffset, 3)

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
                              "levels_p", "levels_k", "carcCells", "CL",   
                              "AIC", "cells", "ncell", "observations"
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
 
#' Calculate the negative log-likelihood of a searcher efficiency model.
#' 
#' @param misses Number of searches when carcass was present but
#'  not found.
#' @param foundOn Search on which carcass was found.
#' @param beta Parameters to be optimized.
#' @param nbeta_p Number of parameters associated with p.
#' @param cellByCarc Which cell each observation belongs to.
#' @param maxmisses Maximum possible number of misses for a carcass.
#' @param cellMM Combined pk model matrix.
#' @param kFixed Value of k if fixed. 
#' @return Negative log likelihood of the observations, given the parameters.
#' @examples
#' NA
#' @export 
#'
pkLogLik <- function(misses, foundOn, beta, nbeta_p, cellByCarc, maxmisses, 
                     cellMM, kFixed = NULL){

  if (length(kFixed) == 1){
    beta <- c(beta, logit(kFixed))
  }

  ncell <- nrow(cellMM)
  nbeta <- length(beta)
  which_p <- 1:nbeta_p
  which_k <- (nbeta_p + 1):nbeta

  beta_p <- beta[which_p]
  beta_k <- beta[which_k]
  Beta <- matrix(0, nrow = nbeta, ncol = 2)
  Beta[which_p, 1] <- beta[which_p]
  Beta[which_k, 2] <- beta[which_k]

  pk <- alogit(cellMM %*% Beta)
  p <- pk[ , 1]
  k <- pk[ , 2]

  powk <- matrix(k, nrow = ncell, ncol = maxmisses + 1)
  powk[ , 1] <- 1
  powk <- matrixStats::rowCumprods(powk)

  pmiss <- matrix(1 - (p * powk[ , 1:(maxmisses + 1)]), nrow = ncell)
  pmiss <- matrixStats::rowCumprods(pmiss)
  pfind <- matrixStats::rowDiffs(1 - pmiss)
  pfind_si <- cbind(pk[ , 1], pfind)

  notFoundCell <- cellByCarc[foundOn == 0]
  notFoundMisses <- misses[foundOn == 0]
  notFoundCellMisses <- cbind(notFoundCell, notFoundMisses)
  foundCell <- cellByCarc[foundOn > 0]
  foundFoundOn <- foundOn[foundOn > 0]
  foundCellFoundOn <- cbind(foundCell, foundFoundOn)

  ll_miss <- sum(log(pmiss[notFoundCellMisses]))
  ll_found <- sum(log(pfind_si[foundCellFoundOn]))
  nll_total <- -(ll_miss + ll_found)
 
  return(nll_total)
}

#' Run a set of pkm models based on predictor inputs
#'
#' Function inputs follow \code{pkm}, with all simpler models being run
#'   and returned as a list of model objects
#'
#' @param formula_p Formula for p; an object of class "\code{\link{formula}}"
#'   (or one that can be coerced to that class): a symbolic description of the
#'   model to be fitted. Details of model specification are given under 
#'   "Details".
#'
#' @param data Dataframe with results from searcher efficiency trials and any
#'   covariates included in \code{formula_p} or {formula_k} (required).
#'
#' @param formula_k Formula for k; an object of class "\code{\link{formula}}"
#'   (or one that can be coerced to that class): a symbolic description of the
#'   model to be fitted. Details of model specification are given under 
#'   "Details".
#'
#' @param obsCol Vector of names of columns in \code{data} where results 
#'   for each search occasion are stored (optional). If no \code{obsCol} are 
#'   provided, \code{pkm} uses as \code{obsCol} all columns with names that 
#'   begin with an \code{"s"} or \code{"S"} and end with a number, e.g., "s1",
#'   "s2", "s3", etc. This option is included as a convenience for the user, 
#'   but care must be taken that other data are not stored in columns with 
#'   names matching that pattern. Alternatively, \code{obsCol} may be 
#'   entered as a vector of names, like \code{c("s1", "s2", "s3")}, 
#'   \code{paste0("s", 1:3)}, or \code{c("initialSearch", "anotherSearch", 
#'   "lastSearch")}.
#'
#' @param kFixed Parameter for user-specified \code{k} value (optional). If a
#'   value is provided, \code{formula_k} is ignored and the model is fit under 
#'   the assumption that the \code{k} parameter is fixed and known to be
#'   \code{fix_k}.
#'
#' @param kInit Initial value used for \code{k} in the optimization.
#'
#' @param CL confidence level
#'
#' @return \code{pkmSet} returns a list of objects, each of class 
#'   "\code{pkm}", which each then a list whose components characterize the 
#'   fit of the specific model.
#'
#' @export 
#'
pkmSet <- function(formula_p, formula_k = NULL, data, obsCol = NULL, 
                   kFixed = NULL, kInit = 0.7, CL = 0.9){

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
    formula_k <- k ~ 1
  }
  if (length(formula_k) == 0 & length(kFixed) == 0){
    message("No formula or fixed value provided for k, fixed at 0.")
    formula_k <- k ~ 1
    kFixed <- 0
  }
  if (length(obsCol) == 1 & length(kFixed) == 0){
    message("Only one observation, k cannot be estimated.")
    kFixed <- 1
  }
  if (length(formula_k) == 0){
    formula_k <- k ~ 1
  }
  nsearch <- length(obsCol)
  ncarc <- nrow(data)
  obsData <- data[ , obsCol]
  obsData <- as.matrix(obsData, ncol = nsearch)

  if (any(rowSums(obsData, na.rm = TRUE) > 1)){
    stop("Carcasses observed more than once. Check data.")
  }
  tots <- apply(obsData, 2, GenEst::trueLength)
  hits <- apply(obsData, 2, sum, na.rm = TRUE) 
  misses <- tots - hits 
  if (isNeverIncreasing(tots) == FALSE){
    message("Observations appear to not be in order, attempting to sort.")

    tots_misses <- data.frame(tots, misses)
    ordering <- do.call(order, c(tots_misses, decreasing = TRUE))
    tots_misses <- tots_misses[ordering, ]
    obsCol <- rownames(tots_misses)
    obsData <- data[ , obsCol]
    obsData <- as.matrix(obsData, ncol = nsearch)
    tots <- apply(obsData, 2, GenEst::trueLength)

    if (isNeverIncreasing(tots) == FALSE){
      stop("Observations are out of order and can't be sorted. Check data.")
    }
  }
  # create the set of models to explore, based on the input parameters
  
  terms_p <- attr(terms(formula_p), "term.labels")
  terms_k <- attr(terms(formula_k), "term.labels")
  nterms_p <- length(terms_p)
  nterms_k <- length(terms_k)
  nformula_p <- 2^(nterms_p)
  nformula_k <- 2^(nterms_k)

  dropComplex_p <- rep(1:nterms_p, choose(nterms_p, 1:nterms_p))
  dropWhich_p <- numeric(0)
  if (nterms_p > 0){
    for (termi in 1:nterms_p){
      specificDrop <- seq(1, choose(nterms_p, (1:nterms_p)[termi]))
      dropWhich_p <- c(dropWhich_p, specificDrop)
    }
  }
  optionFormula_p <- vector("list", nformula_p)
  optionFormula_p[[1]] <- formula_p
  keepFormula_p <- rep(TRUE, nformula_p)
  if (nformula_p > 1){
    for (formi in 2:nformula_p){
      termDropComplex <- combn(terms_p, dropComplex_p[formi - 1])
      termDropSpec <- termDropComplex[ , dropWhich_p[formi - 1]]
      termDrop <- paste(termDropSpec, collapse = " - ")
      formulaUpdate <- paste(format(~.), "-", termDrop)
      updatedFormula <- update.formula(formula_p, formulaUpdate)
      optionFormula_p[[formi]] <- updatedFormula
      keepFormula_p[formi] <- checkComponents(updatedFormula)
    }
    nkeepFormula_p <- sum(keepFormula_p)
    whichKeepFormula_p <- which(keepFormula_p == TRUE)
    keptFormula_p <- vector("list", nkeepFormula_p)
    for (kepti in 1:nkeepFormula_p){
      keptFormula_p[[kepti]] <- optionFormula_p[[whichKeepFormula_p[kepti]]]
    }
  }else{
    keptFormula_p <- optionFormula_p
  }
  
  dropComplex_k <- rep(1:nterms_k, choose(nterms_k, 1:nterms_k))
  dropWhich_k <- numeric(0)
  if (nterms_k > 0){
    for (termi in 1:nterms_k){
      specificDrop <- seq(1, choose(nterms_k, (1:nterms_k)[termi]))
      dropWhich_k <- c(dropWhich_k, specificDrop)
    }
  }
  optionFormula_k <- vector("list", nformula_k)
  optionFormula_k[[1]] <- formula_k
  keepFormula_k <- rep(TRUE, nformula_k)
  if (nformula_k > 1){
    for (formi in 2:nformula_k){
      termDropComplex <- combn(terms_k, dropComplex_k[formi - 1])
      termDropSpec <- termDropComplex[ , dropWhich_k[formi - 1]]
      termDrop <- paste(termDropSpec, collapse = " - ")
      formulaUpdate <- paste(format(~.), "-", termDrop)
      updatedFormula <- update.formula(formula_k, formulaUpdate)
      optionFormula_k[[formi]] <- updatedFormula
      keepFormula_k[formi] <- checkComponents(updatedFormula)
    }
    nkeepFormula_k <- sum(keepFormula_k)
    whichKeepFormula_k <- which(keepFormula_k == TRUE)
    keptFormula_k <- vector("list", nkeepFormula_k)
    for (kepti in 1:nkeepFormula_k){
      keptFormula_k[[kepti]] <- optionFormula_k[[whichKeepFormula_k[kepti]]]
    }
  }else{
    keptFormula_k <- optionFormula_k
  }
  if (length(kFixed) == 1){
    keptFormula_k <- NA
  }

  expandedKeptFormulae <- expand.grid(keptFormula_p, keptFormula_k)
  keptFormula_p <- expandedKeptFormulae[ , 1]
  keptFormula_k <- expandedKeptFormulae[ , 2]
  if (length(kFixed) == 1){
    keptFormula_k <- NULL
  }
  nmod <- nrow(expandedKeptFormulae) 
  output <- vector("list", nmod)
  for (modi in 1:nmod){
    formi_p <- keptFormula_p[modi][[1]]
    formi_k <- keptFormula_k[modi][[1]]
    pkm_i <- tryCatch(
               pkm(formi_p, formi_k, data, obsCol, kFixed, kInit, CL), 
               error = function(x) {"Failed model fit"}
             )

    name_p <- paste(format(formi_p), collapse = "")
    name_p <- gsub("    ", "", name_p)
    name_k <- paste(format(formi_k), collapse = "")
    name_k <- gsub("    ", "", name_k)
    if(length(kFixed) == 1){
      name_k <- paste("k fixed at ", kFixed, sep = "")
    }
    modName <- paste(name_p, "; ", name_k, sep = "")

    output[[modi]] <- pkm_i
    names(output)[modi] <- modName
  }
  class(output) <- c("pkmSet", "list")
  return(output)
}


#' Fit all possible searcher efficiency models across all size classes.
#'
#' Function inputs generally follow \code{pkmSet} and \code{pkm} but with an 
#'   additional size column input and calculation of the set of pkm models for
#'   each of the size classes
#'
#' @param formula_p Formula for p; an object of class "\code{\link{formula}}"
#'   (or one that can be coerced to that class): a symbolic description of the
#'   model to be fitted. Details of model specification are given under 
#'   "Details".
#'
#' @param data Dataframe with results from searcher efficiency trials and any
#'   covariates included in \code{formula_p} or {formula_k} (required).
#'
#' @param formula_k Formula for k; an object of class "\code{\link{formula}}"
#'   (or one that can be coerced to that class): a symbolic description of the
#'   model to be fitted. Details of model specification are given under 
#'   "Details".
#'
#' @param obsCol Vector of names of columns in \code{data} where results 
#'   for each search occasion are stored (optional). If no \code{obsCol} are 
#'   provided, \code{pkm} uses as \code{obsCol} all columns with names that 
#'   begin with an \code{"s"} or \code{"S"} and end with a number, e.g., "s1",
#'   "s2", "s3", etc. This option is included as a convenience for the user, 
#'   but care must be taken that other data are not stored in columns with 
#'   names matching that pattern. Alternatively, \code{obsCol} may be 
#'   entered as a vector of names, like \code{c("s1", "s2", "s3")}, 
#'   \code{paste0("s", 1:3)}, or \code{c("initialSearch", "anotherSearch", 
#'   "lastSearch")}.
#'
#' @param kFixed Parameter for user-specified \code{k} value (optional). If a
#'   value is provided, \code{formula_k} is ignored and the model is fit under 
#'   the assumption that the \code{k} parameter is fixed and known to be
#'   \code{fix_k}.
#'
#' @param init_k Initial value used for \code{k} in the optimization.
#'
#' @param CL confidence level
#'
#' @param sizeclassCol Name of colum in \code{data} where the size classes
#'  are recorded
#'
#' @return \code{pkmSetSize} returns a class-\code{pkmSetSize} list of 
#'   objects, each of which is a class-\code{pkmSet} list of \code{pkm}" 
#'   outputs (each corresponding to the fit of a specific model
#'   within the set of \code{pkm} models fit for the given size class), that
#'   is of length equal to the total number of size classes
#'
#' @export
#'
pkmSetSize <- function(formula_p, formula_k = NULL, data, obsCol = NULL, 
                       sizeclassCol = NULL, kFixed = NULL, kInit = 0.7, 
                       CL = 0.9){

  if (length(sizeclassCol) == 0){
    message("No size class provided, function run as if pkmSet")
    output <- pkmSet(formula_p, formula_k, data, obsCol, kFixed, kInit, CL)
    return(output)
  }
  nsearch <- length(obsCol)
  ncarc <- nrow(data)
  obsData <- data[ , obsCol]
  obsData <- as.matrix(obsData, ncol = nsearch)

  if (any(rowSums(obsData, na.rm = TRUE) > 1)){
    stop("Carcasses observed more than once. Check data.")
  }
  tots <- apply(obsData, 2, GenEst::trueLength)
  hits <- apply(obsData, 2, sum, na.rm = TRUE) 
  misses <- tots - hits 
  if (isNeverIncreasing(tots) == FALSE){
    message("Observations appear to not be in order, attempting to sort.")

    tots_misses <- data.frame(tots, misses)
    ordering <- do.call(order, c(tots_misses, decreasing = TRUE))
    tots_misses <- tots_misses[ordering, ]
    obsCol <- rownames(tots_misses)
    obsData <- data[ , obsCol]
    obsData <- as.matrix(obsData, ncol = nsearch)
    tots <- apply(obsData, 2, GenEst::trueLength)

    if (isNeverIncreasing(tots) == FALSE){
      stop("Observations are out of order and can't be sorted. Check data.")
    }
  }

  sizeclassData <- as.character(data[ , sizeclassCol])
  sizeclasses <- unique(sizeclassData)
  nsizeclasses <- length(sizeclasses)

  out <- vector("list", nsizeclasses)
  names(out) <- sizeclasses
  for (sci in 1:nsizeclasses){
    sizeclassMatch <- which(sizeclassData == sizeclasses[sci])
    data_i <- data[sizeclassMatch, ]
    out[[sci]] <- pkmSet(formula_p, formula_k, data_i, obsCol, kFixed, 
                    kInit, CL
                  )
  }

  return(out)
}

#' Verify that a suite of searcher efficiency models all fit successfully.
#'
#' @param pkmToCheck A \code{pkm} model or a set of them or a suite of sets
#'   associated with multiple sizes
#'
#' @return A single (total) logcal 
#'
#' @export
#'
pkmCheck <- function(pkmToCheck){

  status <- 0
  classSingle <- class(pkmToCheck)
  classSet <- class(pkmToCheck[[1]])
  classSize <- class(pkmToCheck[[1]][[1]]) 
  if ("pkm" %in% classSingle){
    status <- 1
  }
  if ("pkm" %in% classSet){
    ninSet <- length(pkmToCheck)
    checks <- rep(0, ninSet)
    for (modi in 1:ninSet){
      checkClass <- class(pkmToCheck[[modi]])
      if ("pkm" %in% checkClass){
        checks[modi] <- 1
      }
    }
    status <- floor(mean(checks))
  }
  if ("pkm" %in% classSize){
    nsizeclasses <- length(pkmToCheck)
    ninSet <- length(pkmToCheck[[1]])
    checks <- matrix(0, nsizeclasses, ninSet)
    for (sci in 1:nsizeclasses){
      for (modi in 1:ninSet){
        checkClass <- class(pkmToCheck[[sci]][[modi]])
        if ("pkm" %in% checkClass){
          checks[sci, modi] <- 1
        }
      }
    }
    status <- floor(mean(checks))
  }
  output <- as.logical(status)
  return(output)
}

#' Create the  AICc tables for the searcher efficiency models
#' 
#' @param pkmset Set of searcher efficiency models fit to the same
#'   observations
#' @return AICc table
#' @examples
#' NA
#' @export 
#'
pkmSetAICcTab <- function(pkmset){

  nmod <- length(pkmset)
  formulas <- names(pkmset)
  formulas_p <- rep(NA, nmod)
  formulas_k <- rep(NA, nmod)
  AICc <- rep(NA, nmod)
  deltaAICc <- rep(NA, nmod)

  if (nmod == 1){
    splitFormulas <- strsplit(formulas, "; ")[[1]]
    formulas_p <- splitFormulas[1] 
    formulas_k <- splitFormulas[2]
    AICc <- tryCatch(pkmset[[1]]$AICc, error = function(x) {1e7})
    deltaAICc <- 0    
    AICcOrder <- 1
  }else{
    for (modi in 1:nmod){
      splitFormulas_i <- strsplit(formulas[modi], "; ")[[1]]
      formulas_p[modi] <- splitFormulas_i[1] 
      formulas_k[modi] <- splitFormulas_i[2]
      AICc[modi] <- tryCatch(pkmset[[modi]]$AICc, error = function(x) {1e7})
    }
    AICcOrder <- order(AICc)
    deltaAICc <- round(AICc - min(AICc), 3)
    which_fails <- which(AICc == 1e7)
    AICc[which_fails] <- NA
    deltaAICc[which_fails] <- NA
  }

  output <- data.frame(formulas_p, formulas_k, AICc, deltaAICc)
  output <- output[AICcOrder, ]
  colnames(output) <- c("p formula", "k formula", "AICc", "Delta AICc")
  whichAICcNA <- which(is.na(output$AICc))
  whichAICcMax <- which(output$AICc == 1e7)
  if (length(whichAICcNA) > 0){
    message("Models with incorrect specification were removed from output.")
    output <- output[-whichAICcNA, ]
  }
  if (length(whichAICcMax) > 0){
    message("Models that failed during fit were removed from output.")
    output <- output[-whichAICcMax, ]
  }
  return(output)
}

#' Simulate p and k parameters from a fitted pk model.
#'
#' @param n the number of simulation draws
#'
#' @param model A \code{\link{pkm}} object (which is returned from 
#'   \code{pkm()})
#'
#' @param seed optional input to set the seed of the RNG
#'
#' @return list of two matrices of \code{n} simulated \code{p} and \code{k} 
#'   for cells defined by the \code{model} object. 
#'
#' @examples
#' data(pkmdat)
#' pkmod_1 <- pkm(p ~ 1, k ~ 1, data = pkmdat)
#' simulated_pk <- rpk(n = 10, model = pkmod_1)
#' simulated_pk
#'
#' pkmod_2 <- pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' rpk(n = 10, model = pkmod_2)
#' @export
#'
rpk <- function(n = 1, model, seed = NULL){

  if (!"pkm" %in% class(model)){
    stop("model not of class pkm.")
  }

  nbeta_p <- model$nbeta_p 
  nbeta_k <- model$nbeta_k
  which_beta_k <- (nbeta_p + 1):(nbeta_p + nbeta_k)
  kFixed <- model$kFixed
  cellMM_p <- model$cellMM_p
  cellMM_k <- model$cellMM_k
  ncell <- model$ncell
  cellNames <- model$cells[ , "CellNames"]
  meanbeta <- c(model$betahat_p, model$betahat_k)
  varbeta <- model$varbeta
  method <-  "svd"

  if (length(seed) > 0){
    set.seed(seed)
  }
  sim_beta <- mvtnorm::rmvnorm(n, mean = meanbeta, sigma = varbeta, method)
  sim_p <- as.matrix(alogit(sim_beta[ , 1:nbeta_p] %*% t(cellMM_p)))
  colnames(sim_p) <- cellNames

  if (length(kFixed) == 0){
    sim_k <- as.matrix(alogit(sim_beta[ , which_beta_k] %*% t(cellMM_k)))
  } else{
    sim_k <- matrix(kFixed, ncol = ncell, nrow = n)
  }
  colnames(sim_k) <- cellNames

  output <- vector("list", ncell)
  names(output) <- cellNames
  for (celli in 1:ncell){
    cellpk <- cbind(p = sim_p[ , celli], k = sim_k[ , celli])
    output[[celli]] <-  cellpk
  }

  return(output)
}

