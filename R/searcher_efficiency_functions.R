#' @title Fit a single searcher efficiency model.
#' 
#' @description Searcher efficiency is modeled as a function of the number of
#'   times a  carcass has been missed in previous searches and any number of 
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
#' @param quiet Logical indicator of whether or not to print messsages
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
#'  \item{\code{data}}{the data used to fit the model}
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
#'   data(wind_RP)
#'   pkm(formula_p = p ~ Season, formula_k = k ~ 1, data = wind_RP$SE)
#'
#' @export
#'
pkm <- function(formula_p, formula_k = NULL, data, obsCol = NULL,
                kFixed = NULL, kInit = 0.7, CL = 0.95, quiet = FALSE){
  if(sum(obsCol %in% colnames(data)) != length(obsCol)){
    stop("Observation column provided not in data.")
  }
  if (length(obsCol) == 0){
    obsCol <- grep("^[sS].*[0-9]$", names(data), value = TRUE)
    nobsCol <- length(obsCol)
    if (nobsCol == 0){
      stop("No obsCol provided and no appropriate column names found.")
    }
  }
  predCheck <- c(all.vars(formula_p[[3]]), all.vars(formula_k[[3]]))
  if (any(!(predCheck %in% colnames(data)))){
    stop("User-supplied formula includes predictor that is not found in data.")
  }
  if (length(kFixed) >= 1){
    if (!is.numeric(kFixed[1]) || is.na(kFixed[1])){
      stop("User-supplied kFixed must be numeric (or NULL)")
    }
    if (kFixed[1] < 0 | kFixed[1] > 1){
      stop("User-supplied kFixed is outside the supported range [0, 1].")
   }
    if (length(formula_k) > 0 & quiet == FALSE){
      message("Formula and fixed value provided for k, fixed value used.")
      formula_k <- NULL
    }
    if (length(kFixed) > 1){
      kFixed <- kFixed[1]
      if (!quiet){
        message("Vector-valued kFixed. Only the first element will be used.")
      }
    }
  }
  pOnly <- FALSE
  if (length(obsCol) == 1){
    if (!is.null(formula_k) && is.language(formula_k) && quiet == FALSE){
      message("Only one search occasion per carcass. k not estimated.")
    }
    pOnly <- TRUE
    formula_k <- NULL
    kFixed <- 1
  } else {
    # flag to indicate no estimation of k
    if ((is.null(formula_k) || !is.language(formula_k)) & length(kFixed) == 0){
      pOnly <- TRUE
      obsCol <- obsCol[1] # use data from first search only
      formula_k <- NULL
      kFixed <- 1
    }
  }
  nsearch <- length(obsCol)
  obsData <- as.matrix(data[ , obsCol], ncol = nsearch)

  # replace all non-zero/non-one data with NA:
  if (!is.numeric(obsData)){
    obsData[!is.na(obsData) & !(obsData %in% as.character(0:1))] <- NA
    obsData <- matrix(as.numeric(obsData), ncol = nsearch)
  } else {
    obsData[!(obsData %in% 0:1)] <- NA
  }

  # remove rows that are all NAs
  onlyNA <- (rowSums(is.na(obsData)) == nsearch)
  obsData <- as.matrix(obsData[!onlyNA, ], ncol = nsearch)
  data0 <- data[!onlyNA, ]

  if (nrow(data0) == 0){
    stop("No non-missing data present in provided data.")
  }
  if (any(rowSums(obsData, na.rm = TRUE) > 1)){
    stop("Carcasses observed more than once. Check data.")
  }
  if (sum(na.omit(rowDiffs(obsData * is.na(obsData)))) > 0){
    stop("Searches continue after carcass discovery? Check data.")
  }

  ncarc <- nrow(obsData)
  # simplified and vectorized calculations of
  #1. number of times each carcass was missed in searches, and
  #2. which search carcasses were found on (0 if not found)
  misses <- matrixStats::rowCounts(obsData, value = 0, na.rm =T)
  foundOn <- colMaxs(t(obsData) * (1:nsearch), na.rm = T)

  if (length(kFixed) > 0){
    if (kFixed == 0 & any(foundOn > 1)){
#      suggestion <- kSuggest(obsData) # no suggestion
      stop(
        "User-supplied kFixed = 0. However, carcasses were found after ",
        "being missed in previous searches, which indicates k > 0"
      )
    }
  }

  preds_p <- all.vars(formula_p[[3]])
  if (length(preds_p) > 0){
    for (predi in 1:length(preds_p)){
      data0[ , preds_p[predi]] <- as.character(data0[ , preds_p[predi]])
    }
  }
  formulaRHS_p <- formula(delete.response(terms(formula_p)))
  levels_p <- .getXlevels(terms(formulaRHS_p), data0)

  preds_k <- character(0)
  if (is.language(formula_k)){
    preds_k <- all.vars(formula_k[[3]])
    if (length(preds_k) > 0){
      for (predi in 1:length(preds_k)){
        data0[ , preds_k[predi]] <- as.character(data0[ , preds_k[predi]])
      }
    }
    formulaRHS_k <- formula(delete.response(terms(formula_k)))
    levels_k <- .getXlevels(terms(formulaRHS_k), data0)
  }
  if (length(kFixed) == 1){
    preds_k <- character(0)
    formulaRHS_k <- formula(~1)
    formula_k <- c(fixedk = kFixed)
    levels_k <- .getXlevels(terms(formulaRHS_k), data0)
  }

  preds <- unique(c(preds_p, preds_k))
  cells <- combinePreds(preds, data0)
  ncell <- nrow(cells)
  cellNames <- cells$CellNames

  dataMM_p <- model.matrix(formulaRHS_p, data0)
  dataMM_k <- model.matrix(formulaRHS_k, data0)
  dataMM <- t(cbind(dataMM_p, dataMM_k))
  cellMM_p <- model.matrix(formulaRHS_p, cells)
  cellMM_k <- model.matrix(formulaRHS_k, cells)
  cellMM <- cbind(cellMM_p, cellMM_k)

  nbeta_k <- ncol(dataMM_k)
  nbeta_p <- ncol(dataMM_p)
  nbeta <- nbeta_p + nbeta_k
  if (length(preds) == 0){
    carcCells <- rep("all", ncarc)
  } else if (length(preds) == 1){
    carcCells <- data0[ , preds]
  } else if (length(preds) > 1){
    carcCells <- do.call(paste, c(data0[,preds], sep = '.'))
  }
  cellByCarc <- match(carcCells, cellNames)

  pInitCellMean <- tapply(data0[ , obsCol[1]], INDEX = carcCells, FUN = mean)
  pInit <- as.vector(pInitCellMean[match(carcCells, names(pInitCellMean))])
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
             maxmisses = max(misses), foundOn = foundOn, cellMM = cellMM,
             nbeta_p = nbeta_p, kFixed = kFixed
           ), error = function(x) {NA}
         )

  convergence <- MLE$convergence
  betahat <- MLE$par
  betaHessian <- MLE$hessian
  llik <- -MLE$value

  nparam <- length(betahat)
  AIC <- 2*nparam - 2*llik
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

  if (is.na(varbeta)[1]) stop("Unable to estimate variance.")

  varbeta_p <- varbeta[1:nbeta_p, 1:nbeta_p]
  cellMean_p <- cellMM_p %*% betahat_p
  cellVar_p <- cellMM_p %*% varbeta_p %*% t(cellMM_p)
  cellSD_p <- suppressWarnings(sqrt(diag(cellVar_p)))

  if (is.null(kFixed) || is.na(kFixed)){
    which_k <- (nbeta_p + 1):(nbeta)
    varbeta_k <- varbeta[which_k, which_k]
    cellMean_k <- cellMM_k %*% betahat_k
    cellVar_k <- cellMM_k %*% varbeta_k %*% t(cellMM_k)
    cellSD_k <- suppressWarnings(sqrt(diag(cellVar_k)))
  } else {
    cellMean_k <- rep(logit(kFixed), ncell)
    cellSD_k <- rep(0, ncell)
  }

  probs <- list(0.5, (1 - CL) / 2, 1 - (1 - CL) / 2)
  cellTable_p <- lapply(probs, qnorm, mean = cellMean_p, sd = cellSD_p)
  cellTable_p <- matrix(unlist(cellTable_p), ncol = 3)
  cellTable_p <- round(alogit(cellTable_p), 3)
  colnames(cellTable_p) <- c("p_median", "p_lower", "p_upper")
  if (!pOnly){
    cellTable_k <- lapply(probs, qnorm, mean = cellMean_k, sd = cellSD_k)
    cellTable_k <- matrix(unlist(cellTable_k), nrow = ncell, ncol = 3)
    cellTable_k <- round(alogit(cellTable_k), 3)
    colnames(cellTable_k) <- c("k_median", "k_lower", "k_upper")
    if (nsearch == 1){
      cellTable_k[ , names(cellTable_k)] <- kFixed
      formula_k <- ""
      nbeta_k <- 0
      cellMM_k <- NULL
      kFixed <- NULL
    }
    cellTable <- data.frame(cell = cellNames, cellTable_p, cellTable_k)
  } else {
    cellTable <- data.frame(cell = cellNames, cellTable_p)
  }


  output <- list()
  output$call <- match.call()
  output$data <- data
  output$formula_p <- formula_p
  if (!pOnly) output$formula_k <- formula_k
  output$predictors <- preds
  output$predictors_p <- preds_p
  if (!pOnly) output$predictors_k <- preds_k
  output$AIC <- AIC
  output$AICc <- AICc
  output$convergence <- convergence
  output$varbeta <- varbeta
  output$cellMM_p <- cellMM_p
  if (!pOnly) output$cellMM_k <- cellMM_k
  output$nbeta_p <- nbeta_p
  if (!pOnly) output$nbeta_k <- nbeta_k
  output$betahat_p <- betahat_p
  if (!pOnly) output$betahat_k <- betahat_k
  output$levels_p <- levels_p
  if (!pOnly) output$levels_k <- levels_k
  output$cells <- cells
  output$ncell <- ncell
  output$cellwiseTable <- cellTable
  output$CL <- CL
  output$observations <- obsData
  if (!pOnly) output$kFixed <- kFixed
  output$carcCells <- carcCells
  output$loglik <- llik
  output$pOnly <- pOnly
  class(output) <- c("pkm", "list")
  attr(output, "hidden") <- c("data", "predictors_p", "predictors_k", "kFixed",
    "betahat_p", "betahat_k", "cellMM_p", "cellMM_k", "nbeta_p", "nbeta_k",
    "varbeta", "levels_p", "levels_k", "carcCells", "AIC", "cells",
    "ncell", "observations", "loglik", "pOnly")
  return(output)
} # pkm

#' @title Print a \code{\link{pkm}} model object
#'
#' @description Print a \code{\link{pkm}} model object
#'
#' @param x a \code{\link{pkm}} model object
#'
#' @param ... to be passed down
#'
#' @export
#'
print.pkm <- function(x, ...){
  hid <- attr(x, "hidden")
  notHid <- !names(x) %in% hid
  print(x[notHid])
}
 
#' @title Calculate the negative log-likelihood of a searcher efficiency model
#' 
#' @description The function used to calculate the negative-loglikelihood of
#'   a given searcher efficiency model (\code{\link{pkm}}) with a given data
#'   set
#'
#' @param misses Number of searches when carcass was present but
#'  not found.
#'
#' @param foundOn Search on which carcass was found.
#'
#' @param beta Parameters to be optimized.
#'
#' @param nbeta_p Number of parameters associated with p.
#'
#' @param cellByCarc Which cell each observation belongs to.
#'
#' @param maxmisses Maximum possible number of misses for a carcass.
#'
#' @param cellMM Combined pk model matrix.
#'
#' @param kFixed Value of k if fixed. 
#'
#' @return Negative log likelihood of the observations, given the parameters.
#'
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
  powk <- rowCumprods(powk)

  pmiss <- matrix(1 - (p * powk[ , 1:(maxmisses + 1)]), nrow = ncell)
  pmiss <- rowCumprods(pmiss)
  pfind <- rowDiffs(1 - pmiss)
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

#' @title Run a set of pkm models based on predictor inputs
#'
#' @description Run a set of \code{\link{pkm}} models based on all possible 
#'   models, given the predictor inputs. \code{pkmSet}'s inputs generally
#'   follow \code{\link{pkm}}, with all simpler models being run for all 
#'   included distributions and returned as a list of model objects
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
#' @param quiet Logical indicator of whether or not to print messsages
#'
#' @return \code{pkmSet} returns a list of objects, each of class 
#'   "\code{pkm}", which each then a list whose components characterize the 
#'   fit of the specific model.
#'
#' @examples
#'   data(wind_RP)
#'   pkmSet(formula_p = p ~ Season, formula_k = k ~ Season, data = wind_RP$SE)
#'
#' @export 
#'
pkmSet <- function(formula_p, formula_k = NULL, data, obsCol = NULL, 
                   kFixed = NULL, kInit = 0.7, CL = 0.95, quiet = FALSE){

  if (length(kFixed) == 1 & length(formula_k) > 0 & quiet == FALSE){
    message("Formula and fixed value provided for k, fixed value used.")
    formula_k <- NULL
  }
  if (length(which(obsCol %in% colnames(data))) == 1){
    if (length(formula_k) > 0 & quiet == FALSE){
      message("Only one observation, k not estimated.")
    }
    if (length(kFixed) == 1 & quiet == FALSE){
      message("Only one observation, kFixed input ignored.")
    }
    formula_k <- NULL
    kFixed <- NULL
  }
  unfixk <- FALSE
  if (length(formula_k) == 0){
    if (length(kFixed) == 0){
      kFixed <- 0.5
      unfixk <- TRUE
    }
  }

  # create the set of models to explore, based on the input parameters
  terms_p <- attr(terms(formula_p), "term.labels")
  if (length(formula_k) == 0){
    terms_k <- NULL
  }else{
    terms_k <- attr(terms(formula_k), "term.labels")
  }
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
  if (unfixk == TRUE){
    kFixed <- NULL
  }
  nmod <- nrow(expandedKeptFormulae) 
  output <- vector("list", nmod)
  for (modi in 1:nmod){
    formi_p <- keptFormula_p[modi][[1]]
    formi_k <- keptFormula_k[modi][[1]]
    pkm_i <- tryCatch(
       pkm(formula_p = formi_p, formula_k = formi_k, data = data,
       obsCol = obsCol, kFixed = kFixed, kInit = kInit, CL = CL, quiet = quiet),
       error = function(x) {
         paste("Failed model fit: ", geterrmessage(), sep = "")
       }
     )
    name_p <- paste(format(formi_p), collapse = "")
    name_p <- gsub("    ", "", name_p)
    name_k <- paste(format(formi_k), collapse = "")
    name_k <- gsub("    ", "", name_k)
    if(length(kFixed) == 1){
      name_k <- paste("k fixed at ", kFixed, sep = "")
    }
    modName <- paste(name_p, "; ", name_k, sep = "")
    modName <- gsub("NULL", "k not estimated", modName)
    output[[modi]] <- pkm_i
    names(output)[modi] <- modName
  }
  class(output) <- c("pkmSet", "list")
  return(output)
} # pkmSet


#' @title Fit all possible searcher efficiency models across all size classes
#'
#' @description Run a set of \code{\link{pkmSet}} model set runs based on all 
#'   possible models for a suite of size classes. \code{cpmSetSize}'s inputs
#'   generally follow \code{\link{pkmSet}} and \code{\link{pkm}} but with an 
#'   additional size column input and calculation of the set of cpm models for 
#'   each of the size classes.
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
#' @param quiet Logical indicator of whether or not to print messsages
#'
#' @param sizeclassCol Name of colum in \code{data} where the size classes
#'   are recorded
#'
#' @return \code{pkmSetSize} returns a class-\code{pkmSetSize} list of 
#'   objects, each of which is a class-\code{pkmSet} list of \code{pkm}" 
#'   outputs (each corresponding to the fit of a specific model
#'   within the set of \code{pkm} models fit for the given size class), that
#'   is of length equal to the total number of size classes
#'
#' @examples
#'   data(wind_RP)
#'   mod <- pkmSetSize(formula_p = p ~ Season, formula_k = k ~ Season, 
#'            data = wind_RP$SE, sizeclassCol = "Size"
#'           )
#'
#' @export
#'
pkmSetSize <- function(formula_p, formula_k = NULL, data, obsCol = NULL, 
                       sizeclassCol = NULL, kFixed = NULL, kInit = 0.7, 
                       CL = 0.95, quiet = FALSE){

  if (length(sizeclassCol) == 0){
    out <- vector("list", length = 1)
    names(out) <- "all"
    out[[1]] <- pkmSet(formula_p, formula_k, data, obsCol, kFixed, kInit, 
                  CL, quiet = TRUE
                )
    return(out)
  }
  if ((sizeclassCol %in% colnames(data)) == FALSE){
    stop("sizeclassCol not in data set.")
  }
  nsearch <- length(obsCol)
  ncarc <- nrow(data)
  obsData <- data[ , obsCol]
  obsData <- as.matrix(obsData, ncol = nsearch)

  sizeclassData <- as.character(data[ , sizeclassCol])
  sizeclasses <- unique(sizeclassData)
  nsizeclasses <- length(sizeclasses)

  out <- vector("list", nsizeclasses)
  names(out) <- sizeclasses
  for (sci in 1:nsizeclasses){
    sizeclassMatch <- which(sizeclassData == sizeclasses[sci])
    data_i <- data[sizeclassMatch, ]
    out[[sci]] <- pkmSet(formula_p, formula_k, data_i, obsCol, kFixed, 
                    kInit, CL, quiet
                  )
  }
  class(out) <- c("pkmSetSize", "list")
  return(out)
} #pkmSetSize

#' @title Create the AICc tables for a set of searcher efficiency models
#' 
#' @description Generates model comparison tables based on AICc values for
#'   a set of CP models generated by \code{\link{pkmSet}}
#' 
#' @param pkmset Set of searcher efficiency models fit to the same
#'   observations
#' 
#' @param quiet Logical indicating if messages should be printed
#' 
#' @return AICc table
#' 
#' @examples
#'   data(wind_RP)
#'   mod <- pkmSet(formula_p = p ~ Season, formula_k = k ~ Season, 
#'            data = wind_RP$SE
#'          )
#'  pkmSetAICcTab(mod)
#'
#' @export 
#'
pkmSetAICcTab <- function(pkmset, quiet = FALSE){

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
  } else {
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
  colnames(output) <- c("p Formula", "k Formula", "AICc", "Delta AICc")
  whichAICcNA <- which(is.na(output$AICc))
  whichAICcMax <- which(output$AICc == 1e7)
  if (length(whichAICcNA) > 0 & quiet == FALSE){
    message("Models with incorrect specification were removed from output.")
    output <- output[-whichAICcNA, ]
  }
  if (length(whichAICcMax) > 0 & quiet == FALSE){
    message("Models that failed during fit were removed from output.")
    output <- output[-whichAICcMax, ]
  }
  return(output)  # pkmSetAICcTab
}

#' @title Simulate parameters from a fitted pk model
#'
#' @description Simulate parameters from a \code{\link{pkm}} model object
#'
#' @param n the number of simulation draws
#'
#' @param model A \code{\link{pkm}} object (which is returned from 
#'   \code{pkm()})
#'
#' @param seed optional input to set the seed of the RNG
#'
#' @param kFill what value to fill k with if k was not estimated
#'
#' @return list of two matrices of \code{n} simulated \code{p} and \code{k} 
#'   for cells defined by the \code{model} object. 
#'
#' @examples
#'   data(wind_RP)
#'   mod <- pkm(formula_p = p ~ 1, formula_k = k ~ Season, data = wind_RP$SE)
#'   rpk(n = 10, model = mod)
#'
#' @export
#'
rpk <- function(n = 1, model, kFill = NULL, seed = NULL){

  if (!"pkm" %in% class(model)) stop("model not of class pkm")
  if (anyNA(model$varbeta) || sum(diag(model$varbeta) < 0) > 0){
    stop("Variance in pkm not well-defined. Cannot simulate.")
  }
  if (!model$pOnly && !is.null(kFill) && !is.na(kFill)){
    warning("Model includes k. Ignoring kFill.")
  }
  if (model$pOnly){
    if (is.null(kFill) || is.na(kFill)){
      stop("k not included in 'model' and kFill not provided. ",
        "Cannot simulate pk.")
    }
    if (!is.numeric(kFill[1])){
      stop("kFill must be numeric")
    }
    if (kFill[1] < 0 || kFill[1] > 1){
      stop("kFill must be in [0, 1]")
    }
    if (length(kFill) > 1){
      warning("length(kFill) > 1 and only the first element will be used")
    }
    kFill <- kFill[1]
    betahat_k <- NULL
  } else {
    nbeta_k <- model$nbeta_k
    which_beta_k <- (model$nbeta_p + 1):(model$nbeta_p + nbeta_k)
    kFill <- model$kFixed
    cellMM_k <- model$cellMM_k
    betahat_k <- model$betahat_k
  }
  nbeta_p <- model$nbeta_p 
  cellMM_p <- model$cellMM_p
  ncell <- model$ncell
  cellNames <- model$cells[ , "CellNames"]

  meanbeta <- c(model$betahat_p, betahat_k)
  varbeta <- model$varbeta
  method <-  "svd"

  if (length(seed) > 0 && !is.na(seed[1])){
    set.seed(as.numeric(seed[1]))
  }
  sim_beta <- rmvnorm(n, mean = meanbeta, sigma = varbeta, method =  method)
  sim_p <- as.matrix(alogit(sim_beta[ , 1:nbeta_p] %*% t(cellMM_p)))
  colnames(sim_p) <- cellNames

  if (length(kFill) == 0 || is.na(kFill)){
    sim_k <- as.matrix(alogit(sim_beta[ , which_beta_k] %*% t(cellMM_k)))
  } else {
    sim_k <- matrix(kFill, ncol = ncell, nrow = n)
  }
  colnames(sim_k) <- cellNames

  output0 <- lapply(cellNames, function(x) cbind(sim_p[, x], sim_k[, x]))
  names(output0) <- cellNames

  return(output)
}

#' @title Suggest a k
#'
#' @description Suggest a value for k based on a weighted mean of the 
#'   observed decay
#'
#' @param obsData a matrix of carcasses (rows) x searched (columns)
#'
#' @return the weighted mean of the observed proportional decay in p
#'
#' @export
#'
kSuggest <- function(obsData){
  nsearch <- ncol(obsData)
  if (nsearch == 1){
    message("only one observation, k not informed by data")
    return(numeric(0))
  }
  navail <- apply(obsData, 2, trueLength)
  nfound <- apply(obsData, 2, sum, na.rm = TRUE) 
  pfound <- nfound / navail
  pfoundRatios <- rep(NA, nsearch - 1)
  for (searchi in 1:(nsearch - 1)){
    pfoundRatios[searchi] <- pfound[searchi + 1]/pfound[searchi]
  }
  suggestion <- round(weighted.mean(pfoundRatios, navail[2:nsearch]), 3)
  return(suggestion)
}

#' @title Check if a pk model is well-fit
#'
#' @description Run a check the arg is a well-fit pkm object
#'
#' @param pkmod A \code{\link{pkm}} object to test
#'
#' @return logical value indicating a failed fit (TRUE) or successful (FALSE)
#'
#' @export
#'
pkmFail <- function(pkmod){
  out <- !("pkm" %in% class(pkmod)) ||
         anyNA(pkmod) ||
         sum(diag(pkmod$varbeta) < 0) > 0
  return(out)
}


#' @title Check if pkm models fail
#' 
#' @description Run a check on each model within a \code{\link{pkmSet}} object
#'   to determine if it failed or not
#'
#' @param pkmSetToCheck A \code{\link{pkmSet}} object to test
#'
#' @return A vector of logical values indicating if each of the models failed
#'
#' @export
#'
pkmSetFail <- function(pkmSetToCheck){
  nmodsInSet <- length(pkmSetToCheck)
  out <- logical(nmodsInSet)
  names(out) <- names(pkmSetToCheck)
  for (modi in 1:nmodsInSet){
    out[modi] <- pkmFail(pkmSetToCheck[[modi]])
  }
  return(out)
}

#' @title Check if all of the pkm models fail
#'
#' @description Run a check on each model within a \code{\link{pkmSetSize}}
#'   object to determine if they all failed or not
#'
#' @param pkmSetSizeToCheck A \code{\link{pkmSetSize}} object to test
#'
#' @return A list of logical vectors indicating which models failed
#'
#' @export
#'
pkmSetSizeFail <- function(pkmSetSizeToCheck){
  out <- lapply(pkmSetSizeToCheck, pkmSetFail)
  return(out)



#' @title Remove failed pkm models from a \code{\link{pkmSet}} object
#'
#' @description Remove all failed models within a \code{\link{pkmSet}} object
#'
#' @param pkmSetToTidy A \code{\link{pkmSet}} object to tidy
#'
#' @return A \code{\link{pkmSet}} object with failed models removed
#'
#' @export
#'
pkmSetFailRemove <- function(pkmSetToTidy){

  nmodsInSet <- length(pkmSetToTidy)
  fails <- pkmSetFail(pkmSetToTidy)
  pass <- fails == FALSE
  npasses <- sum(pass)
  passes <- which(pass)
  out <- vector("list", length = npasses)
  names(out) <- names(pkmSetToTidy[passes])
  for (passi in 1:npasses){
    out[[passi]] <- pkmSetToTidy[[passes[passi]]]
  }
  class(out) <- c("pkmSet", "list")
  return(out)
}

#' @title Remove failed pkm models from a \code{\link{pkmSetSize}} object
#'
#' @description Remove failed models from a \code{\link{pkmSetSize}} object
#'
#' @param pkmSetSizeToTidy A list of \code{\link{pkmSetSize}} objects to tidy
#'
#' @return A list of \code{\link{pkmSet}} objects with failed models removed
#'
#' @export
#'
pkmSetSizeFailRemove <- function(pkmSetSizeToTidy){

  out <- list()
  for (sci in names(pkmSetSizeToTidy)){
    out[[sci]] <- pkmSetFailRemove(pkmSetSizeToTidy[[sci]])
  }
  return(out)
}

#' @title Return the model with the greatest log-likelihood
#'
#' @description  Compares all fitted models in a list and returns the model
#'  with the greatest log-likelihood
#'
#' @param modelSet a list of fitted models with a \code{loglik} element. Models
#'  may be \code{pkm}, \code{cpm}, \code{survreg} objects or any objects with a
#'  \code{loglik} component.
#'
#' @return The model object with the greatest log-likelihood among
#'  the models in \code{modelSet}
#'
#' @export
#'
fullMod <- function(modelSet){
  llvec <- sapply(modelSet, "[[", "loglik")
  out <- modelSet[[which(llvec == max(llvec))]]
  return(out)
}
#' @title Calculate decayed searcher efficiency
#'
#' @description Calculate searcher efficiency after some searches under 
#'   pk values
#'
#' @param days search days
#'
#' @param pk \code{p} and \code{k} values
#'
#' @return searcher efficiency that matches the output of ppersist
#'
#' @export 
#'
SEsi <- function(days, pk){ 
  if (is.null(dim(pk)) || nrow(pk) == 1) return (SEsi0(days, pk))
  npk <- nrow(pk)
  nsearch <- length(days) - 1
  ind1 <- rep(1:nsearch, times = nsearch:1)
  ind2 <- ind1 + 1
  ind3 <- unlist(lapply(1:nsearch, function(x) x:nsearch)) + 1
  schedule <- cbind(days[ind1], days[ind2], days[ind3])
  schedule.index <- cbind(ind1, ind2, ind3)
  nmiss <- schedule.index[, 3] - schedule.index[, 2]
  maxmiss <- max(nmiss)
  if (maxmiss == 0) {
      pfind.si <- pk[, 1]
  } else if (maxmiss == 1) {
      pfind.si <- cbind(pk[, 1], (1 - pk[, 1]) * pk[, 2] * pk[, 1])
  } else {
      powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(npk, maxmiss + 1))
      powk[ , 1] <- 1
      powk <- rowCumprods(powk)
      pfind.si <- pk[, 1] * powk * cbind(
        rep(1, npk), rowCumprods(1 - (pk[, 1] * powk[, 1:maxmiss]))
      )
  }
  return(t(pfind.si)) 
}

#' @title Calculate decayed searcher efficiency for a single pk
#'
#' @description Calculate searcher efficiency after some searches for a single 
#'   pk combination
#'
#' @param days search days
#'
#' @param pk pk combination
#'
#' @return searcher efficiency that matches the output of ppersist
#'
#' @export 
#'
SEsi0 <- function(days, pk){ 
  nsearch <- length(days) - 1
  ind1 <- rep(1:nsearch, times = nsearch:1)
  ind2 <- ind1 + 1
  ind3 <- unlist(lapply(1:nsearch, function(x) x:nsearch)) + 1
  schedule <- cbind(days[ind1], days[ind2], days[ind3])
  schedule.index <- cbind(ind1, ind2, ind3)
  nmiss <- schedule.index[, 3] - schedule.index[, 2]
  maxmiss <- max(nmiss)
  if (maxmiss == 0) {
    pfind.si <- pk[1]
  } else if (maxmiss == 1) {
    pfind.si <- c(pk[1], (1 - pk[1]) * pk[2] * pk[1])
  } else {
    powk <- rep(pk[2], maxmiss + 1)
    powk[1] <- 1
    powk <- cumprod(powk)
    pfind.si <- pk[1] * powk * c(1, cumprod(1 - (pk[1] * powk[1:maxmiss])))
  }
  return(pfind.si)
}
