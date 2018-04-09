#' Fit a single carcass persistence model.
#' 
#' Carcass persistence is modeled as survival function where the one or both  
#'   parameter(s) can depend on any number of covariates. Format and usage 
#'   parallel that of common \code{R} functions such as \code{lm}, \code{glm},
#'   and \code{gam}. However, the input data (\code{data}) are structured 
#'   differently to accommodate the survival model approach (see "Details"),
#'   and model formulas may be entered for both \code{l} ("location") and 
#'   \code{s} ("scale").
#'
#' The probability of a carcass persisting to a particular time is dictated
#'   by the specific distribution chosen and its underlying location (l) and 
#'   scale (s) parameters (for all models except the exponential, which only 
#'   has a location parameter). Both \code{l} and \code{s} may depend on 
#'   covariates such as ground cover, season, species, etc., and a separate 
#'   model format (\code{formula_l} and \code{formula_s}) may be entered for 
#'   each. The models are entered as they would be in the familiar \code{lm} 
#'   or \code{glm} functions in R. For example, \code{l} might vary with 
#'   \code{visibility}, \code{season}, and \code{site}, while \code{s} varies 
#'   only with \code{visibility}. A user might then enter 
#'   \code{l ~ visibility + season + site} for \code{formula_l} and 
#'   \code{s ~ visibility} for \code{formula_s}. Other R conventions for 
#'   defining formulas may also be used, with \code{covar1:covar2} for the 
#'   interaction between covariates 1 and 2 and \code{covar1 * covar2} as 
#'   short-hand for \code{covar1 + covar2 + covar1:covar2}.
#'
#' Carcass persistence \code{data} must be entered in a data frame with data 
#'   in each row giving the fate of a single carcass in the trials. There
#'   must be a column for each of the last time the carcass was observed 
#'   present and the first time the carcass was observed absent (or NA if the
#'   carcass was always present). Additional columns with values for
#'   categorical covariates (e.g., visibility = E, M, or D) may also be 
#'   included.
#'
#' @param formula_l Formula for location; an object of class 
#'  "\code{\link{formula}}" (or one that can be coerced to that class):
#'  a symbolic description of the model to be fitted. Details of model 
#'  specification are given under 'Details'.
#'
#' @param formula_s Formula for scale; an object of class 
#'   "\code{\link{formula}}" (or one that can be coerced to that class):
#'   a symbolic description of the model to be fitted. Details of model 
#'   specification are given under 'Details'.
#'
#' @param data Dataframe with results from carcass persistence trials and any
#'   covariates included in \code{formula_l} or {formula_s} (required).
#'
#' @param left Name of columns in \code{data} where the time of last present
#'   observation is stored.
#'
#' @param right Name of columns in \code{data} where the time of first absent
#'   observation is stored.
#'
#' @param dist Distribution name ("exponential", "weibull", "loglogistic", or 
#'   "lognormal")
#'
#' @param CL confidence level
#'
#' @return \code{cpm} returns an object of class "\code{cpm}", which is a list
#'   whose components characterize the fit of the model. Due to the large 
#'   number and complexity of components, only a subset of them is printed 
#'   automatically; the rest can be viewed/accessed directly via the \code{$}
#'   operator if desired.
#'
#' The following components are displayed automatically:
#'
#' \describe{
#'  \item{\code{call}}{the function call to fit the model}
#'  \item{\code{formula_l}}{the model formula for the \code{p} parameter}
#'  \item{\code{formula_s}}{the model formula for the \code{k} parameter}
#'  \item{\code{distribution}}{distribution used}
#'  \item{\code{predictors}}{list of covariates of \code{l} and/or \code{s}}
#'  \item{\code{cellwiseTable_ls}}{summary statistics for estimated cellwise 
#'    \code{l} and \code{s}, including the medians and upper & lower bounds
#'    on CIs for each parameter, indexed by cell (or combination of
#'    covariate levels).}
#'  \item{\code{cellwiseTable_ab}}{summary statistics for estimated cellwise 
#'    \code{pda} and \code{pdb}, including the medians and upper & lower 
#'    bounds on CIs for each parameter, indexed by cell (or combination of
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
#'   \item{\code{betahat_l}}{parameter estimates for the terms in the 
#'     regression model for for \code{l}}
#'   \item{\code{betahat_s}}{parameter estimates for the terms in the 
#'     regression model for for \code{s}. If dist = "exponential", \code{s} 
#'     is set at 1 and not calculated.}
#'   \item{\code{varbeta}}{the variance-covariance matrix of the estimators
#'     for \code{c(betahat_l, betahat_s}.}
#'   \item{\code{cellMM_l}}{a cellwise model (design) matrix for covariate 
#'     structure of \code{l_formula}}
#'   \item{\code{cellMM_s}}{a cellwise model(design) matrix for covariate 
#'     structure of \code{s_formula}}
#'   \item{\code{levels_l}}{all levels of each covariate of \code{l}}
#'   \item{\code{levels_s}}{all levels of each covariate of \code{s}}
#'   \item{\code{nbeta_l}}{number of parameters fit for \code{l}}
#'   \item{\code{nbeta_s}}{number of parameters fit for \code{s}}
#'   \item{\code{cells}}{cell structure of the pk-model, i.e., combinations of
#'     all levels for each covariate of \code{p} and \code{k}. For example, if
#'     \code{covar1} has levels \code{"a"}, \code{"b"}, and \code{"c"}, and
#'     \code{covar2} has levels \code{"X"} and \code{"Y"}, then the cells 
#'     would consist of \code{a.X}, \code{a.Y}, \code{b.X}, \code{b.Y}, 
#'     \code{c.X}, and \code{c.Y}.}
#'  \item{\code{ncell}}{total number of cells}
#'  \item{\code{predictors_l}}{list of covariates of \code{l}}
#'  \item{\code{predictors_s}}{list of covariates of \code{s}}
#'  \item{\code{observations}}{observations used to fit the model}
#'  \item{\code{carcCells}}{the cell to which each carcass belongs}
#'  \item{\code{AIC}}{the 
#'    \href{https://en.wikipedia.org/wiki/Akaike_information_criterion}{AIC}
#'    value for the fitted model}
#'  \item{\code{CL}}{the input \code{CL}}
#'}
#'
#' @export
#'
cpm <- function(formula_l, formula_s = NULL, data = NULL, left = NULL,
                right = NULL, dist = "weibull", CL = 0.9){

  if (length(formula_s) != 0 & dist == "exponential"){
    msg <- paste("Formula given for scale, but exponential distribution ",
             "chosen, which does have a scale parameter. Formula ignored.", 
             sep = "")
    message(msg)
    formula_s <- formula(s ~ 1)
  } 
  if (length(formula_s) == 0){
    if (dist != "exponential"){
      message("No formula given for scale, intercept-only model used.")
    }
    formula_s <- formula(s ~ 1)
  } 

  formulaRHS_l <- formula(delete.response(terms(formula_l)))
  preds_l <- all.vars(formulaRHS_l)
  levels_l <- .getXlevels(terms(formulaRHS_l), data)
  formulaRHS_s <- formula(delete.response(terms(formula_s)))
  preds_s <- all.vars(formulaRHS_s)
  predCheck <- c(preds_l, preds_s)
  levels_s <- .getXlevels(terms(formulaRHS_s), data)

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
  if (sum(predCheck %in% colnames(data)) != length(predCheck)){
    stop("Predictor(s) in formula(e) not found in data.")
  }

  preds <- unique(c(preds_l, preds_s) )
  cells <- combinePreds(preds, data)
  ncell <- nrow(cells)
  cellNames <- cells$CellNames

  dataMM_l <- model.matrix(formulaRHS_l, data)
  dataMM_s <- model.matrix(formulaRHS_s, data)
  dataMM <- t(cbind(dataMM_l, dataMM_s))
  cellMM_l <- model.matrix(formulaRHS_l, cells)
  cellMM_s <- model.matrix(formulaRHS_s, cells)
  cellMM <- cbind(cellMM_l, cellMM_s)

  nbeta_l <- ncol(dataMM_l)
  nbeta_s <- ncol(dataMM_s)
  nbeta <- nbeta_l + nbeta_s

  ncarc <- nrow(data)
  cellByCarc <- numeric(ncarc)
  for (celli in 1:ncell){
    groupPattern <- cellMM[celli, ]
    matchingMatrix <- dataMM == groupPattern
    matchingParts <- apply(matchingMatrix, 2, sum)
    matchingTotal <- matchingParts == ncol(cellMM)
    cellByCarc[matchingTotal] <- celli
  }
  carcCells <- cellNames[cellByCarc]

  t1 <- data[ , left]
  t2 <- data[ , right]
  event <- rep(3, length(t1))
  event[is.na(t2) | is.infinite(t2)] <- 0
  event[round(t1, 3) == round(t2, 3)] <- 1
  t1 <- pmax(t1, 0.0001)
  tevent <- survival::Surv(time = t1, time2 = t2, event = event, 
              type = "interval")
  init_formRHS <- as.character(formulaRHS_l)[-1]
  init_form <- reformulate(init_formRHS, response = "tevent")
  init_mod <- survival::survreg(formula = init_form, data = data, dist = dist)
  init_l <- init_mod$coef
  names(init_l) <- paste("l_", names(init_l), sep = "")
  init_s <- rep(init_mod$scale, nbeta_s)
  names(init_s) <- paste("s_", colnames(cellMM_s), sep = "")
  betaInit <- c(init_l, init_s)

  MLE <- tryCatch(
           optim(par = betaInit, fn = cpLogLik, method = "BFGS", 
             t1 = t1, t2 = t2, cellMM = cellMM, dist = dist, hessian = TRUE,
             nbeta_l = nbeta_l, cellByCarc = cellByCarc, dataMM = dataMM, 
             control = list(maxit = 5000)
           ), error = function(x) {NA}
         )
  betahat <- MLE$par
  convergence <- MLE$convergence
  betaHessian <- MLE$hessian
  if (dist == "exponential"){
    which_s <- (nbeta_l + 1):nbeta
    betaHessian <- betaHessian[-which_s, -which_s]
  }
  llik <- MLE$value  

  nparam <- length(betahat)  
  if (dist == "exponential"){
    nparam <- length(betahat) - 1
  }
  AIC <- 2 * llik + 2 * nparam
  AICcOffset <- (2 * nparam * (nparam + 1)) / (ncarc - nparam - 1)
  AICc <- round(AIC + AICcOffset, 3)

  betahat_l <- betahat[1:nbeta_l]
  names(betahat_l) <- colnames(dataMM_l)
  betahat_s <- betahat[(nbeta_l + 1):(nbeta)]
  names(betahat_s) <- colnames(dataMM_s)

  varbeta <- tryCatch(solve(betaHessian), error = function(x) {NA})
  if (is.na(varbeta)[1]){
    stop("Model generates unstable variance estimate.")
  }
  varbeta_l <- varbeta[1:nbeta_l, 1:nbeta_l]
  cellMean_l <- cellMM_l %*% betahat_l
  cellVar_l <- cellMM_l %*% varbeta_l %*% t(cellMM_l)
  cellSD_l <- sqrt(diag(cellVar_l))

  if (dist == "exponential"){
    cellMean_s <- 1
    cellSD_s <- 0
  } else {
    which_s <- (nbeta_l + 1):(nbeta)
    varbeta_s <- varbeta[which_s, which_s]
    cellMean_s <- cellMM_s %*% betahat_s
    cellVar_s <- cellMM_s %*% varbeta_s %*% t(cellMM_s)
    cellSD_s <- sqrt(diag(cellVar_s))
  }

  probs <- data.frame(c(0.5, (1 - CL) / 2, 1 - (1 - CL) / 2))
  cellTable_l <- apply(probs, 1, qnorm, mean = cellMean_l, sd = cellSD_l)
  cellTable_l <- round(matrix(cellTable_l, nrow = ncell, ncol = 3), 5)
  colnames(cellTable_l) <- c("l_median", "l_lower", "l_upper")
  cellTable_s <- apply(probs, 1, qnorm, mean = cellMean_s, sd = cellSD_s)
  cellTable_s <- round(matrix(cellTable_s, nrow = ncell, ncol = 3), 5)
  colnames(cellTable_s) <- c("s_median", "s_lower", "s_upper")
  cellTable_ls <- data.frame(cell = cellNames, cellTable_l, cellTable_s)

  if (dist == "exponential"){
    cellTable_a <- matrix("-", nrow = ncell, ncol = 3)
    colnames(cellTable_a) <- c("pda_median", "pda_lower", "pda_upper")
    cellTable_b <- round(exp(cellTable_l), 5)
    colnames(cellTable_b) <- c("pdb_median", "pdb_lower", "pdb_upper")
  }
  if (dist == "weibull"){
    cellTable_a <- round(1 / cellTable_s, 5)
    colnames(cellTable_a) <- c("pda_median", "pda_lower", "pda_upper")
    cellTable_b <- round(exp(cellTable_l), 5)
    colnames(cellTable_b) <- c("pdb_median", "pdb_lower", "pdb_upper")
  }
  if (dist == "lognormal"){
    cellTable_a <- round(cellTable_s^2, 5)
    colnames(cellTable_a) <- c("pda_median", "pda_lower", "pda_upper")
    cellTable_b <- round(cellTable_l, 5)
    colnames(cellTable_b) <- c("pdb_median", "pdb_lower", "pdb_upper")
  }
  if (dist == "loglogistic"){
    cellTable_a <- round(1 / cellTable_s, 5)
    colnames(cellTable_a) <- c("pda_median", "pda_lower", "pda_upper")
    cellTable_b <- round(exp(cellTable_l), 5)
    colnames(cellTable_b) <- c("pdb_median", "pdb_lower", "pdb_upper")
  }  
  cellTable_ab <- data.frame(cell = cellNames, cellTable_a, cellTable_b)


  if (dist == "exponential"){
    nbeta_s <- 0
  }
  output <- list()
  output$call <- match.call()
  output$formula_l <- formula_l
  output$formula_s <- formula_s
  output$distribution <- dist
  output$predictors <- preds 
  output$predictors_l <- preds_l
  output$predictors_s <- preds_s
  output$AIC <- AIC
  output$AICc <- AICc
  output$convergence <- convergence
  output$varbeta <- varbeta
  output$cellMM_l <- cellMM_l
  output$cellMM_s <- cellMM_s
  output$nbeta_l <- nbeta_l  
  output$nbeta_s <- nbeta_s
  output$betahat_l <- betahat_l
  output$betahat_s <- betahat_s
  output$levels_l <- levels_l
  output$levels_s <- levels_s
  output$cells <- cells
  output$ncell <- ncell
  output$cellwiseTable_ls <- cellTable_ls
  output$cellwiseTable_ab <- cellTable_ab
  output$observations <- data[ , c(left, right)]
  output$carcCells <- carcCells
  output$CL <- CL
  class(output) <- c("cpm", "list")
  attr(output, "hidden") <- c("predictors_l", "predictors_s", 
                              "betahat_l", "betahat_s", "cellMM_l", 
                              "cellMM_s", "nbeta_l", "nbeta_s", "varbeta",
                              "levels_l", "levels_s", "carcCells", "CL", 
                              "AIC", "cells", "ncell", "observations"
                            )
  return(output)
}

#' @export
#'
print.cpm <- function(model){
  hid <- attr(model, "hidden")
  notHid <- !names(model) %in% hid
  print(model[notHid])
}
 
#' Calculate the negative log-likelihood of a carcass persistence model.
#' 
#' @param t1 last times observed present
#' @param t2 first times observed absent
#' @param beta Parameters to be optimized.
#' @param nbeta_l Number of parameters associated with l.
#' @param cellByCarc Which cell each observation belongs to.
#' @param cellMM Combined model matrix.
#' @param dataMM Combined model matrix expanded to the data.
#' @param dist Name of distribution. 
#' @return Negative log likelihood of the observations, given the parameters.
#' @examples
#' NA
#' @export 
#'
cpLogLik <- function(t1, t2, beta, nbeta_l, cellByCarc, cellMM, dataMM, dist){
  t2[which(is.na(t2))] <- Inf
  ncell <- nrow(cellMM)
  nbeta <- length(beta)
  nbeta_s <- nbeta - nbeta_l
  which_l <- 1:nbeta_l
  which_s <- (nbeta_l + 1):nbeta
  beta_l <- beta[which_l]
  beta_s <- beta[which_s]
  if (dist == "exponential"){
    beta_s <- c(1, rep(0, nbeta_s - 1))
  } 
  dataMM_l <- matrix(dataMM[which_l, ], ncol = nbeta_l, byrow = TRUE)
  dataMM_s <- matrix(dataMM[which_s, ], ncol = nbeta_s, byrow = TRUE)
  Beta_l <- dataMM_l %*% beta_l 
  Beta_s <- dataMM_s %*% beta_s  
  psurv_t1 <- survival::psurvreg(t1, Beta_l, Beta_s, dist)
  psurv_t2 <- survival::psurvreg(t2, Beta_l, Beta_s, dist)
  psurv_t2[which(is.na(psurv_t2))] <- 1
  lik <- psurv_t2 - psurv_t1
  too_small <- t1 + .Machine$double.eps >= t2
  lik[too_small] <- survival::dsurvreg(t2, Beta_l, Beta_s, dist)
  lik <- pmax(lik, .Machine$double.eps) 
  nll <- -sum(log(lik))
  return(nll)
}


#' Simulate parameters from a fitted cp model.
#'
#' @param n the number of simulation draws
#'
#' @param model A \code{\link{ckm}} object (which is returned from 
#'   \code{cpm()})
#'
#' @param type The type of parameters requested. \code{"survreg"} or 
#'   \code{"ppersist"}
#'
#' @param seed optional input to set the seed of the RNG
#'
#' @return list of two matrices of \code{n} simulated \code{l} and \code{s} 
#'   (if \code{type = "survreg"}) or \code{a} and \code{b} (if \code{type = 
#'   "ppersist"})for cells defined by the \code{model} object. 
#'
#' @examples
#'  NA
#' @export
#'
rcp <- function(n = 1, model, seed = NULL, type = "survreg"){

  if (!"cpm" %in% class(model)){
    stop("model not of class cpm.")
  }
  if (!type %in% c("survreg", "ppersist")){
    stop(paste("type ", type, " is not supported.", sep = ""))
  }
  dist <- model$dist
  nbeta_l <- model$nbeta_l 
  nbeta_s <- model$nbeta_s
  if (dist == "exponential"){
    nbeta_s <- 1
  } 
  which_beta_s <- (nbeta_l + 1):(nbeta_l + nbeta_s)
  cellMM_l <- model$cellMM_l
  cellMM_s <- model$cellMM_s
  ncell <- model$ncell
  cellNames <- model$cells[ , "CellNames"]
  meanbeta <- c(model$betahat_l, model$betahat_s)
  if (dist == "exponential"){
    varbeta <- rbind(cbind(model$varbeta, 0), 0)
  } else{
    varbeta <- model$varbeta
  }
  method <-  "svd"

  if (length(seed) > 0){
    set.seed(seed)
  }
  sim_beta <- mvtnorm::rmvnorm(n, mean = meanbeta, sigma = varbeta, method)

  sim_l <- as.matrix(sim_beta[ , 1:nbeta_l] %*% t(cellMM_l))
  sim_s <- as.matrix(sim_beta[ , which_beta_s] %*% t(cellMM_s))

  if (type == "ppersist"){
    if (dist == "exponential"){
      sim_a <- matrix(NA, nrow = n, ncol = ncell)
      sim_b <- exp(sim_l)
    }
    if (dist == "weibull"){
      sim_a <- 1 / sim_s
      sim_b <- exp(sim_l)
    }
    if (dist == "lognormal"){
      sim_a <- sim_s^2
      sim_b <- sim_l
    }
    if (dist == "loglogistic"){
      sim_a <- 1 / sim_s
      sim_b <- exp(sim_l)
    } 
    sim_p1 <- sim_a
    sim_p2 <- sim_b
  } else{
    sim_p1 <- sim_l
    sim_p2 <- sim_s    
  }

  colnames(sim_p1) <- cellNames
  colnames(sim_p2) <- cellNames


  paramNames <- switch(type, 
                  "survreg" = c("l", "s"), "ppersist" = c("pda", "pdb"))

  output <- vector("list", ncell)
  names(output) <- cellNames
  for (celli in 1:ncell){
    cellp12 <- cbind(sim_p1[ , celli], sim_p2[ , celli])
    colnames(cellp12) <- paramNames
    output[[celli]] <-  cellp12
  }

  return(output)
}


#' Run a set of cpm models based on predictor inputs
#'
#' Function inputs generally follow \code{cpm}, with all simpler models being 
#'   run for all included distributions and returned as a list of model 
#'   objects
#'
#' @param formula_l Formula for location; an object of class 
#'  "\code{\link{formula}}" (or one that can be coerced to that class):
#'  a symbolic description of the model to be fitted. Details of model 
#'  specification are given under 'Details'.
#'
#' @param formula_s Formula for scale; an object of class 
#'   "\code{\link{formula}}" (or one that can be coerced to that class):
#'   a symbolic description of the model to be fitted. Details of model 
#'   specification are given under 'Details'.
#'
#' @param data Dataframe with results from carcass persistence trials and any
#'   covariates included in \code{formula_l} or {formula_s} (required).
#'
#' @param left Name of columns in \code{data} where the time of last present
#'   observation is stored.
#'
#' @param right Name of columns in \code{data} where the time of first absent
#'   observation is stored.
#'
#' @param dists Names of the distributions (from "exponential", "weibull", 
#'   "loglogistic", and "lognormal") that are to be included
#'
#' @param CL confidence level
#'
#' @return \code{cpmSet} returns a class-\code{cpmSet} list of objects, each
#'   of class "\code{cpm}", which each then a list whose components 
#'   characterize the fit of the specific model.
#'
cpmSet <- function(formula_l, formula_s = NULL, data, left = NULL, 
                   right = NULL, dists = c("exponential", "weibull",
                   "lognormal", "loglogistic"), CL = 0.9){

  if (length(formula_s) == 0){
    formula_s <- formula(s ~ 1)
  }

  terms_l <- attr(terms(formula_l), "term.labels")
  terms_s <- attr(terms(formula_s), "term.labels")
  nterms_l <- length(terms_l)
  nterms_s <- length(terms_s)
  nformula_l <- 2^(nterms_l)
  nformula_s <- 2^(nterms_s)

  dropComplex_l <- rep(1:nterms_l, choose(nterms_l, 1:nterms_l))
  dropWhich_l <- numeric(0)
  if (nterms_l > 0){
    for (termi in 1:nterms_l){
      specificDrop <- seq(1, choose(nterms_l, (1:nterms_l)[termi]))
      dropWhich_l <- c(dropWhich_l, specificDrop)
    }
  }
  optionFormula_l <- vector("list", nformula_l)
  optionFormula_l[[1]] <- formula_l
  keepFormula_l <- rep(TRUE, nformula_l)
  if (nformula_l > 1){
    for (formi in 2:nformula_l){
      termDropComplex <- combn(terms_l, dropComplex_l[formi - 1])
      termDropSpec <- termDropComplex[ , dropWhich_l[formi - 1]]
      termDrop <- paste(termDropSpec, collapse = " - ")
      formulaUpdate <- paste(format(~.), "-", termDrop)
      updatedFormula <- update.formula(formula_l, formulaUpdate)
      optionFormula_l[[formi]] <- updatedFormula
      keepFormula_l[formi] <- checkComponents(updatedFormula)
    }
    nkeepFormula_l <- sum(keepFormula_l)
    whichKeepFormula_l <- which(keepFormula_l == TRUE)
    keptFormula_l <- vector("list", nkeepFormula_l)
    for (kepti in 1:nkeepFormula_l){
      keptFormula_l[[kepti]] <- optionFormula_l[[whichKeepFormula_l[kepti]]]
    }
  }else{
    keptFormula_l <- optionFormula_l
  }
  
  dropComplex_s <- rep(1:nterms_s, choose(nterms_s, 1:nterms_s))
  dropWhich_s <- numeric(0)
  if (nterms_s > 0){
    for (termi in 1:nterms_s){
      specificDrop <- seq(1, choose(nterms_s, (1:nterms_s)[termi]))
      dropWhich_s <- c(dropWhich_s, specificDrop)
    }
  }
  optionFormula_s <- vector("list", nformula_s)
  optionFormula_s[[1]] <- formula_s
  keepFormula_s <- rep(TRUE, nformula_s)
  if (nformula_s > 1){
    for (formi in 2:nformula_s){
      termDropComplex <- combn(terms_s, dropComplex_s[formi - 1])
      termDropSpec <- termDropComplex[ , dropWhich_s[formi - 1]]
      termDrop <- paste(termDropSpec, collapse = " - ")
      formulaUpdate <- paste(format(~.), "-", termDrop)
      updatedFormula <- update.formula(formula_s, formulaUpdate)
      optionFormula_s[[formi]] <- updatedFormula
      keepFormula_s[formi] <- checkComponents(updatedFormula)
    }
    nkeepFormula_s <- sum(keepFormula_s)
    whichKeepFormula_s <- which(keepFormula_s == TRUE)
    keptFormula_s <- vector("list", nkeepFormula_s)
    for (kepti in 1:nkeepFormula_s){
      keptFormula_s[[kepti]] <- optionFormula_s[[whichKeepFormula_s[kepti]]]
    }
  }else{
    keptFormula_s <- optionFormula_s
  }

  expandedKeptFormulae <- expand.grid(keptFormula_l, keptFormula_s, dists)
  keptFormula_l <- expandedKeptFormulae[ , 1]
  keptFormula_s <- expandedKeptFormulae[ , 2]
  dists <- as.character(expandedKeptFormulae[ , 3])
  nmod <- nrow(expandedKeptFormulae) 
  preoutput <- vector("list", nmod)
  for (modi in 1:nmod){
    formi_l <- keptFormula_l[modi][[1]]
    formi_s <- keptFormula_s[modi][[1]]
    disti <- dists[modi]
    if (disti == "exponential"){
      formi_s <- NULL
    }
    cpm_i <- tryCatch(
               cpm(formi_l, formi_s, data, left, right, disti, CL), 
               error = function(x) {"Failed model fit"}
             )
    name_d <- disti
    name_l <- paste(format(formi_l), collapse = "")
    name_l <- gsub("    ", "", name_l)
    name_s <- paste(format(formi_s), collapse = "")
    name_s <- gsub("    ", "", name_s)
    modName <- paste("dist: ", name_d, "; ", name_l, "; ", name_s, sep = "")

    preoutput[[modi]] <- cpm_i
    names(preoutput)[modi] <- modName
  }
  uniqueMods <- unique(names(preoutput))
  nuniqueMods <- length(uniqueMods)
  output <- vector("list", nuniqueMods)
  names(output) <- uniqueMods
  for (modi in 1:nuniqueMods){
    output[[modi]] <- preoutput[[uniqueMods[modi][1]]]
  }
  class(output) <- c("cpmSet", "list")
  return(output)
}


#' Fit all possible carcass persistence models across all size classes.
#'
#' Function inputs generally follow \code{cpmSet} and \code{cpm} but with an 
#'   additional size column input and calculation of the set of cpm models for
#'   each of the size classes
#'
#' @param formula_l Formula for location; an object of class 
#'  "\code{\link{formula}}" (or one that can be coerced to that class):
#'  a symbolic description of the model to be fitted. Details of model 
#'  specification are given under 'Details'.
#'
#' @param formula_s Formula for scale; an object of class 
#'   "\code{\link{formula}}" (or one that can be coerced to that class):
#'   a symbolic description of the model to be fitted. Details of model 
#'   specification are given under 'Details'.
#'
#' @param data Dataframe with results from carcass persistence trials and any
#'   covariates included in \code{formula_l} or {formula_s} (required).
#'
#' @param left Name of columns in \code{data} where the time of last present
#'   observation is stored.
#'
#' @param right Name of columns in \code{data} where the time of first absent
#'   observation is stored.
#'
#' @param dists Names of the distributions (from "exponential", "weibull", 
#'   "loglogistic", and "lognormal") that are to be included
#'
#' @param CL confidence level
#'
#' @param sizeclassCol Name of colum in \code{data} where the size classes
#'  are recorded
#'
#' @return \code{cpmSetSize} returns a class-\code{cpmSetSize} list of 
#'   objects, each of which is a class-\code{cpmSet} list of \code{cpm}" 
#'   outputs (each corresponding to the fit of a specific model
#'   within the set of \code{cpm} models fit for the given size class), that
#'   is of length equal to the total number of size classes
#'
cpmSetSize <- function(formula_l, formula_s = NULL, data, left = NULL, 
                       right = NULL, dists = c("exponential", "weibull", 
                       "lognormal", "loglogistic"), sizeclassCol = NULL, 
                       CL = 0.9){

  if (length(sizeclassCol) == 0){
    message("No size class provided, function run as if pkmSet")
    output <- cpmSet(formula_l, formula_s, data, left, right, dists, CL)
    return(output)
  }

  sizeclassData <- as.character(data[ , sizeclassCol])
  sizeclasses <- unique(sizeclassData)
  nsizeclasses <- length(sizeclasses)

  out <- vector("list", nsizeclasses)
  names(out) <- sizeclasses
  for (sci in 1:nsizeclasses){
    sizeclassMatch <- which(sizeclassData == sizeclasses[sci])
    data_i <- data[sizeclassMatch, ]
    out[[sci]] <- cpmSet(formula_l, formula_s, data_i, left, right, dists, CL) 
  }

  return(out)
}

#' Verify that a suite of carcass persistence models all fit successfully.
#'
#' @param cpmToCheck A \code{cpm} model or a set of them or a suite of sets
#'   associated with multiple sizes
#'
#' @return A single (total) logcal 
#'
#' @export
#'
cpmCheck <- function(cpmToCheck){

  status <- 0
  classSingle <- class(cpmToCheck)
  classSet <- class(cpmToCheck[[1]])
  classSize <- class(cpmToCheck[[1]][[1]]) 
  if ("cpm" %in% classSingle){
    status <- 1
  }
  if ("cpm" %in% classSet){
    ninSet <- length(cpmToCheck)
    checks <- rep(0, ninSet)
    for (modi in 1:ninSet){
      checkClass <- class(cpmToCheck[[modi]])
      if ("cpm" %in% checkClass){
        checks[modi] <- 1
      }
    }
    status <- floor(mean(checks))
  }
  if ("cpm" %in% classSize){
    nsizeclasses <- length(cpmToCheck)
    ninSet <- length(cpmToCheck[[1]])
    checks <- matrix(0, nsizeclasses, ninSet)
    for (sci in 1:nsizeclasses){
      for (modi in 1:ninSet){
        checkClass <- class(cpmToCheck[[sci]][[modi]])
        if ("cpm" %in% checkClass){
          checks[sci, modi] <- 1
        }
      }
    }
    status <- floor(mean(checks))
  }
  output <- as.logical(status)
  return(output)
}


#' Create the  AICc tables for a set of carcass persistence models
#' 
#' @param cpmset Set of carcass persistence models fit to the same
#'   observations
#' @return AICc table
#' @examples
#' NA
#' @export 
#'
cpmSetAICcTab <- function(cpmset){

  nmod <- length(cpmset)
  formulas <- names(cpmset)
  dists <- rep(NA, nmod)
  formulas_l <- rep(NA, nmod)
  formulas_s <- rep(NA, nmod)
  AICc <- rep(NA, nmod)
  deltaAICc <- rep(NA, nmod)

  if (nmod == 1){
    splitFormulas <- strsplit(formulas, "; ")[[1]]
    dists <- strsplit(splitFormulas[1], "dist: ")[[1]][2]
    formulas_l <- splitFormulas[2] 
    formulas_s <- splitFormulas[3]
    AICc <- tryCatch(cpmset[[1]]$AICc, error = function(x) {1e7})
    deltaAICc <- 0    
    AICcOrder <- 1
  }else{
    for (modi in 1:nmod){
      splitFormulas_i <- strsplit(formulas[modi], "; ")[[1]]
      dists[modi] <- strsplit(splitFormulas_i, "dist: ")[[1]][2]
      formulas_l[modi] <- splitFormulas_i[2] 
      formulas_s[modi] <- splitFormulas_i[3]
      AICc[modi] <- tryCatch(cpmset[[modi]]$AICc, error = function(x) {1e7})
    }
    AICcOrder <- order(AICc)
    deltaAICc <- round(AICc - min(AICc), 3)
    which_fails <- which(AICc == 1e7)
    AICc[which_fails] <- NA
    deltaAICc[which_fails] <- NA
  }

  output <- data.frame(dists, formulas_l, formulas_s, AICc, deltaAICc)
  output <- output[AICcOrder, ]
  colnames(output) <- c("dist", "l formula", "s formula", "AICc", 
                        "Delta AICc")
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

#' Calculate the probability of persistence to detection 
#' 
#' @param pda parameter a.
#' @param pdb parameter b.
#' @param dist Distribution used.
#' @param t_arrive0 Beginning of arrival window.
#' @param t_arrive1 End of arrival window.
#' @param t_search Search time.
#' @return Probability of persistence of detection to at t_search, 
#'          given arrival between t_arrive0 and t_arrive1
#' @examples
#' NA
#' @export 
#'
ppersist <- function(pda, pdb, dist, t_arrive0, t_arrive1, t_search){

  dist <- tolower(dist)

  if (dist == "weibull"){

    sa0 <- pgamma(outer(1 / pdb, t_search - t_arrive0)^pda, 1 / pda)
    sa1 <- pgamma(outer(1 / pdb, t_search - t_arrive1)^pda, 1 / pda)
    a1a0 <- outer(pdb, 1 / (t_arrive1 - t_arrive0))
    probs <- (sa0 - sa1) * gamma(1 + 1 / pda) * a1a0
    probs <- t(probs)

  } else if (dist == "exponential"){

    a1a0 <- outer(t_arrive1 - t_arrive0, 1 / pdb)
    a0s <- outer(t_arrive0 - t_search, 1 / pdb)
    a1s <- outer(t_arrive1 - t_search, 1 / pdb)
    probs <- (exp(a1s) - exp(a0s)) / (a1a0)

  } else if (dist == "lognormal"){

    root_pda <- sqrt(pda)
    exp_value <- exp((pda / 2) + pdb)
    tt <- t_search - t_arrive0
    p1 <- pnorm(outer(pdb, -log(tt), "+") / root_pda)
    p2 <- pnorm(outer(-pdb, log(tt), "+") / root_pda - root_pda) * exp_value
    part0 <- t(p1) * tt + t(p2)
    tt <- t_search - t_arrive1
    p1 <- pnorm(outer(pdb, -log(tt), "+") / root_pda)
    p2 <- pnorm(outer(-pdb, log(tt), "+") / root_pda - root_pda) * exp_value
    part1 <- t(p1) * tt + t(p2)
    probs <- -(part1 - part0) / (t_arrive1 - t_arrive0)

  } else if (dist == "loglogistic" | dist == "log-logistic"){
    yox <- function(x, y) y/x
    t1 <- t_search-t_arrive1
    t0 <- t_search-t_arrive0
    tob <- outer(pdb, t1, "yox")
    part1 <- t1/t(1 + tob^pda) * 
        t(gsl::hyperg_2F1(1, 1, 1 + 1/pda, 1/(1 + tob^(-pda))))
    tob <- outer(pdb, t0, "yox")
    part0 <- t0 / t(1 + tob^pda) *
        t(gsl::hyperg_2F1(1, 1, 1 + 1/pda, 1/(1 + tob^(-pda))))
    prob <- (part0 - part1)/(t_arrive1 - t_arrive0)
  }
  return(probs)
}

#' Calculate the probability of persistence to for a loglogistic 
#' 
#' @param pda parameter a.
#' @param pdb parameter b.
#' @param t_arrive0 Beginning of arrival window.
#' @param t_arrive1 End of arrival window.
#' @param t_search Search time.
#' @return Probability of persistence of detection to at t_search, 
#'          given arrival between t_arrive0 and t_arrive1
#' @examples
#' NA
#' @export 
#'
ppersist_loglogistic <- function(t_arrive0, t_arrive1, t_search, pda, pdb){

  t1 <- t_search-t_arrive1 
  t0 <- t_search-t_arrive0
  part1 <- ifelse(t1 == 0, 0, t1 / (1 + (t1 / pdb)^pda) *  
             gsl::hyperg_2F1(1, 1, 1 + 1 / pda, 1 / (1 + (t1 / pdb)^(-pda))))
  part0 <- t0 / (1 + (t0 / pdb)^pda) * gsl::hyperg_2F1(1, 1, 1 + 1 / pda, 
                                         1 / (1 + (t0 / pdb)^(-pda)))
  out <- -(part1 - part0)/(t_arrive1 - t_arrive0)
  return(out)
}

