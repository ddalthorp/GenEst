
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
#' (\code{pformula} and \code{kformula}) may be entered for each. The models 
#' are entered as they would be in the familiar \code{lm} or \code{glm} 
#' functions in R. For example, \code{p} might vary with \code{visibility}, 
#' \code{season}, and \code{site}, while \code{k} varies only with 
#' \code{visibility}. A user might then enter \code{p ~ visibility + season + 
#' site} for \code{pformula} and \code{k ~ visibility} for \code{kformula}. 
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
#' @param pformula Formula for p; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 
#' 'Details'.
#'
#' @param kformula Formula for k; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 
#; 'Details'.
#'
#' @param data Dataframe with results from searcher efficiency trials 
#' (required).
#'
#' @param observation_columns Vector of names of columns in \code{data}
#' where results for each search occasion are stored (optional). If no \code{
#' observation_columns} are provided, \code{pkm} uses as 
#' \code{observation_columns} all columns with names that begin with an
#' \code{'s'} or \code{'S'} and end with a number, e.g., 's1', 's2', 
#'  's3', etc. This option is included as a convenience for the user, but care
#' must be taken that other data are not stored in columns with names matching 
#' that pattern. Alternatively, \code{observation_columns} may be entered as
#' a vector of names, like \code{c('s1', 's2', 's3')}, 
#' \code{paste0('s', 1:3)}, or \code{c('initialSearch', 'anotherSearch', 
#' 'lastSearch')}.
#'
#' @param k Parameter for user-specified \code{k} value (optional). If a
#' value is provided, \code{kformula} is ignored and the model is fit under 
#' the assumption that the \code{k} parameter is fixed and known to be
#' \code{fixk}.
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
#'  \item{\code{stats}}{summary statistics for estimated \code{p} and 
#'    \code{k}, including the medians and upper & lower bounds on 95% CIs for
#'    each parameter, indexed by cell (or combination of covariate levels).}
#'  \item{\code{AIC}}{the 
      \href{https://en.wikipedia.org/wiki/Akaike_information_criterion}{AIC}
#'    value for the fitted model}
#'  \item{\code{AICc}}{the AIC value as corrected for small sample size}
#'  \item{\code{convergence}}{convergence status of the numerical optimization 
#'    to find the maximum likelihood estimates of \code{p} and \code{k}. A value 
#'    of \code{0} indicates that the model was fit successfully. For help in
#'    deciphering other values, see \code{\link{optim}}.}
#' }
#'
#' The following components are not printed automatically but can be accessed
#' via the \code{$} operator:
#' \describe{
#'   \item{\code{pformula}}{the model formula for the \code{p} parameter}
#'   \item{\code{kformula}}{the model formula for the \code{k} parameter}
#'   \item{\code{betahat_p}}{parameter estimates for the terms in the 
#'     regression model for for \code{p} (logit scale)}
#'   \item{\code{betahat_k}}{parameter estimates for the terms in the 
#'     regression model for for \code{k} (logit scale). If \code{k} is fixed 
#'     and known, \code{betahat_k} is not calculated.}
#'   \item{\code{varbeta}}{the variance-covariance matrix of the estimators
#'     for \code{c(betahat_p, betahat_k}.}
#'   \item{\code{miniXp}}{a simplified design matrix for covariate structure 
#'     of \code{pformula}}
#'   \item{\code{miniXk}}{a simplified design matrix for covariate structure 
#'     of \code{kformula}}
#'   \item{\code{plevels}}{all levels of each covariate of \code{p}}
#'   \item{\code{klevels}}{all levels of each covariate of \code{k}}
#'   \item{\code{np, nk}}{number of parameters to fit the \code{p} and 
#'     \code{k} models}
#'   \item{\code{cells}}{cell structure of the pk-model, i.e., combinations of
#'     all levels for each covariate of \code{p} and \code{k}. For example, if
#'     \code{covar1} has levels \code{"a"}, \code{"b"}, and \code{"c"}, and
#'     \code{covar2} has levels \code{"X"} and \code{"Y"}, then the cells 
#'     would consist of \code{a.X}, \code{a.Y}, \code{b.X}, \code{b.Y}, 
#'     \code{c.X}, and \code{c.Y}.}
#'   \item{\code{Ncells}}{total number of cells}
#'}
#'
#' @examples
#' head(pkmdat)
#' pkm(p ~ visibility, k ~ 1, data = pkmdat)
#' pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' pkm(p ~ visibility, k = 0.7, data = pkmdat)
#' @export

pkm <- function(pformula, kformula = NULL, data, observation_columns = NULL, 
                k = NULL){
  ans <- list()
  ans$call <- match.call()
  if (missing(observation_columns))
    observation_columns <- grep("^[sS].*[0-9]$", names(data), value = TRUE)
  zeros <- matrixStats::rowCounts(as.matrix(data[, observation_columns]), value = 0, na.rm = T)
  foundInd <- which(matrixStats::rowCounts(
    as.matrix(data[, observation_columns]),  value = 1, na.rm = T) == 1
  )
  found <- numeric(length(zeros))
  found[foundInd] <- zeros[foundInd] + 1
  if (length(attr(terms(pformula), 'factors')) > 0){
    pformula <- as.formula(reformulate(dimnames(attr(terms(pformula), 'factors'))[[2]]))
  } else {
    pformula <- as.formula(~1)
  }
  if (!missing(kformula) & missing(k)){
    if (length(attr(terms(kformula), 'factors')) > 0){
      kformula <- as.formula(reformulate(dimnames(attr(terms(kformula), 'factors'))[[2]]))
    } else {
      kformula <- as.formula(~1)
    }
  }
  nmp <- dimnames(attr(terms(pformula), 'factors'))[[1]]
  if (!missing(kformula)) {
    nmk <- dimnames(attr(terms(kformula), 'factors'))[[1]]
  } else {
    nmk <- NULL
  }
  predictors <- unique(c(nmp, nmk))
  fct <- combine_factors(predictors = predictors, data = data)
  tmp <- se_model_fit(
    pequation = pformula,
    kequation = kformula,
    factor_combination_table = fct,
    data = data,
#    observations = cbind(zeros = zeros, found  = found),
    zeros = zeros,
    found = found,
    observation_columns = observation_columns,
    init_k_value = 0.7,
    fix_k = ifelse(missing(k), FALSE, TRUE),
    fix_k_value = k
  )
  ans$pformula <- pformula
  if (!missing(k)){
    ans$kformula <- c(fixedk = k)
  } else {
    ans$kformula <- kformula
  }
  ans$betahat_p <- tmp$betaphat
  names(ans$betahat_p) <- dimnames(tmp$miniXp)[[2]]
  if (missing(k)){
    ans$betahat_k <- tmp$betakhat
    names(ans$betahat_k) <- dimnames(tmp$miniXk)[[2]]
  }
  ans$varbeta <- tmp$vartheta
  ans$plevels <- .getXlevels(terms(pformula), data)
  if (missing(k) && is.language(kformula))
    ans$klevels <- .getXlevels(terms(kformula), data)
  ans$miniXp <- tmp$miniXp
  ans$miniXk <- tmp$miniXk
  xp <- ans$miniXp
  bhatp <- ans$betahat_p
  np <- dim(xp)[2]
  xk <- ans$miniXk
  bhatk <- ans$betahat_k
  nk <- dim(xk)[2]
  bpMu <- xp %*% bhatp
  bpVar <- xp %*% ans$varbeta[1:np, 1:np] %*% t(xp)
  pmedian <- alogit(qnorm(0.5, mean = bpMu, sd = sqrt(diag(as.matrix(bpVar)))))
  plwr <- alogit(qnorm(0.025, mean = bpMu, sd = sqrt(diag(as.matrix(bpVar)))))
  pupr <- alogit(qnorm(0.975, mean = bpMu, sd = sqrt(diag(as.matrix(bpVar)))))
  if (missing(k)){
    bkMu <- xk %*% ans$varbeta[(np+1):(np+nk)]
    bkVar <- xk %*% ans$varbeta[(1+np):(np + nk), (1+np):(np + nk)] %*% t(xk)
    kmedian <- alogit(qnorm(0.5, mean = bkMu, sd = sqrt(diag(as.matrix(bkVar)))))
    klwr <- alogit(qnorm(0.025, mean = bkMu, sd = sqrt(diag(as.matrix(bkVar)))))
    kupr <- alogit(qnorm(0.975, mean = bkMu, sd = sqrt(diag(as.matrix(bkVar)))))
  } else {
    kmedian <- rep(ans$kformula, length(pmedian))
    klwr <- kmedian
    kupr <- kmedian
  }
  ans$predictors <- predictors
  ans$stats <- data.frame(
    Cell = fct$CellNames,
    pmedian = pmedian,
    plwr = plwr,
    pupr = pupr,
    kmedian = kmedian,
    klwr = klwr,
    kupr = kupr
  )
  ans$cells <- fct
  ans$Ncells <- dim(ans$stats)[1]
  ans$np <- np
  ans$nk <- nk
  ans$AIC <- tmp$AIC
  ans$AICc <- tmp$AICc
  ans$convergence <- tmp$convergence
  if (!missing(k) & !missing(kformula))
    ans$NOTE <- paste0("Both kformula and fixed k were entered by user. k = ",
      k, " is assumed.")
  class(ans) <- c("pkm", "list")
  attr(ans, "hidden") <- c(
    "pformula", "kformula", "betahat_p", "betahat_k", "varbeta",
    "miniXp", "miniXk", "plevels", "klevels", 'np', 'nk', 'cells', 'Ncells'
  )
  return(ans)
}

#' @export
print.pkm <- function (x) {
  hid <- attr(x, "hidden")
  print(x[!names(x) %in% hid])
}

#' Simulate p and k parameters from a fitted pk model.
#'
#' @param nsim the number of simulation draws
#
#' @param pkmodel A \code{\link{pkm}} object (which is returned from \code{pkm()})
#
#' @return Array of \code{nsim} simulated pairs of \code{p} and \code{k} for
#'  cells defined by the \code{pkmodel} object.
#' @examples
#' pkmod1 <- pkm(p ~ 1, k ~ 1, data = pkmdat)
#' simulated_pk <- rpk(nsim = 10, pkmodel = pkmod1)
#' simulated_pk
#' boxplot(simulated_pk[,, 1])
#'
#' pkmod2 <- pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' rpk(nsim = 10, pkmodel = pkmod2)
#' @export

rpk <- function(nsim, pkmodel){
  np <- pkmodel$np; nk <- pkmodel$nk
  betaSim <- mvtnorm::rmvnorm(nsim,
    mean = c(pkmodel$betahat_p, pkmodel$betahat_k),
    sigma = pkmodel$varbeta, method = "svd")
  pSim <- alogit(betaSim[,1:np]%*%t(pkmodel$miniXp))
  if (is.language(pkmodel$kformula)){
    kSim <- alogit(betaSim[,(np + 1):(np + pkmodel$nk)] %*%t (pkmodel$miniXk))
  } else {
    kSim <- matrix(pkmodel$kformula, ncol = pkmodel$Ncells, nrow = nsim)
  }
  ans <- array(
    dim = c(nsim, 2, pkmodel$Ncells),
    dimnames = list(NULL, c('p','k'), pkmodel$cells[,'CellNames'])
  )
  for(k in 1:pkmodel$Ncells){
    ans[ , , k] <- cbind(pSim[,k], kSim[,k])
  }
  ans
}