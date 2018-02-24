
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
#' @param data Dataframe with results from searcher efficiency trials and any
#' covariates included in \code{pformula} or {kformula} (required).
#'
#' @param kformula Formula for k; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 
#; 'Details'.
#'
#' @param obs_cols Vector of names of columns in \code{data} where results 
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
#' @param fixed_k Parameter for user-specified \code{k} value (optional). If a
#' value is provided, \code{kformula} is ignored and the model is fit under 
#' the assumption that the \code{k} parameter is fixed and known to be
#' \code{fix_k}.
#'
#' @param init_k Initial value used for \code{k} in the optimization.
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
#'  \item{\code{cellwise_pk}}{summary statistics for estimated cellwise 
#'    \code{p} and \code{k}, including the medians and upper & lower bounds
#'    on 95% CIs for each parameter, indexed by cell (or combination of
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
#'   \item{\code{p_formula}}{the model formula for the \code{p} parameter}
#'   \item{\code{k_formula}}{the model formula for the \code{k} parameter}
#'   \item{\code{beta_hat_p}}{parameter estimates for the terms in the 
#'     regression model for for \code{p} (logit scale)}
#'   \item{\code{beta_hat_k}}{parameter estimates for the terms in the 
#'     regression model for for \code{k} (logit scale). If \code{k} is fixed 
#'     and known, \code{betahat_k} is not calculated.}
#'   \item{\code{beta_var}}{the variance-covariance matrix of the estimators
#'     for \code{c(betahat_p, betahat_k}.}
#'   \item{\code{mm_p_cells}}{a cellwise design matrix for covariate structure
#'     of \code{p_formula}}
#'   \item{\code{mm_k_cells}}{a cellwise design matrix for covariate structure 
#'     of \code{k_formula}}
#'   \item{\code{p_levels}}{all levels of each covariate of \code{p}}
#'   \item{\code{k_levels}}{all levels of each covariate of \code{k}}
#'   \item{\code{n_beta_p, n_beta_k}}{number of parameters to fit the \code{p}
#'     and \code{k} models}
#'   \item{\code{cells}}{cell structure of the pk-model, i.e., combinations of
#'     all levels for each covariate of \code{p} and \code{k}. For example, if
#'     \code{covar1} has levels \code{"a"}, \code{"b"}, and \code{"c"}, and
#'     \code{covar2} has levels \code{"X"} and \code{"Y"}, then the cells 
#'     would consist of \code{a.X}, \code{a.Y}, \code{b.X}, \code{b.Y}, 
#'     \code{c.X}, and \code{c.Y}.}
#'   \item{\code{n_cells}}{total number of cells}
#'  \item{\code{p_predictors}}{list of covariates of \code{p}}
#'  \item{\code{k_predictors}}{list of covariates of \code{k}}
#'  \item{\code{observations}}{observations used to fit the model}
#'  \item{\code{fixed_k}}{the input \code{fixed_k}}
#'}
#'
#' @examples
#' data(pkmdat)
#' pkm(p ~ visibility, k ~ 1, data = pkmdat)
#' pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' pkm(p ~ visibility, k = 0.7, data = pkmdat)
#' @export
#'
pkm <- function(pformula, kformula = NULL, data, obs_cols = NULL, 
                fixed_k = NULL, k_init = 0.7){

  if(length(obs_cols) == 0){
    obs_cols <- grep("^[sS].*[0-9]$", names(data), value = TRUE)
    n_obs_cols <- length(obs_cols)
    if(n_obs_cols == 0){
      stop("No observation columns provided and no appropriate column names.")
    }
  }
  if(length(kformula) > 0 & length(fixed_k) == 1){
    message("Both formula and fixed value provided for k, fixed value used.")
  }
  if(length(kformula) == 0 & length(fixed_k) == 0){
    message("No formula or fixed value provided for k, fixed at 1.")
    fixed_k <- 1
  }
  if(length(obs_cols) == 1 & length(fixed_k) == 0){
    message("Only one observation, k cannot be estimated, fixed at 1")
    fixed_k <- 1
  }

  n_searches <- length(obs_cols)
  n_carcasses <- nrow(data)

  obs_data <- data[ , obs_cols]
  obs_data <- as.matrix(obs_data, ncol = n_searches)
  first_obs <- obs_data[ , 1]
  miss_data <- apply(obs_data, 2, match, 0)

  misses <- apply(miss_data, 1, sum, na.rm = TRUE)
  max_misses <- max(misses)
  found <- apply(obs_data, 1, sum, na.rm = TRUE)
  carcasses_found <- which(found == 1)
  found_on <- numeric(n_carcasses)
  found_on[carcasses_found] <- misses[carcasses_found] + 1

  p_preds <- all.vars(pformula[[3]])
  n_p_preds <- length(p_preds)
  p_formula <- formula(delete.response(terms(pformula)))
  p_levels <- .getXlevels(terms(p_formula), data)

  k_preds <- character(0)
  if(length(kformula) > 0){
    k_preds <- all.vars(kformula[[3]])
    n_k_preds <- length(k_preds)
    k_formula <- formula(delete.response(terms(kformula)))
    k_formula_out <- k_formula
    k_levels <- .getXlevels(terms(k_formula), data)
  }
  if(length(fixed_k) == 1){
    k_preds <- character(0)
    n_k_preds <- length(k_preds)
    k_formula <- formula(~1)  
    k_formula_out <- c(fixedk = fixed_k)
    k_levels <- .getXlevels(terms(k_formula), data)
  }

  pk_preds <- unique(c(p_preds, k_preds))
  cells <- combine_preds(pk_preds, data)
  n_cells <- nrow(cells)
  cell_names <- cells$CellNames

  mm_p_data <- model.matrix(p_formula, data)
  mm_k_data <- model.matrix(k_formula, data)
  mm_pk_data <- t(cbind(mm_p_data, mm_k_data))
  mm_p_cells <- model.matrix(p_formula, cells)
  mm_k_cells <- model.matrix(k_formula, cells)
  mm_pk_cells <- cbind(mm_p_cells, mm_k_cells)

  n_beta_k <- ncol(mm_k_data)
  n_beta_p <- ncol(mm_p_data)
  n_beta_pk <- n_beta_p + n_beta_k

  carcass_cell <- numeric(n_carcasses)
  for(k in 1:n_cells){
    group_pattern <- mm_pk_cells[k, ]
    matching_matrix <- mm_pk_data == group_pattern
    matching_parts <- apply(matching_matrix, 2, sum)
    matching_total <- matching_parts == ncol(mm_pk_cells)
    carcass_cell[matching_total] <- k
  }

  p_init <- numeric(n_carcasses)
  for(k in 1:n_cells){
    in_cell <- which(carcass_cell == k)
    cell_mean_p_init <- mean(first_obs[in_cell])
    p_init[in_cell] <- cell_mean_p_init
  }
  p_init[which(p_init < 0.1)] <- 0.1
  p_init[which(p_init > 0.9)] <- 0.9 

  p_cell_matrix <- solve(t(mm_p_data) %*% mm_p_data)
  p_cell_impact <- t(mm_p_data) %*% logit(p_init)
  beta_p_init <- p_cell_matrix %*% p_cell_impact
  beta_k_init <- logit(rep(k_init, n_beta_k))
  beta_init <- c(beta_p_init, beta_k_init)

  if(length(fixed_k) == 1){
    beta_init <- beta_init[-length(beta_init)]
  }

  MLE <- tryCatch(optim(par = beta_init, fn = pkLogLik, method = "BFGS",
                        hessian = T, 
                        carcass_cell = carcass_cell, 
                        misses = misses, max_misses = max_misses,
                        found_on = found_on, 
                        mm_pk_cells = mm_pk_cells, 
                        n_beta_p = n_beta_p,
                        fixed_k = fixed_k), 
                   error = function(x) {NA})

  convergence <- MLE$convergence
  beta_hat <- MLE$par
  beta_hessian <- MLE$hessian
  llik <- MLE$value

  n_param <- length(beta_hat)  
  AIC <- 2 * llik + 2 * n_param
  AICc_offset <- (2 * n_param * (n_param + 1)) / (n_carcasses - n_param - 1)
  AICc <- AIC + AICc_offset

  beta_hat_p <- beta_hat[1:n_beta_p]
  names(beta_hat_p) <- colnames(mm_p_data)
  beta_hat_k <- NULL
  if(length(fixed_k) == 0){
    which_k <- (n_beta_p + 1):(n_beta_pk)
    beta_hat_k <- beta_hat[which_k]
    names(beta_hat_k) <- colnames(mm_k_data)
  }

  beta_var <- tryCatch(solve(beta_hessian), error = function(x) {NA})
  if(is.na(beta_var)[1]){
    stop("Model generates unstable variance estimate.")
  }
  beta_var_p <- beta_var[1:n_beta_p, 1:n_beta_p]
  p_cell_means <- mm_p_cells %*% beta_hat_p
  p_cell_vars <- mm_p_cells %*% beta_var_p %*% t(mm_p_cells)
  p_cell_sds <- sqrt(diag(p_cell_vars))

  if(length(fixed_k) == 0){
    which_k <- (n_beta_p + 1):(n_beta_pk)
    beta_var_k <- beta_var[which_k, which_k]
    k_cell_means <- mm_k_cells %*% beta_hat_k
    k_cell_vars <- mm_k_cells %*% beta_var_k %*% t(mm_k_cells)
    k_cell_sds <- sqrt(diag(k_cell_vars))
  }else{
    k_cell_means <- rep(fixed_k, n_cells)
    k_cell_sds <- rep(0, n_cells)
  }

  probs <- data.frame(c(0.5, 0.025, 0.975))
  cell_p_table <- apply(probs, 1, qnorm, mean = p_cell_means, sd = p_cell_sds)
  cell_p_table <- matrix(cell_p_table, nrow = n_cells, ncol = 3)
  cell_p_table <- alogit(cell_p_table)
  colnames(cell_p_table) <- c("p_median", "p_lower_95", "p_upper_95")
  cell_k_table <- apply(probs, 1, qnorm, mean = k_cell_means, sd = k_cell_sds)
  cell_k_table <- matrix(cell_k_table, nrow = n_cells, ncol = 3)
  cell_k_table <- alogit(cell_k_table)
  colnames(cell_k_table) <- c("k_median", "k_lower_95", "k_upper_95")
  cell_pk_table <- data.frame(cell = cell_names, cell_p_table, cell_k_table)

  output <- list()
  output$call <- match.call()
  output$p_formula <- p_formula
  output$k_formula <- k_formula_out
  output$predictors <- pk_preds
  output$p_predictors <- p_preds
  output$k_predictors <- k_preds
  output$AIC <- AIC
  output$AICc <- AICc
  output$convergence <- convergence
  output$beta_var <- beta_var
  output$mm_p_cells <- mm_p_cells
  output$mm_k_cells <- mm_k_cells
  output$n_beta_p <- n_beta_p  
  output$n_beta_k <- n_beta_k
  output$beta_hat_p <- beta_hat_p
  output$beta_hat_k <- beta_hat_k
  output$p_levels <- p_levels
  output$k_levels <- k_levels
  output$cells <- cells
  output$n_cells <- n_cells
  output$cellwise_pk <- cell_pk_table
  output$observations <- obs_data
  output$fixed_k <- fixed_k
  class(output) <- c("pkm", "list")
  attr(output, "hidden") <- c("p_predictors", "k_predictors", "fixed_k",
                              "beta_hat_p", "beta_hat_k",  
                              "mm_p_cells", "mm_k_cells", 
                              "n_beta_p", "n_beta_k", 
                              "p_levels", "k_levels",
                              "convergence", "beta_var", "AIC",
                              "cells", "n_cells", "observations")
  return(output)
}

#' @export
#'
print.pkm <- function(pk_model){
  hid <- attr(pk_model, "hidden")
  which_not_hid <- !names(pk_model) %in% hid
  print(pk_model[which_not_hid])
}
  
#' Run a set of pkm models based on predictor inputs
#'
#'
#'
pkm_set <- function(pformula, kformula = NULL, data, obs_cols = NULL, 
                    fixed_k = NULL, k_init = 0.7){

  if(length(kformula) > 0 & length(fixed_k) == 1){
    message("Both formula and fixed value provided for k, fixed value used.")
    kformula <- k ~ 1
  }
  if(length(kformula) == 0 & length(fixed_k) == 0){
    message("No formula or fixed value provided for k, fixed at 1.")
    kformula <- k ~ 1
    fixed_k <- 1
  }
  if(length(obs_cols) == 1 & length(fixed_k) == 0){
    message("Only one observation, k cannot be estimated, fixed at 1")
    fixed_k <- 1
  }
  if(length(kformula) == 0){
    kformula <- k ~ 1
  }

  p_terms <- attr(terms(pformula), "term.labels")
  k_terms <- attr(terms(kformula), "term.labels")
  n_p_terms <- length(p_terms)
  n_k_terms <- length(k_terms)
  n_p_formulae <- 2^(n_p_terms)
  n_k_formulae <- 2^(n_k_terms)

  p_drop_complex <- rep(1:n_p_terms, choose(n_p_terms, 1:n_p_terms))
  p_drop_which <- numeric(0)
  if(n_p_terms > 0){
    for(i in 1:n_p_terms){
      specifics_to_drop <- seq(1, choose(n_p_terms, (1:n_p_terms)[i]))
      p_drop_which <- c(p_drop_which, specifics_to_drop)
    }
  }
  pformula_ops <- vector("list", n_p_formulae)
  pformula_ops[[1]] <- pformula
  pformula_ops_keep <- rep(TRUE, n_p_formulae)
  if(n_p_formulae > 1){
    for(i in 2:n_p_formulae){
      terms_drop_complex <- combn(p_terms, p_drop_complex[i - 1])
      terms_drop_spec <- terms_drop_complex[ , p_drop_which[i - 1]]
      terms_drop <- paste(terms_drop_spec, collapse = " - ")
      formula_update <- paste(format(~.), "-", terms_drop)
      updated_formula <- update.formula(pformula, formula_update)
      pformula_ops[[i]] <- updated_formula
      pformula_ops_keep[i] <- check_component_terms_included(updated_formula)
    }
    n_pformula_ops_keep <- sum(pformula_ops_keep)
    which_pformula_ops_keep <- which(pformula_ops_keep == TRUE)
    pformula_ops_kept <- vector("list", n_pformula_ops_keep)
    for(i in 1:n_pformula_ops_keep){
      pformula_ops_kept[[i]] <- pformula_ops[[which_pformula_ops_keep[i]]]
    }
  }else{
    pformula_ops_kept <- pformula_ops
  }
  
  k_drop_complex <- rep(1:n_k_terms, choose(n_k_terms, 1:n_k_terms))
  k_drop_which <- numeric(0)
  if(n_k_terms > 0){
    for(i in 1:n_k_terms){
      specifics_to_drop <- seq(1, choose(n_k_terms, (1:n_k_terms)[i]))
      k_drop_which <- c(k_drop_which, specifics_to_drop)
    }
  }
  kformula_ops <- vector("list", n_k_formulae)
  kformula_ops[[1]] <- kformula
  kformula_ops_keep <- rep(TRUE, n_k_formulae)
  if(n_k_formulae > 1){
    for(i in 2:n_k_formulae){
      terms_drop_complex <- combn(k_terms, k_drop_complex[i - 1])
      terms_drop_spec <- terms_drop_complex[ , k_drop_which[i - 1]]
      terms_drop <- paste(terms_drop_spec, collapse = " - ")
      formula_update <- paste(format(~.), "-", terms_drop)
      updated_formula <- update.formula(kformula, formula_update)
      kformula_ops[[i]] <- updated_formula
      kformula_ops_keep[i] <- check_component_terms_included(updated_formula)
    }
    n_kformula_ops_keep <- sum(kformula_ops_keep)
    which_kformula_ops_keep <- which(kformula_ops_keep == TRUE)
    kformula_ops_kept <- vector("list", n_kformula_ops_keep)
    for(i in 1:n_kformula_ops_keep){
      kformula_ops_kept[[i]] <- kformula_ops[[which_kformula_ops_keep[i]]]
    }
  }else{
    kformula_ops_kept <- kformula_ops
  }

  if(length(fixed_k) == 1){
    kformula_ops_kept <- NA
  }
  pkformulae <- expand.grid(pformula_ops_kept, kformula_ops_kept)
  pformulae <- pkformulae[ , 1]
  kformulae <- pkformulae[ , 2]
  if(length(fixed_k) == 1){
    kformulae <- NULL
  }
  n_mods <- length(pformulae) 
  output <- vector("list", n_mods)
  for(i in 1:n_mods){
    pform_i <- pformulae[i][[1]]
    kform_i <- kformulae[i][[1]]
    pkm_i <- tryCatch(pkm(pform_i, kform_i, data, obs_cols, fixed_k, k_init), 
                      error = function(x) {"Failed model fit"})

    p_name <- paste(format(pform_i), collapse = "")
    p_name <- gsub("    ", "", p_name)
    k_name <- paste(format(kform_i), collapse = "")
    k_name <- gsub("    ", "", k_name)
    if(length(fixed_k) == 1){
      k_name <- paste("k fixed at ", fixed_k, sep = "")
    }
    mod_name <- paste(p_name, "; ", k_name, sep = "")

    output[[i]] <- pkm_i
    names(output)[i] <- mod_name
  }
  return(output)
}

#' Calculate the negative log-likelihood of a searcher efficiency model.
#' 
#' @param misses Number of searches when carcass was present but
#'  not found.
#' @param found_on Search on which carcass was found.
#' @param beta Parameters to be optimized.
#' @param n_beta_p Number of parameters associated with p
#' @param carcass_cell Which cell each observation belongs to.
#' @param max_misses Maximum possible number of misses for a carcass.
#' @param mm_pk_cells Combined pk model matrix.
#' @param fixed_k_value Value of k if fixed. 
#' @return Negative log likelihood of the observations, given the parameters.
#' @examples
#' NA
#' @export 
#'
pkLogLik <- function(misses, found_on, beta, n_beta_p, 
                         carcass_cell, max_misses, mm_pk_cells, 
                         fixed_k = NULL){

  if(length(fixed_k) == 1){
    beta <- c(beta, logit(fixed_k))
  }

  n_cells <- nrow(mm_pk_cells)
  n_pk <- length(beta)
  which_p <- 1:n_beta_p
  which_k <- (n_beta_p + 1):n_pk

  beta_p <- beta[which_p]
  beta_k <- beta[which_k]
  Beta <- matrix(0, nrow = n_pk, ncol = 2)
  Beta[which_p, 1] <- beta[which_p]
  Beta[which_k, 2] <- beta[which_k]

  pk <- alogit(mm_pk_cells %*% Beta)
  p <- pk[ , 1]
  k <- pk[ , 2]

  powk <- matrix(k, nrow = n_cells, ncol = max_misses + 1)
  powk[ , 1] <- 1
  powk <- matrixStats::rowCumprods(powk)

  pmiss <- matrix(1 - (p * powk[ , 1:(max_misses + 1)]), nrow = n_cells)
  pmiss <- matrixStats::rowCumprods(pmiss)
  pfind <- matrixStats::rowDiffs(1 - pmiss)
  pfind_si <- cbind(pk[ , 1], pfind)

  not_found_cell <- carcass_cell[found_on == 0]
  not_found_misses <- misses[found_on == 0]
  not_found_cell_misses <- cbind(not_found_cell, not_found_misses)
  found_cell <- carcass_cell[found_on > 0]
  found_found_on <- found_on[found_on > 0]
  found_cell_found_on <- cbind(found_cell, found_found_on)

  lls_miss <- log(pmiss[not_found_cell_misses])
  ll_miss <- sum(lls_miss)
  lls_found <- log(pfind_si[found_cell_found_on])
  ll_found <- sum(lls_found)
  ll_miss_found <- ll_miss + ll_found
  nll_miss_found <- -ll_miss_found
 
  return(nll_miss_found)
}
  
#' Simulate p and k parameters from a fitted pk model.
#'
#' @param n the number of simulation draws
#'
#' @param pk_model A \code{\link{pkm}} object (which is returned from 
#'  \code{pkm()})
#'
#' @param seed optional input to set the seed of the RNG
#'
#' @return list of two matrices of \code{n} simulated \code{p} and \code{k} 
#'  for cells defined by the \code{pk_model} object. 
#'
#' @examples
#' data(pkmdat)
#' pk_mod_1 <- pkm(p ~ 1, k ~ 1, data = pkmdat)
#' simulated_pk <- rpk(n_sim = 10, pk_model = pk_mod_1)
#' simulated_pk
#'
#' pk_mod_2 <- pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' rpk(n_sim = 10, pk_model = pk_mod_2)
#' @export
#'
rpk <- function(n = 1, pk_model, seed = NULL){

  if(!"pkm" %in% class(pk_model)){
    stop("pk_model not of class pkm.")
  }

  n_beta_p <- pk_model$n_beta_p 
  n_beta_k <- pk_model$n_beta_k
  which_beta_k <- (n_beta_p + 1):(n_beta_p + n_beta_k)
  fixed_k <- pk_model$fixed_k
  mm_p_cells <- pk_model$mm_p_cells
  mm_k_cells <- pk_model$mm_k_cells
  n_cells <- pk_model$n_cells
  cell_names <- pk_model$cells[,'CellNames']
  beta_mean <- c(pk_model$beta_hat_p, pk_model$beta_hat_k)
  beta_var <- pk_model$beta_var
  method <-  "svd"

  if(length(seed) > 0){
    set.seed(seed)
  }
  beta_sim <- mvtnorm::rmvnorm(n, mean = beta_mean, sigma = beta_var, method)
  p_sim <- as.matrix(alogit(beta_sim[ , 1:n_beta_p] %*% t(mm_p_cells)))
  colnames(p_sim) <- cell_names
  rownames(p_sim) <- sprintf("sim_%d", 1:n)

  if(length(fixed_k) == 0){
    k_sim <- as.matrix(alogit(beta_sim[ , which_beta_k] %*% t(mm_k_cells)))
  }else{
    k_sim <- matrix(fixed_k, ncol = n_cells, nrow = n)
  }
  colnames(k_sim) <- cell_names
  rownames(k_sim) <- sprintf("sim_%d", 1:n)

  output <- list(p_sim, k_sim)
  names(output) <- c("p_sim", "k_sim")
  return(output)
}

#' Create the  AICc tables for the searcher efficiency models
#' 
#' @param pk_model_set Set of searcher efficiency models fit to the same
#'  observations
#' @return AICc table
#' @examples
#' NA
#' @export 
#'
pkm_set_aicc_tab <- function(pk_model_set){

  n_models <- length(pk_model_set)
  pk_formulae <- names(pk_model_set)
  p_formulae <- rep(NA, n_models)
  k_formulae <- rep(NA, n_models)
  AICc <- rep(NA, n_models)
  delta_AICc <- rep(NA, n_models)

  if(n_models == 1){
    split_pk_formulae <- strsplit(pk_formulae, "; ")[[1]]
    p_formulae <- split_pk_formulae[1] 
    k_formulae <- split_pk_formulae[2]
    AICc <- tryCatch(pk_model_set[[1]]$AICc, error = function(x) {1e7})
    delta_AICc <- 0    
    AICc_order <- 1
  }else{
    for(i in 1:n_models){
      split_pk_formulae_i <- strsplit(pk_formulae[i], "; ")[[1]]
      p_formulae[i] <- split_pk_formulae_i[1] 
      k_formulae[i] <- split_pk_formulae_i[2]
      AICc[i] <- tryCatch(pk_model_set[[i]]$AICc, error = function(x) {1e7})
    }
    AICc_order <- order(AICc)
    delta_AICc <- AICc - min(AICc)
    which_fails <- which(AICc == 1e7)
    AICc[which_fails] <- NA
    delta_AICc[which_fails] <- NA
  }

  output <- data.frame(p_formulae, k_formulae, AICc, delta_AICc)
  output <- output[AICc_order, ]
  colnames(output) <- c("p formula", "k formula", "AICc", "Delta AICc")
  which_AICc_NA <- which(is.na(output$AICc))
  which_AICc_max <- which(output$AICc == 1e7)
  if(length(which_AICc_NA) > 0){
    message("Models with incorrect specification were removed from output.")
    output <- output[-which_AICc_NA, ]
  }
  if(length(which_AICc_max) > 0){
    message("Models that failed during fit were removed from output.")
    output <- output[-which_AICc_max, ]
  }
  return(output)
}



