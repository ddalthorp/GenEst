# cpm and rcp -- functions that mirror the usage of pkm and rpk
cpm <- function(cpformula, data = NULL, dist = "weibull", left = "CPmin", right = "CPmax"){
  # NOTE: names in arg list reflect names used in "survival" package
  # parse the persistence data into intervals for "survival" functions
  t1 <- data[ , left]
  t2 <- data[ , right]
  event <- rep(3, length(t1))
  event[is.na(t2) | is.infinite(t2)] <- 0
  event[round(t1, 3) == round(t2, 3)] <- 1
  t1 <- pmax(t1, 0.0001)

  # format the cpformula for use in survival::survreg
  if (length(cpformula) == 2 ||
      !exists(as.character(cpformula[[2]])) ||
      !("Surv" %in% class(get(as.character(cpformula[[2]]))))){
    cpt <- survival::Surv(time = t1, time2 = t2, event = event, type = "interval")
    if (length(attr(terms(cpformula), "factors")) == 0){
      cpformula <- cpt ~ 1
    } else {
      cpformula <- as.formula(
        reformulate(dimnames(attr(terms(cpformula), "factors"))[[2]],
        response = "cpt")
      )
    }
  }
  survival::survreg(cpformula, data = data, dist = dist) # what if only one class?
}

rcp <- function(n, cpmod){ # "n" better matches format of rnorm, etc. than "nsim" does
  if (length(cpmod$coef) == 1){ # no covariates
    fct <- data.frame(group="all",CellNames="all")
    Ncells <- 1
    ans <- array(dim = c(nsim, 2, Ncells))
    if (cpmod$dist == "exponential"){
      betasim <- mvtnorm::rmvnorm(nsim, cpmod$coef, cpmod$var)
    } else {
      betasim <- mvtnorm::rmvnorm(nsim, c(cpmod$coef, log(cpmod$scale)), cpmod$var)
    }
    ans[ , 2, 1] <- switch(cpmod$dist,
        exponential = exp(betasim[ ,1]),
        weibull = exp(betasim[ ,1]),
        loglogistic = exp(betasim[ ,1]),
        lognormal = betasim[ ,1]
      )
    ans[ , 1, 1] <- switch(cpmod$dist,
      exponential = rep(NA, nsim),
      weibull = 1/(exp(betasim[ , 2])),
      loglogistic = 1/(exp(betasim[ , 2])),
      lognormal = exp(betasim[ , 2])^2
    )
  } else {
    fct <- expand.grid(cpmod$xlevels)
    if (dim(fct)[2] == 1) fct$CellNames <- fct[ ,1] else fct$CellNames <- paste0(fct[ ,1],".",fct[ ,2])
    miniX <- model.matrix(reformulate(dimnames(attr(cpmod$terms, "factors"))[[2]]), fct)
    Ncells <- dim(miniX)[1]
    if (cpmod$dist == "exponential"){
      betasim <- mvtnorm::rmvnorm(nsim, cpmod$coef, cpmod$var)
    } else {
      betasim <- mvtnorm::rmvnorm(nsim, c(cpmod$coef, log(cpmod$scale)), cpmod$var)
    }
    ans <- array(
      dim = c(nsim, 2, Ncells),
      dimnames = list(NULL, c("pda", "pdb"), fct[ ,"CellNames"])
    )
    for (ci in 1:Ncells){
      ans[ , 2, ci] <- switch(cpmod$dist,
          exponential = exp(betasim[ , 1:Ncells] %*% miniX[ci, ]),
          weibull = exp(betasim[ , 1:Ncells] %*% miniX[ci, ]),
          loglogistic = exp(betasim[ , 1:Ncells] %*% miniX[ci, ]),
          lognormal = betasim[ , 1:Ncells] %*% miniX[ci, ]
        )
      ans[ , 1, ci] <- switch(cpmod$dist,
        exponential = rep(NA, nsim),
        weibull = 1/(exp(betasim[ , Ncells + 1])),
        loglogistic = 1/(exp(betasim[ , Ncells + 1])),
        lognormal = exp(betasim[ , Ncells + 1])^2
      )
    }
  }
  ans
}
