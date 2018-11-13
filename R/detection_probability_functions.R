#' @title Estimate all carcass-level detection rates and arrival intervals
#'
#' @description Estimate g values and arrival intervals for a set of carcasses
#'   from fitted pk and cp models and search data
#'
#' @param data_CO Carcass Observation data
#'
#' @param data_SS Search Schedule data
#'
#' @param COdate Column name for the date found data
#'
#' @param model_SE Searcher Efficiency model (or list of models if there are
#'   multiple size classes)
#'
#' @param model_CP Carcass Persistence model (or list of models if there are
#'   multiple size classes)
#'
#' @param unitCol Column name for the unit indicator
#'
#' @param SSdate Column name for the date searched data. Optional.
#'   If not provided, \code{estg} will try to find the SSdate among
#'   the columns in data_SS. See \code{\link{prepSS}}.
#'
#' @param sizeCol Name of column in \code{data_CO} where the size classes
#'   are recorded. Optional. If not provided, no distinctions are made among
#'   sizes. \code{sizeCol} not only identifies what the name of the size
#    column is, it also identifies that the model should include size as a 
#'   segregating class
#'
#' @param seed_SE seed for random draws of the SE model
#'
#' @param seed_CP seed for random draws of the CP model
#'
#' @param seed_g seed for random draws of the gs
#'
#' @param nsim the number of simulation draws
#'
#' @param max_intervals maximum number of arrival interval intervals to 
#'   consider for each carcass. Optional. Limiting the number of search 
#'   intervals can greatly increase the speed of calculations with only a 
#'   slight reduction in accuracy in most cases.
#'
#' @return list of [1] g estimates (ghat) and [2] arrival interval estimates 
#'   (Aj) for each of the carcasses. The row names of the Aj matrix are the
#'   names of the units where each carcass was found.
#'
#' @examples
#'  data(mock)
#'  model_SE <- pkm(formula_p = p ~ HabitatType, formula_k = k ~ 1,
#'               data = mock$SE)
#'  model_CP <- cpm(formula_l = l ~ Visibility, formula_s = s ~ Visibility, 
#'                data = mock$CP, dist = "weibull",
#'                left = "LastPresentDecimalDays", 
#'                right = "FirstAbsentDecimalDays"
#'              )
#'  ghat <- estg(data_CO = mock$CO, COdate = "DateFound",  data_SS = mock$SS,
#'            model_SE = model_SE, model_CP = model_CP, unitCol = "Unit", nsim = 100)
#'
#' @export
#'
estg <- function(data_CO, COdate, data_SS, SSdate = NULL,
                 model_SE, model_CP, sizeCol = NULL, unitCol = NULL,
                 nsim = 1000, max_intervals = 8,
                 seed_SE = NULL, seed_CP = NULL, seed_g = NULL){

  SSdat <- prepSS(data_SS) # SSdat name distinguishes this as pre-formatted
  SSdat$searches_unit[ , 1] <- 1 # set t0 as start of period of inference
  t0date <- SSdat$date0
  dates_CO <- checkDate(data_CO[ , COdate])
  if (is.null(dates_CO)) stop("dates_CO not properly formatted as dates")
  COdat <- data_CO # format data_CO
  COdat[ , COdate] <- dateToDay(dates_CO, t0date)
  names(COdat)[names(COdat) == COdate] <- "day" # distinguish integers
  if (is.null(sizeCol) || is.na(sizeCol)){
    sizeCol <- "placeholder"
    COdat[ , sizeCol] <- "value"
    model_SE <- list("value" = model_SE)
    model_CP <- list("value" = model_CP)
  } else {
    if (!(sizeCol %in% colnames(COdat))){
      stop("size class column not in carcass data.")
    }
    if (length(setdiff(names(model_SE), names(model_CP))) > 0) {
      stop("model_SE and model_CP must encompass the same size classes")
    }
    if (!all(COdat[, sizeCol] %in% names(model_SE))){
      stop("no SE model for some size class represented in data_CO")
    }
  }
  sizeclass <- as.list(as.character(COdat[, sizeCol]))
  sizeclasses <- unique(unlist(sizeclass))
  nsizeclass <- length(sizeclasses)

# data pre-processing
# create lists of arrays for SS (days) and cells (SE and CP)
  for (sc in sizeclasses){
    if (!("pkm" %in% class(model_SE[[sc]]))){
      stop("Invalid pk model ")
    }
    if (sum(diag(model_SE[[sc]]$varbeta) < 0) > 0){
      stop("
        Cannot estimate variance in user-supplied pk model for size '", sc,
        "' Aborting calculation of ghat."
      )
    }
  }
  preds_SE <- lapply(model_SE, function(x) x$predictors)
  preds_CP <- lapply(model_CP, function(x) x$predictors)
  preds <- mapply(function(x, y) unique(c(x, y)), preds_SE, preds_CP)
  dist <- unlist(lapply(model_CP, function(x) x$dist))
  COpreds <- lapply(preds, function(x) x[x %in% names(COdat)])
  SSpreds <- lapply(preds, function(x) x[!(x %in% names(COdat))])
  if (max(unlist(lapply(SSpreds, length))) > 1){
    stop("At most 1 SS predictor is allowed per size class.")
  }
  if (length(unlist(SSpreds)) > 0 && !all(unlist(SSpreds) %in% names(SSdat))){
    stop("Model predictor missing from both CO and SS data.")
  }
  pksim <- lapply(model_SE, function(x) rpk(nsim, x))
  names(pksim) <- names(model_SE)

  cpsim <- lapply(model_CP, rcp, n = nsim, type = "ppersist")

  X <- dim(COdat)[1]
  days <- list()
  for (xi in 1:X){
    toi <- COdat$day[xi]
    SSxi <- SSdat$searches_unit[COdat[xi, unitCol], ] * SSdat$days
    SSxi <- c(0, SSxi[SSxi > 0])
    days[[xi]] <- SSxi[SSxi <= toi]
    if (!is.null(max_intervals)){
      dlen <- length(days[[xi]])
      if (dlen > max_intervals + 1)
        days[[xi]] <- days[[xi]][(dlen - max_intervals):dlen]
    }
  }

  cells <- list()
  # take care of the SS preds
  if (sum(unlist(lapply(SSpreds, length))) == 0){
    for (xi in 1:X){
      cells[[xi]] <- list()
      cells[[xi]][[sizeCol]] <- COdat[xi, sizeCol]
      pcol <- preds_SE[[COdat[xi, sizeCol]]]
      if (length(pcol) == 0) {
        cells[[xi]]$SEcell <- "all"
      } else {
        cells[[xi]]$SEcell <- paste(COdat[xi, pcol], collapse = ".")
      }
      cells[[xi]]$SErep <- length(days[[xi]]) - 1
      pcol <- preds_CP[[COdat[xi, sizeCol]]]
      if (length(pcol) == 0) {
        cells[[xi]]$CPcell <- "all"
      } else {
        cells[[xi]]$CPcell <- paste(COdat[xi, pcol], collapse = ".")
      }
      cells[[xi]]$CPrep <- length(days[[xi]]) - 1
    }
  } else {
    for (xi in 1:X){
      cells[[xi]] <- list()
      SEc <- SEr <- CPc <- CPr <- NULL
      sz <- as.character(COdat[xi, sizeCol])
      cells[[xi]][[sizeCol]] <- sz
      # interpret the SE predictors
      nse <- length(preds_SE[[sz]])
      if (nse == 0){
        cells[[xi]]$SEcell <- "all"
        cells[[xi]]$SErep <- length(days[[xi]]) - 1
      } else {
        for (sei in 1:nse){
          predi <- preds_SE[[sz]][sei]
          if (predi %in% SSpreds){
            ssvec <- (SSdat[[predi]][which(SSdat[["days"]] %in% days[[xi]])])
            ssvec <- ssvec[-1]
            SEc <- paste(SEc, unique(ssvec),
              sep = ifelse(is.null(SEc), "", "."))
            SEr <- table(ssvec)[unique(ssvec)]
          } else {
            SEc <- paste(SEc, COdat[xi, predi],
              sep = ifelse(is.null(SEc), "", "."))
          }
        }
        cells[[xi]]$SEcell <- SEc
        if (is.null(SEr)){
          cells[[xi]]$SErep <- length(days[[xi]]) - 1
        } else {
          cells[[xi]]$SErep <- SEr
        }
      }
      # interpret the CP predictors
      nse <- length(preds_CP[[sz]])
      if (nse == 0){
        cells[[xi]]$CPcell <- "all"
        cells[[xi]]$CPrep <- length(days[[xi]]) - 1
      } else {
        for (sei in 1:nse){
          predi <- preds_CP[[sz]][sei]
          if (predi %in% SSpreds){
            ssvec <- (SSdat[[predi]][which(SSdat[["days"]] %in% days[[xi]])])
            ssvec <- ssvec[-1]
            CPc <- paste(CPc, unique(ssvec),
              sep = ifelse(is.null(CPc), "", "."))
            CPr <- table(ssvec)[unique(ssvec)]
          } else {
            CPc <- paste(CPc, COdat[xi, predi],
              sep = ifelse(is.null(CPc), "", "."))
          }
        }
        cells[[xi]]$CPcell <- CPc
        if (is.null(CPr)){
          cells[[xi]]$CPrep <- length(days[[xi]]) - 1
        } else {
          cells[[xi]]$CPrep <- CPr
        }
      }
    }
  }
  # the calculation
  ghat <- matrix(0, nrow = X, ncol = nsim)
  Aj <- matrix(0, nrow = X, ncol = nsim)
  set.seed(seed_g)
  for (xi in 1:X){
    if (COdat$day[xi] == 0) next # cleanout: leaves initial 0s in ghat and Aj
    SSxi <- SSdat$searches_unit[COdat[xi, unitCol], ] * SSdat$days
    SSxi <- c(0, SSxi[SSxi > 0])
    # calculate SE
    sz <- cells[[xi]][[sizeCol]]
    SEr <- cells[[xi]]$SErep
    oi <- length(days[[xi]]) - 1
    rng <- 0
    pOigAj <- NULL # virtually identical calcs done for pAjgOi, but in reverse
    for (sei in 1:length(SEr)){
      rng <- max(rng) + 1:SEr[sei]
      pOigAj <- cbind(pOigAj, SEsi_left(
        oi = oi,
        pk = pksim[[sz]][[cells[[xi]]$SEcell[sei]]],
        rng = rng
      ))
    }
    # multiply by ppersist
    CPr <- cells[[xi]]$CPrep
    rng <- 0
    for (cpi in 1:length(CPr)){
      rng <- max(rng) + 1:CPr[cpi]
      pOigAj[, rng] <- pOigAj[, rng] * t(ppersist(
        pda = cpsim[[sz]][[cells[[xi]]$CPcell[cpi]]][ , "pda"],
        pdb = cpsim[[sz]][[cells[[xi]]$CPcell[cpi]]][ , "pdb"],
        dist = model_CP[[sz]]$distribution,
        t_arrive0 = days[[xi]][rng],
        t_arrive1 = days[[xi]][rng + 1],
        t_search = rep(max(days[[xi]]), length(rng))
      ))
    }

    parrive <- diff(days[[xi]][1:(oi+1)])/days[[xi]][oi+1]
    pAjgOi <- t(pOigAj) * parrive; pAjgOi <- t(t(pAjgOi)/colSums(pAjgOi))
    Aj[xi, ] <- # sim arrival intervals (relative to cind's ss)
       rowSums(matrixStats::rowCumsums(t(pAjgOi)) < runif(nsim)) +
         (sum(SSxi <= min(days[[xi]])))
    xuint <- unique(Aj[xi, ]) # unique xi arrival intervals (in SSxi)
    for (aj in xuint){
      # calculate simulated ghat associated with the given carcass and 
      #   interval (there is much redundant calculation here that could be sped
      #   up substantially with clever bookkeeping)
      simInd <- which(Aj[xi, ] == aj)
      top <- length(SSxi)
      if (!is.null(max_intervals)){
        # the calculations on RHS are more critical and less time consuming
        # calculation-wise, so for now...
        #top <- min(aj + max_intervals, top)
      }
      # use an adjusted search schedule because we "know" when carcass arrived
      # which cell is "active" for the given arrival interval?
      cpi <- findInterval(aj, c(0, min(xuint) + cumsum(cells[[xi]]$CPrep)),
        rightmost.closed = T)
      pda <- cpsim[[sz]][[cells[[xi]]$CPcell[cpi]]][simInd , "pda"]
      pdb <- cpsim[[sz]][[cells[[xi]]$CPcell[cpi]]][simInd , "pdb"]
      ppersu <- ppersist(
        pda = pda,
        pdb = pdb,
        dist = model_CP[[sz]]$distribution,
        t_arrive0 = rep(SSxi[aj], top - aj),
        t_arrive1 = rep(SSxi[aj + 1], top - aj),
        t_search = SSxi[(aj + 1):top]
      )
      pki <- findInterval(aj, c(0, min(xuint) + cumsum(cells[[xi]]$SErep)),
        rightmost.closed = T)
      SE <- t(SEsi_right(
        top - aj,
        pksim[[sz]][[cells[[xi]]$SEcell[pki]]][simInd , ]
      ))
      if (aj < top - 1){
        ghat[xi, simInd] <- colSums(SE * ppersu)
      } else {
        ghat[xi, simInd] <- as.vector(SE) * as.vector(ppersu)
      }
    }
  }
  rownames(Aj) <- COdat[ , unitCol]
  out <- list("ghat" = ghat, "Aj" = Aj) # ordered by relevance to user
  return(out)
}

#' @title Calculate conditional probability of observation at a search
#'
#' @description Calculate the conditional probability of observing a carcass 
#'   at search oi as a function arrival interval (assuming carcass is not
#'   removed by scavengers before the time of the final search)
#'   
#' @param oi number of searches after arrival
#'
#' @param pk numeric array of searcher efficiency p and k parameters
#'  (p = pk[ , 1] and k = pk[ , 2])
#'
#' @param rng optional parameter giving the range of intervals to consider
#'
#' @return numeric array of probability of observing a carcass at oi for
#'   given that it arrived in intervals 1:oi if rng = NULL (or in intervals
#'   \code{rng}), assuming the carcass had not been previously discovered or
#'   removed by scavengers
#'
#' @export
#'
SEsi_left <- function (oi, pk, rng = NULL){
  # oi is the index for the search occasion (excluding t0)
  # pk is nsim x 2 array of simulated p and k parameters
  # rng is the intervals for which to calculate answer
  if (is.null(rng)) rng <- 1:oi
  if (is.null(dim(pk)) || nrow(pk) == 1) return(SEsi0(0:oi, pk))
  npk <- nrow(pk)
  nmiss <- oi - rng
  maxmiss <- max(nmiss)
  if (maxmiss == 0){
    pfind.si <- matrix(pk[, 1], ncol = 1)
  }
  else if (maxmiss == 1) {
    pfind.si <- cbind(pk[, 1], (1 - pk[, 1]) * pk[, 2] * pk[, 1])
  }
  else {
    powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(npk, maxmiss + 1))
    powk[, 1] <- 1
    powk <- matrixStats::rowCumprods(powk)
    pfind.si <- pk[, 1] * powk * cbind(
      rep(1, npk),
      matrixStats::rowCumprods(1 - (pk[, 1] * powk[, 1:maxmiss]))
    )
  }
  return(pfind.si[ , oi - rng + 1])
}

#' @title Calculate conditional probability of observation after a series of 
#'   searches
#'
#' @description Calculate the conditional probability of observing a carcass 
#'   after i = 1:nsi searches (assuming carcass is not previous discovered by 
#'   searchers or removed by scavengers)
#'
#' @param nsi number of searches after arrival
#'
#' @param pk numeric array of searcher efficiency p and k parameters
#'  (p = pk[ , 1] and k = pk[ , 2])
#'
#' @return numeric nsi x dim(pk)[1] array of probabilities of observing a
#'  carcass after 1:nsi searches (assuming that the carcass had not been
#' previously discovered or removed by scavengers
#'
#' @export
#'
SEsi_right <- function(nsi, pk){
  # oi is the index for the search occasion (excluding t0)
  # pk is nsim x 2 array of simulated p and k parameters
  # rng is the intervals for which to calculate answer
  if (is.null(dim(pk)) || nrow(pk) == 1) return(t(SEsi0(0:nsi, pk)))
  npk <- nrow(pk)
  nmiss <- 1:nsi - 1
  maxmiss <- max(nmiss)
  if (maxmiss == 0){
    pfind.si <- matrix(pk[, 1], ncol = 1)
  }
  else if (maxmiss == 1) {
    pfind.si <- cbind(pk[, 1], (1 - pk[, 1]) * pk[, 2] * pk[, 1])
  }
  else {
    powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(npk, maxmiss + 1))
    powk[, 1] <- 1
    powk <- matrixStats::rowCumprods(powk)
    pfind.si <- pk[, 1] * powk * cbind(
      rep(1, npk),
      matrixStats::rowCumprods(1 - (pk[, 1] * powk[, 1:maxmiss]))
    )
  }
  return(pfind.si)
}

#' @title Estimate generic g
#'
#' @description Generic g estimation by simulation from given SE model and CP 
#'   models under a specific search schedule.
#'
#' The g estimated by \code{estgGeneric} is a generic aggregate detection 
#'   probability and represents the probability of detecting a carcass that 
#'   arrives at a (uniform) random time during the period monitored, for each
#'   of the possible cell combinations, given the SE and CP models. This 
#'   is somethat different from the GenEst estimation of g when the purpose 
#'   is to estimate total mortality (M), in which case the detection 
#'   probability varies with carcass arrival interval and is difficult to 
#'   summarize statistically. The \code{estgGeneric} estimate is a useful 
#'   "big picture" summary of detection probability, but would be difficult
#'   to work with for estimating M with precision.
#'
#' @param nsim the number of simulation draws
#'
#' @param days Search schedule data as a vector of days searched
#'
#' @param model_SE Searcher Efficiency model (\code{pkm} object)
#'
#' @param model_CP Carcass Persistence model (\code{cpm} object)
#'
#' @param seed_SE seed for random draws of the SE model
#'
#' @param seed_CP seed for random draws of the CP model
#'
#' @return \code{gGeneric} object that is a list of [1] a list of g estimates,
#'    with one element in the list corresponding to each of the cells from the
#'    cross-model combination and [2] a table of predictors and cell names 
#'    associated with the gs
#'
#' @examples
#'   data(mock)
#'   model_SE <- pkm(formula_p = p ~ HabitatType, formula_k = k ~ 1,
#'                 data = mock$SE)
#'   model_CP <- cpm(formula_l = l ~ Visibility, formula_s = s ~ Visibility, 
#'                 data = mock$CP, left = "LastPresentDecimalDays", 
#'                 right = "FirstAbsentDecimalDays")
#'   avgSS <- averageSS(mock$SS)
#'   ghatsGeneric <- estgGeneric(days = avgSS, model_SE = model_SE,
#'    model_CP = model_CP)
#'
#' @export
#'
estgGeneric <- function(days, model_SE, model_CP, nsim = 1000, seed_SE = NULL,
  seed_CP = NULL){

  if (!is.vector(days) || !is.numeric(days))
    stop(" 'days' must be a numeric vector")
  if (!("pkm" %in% class(model_SE))) stop("Invalid pk model")
  vbhat <- diag(model_SE$varbeta)
  if (anyNA(vbhat) || sum(vbhat < 0) > 0)
    stop("Cannot estimate variance for model_SE. Aborting estimation.")
  preds_SE <- model_SE$predictors
  preds_CP <- model_CP$predictors
  data_SE <- model_SE$data
  data_CP <- model_CP$data
  preds <- combinePredsAcrossModels(preds_CP, preds_SE, data_CP, data_SE)
  set.seed(seed_SE)
  sim_SE <- rpk(n = nsim, model = model_SE)
  sim_CP <- rcp(n = nsim, model = model_CP, type = "ppersist")
  dist <- tolower(model_CP$dist)

  ncell <- nrow(preds)
  ghat <- vector("list", ncell)
  for (celli in 1:ncell){
    cell_SE <- preds$CellNames_SE[celli]
    cell_CP <- preds$CellNames_CP[celli]
    param_SE <- sim_SE[[cell_SE]]
    param_CP <- sim_CP[[cell_CP]]
    ghat[[celli]] <- calcg(days, param_SE, param_CP, dist)
  }  
  names(ghat) <- preds$CellNames
  out <- list("ghat" = ghat, "predictors" = preds)
  class(out) <- c("gGeneric", "list")
  return(out)
}

#' @title Calculate cell-level generic detection probability
#'
#' @description Calculate detection probability (g) given SE and CP parameters
#'  and a search schedule.
#'
#' The g given by \code{calcg} is a generic aggregate detection
#'  probability and represents the probability of detecting a carcass that
#'  arrives at a (uniform) random time during the time spanned by the search
#'  schedule for the the given SE and CP parameters. This differs the GenEst
#'  estimation of g when the purpose is to estimate total mortality (M), in
#'  which case the detection probability varies with carcass arrival interval
#'  and is difficult to summarize statistically. \code{calcg} provides a 
#'  useful "big picture" summary of detection probability, but would be 
#'  difficult to work with for estimating M with precision.
#'
#' @param days Search schedule (vector of days searched)
#'
#' @param param_SE numeric array of searcher efficiency parameters (p and k)
#'
#' @param param_CP numeric array of carcass persistence parameters (a and b)
#'
#' @param dist distribution for the CP model
#'
#' @export
#'
calcg <- function(days, param_SE, param_CP, dist){
  samtype <- ifelse(length(unique(diff(days))) == 1, "Formula", "Custom")
  nsearch <- length(days) - 1
  n <- nrow(param_SE)
  if (dist == "exponential"){
    pdb <- param_CP[ , "pdb"]
    pda <- 1/pdb
    pdb0 <- exp(mean(log(pdb)))
    pda0 <- 1/pdb0
  } else {
    pda <- param_CP[ , "pda"]
    pdb <- param_CP[ , "pdb"]
    if (dist %in% c("lognormal", "Lognormal")){
      pdb0 <- mean(pdb)
    } else {
      pdb0 <- exp(mean(log(pdb)))
    }
    pda0 <- 1/mean(1/pda)
  }
  pk <- param_SE
  f0 <- mean(pk[, 1])
  k0 <- mean(pk[, 2])

 ###1. setting estimation control parameters
  ind1 <- rep(1:nsearch, times = nsearch:1)
  ind2 <- ind1 + 1
  ind3 <- unlist(lapply(1:nsearch, function(x) x:nsearch)) + 1
  schedule.index <- cbind(ind1, ind2, ind3)
  schedule <- cbind(days[ind1], days[ind2], days[ind3])

  nmiss <- schedule.index[,3] - schedule.index[,2]
  maxmiss <- max(nmiss)

  powk <- cumprod(c(1, rep(k0, maxmiss))) 
  notfind <- cumprod(1 - f0*powk[-length(powk)])
  nvec <- c(1, notfind) * f0

  # conditional probability of finding a carcass on the ith search (row) after
  # arrival for given (simulated) searcher efficiency (column)
  pfind.si <- nvec * powk

  diffs <- cbind(schedule[,2] - schedule[,1], schedule[,3] - schedule[,2])
  intxsearch <- unique(diffs, MAR = 1)
  ppersu <- ppersist(dist = dist, t_arrive0 = 0, t_arrive1 = intxsearch[,1],
    t_search = intxsearch[,1] + intxsearch[,2], pda = pda0, pdb = pdb0)
  arrvec <- (schedule[,2] - schedule[,1]) / max(days)
  prob_obs <- numeric(dim(schedule)[1])
  for (i in 1:length(prob_obs)){
    ind <- which(
               abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
               abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001
             )
    prob_obs[i] <- pfind.si[nmiss[i] + 1] * ppersu[ind, ] * arrvec[i]
  }

 ###2. estimation of g
 # assumes uniform arrivals
  schedule <- schedule[ind2 >= ind3 - maxmiss + 1, ]
  schedule.index <- cbind(ind1, ind2, ind3)[ind2 >= ind3 - maxmiss + 1,]
  nmiss <- schedule.index[ , 3] - schedule.index[ , 2]
  maxmiss <- max(nmiss)

  if (maxmiss == 0) {
    pfind.si <- pk[ , 1]
  } else if (maxmiss == 1){
    pfind.si <- cbind(pk[ , 1], (1 - pk[ , 1]) * pk[ , 2] * pk[ , 1])
  } else {
    powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(n, maxmiss + 1))
    powk[ , 1] <- 1
    powk <- matrixStats::rowCumprods(powk)
    val <- 1 - (pk[ , 1] * powk[ , 1:maxmiss])
    if (is.null(dim(val))) val <- matrix(val, nrow = 1)
    pfind.si <- pk[ , 1] * powk * cbind(rep(1, n), matrixStats::rowCumprods(val))
  }
  diffs <- cbind(schedule[,2] - schedule[,1], schedule[,3] - schedule[,2])
  intxsearch <- unique(diffs, MAR = 1)
  ppersu <- ppersist(dist = dist, t_arrive0 = 0, t_arrive1 = intxsearch[ , 1],
              t_search = intxsearch[ , 1] + intxsearch[ , 2],
              pda = param_CP[ , 1], pdb = param_CP[ , 2]
            )
  arrvec <- (schedule[ , 2] - schedule[ , 1]) / max(days)
  prob_obs <- numeric(n)
  if (maxmiss > 0){
    for (i in 1:dim(schedule)[1]){
      ind <- which(
               abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
               abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001)
      prob_obs <- prob_obs +
                  pfind.si[ , nmiss[i] + 1] * ppersu[ind, ] * arrvec[i]
    }
  } else {
    for (i in 1:dim(schedule)[1]){
      ind <- which(
               abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
               abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001)
     prob_obs <- prob_obs + pfind.si[nmiss[i] + 1] * ppersu[ind, ] * arrvec[i]
    }
  }
  return(prob_obs)
}

#' @title Estimate generic detection probability for multiple size classes
#'
#' @description Generic g estimation for a combination of SE model and CP
#'   model under a given search schedule
#'
#' The g estimated by \code{estgGenericSize} is a generic aggregate detection
#'   probability and represents the probability of detecting a carcass that 
#'   arrives at a (uniform) random time during the period monitored, for each
#'   of the possible cell combinations, given the SE and CP models. This 
#'   is somethat different from the GenEst estimation of g when the purpose 
#'   is to estimate total mortality (M), in which case the detection 
#'   probability varies with carcass arrival interval and is difficult to 
#'   summarize statistically. The \code{estgGeneric} estimate is a useful 
#'   "big picture" summary of detection probability, but would be difficult
#'   to work with for estimating M with precision.
#'
#' @param nsim the number of simulation draws
#'
#' @param days Search schedule data as a vector of days searched 
#'
#' @param modelSetSize_SE Searcher Efficiency model set for multiple sizes
#'
#' @param modelSetSize_CP Carcass Persistence model set for multiple sizes
#'
#' @param modelSizeSelections_SE vector of SE models to use, one for each 
#'  size. Size names are required, and names must match those of
#'  modelSetSize_SE. E.g., 
#'  \code{c(lrg = "p ~ Visibility; k ~ 1", sml = "p ~ 1; k ~ 1")}.
#'  Model formulas are read as text and must have exact matches among models
#'  listed in modelSetSize_SE. For example, if one of the
#'  \code{modelSizeSelections_SE} elements is
#'  \code{lrg = "p ~ Visibility; k ~ 1"}, then \code{"p ~ Visibility; k ~ 1"}
#'  must be in \code{names(modelSizeSelections_SE)[["lrg"]]}.
#'
#' @param modelSizeSelections_CP vector of CP models to use, one for each size
#'
#' @param seed_SE seed for random draws of the SE model
#'
#' @param seed_CP seed for random draws of the CP model
#'
#' @return list of g estimates, with one element in the list corresponding
#'    to each of the cells from the cross-model combination
#'
#' @examples
#'   data(mock)
#'   pkmModsSize <- pkm(formula_p = p ~ HabitatType,
#'                    formula_k = k ~ HabitatType, data = mock$SE,
#'                    obsCol = c("Search1", "Search2", "Search3", "Search4"),
#'                    sizeCol = "Size", allCombos = TRUE)
#'   cpmModsSize <- cpm(formula_l = l ~ Visibility,
#'                    formula_s = s ~ Visibility, data = mock$CP,
#'                    left = "LastPresentDecimalDays",
#'                    right = "FirstAbsentDecimalDays",
#'                    dist = c("exponential", "lognormal"),
#'                    sizeCol = "Size", allCombos = TRUE)
#'
#'   pkMods <- c("S" = "p ~ 1; k ~ 1", "L" = "p ~ 1; k ~ 1",
#'              "M" = "p ~ 1; k ~ 1", "XL" = "p ~ 1; k ~ 1"
#'             )
#'   cpMods <- c("S" = "dist: exponential; l ~ 1; NULL", 
#'               "L" = "dist: exponential; l ~ 1; NULL",
#'               "M" = "dist: exponential; l ~ 1; NULL",
#'               "XL" = "dist: exponential; l ~ 1; NULL"
#'             )
#'   avgSS <- averageSS(mock$SS)
#'   gsGeneric <- estgGenericSize(nsim = 1000, days = avgSS,
#'                  modelSetSize_SE = pkmModsSize,
#'                  modelSetSize_CP = cpmModsSize,
#'                  modelSizeSelections_SE = pkMods,
#'                  modelSizeSelections_CP = cpMods
#'                )
#'
#' @export
#'
estgGenericSize <- function(days, modelSetSize_SE, modelSetSize_CP,
    modelSizeSelections_SE, modelSizeSelections_CP,
    nsim = 1000, seed_SE = NULL, seed_CP = NULL){

  if (!("pkmSetSize" %in% class(modelSetSize_SE))){
    stop("modelSetSize_SE must be a pkmSetSize object")
  }
  if (!("cpmSetSize" %in% class(modelSetSize_CP))){
    stop("modelSetSize_CP must be a cpmSetSize object")
  }
  sizeclasses_SE <- names(modelSetSize_SE)
  sizeclasses_CP <- names(modelSetSize_CP)
  if (!all(sizeclasses_SE %in% sizeclasses_CP) ||
      !all(sizeclasses_CP %in% sizeclasses_SE)){
    stop("Size classes don't match between SE and CP model sets")
  }
  sizeclasses <- unique(c(sizeclasses_SE, sizeclasses_CP))
  nsizeclass <- length(sizeclasses)
  # check whether k is included in every model. If not, error.
  for (sci in 1:nsizeclass){
    if (modelSetSize_SE[[sci]][[modelSizeSelections_SE[sci]]]$pOnly){
     stop("k required for SE model for size = ", sci)
    }
  }
  ghats <- list()
  for (sci in sizeclasses){
    if (any(unlist(lapply(modelSetSize_SE[[sci]], function(x) x$pOnly))))
      stop("No k included in SE model. Cannot estimate g")
    model_SEsci <- modelSizeSelections_SE[[sci]]
    model_SE <- modelSetSize_SE[[sci]][[model_SEsci]]
    model_CPsci <- modelSizeSelections_CP[[sci]]
    model_CP <- modelSetSize_CP[[sci]][[model_CPsci]]
    ghats[[sci]] <- estgGeneric(nsim = nsim, days = days,
      model_SE = model_SE, model_CP = model_CP,
      seed_SE = seed_SE, seed_CP = seed_CP)
  }

  class(ghats) <- c("gGenericSize", "list")
  return(ghats)
}

#' @title Tabulate an average search schedule from a multi-unit SS data table
#'
#' @description Given a multi-unit Search Schedule data table, produce an 
#'   average search schedule for use in generic detection probability 
#'   estimation. 
#'
#' @param data_SS a multi-unit SS data table, for which the average interval 
#'   will be tabulated. It is assumed that \code{data_SS} is properly 
#'   formatted, with a column of search dates and a column of 1s and 0s for 
#'   each unit indicating whether the unit was searched on the given date).
#'   Other columns are optional, but optional columns should not all contain
#'   at least on value that is not a 1 or 0.
#'
#' @param SSdate Column name for the date searched data (optional).
#'   if no \code{SSdate} is provided, \code{data_SS} will be parsed
#'   to extract the dates automatically. If there is more than one column with
#'   dates, then an error will be thrown and the user will be required to
#'   provide the name of the desired dates column.
#'
#' @return vector of the average search schedule
#'
#' @examples 
#'   data(mock)
#'   avgSS <- averageSS(mock$SS)
#'
#' @export
#'
averageSS <- function(data_SS, SSdate = NULL){
  SSdat <- prepSS(data_SS, SSdate = SSdate)
  schedules <- t(SSdat$searches_unit) * SSdat$days
  nintervals <- length(SSdat$days) - matrixStats::colCounts(schedules, value = 0)
  maxdays <- matrixStats::colMaxs(schedules)
  aveSS <- seq(0, max(maxdays), round(mean(maxdays/nintervals)))
  return(aveSS)
}
  
#' @title Summarize the gGeneric list to a simple table
#'
#' @description methods for \code{summary} applied to a \code{gGeneric} list
#'
#' @param object gGeneric output list (each element is a named vector of 
#'   gGeneric values for a cell in the model combinations)
#'
#' @param ... arguments to be passed down
#'
#' @param CL confidence level
#'
#' @return a summary table of g values (medians and confidence bounds) for 
#'   each cell combination within the gGeneric list
#'
#' @examples 
#'   data(mock)
#'   model_SE <- pkm(formula_p = p ~ HabitatType, formula_k = k ~ 1,
#'                 data = mock$SE)
#'   model_CP <- cpm(formula_l = l ~ Visibility, formula_s = s ~ Visibility, 
#'                 data = mock$CP, left = "LastPresentDecimalDays", 
#'                 right = "FirstAbsentDecimalDays")
#'   avgSS <- averageSS(mock$SS)
#'   ghatsGeneric <- estgGeneric(nsim = 1000, avgSS, model_SE, model_CP,
#'                     seed_SE = 1, seed_CP = 1)
#'   summary(ghatsGeneric)
#'
#' @export
#'
summary.gGeneric <- function(object, ..., CL = 0.90){
  ghats <- object$ghat
  preds <- object$predictors
  cells <- names(ghats)
  ncell <- length(cells)
  predsByCell <- strsplit(cells, "\\.") 
  npred <- length(predsByCell[[1]])

  predsTab <- preds[ , -grep("CellNames", colnames(preds))]
  predsTab <- as.matrix(predsTab, ncol = npred, nrow = ncell)
  predNames <- colnames(preds)[-grep("CellNames", colnames(preds))]
  if (length(predNames) == 1 & predNames[1] == "group" & cells[1] == "all"){
    predNames <- "Group"
  }

  tableProbs <- c((1 - CL) / 2, 0.25, 0.5, 0.75, 1 - (1 - CL) / 2)

  colnames(predsTab) <- predNames
  gTab <- matrix(NA, ncell, 5)
  for (celli in 1:ncell){
    gspec <- ghats[[celli]]
    quants <- quantile(gspec, prob = tableProbs)
    gTab[celli, ] <- round(quants, 3)
  }

  out <- data.frame(predsTab, gTab)
  colnames(out)[npred + (1:5)] <- names(quants)
  return(out)
}

#' @title Summarize the gGenericSize list to a list of simple tables
#'
#' @description methods for \code{summary} applied to a \code{gGenericSize}
#'   list
#'
#' @param object gGenericSize output list (each element is a size-named 
#'   list of named vectors of gGeneric values for a cell in the model 
#'   combinations)
#'
#' @param ... arguments to be passed down
#'
#' @param CL confidence level
#'
#' @return a list of summary tables of g values (medians and confidence 
#'   bounds) for each cell combination within the gGeneric list
#'
#' @examples
#'   data(mock)
#'   pkmModsSize <- pkm(formula_p = p ~ HabitatType,
#'                    formula_k = k ~ HabitatType, data = mock$SE,
#'                    obsCol = c("Search1", "Search2", "Search3", "Search4"),
#'                    sizeCol = "Size", allCombos = TRUE)
#'   cpmModsSize <- cpm(formula_l = l ~ Visibility,
#'                    formula_s = s ~ Visibility, data = mock$CP,
#'                    left = "LastPresentDecimalDays",
#'                    right = "FirstAbsentDecimalDays",
#'                    dist = c("exponential", "lognormal"),
#'                    sizeCol = "Size", allCombos = TRUE)
#'   pkMods <- c("S" = "p ~ 1; k ~ 1", "L" = "p ~ 1; k ~ 1",
#'              "M" = "p ~ 1; k ~ 1", "XL" = "p ~ 1; k ~ 1"
#'             )
#'   cpMods <- c("S" = "dist: exponential; l ~ 1; NULL", 
#'               "L" = "dist: exponential; l ~ 1; NULL",
#'               "M" = "dist: exponential; l ~ 1; NULL",
#'               "XL" = "dist: exponential; l ~ 1; NULL"
#'             )
#'   avgSS <- averageSS(mock$SS)
#'   gsGeneric <- estgGenericSize(nsim = 1000, days = avgSS,
#'                  modelSetSize_SE = pkmModsSize,
#'                  modelSetSize_CP = cpmModsSize,
#'                  modelSizeSelections_SE = pkMods,
#'                  modelSizeSelections_CP = cpMods
#'                )
#'  summary(gsGeneric)
#'
#' @export
#'
summary.gGenericSize <- function(object, ..., CL = 0.90){

  nsizeclass <- length(object)
  out <- vector("list", length = nsizeclass)
  names(out) <- names(object)
  for (sci in 1:nsizeclass){
    out[[sci]] <- summary(object[[sci]], ..., CL = CL)
  }
  return(out)
}

#' @title Create search schedule data into an prepSS object for convenient 
#'  splits analyses
#'
#' @description Since data_SS columns largely have a specific, required
#'   format, the \code{prepSS} function can often automatically decipher the
#'   data, but the user may specify explicit instructions for parsing the data
#'   for safety if desired. If the data are formatted properly, the automatic
#'   parsing is reliable in most cases. There are two exceptions. (1) If
#'   there is more than one column with possible dates (formatted as formal
#'   dates (as class \code{Date}, \code{POSIXlt} or \code{POSIXct}) or
#'   character strings or factors that can be unambiguously interpreted as
#'   dates (with assumed format "2018-05-15" or "2018/5/15"). In that case,
#'   the user must specify the desired dates as \code{dateColumn}. (2) If
#'   there is a covariate column consisting entirely of 0s and 1s. In that
#'   case, the user must specify the column(s) in \code{covars}.
#'
#' @param data_SS data frame or matrix with search schedule parameters,
#'  including columns for search dates, covariates (describing characteristics
#'  of the search intervals), and each unit (with 1s and 0s to indicate 
#'  whether the given unit was searched (= 1) or not (= 0) on the given date)
#'
#' @param SSdate name of the column with the search dates in it
#'  (optional). If no \code{SSdate} is given, \code{prepSS} will
#'  try to find the date column based on data formats. If there is exactly one
#'  column that can be interpreted as dates, that column will be taken as the
#'  dates searched. If more than one date column is found, \code{prepSS} exits
#'  with an error message.
#'
#' @param preds vector of character strings giving the names of columns to be
#'  interpreted as potential covariates (optional). Typically, it is not
#'  necessary for a user to provide a value for \code{preds}. It is used only
#'  to identify specific columns of 1s and 0s as covariates rather than as
#'  search schedules.
#'
#' @return \code{prepSS} object that can be conveniently used in the splitting
#'  functions.
#'
#' @examples
#'  data(mock)
#'  prepSS(mock$SS)
#'
#' @export
#'
prepSS <- function(data_SS, SSdate = NULL, preds = NULL){
  if ("prepSS" %in% class(data_SS)) return(data_SS)
  if (length(intersect(class(data_SS), c("data.frame", "matrix"))) == 0){
    stop("data_SS must be a data frame or matrix")
  } else if (is.null(colnames(data_SS))){
    stop("data_SS columns must be named")
  }
  # if SSdate not provided, extract search dates (if possible)
  if (is.null(SSdate)){
    for (coli in colnames(data_SS)){
      tmp <- checkDate(data_SS[, coli])
      if (!is.null(tmp)){
        if (!is.null(SSdate)){
          stop(
            "more than 1 date column in data_SS, and ",
            "SSdate does not specify which to use."
          )
        } else {
          SSdate <- coli
          dates <- tmp
        }
      }
    }
    if (is.null(SSdate)){
      stop("no columns can be interpreted as dates")
    }
  } else {
    if (length(SSdate) > 1 || !is.character(SSdate)){
      stop("'SSdate' must be NULL or the name of a single column")
    }
    dates <- checkDate(data_SS[, SSdate])
    if (is.null(dates)){
      stop(paste(SSdate, "is not properly formatted as dates"))
    }
  }
  # extract units
  unitNames <- NULL
  preds <- SSdate
  for (coli in colnames(data_SS)){
    if (coli %in% preds) next
    if (!is.numeric(data_SS[, coli])){
      preds <- c(preds, coli)
      next
    }
    if (sum(!(data_SS[, coli] %in% 0:1)) > 0){
      preds <- c(preds, coli)
      next
    } else {
      unitNames <- c(unitNames, coli)
    }
  }
  if (grepl("-",paste(unitNames, collapse = ''))){
    stop("Unit names must not contain hyphens ( - )")
  }
  date0 <- min(dates)
  ans <- list()
  ans$date0 <- date0
  ans$days <- as.numeric(difftime(dates, date0, units = "days"))
  if (any(diff(ans$days) <= 0)){
    stop("search dates must be in increasing order")
  }
  ans[[SSdate]] <- dates
  for (i in 1:length(preds)){
    if (preds[i] == SSdate) next
    if (is.factor(data_SS[, preds[i]])){
      ans[[preds[i]]] <- as.character(data_SS[,preds[i]])
    } else {
      ans[[preds[i]]] <- data_SS[,preds[i]]
    }
  }
  ans$searches_unit <- t(as.matrix(data_SS[, unitNames]))
  ans$unit <- unitNames
  class(ans) <- c("prepSS", "list")
  return(ans)
}