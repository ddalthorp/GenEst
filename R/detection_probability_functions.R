#' Estimate g values and arrival intervals for a set of carcasses
#'   from fitted pk and cp models and search data
#'
#' @param nsim the number of simulation draws
#' @param data_CO Carcass Observation data
#' @param data_SS Search Schedule data
#' @param model_SE Searcher Efficiency model (or list of models if there are
#'   multiple size classes)
#' @param model_CP Carcass Persistence model (or list of models if there are
#'   multiple size classes)
#' @param seed_SE seed for random draws of the SE model
#' @param seed_CP seed for random draws of the CP model
#' @param seed_g seed for random draws of the gs
#' @param kFill value(s) to fill in for missing k when not existing in the
#'   model(s)
#' @param unitCol Column name for the unit indicator
#' @param dateFoundCol Column name for the date found data
#' @param datesSearchedCol Column name for the date searched data
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'   are recorded
#' @param cleanoutCarcs of which carcasses (if any) were found on cleanout
#'   searches
#' @param max_intervals maximum number of arrival interval intervals to consider
#'  for each carcass
#' @return list of [1] g estimates, [2] arrival interval (Aj) estimates,
#'   [3]  estimates of SE parameters (pk), and [4] estimates CP parameters
#'   (ab) for each of the carcasses. The row names of the Aj matrix are the
#'   names of the units where each carcass was found.
#' @examples NA
#' @export

estg <- function(nsim = nsim, data_CO, data_SS, model_SE, model_CP,
                    seed_SE = NULL, seed_CP = NULL, seed_g = NULL,
                    kFill = NULL, unitCol = "Unit",
                    dateFoundCol = "DateFound",
                    dateSearchedCol = "DateSearched", sizeclassCol = NULL,
                    cleanoutCarcs = NULL, max_intervals = NULL){
# sizeclassCol not only identifies what the name of the size column is, it
# also identifies that the model should include size as a segregating class
  sizeCol <- sizeclassCol
  SSdat <- SS(data_SS)
  SSdat$searches_unit[ , 1] <- 1 # set t0 as start of period of inference
  t0date <- SSdat$date0
  COdat <- data_CO
  COdat[ , dateFoundCol] <- dateToDay(COdat[ , dateFoundCol], t0date)
  names(COdat)[names(COdat) == dateFoundCol] <- "day" # to distinguish integers
  if (is.null(sizeCol)){
    sizeCol <- "placeholder"
    COdat[ , sizeCol] <- "value"
    model_SE <- list("value" = model_SE)
    model_CP <- list("value" = model_CP)
    if (!is.null(kFill)) names(kFill) <- "value"
  } else {
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not in carcass data.")
    }
  }
  sizeclass <- as.list(as.character(COdat[, sizeCol]))
  sizeclasses <- unique(unlist(sizeclass))
  nsizeclass <- length(sizeclasses)

# step 3: create lists of arrays for SS, SE cells, CP cells for each carcass
  for (sci in 1:nsizeclass){
    sc <- sizeclasses[sci]
    if (is.na(model_SE[[sc]]$cellwiseTable[1, "k_median"])){
      if (is.null(kFill)){
        kFill[[sc]] <- kFillPropose(model_SE[[sc]])
        if (is.na(kFill[[sc]])){
          stop(
            "Searcher efficiency model does not include estimate ",
            "for k and kFill was not specified."
          )
        }
      }
    }
  }
  SE_preds <- lapply(model_SE, function(x) x$predictors)
  CP_preds <- lapply(model_CP, function(x) x$predictors)
  preds <- mapply(function(x, y) unique(c(x, y)), SE_preds, CP_preds)
  SE_data <- lapply(model_SE, function(x) x$data)
  CP_data <- lapply(model_CP, function(x) x$data)
  dist <- unlist(lapply(model_CP, function(x) x$dist))
  COpreds <- lapply(preds, function(x) x[x %in% names(data_CO)])
  SSpreds <- lapply(preds, function(x) x[!(x %in% names(data_CO))])
  if (max(unlist(lapply(SSpreds, length))) > 1){
    stop("At most 1 SS predictor is allowed per size class.")
  }
  if (!all(unlist(SSpreds) %in% names(SSdat))){
    stop("Model predictor missing from both CO and SS data.")
  }
  if (!is.null(kFill)) {
    k <- as.list(kFill)
    pksim <- mapply(function(x, y) rpk(nsim, x, seed_SE, kFill = y), model_SE, k)
  } else {
    pksim <- lapply(model_SE, function(x) rpk(nsim, x, seed_SE))
  }
  cpsim <- lapply(model_CP, function(x) rcp(nsim, x, seed_CP, typ = "ppersist"))

  X <- dim(COdat)[1]
  days <- list()
  for (xi in 1:X){
    toi <- COdat$day[xi]
    SSxi <- SSdat$searches_unit[COdat[xi, unitCol], ] * SSdat$days
    SSxi <- c(0, SSxi[SSxi > 0])
    days[[xi]] <- SSxi[SSxi <= toi]
    if (!is.null(max_intervals)){
      dlen <- length(days[[xi]])
      if (dlen > max_intervals + 1) days[[xi]] <- days[[xi]][(dlen - max_intervals):dlen]
    }
  }

  cells <- list()
  # take care of the SS preds
  if (sum(unlist(lapply(SSpreds, length))) == 0){
    for (xi in 1:X){
      cells[[xi]] <- list()
      cells[[xi]][[sizeCol]] <- "value"
      pcol <- SE_preds[[COdat[xi, sizeCol]]]
      if (length(pcol) == 0) {
        cells[[xi]]$SEcell <- "all"
      } else {
        cells[[xi]]$SEcell <- paste(COdat[xi, pcol], sep = ".")
      }
      cells[[xi]]$SErep <- length(days[[xi]]) - 1
      pcol <- CP_preds[[COdat[xi, sizeCol]]]
      if (length(pcol) == 0) {
        cells[[xi]]$CPcell <- "all"
      } else {
        cells[[xi]]$CPcell <- paste(COdat[xi, pcol], sep = ".")
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
      nse <- length(SE_preds[[sz]])
      if (nse == 0){
        cells[[xi]]$SEcell <- "all"
        cells[[xi]]$SErep <- length(days[[xi]]) - 1
      } else {
        for (sei in 1:nse){
          predi <- SE_preds[[sz]][sei]
          if (predi %in% SSpreds){
            ssvec <- (SSdat[[predi]][which(SSdat[["days"]] %in% days[[xi]])])[-1]
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
      nse <- length(CP_preds[[sz]])
      if (nse == 0){
        cells[[xi]]$CPcell <- "all"
        cells[[xi]]$CPrep <- length(days[[xi]]) - 1
      } else {
        for (sei in 1:nse){
          predi <- CP_preds[[sz]][sei]
          if (predi %in% SSpreds){
            ssvec <- (SSdat[[predi]][which(SSdat[["days"]] %in% days[[xi]])])[-1]
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
    if (COdat$day[xi] == 0) next # cleanout

    SSxi <- SSdat$searches_unit[COdat[xi, unitCol], ] * SSdat$days
    SSxi <- c(0, SSxi[SSxi > 0])
    # calculate SE
    sz <- cells[[xi]][[sizeCol]]
    SEr <- cells[[xi]]$SErep
    oi <- length(days[[xi]]) - 1
    rng <- 0
    pOigAj <- NULL
    for (sei in 1:length(SEr)){
      rng <- max(rng) + 1:SEr[sei]
      pOigAj <- cbind(pOigAj, SEsi_left(
        oi = oi,
        pk = pksim[[sz]][[cells[[xi]]$SEcell[sei]]],
        rng = rng
      ))
    }
    # multiply by ppersist
    for (cpi in 1:length(cells[[xi]]$CPrep)){
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
    }
    parrive <- diff(days[[xi]][1:(oi+1)])/days[[xi]][oi+1]
    pAjgOi <- t(pOigAj) * parrive; pAjgOi <- t(t(pAjgOi)/colSums(pAjgOi))
    Aj[xi, ] <- # simulated arrival intervals (relative to cind's ss)
       rowSums(matrixStats::rowCumsums(t(pAjgOi)) < runif(nsim)) +
         (sum(SSxi <= min(days[[xi]])))
##########
    xuint <- unique(Aj[xi, ]) # unique x intervals
    for (aj in xuint){
      # calculate simulated ghat associated with the given carcass and interval
      simInd <- which(Aj[xi, ] == aj)
      top <- length(SSxi)
      if (!is.null(max_intervals)){
        top <- min(aj + max_intervals, top)
      }
      # use an adjusted search schedule because we "know" when carcass arrived
      cpi <- findInterval(aj, c(0, cumsum(cells[[xi]]$CPrep)), rightmost = T)
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
      pki <- findInterval(aj, c(0, cumsum(cells[[xi]]$SErep)), rightmost = T)
      SE <- t(SEsi_right(
        min(max_intervals, length(SSxi) - aj),
        pksim[[sz]][[cells[[xi]]$SEcell[pki]]][simInd , ]
      ))
      if (aj < top - 1){
        ghat[xi, simInd] <- colSums(SE * ppersu)
      } else {
        ghat[xi, simInd] <- as.vector(SE) * as.vector(ppersu)
      }
    }
  }
  rownames(Aj) <- data_CO[ , unitCol]
  out <- list("ghat" = ghat, "Aj" = Aj, "pk" = pksim, "ab" = cpsim)
  return(out)
}
#' Calculate the conditional probability of observing a carcass at search oi
#' as a function arrival interval (assuming carcass is not removed by scavengers
#' before the time of the final search)
#'   
#' @param oi number of searches after arrival
#' @param pk numeric array of searcher efficiency p and k parameters
#'  (p = pk[ , 1] and k = pk[ , 2])
#' @param rng optional parameter giving the range of intervals to consider
#' @return numeric array of probability of observing a carcass at oi for
#'  given that it arrived in intervals 1:oi if rng = NULL (or in intervals rng),
#'  assuming the carcass had not been previously discovered or removed by
#'  scavengers
#' @examples NA
#' @export
SEsi_left <- function (oi, pk, rng = NULL){
  # oi is the index for the search occasion (excluding t0)
  # pk is nsim x 2 array of simulated p and k parameters
  # rng is the intervals for which to calculate answer
  if (is.null(rng)) rng <- 1:oi
  if (is.null(dim(pk)) || nrow(pk) == 1) return(SEsi0(days, pk))
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

#' Calculate the conditional probability of observing a carcass after i = 1:nsi
#' searches (assuming carcass is not previous discovered by searchers or removed
#' by scavengers)
#'
#' @param nsi number of searches after arrival
#' @param pk numeric array of searcher efficiency p and k parameters
#'  (p = pk[ , 1] and k = pk[ , 2])
#' @return numeric nsi x dim(pk)[1] array of probabilities of observing a
#'  carcass after 1:nsi searches (assuming that the carcass had not been
#' previously discovered or removed by scavengers
#' @export
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

#' Propose a k value if it is not in the model table
#'
#' @description Based on the call to the pkm function
#'
#' @param model searcher efficiency model
#' @return proposed k value
#' @examples NA
#' @export 
#'
kFillPropose <- function(model){

  proposal <- model$cellwiseTable[1, c("k_median", "k_lower", "k_upper")]
  if (any(is.na(proposal))){
    return(NA)
  }
  if (proposal[1] == proposal[2] & proposal[1] == proposal[3]){
    proposal <- proposal[1]
  } else{
    proposal <- NULL
  }
  return(proposal)
}

#' Generic g estimation for a combination of SE model and CP model
#'   under a given search schedule
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
#' @param n the number of simulation draws
#' @param data_SS Search schedule data as a vector of days searched 
#' @param model_SE Searcher Efficiency model
#' @param model_CP Carcass Persistence model
#' @param seed_SE seed for random draws of the SE model
#' @param seed_CP seed for random draws of the CP model
#' @param kFill value to fill in for missing k when not existing in the model
#' @return gGeneric object that is a list of [1] a list of g estimates,
#'    with one element in the list corresponding to each of the cells from the 
#'    cross-model combination and [2] a table of predictors and cell names 
#'    associated with the gs
#' @export
#'
estgGeneric <- function(n = 1, data_SS, model_SE, model_CP, seed_SE = NULL,
                         seed_CP = NULL, kFill = NULL){
  
  if (is.vector(data_SS)){
    SS <- data_SS
  } else{
    msg <- paste0(class(data_SS), " is not a supported data type for data_SS")
    stop(msg)
  }

  if (is.na(model_SE$cellwiseTable[1, "k_median"])){
    if (is.null(kFill)){
      kFill <- kFillPropose(model_SE)
      if (is.na(kFill)){
        msg <- paste("Searcher efficiency model does not include estimate ",
                 "for k and kFill was not specified.", sep = "")
        stop(msg)
      }
    }
  }
  preds_SE <- model_SE$predictors
  preds_CP <- model_CP$predictors
  data_SE <- model_SE$data
  data_CP <- model_CP$data
  preds <- combinePredsAcrossModels(preds_CP, preds_SE, data_CP, data_SE)

  sim_SE <- rpk(n, model_SE, seed_SE, kFill)
  sim_CP <- rcp(n, model_CP, seed_CP, type = "ppersist") 
  dist <- tolower(model_CP$dist)

  ncell <- nrow(preds)
  ghat <- vector("list", ncell)
  for (celli in 1:ncell){
    cell_SE <- preds$CellNames_SE[celli]
    cell_CP <- preds$CellNames_CP[celli]
    param_SE <- sim_SE[[cell_SE]]
    param_CP <- sim_CP[[cell_CP]]
    ghat[[celli]] <- estgGenericCell(SS, param_SE, param_CP, dist, kFill)
  }  
  names(ghat) <- preds$CellNames
  out <- list("ghat" = ghat, "predictors" = preds)
  class(out) <- c("gGeneric", "list")
  return(out)
}

#' Generic g estimation for a single cell of a combination of SE model and 
#'   CP model under a given search schedule
#'
#' The g estimated by \code{gGenericCell} is a generic aggregate detection 
#'   probability and represents the probability of detecting a carcass that 
#'   arrives at a (uniform) random time during the period monitored, for one 
#'   cell across both the SE and CP models. This is somethat different from 
#'   the GenEst estimation of g when the purpose is to estimate total
#'   mortality (M), in which case the detection probability varies with 
#'   carcass arrival interval and is difficult to summarize statistically.
#'   The \code{gGenericCell} estimate is a useful "big picture" summary
#'   of detection probability, but would be difficult to work with for 
#'   estimating M with precision.
#'
#' @param SS Search schedule (vector of days searched)
#' @param param_SE Searcher efficiency parameters (p and k
#' @param param_CP Carcass persistence parameters (a and b)
#' @param dist distribution for the CP model
#' @param kFill value to fill in for missing k when not existing in the model
#'
#' @export
#'
estgGenericCell <- function(SS, param_SE, param_CP, dist, kFill){

  samtype <- ifelse(length(unique(diff(SS))) == 1, "Formula", "Custom")
  nsearch <- length(SS) - 1
  n <- nrow(param_SE)

  if (dist == "exponential"){
    pdb <- param_CP[ , "pdb"]

    pda <- 1/pdb
    pdb0 <- exp(mean(log(pdb)))
    pda0 <- 1/pdb0
  } else {

    pda <- param_CP[ , "pda"]
    pdb <- param_CP[ , "pdb"]
    pdb0 <- exp(mean(log(pdb)))
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
  schedule <- cbind(SS[ind1], SS[ind2], SS[ind3])

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
  ppersu <- ppersist(dist, t_arrive0 = 0, t_arrive1 = intxsearch[,1],
              t_search = intxsearch[,1] + intxsearch[,2],
               pda = pda0, pdb = pdb0
            )
  arrvec <- (schedule[,2] - schedule[,1]) / max(SS)
  prob_obs <- numeric(dim(schedule)[1])
  for (i in 1:length(prob_obs)){
    match <- which(
               abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
               abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001
             )
    prob_obs[i] <- pfind.si[nmiss[i] + 1] * ppersu[match, ] * arrvec[i]
  }
  ggnm <- numeric(maxmiss + 1)
  for (missi in 0:maxmiss){
    ggnm[missi + 1] <- sum(prob_obs[nmiss == missi])
  } 

  # if more than 10 searches, truncate search schedule 
  # Correct bias by multiplying the final g's by gadj = sum(ggnm)/ggnm[iskip]

  if (nsearch > 10){
    iskip <- min(which(cumsum(ggnm)/sum(ggnm) > 0.99)) + 1

    gadj <- sum(ggnm)/sum(ggnm[1:iskip])
  } else {
    iskip <- maxmiss
    gadj <- 1
  }

 ###2. estimation of g
 # assumes uniform arrivals

  schedule <- schedule[ind2 >= ind3 - iskip + 1, ]
  schedule.index <- cbind(ind1, ind2, ind3)[ind2 >= ind3 - iskip + 1,]
  nmiss <- schedule.index[ , 3] - schedule.index[ , 2]
  maxmiss <- max(nmiss)

  if (maxmiss == 0) {
    pfind.si <- pk[ , 1]
  } else if (maxmiss == 1){
    pfind.si <- cbind(pk[ , 1], (1 - pk[ , 1]) * pk[ , 2] * pk[ , 1])
  } else {
    powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(n, maxmiss + 1))
    powk[ , 1] <- 1
    powk <- rowCumprods(powk)
    val <- 1 - (pk[ , 1] * powk[ , 1:maxmiss])
    if (is.null(dim(val))){
      val <- matrix(val, nrow = 1)
    }
    pfind.si <- pk[ , 1] * powk * 
                cbind(rep(1, n), rowCumprods(val))
  }
  diffs <- cbind(schedule[,2] - schedule[,1], schedule[,3] - schedule[,2])
  intxsearch <- unique(diffs, MAR = 1)
  ppersu <- ppersist(dist, t_arrive0 = 0, t_arrive1 = intxsearch[ , 1],
              t_search = intxsearch[ , 1] + intxsearch[ , 2],
              pda = param_CP[ , 1], pdb = param_CP[ , 2]
            )

  arrvec <- (schedule[ , 2] - schedule[ , 1]) / max(SS) 

  prob_obs <- numeric(n)

  if (maxmiss > 0){
    for (i in 1:dim(schedule)[1]){
      match <- which(
               abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
               abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001)
      prob_obs <- prob_obs +
                  pfind.si[ , nmiss[i] + 1] * ppersu[match, ] * arrvec[i]
    }
  } else {
    for (i in 1:dim(schedule)[1]){
      match <- which(
               abs(intxsearch[,1] - (schedule[i,2] - schedule[i,1])) < 0.001 &
               abs(intxsearch[,2] - (schedule[i,3] - schedule[i,2])) < 0.001)
      prob_obs <- prob_obs +
                  pfind.si[nmiss[i] + 1] * ppersu[match, ] * arrvec[i]
    }
  }

  return(prob_obs)
}

#' Generic g estimation for a combination of SE model and CP model 
#'   under a given search schedule
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
#' @param n the number of simulation draws
#' @param data_SS Search schedule data as a vector of days searched 
#' @param modelSetSize_SE Searcher Efficiency model set for multiple sizes
#' @param modelSetSize_CP Carcass Persistence model set for multiple sizes
#' @param modelSizeSelections_SE vector of SE models to use, one for each size
#' @param modelSizeSelections_CP vector of CP models to use, one for each size
#' @param seed_SE seed for random draws of the SE model
#' @param seed_CP seed for random draws of the CP model
#' @param kFill values to fill in for missing k when not existing in the model
#' @return list of g estimates, with one element in the list corresponding
#'   to each of the cells from the cross-model combination
#' @export
#'
estgGenericSize <- function(n = 1, data_SS, modelSetSize_SE, 
                               modelSetSize_CP, modelSizeSelections_SE, 
                               modelSizeSelections_CP, seed_SE = NULL, 
                               seed_CP = NULL, kFill = NULL){

  sizeclasses_SE <- names(modelSetSize_SE)
  sizeclasses_CP <- names(modelSetSize_CP)
  if (!all(sizeclasses_SE %in% sizeclasses_CP) | 
      !all(sizeclasses_CP %in% sizeclasses_SE)){
    stop("Size classes don't match between SE and CP model sets")
  }
  sizeclasses <- unique(c(sizeclasses_SE, sizeclasses_CP))
  nsizeclass <- length(sizeclasses)
  ghats <- vector("list", length = nsizeclass)
  for (sci in 1:nsizeclass){
    sc <- sizeclasses[sci]
    model_SEsci <- modelSizeSelections_SE[[sc]]
    model_SE <- modelSetSize_SE[[sc]][[model_SEsci]]
    model_CPsci <- modelSizeSelections_CP[[sc]]
    model_CP <- modelSetSize_CP[[sc]][[model_CPsci]]
    ghats[[sci]] <- estgGeneric(n, data_SS, model_SE, model_CP, 
                      seed_SE, seed_CP, kFill[sc]
                    )
  }
  names(ghats) <- sizeclasses
  class(ghats) <- c("gGenericSize", "list")

  return(ghats)
}

#' Tabulate an average search schedule from a multi-unit SS data table
#'
#' @param data_SS a multi-unit SS data table, for which the average interval 
#'   will be tabulated
#' @param datesSearchedCol Column name for the date searched data
#' @param unitPrefix prefix for the unit identifiers, which correspond to 
#'   columns in \code{data_ss}
#' @return vector of the average search schedule
#' @export
#'
averageSS <- function(data_SS, datesSearchedCol = "DateSearched", 
                      unitPrefix = "Unit"){

  if (!datesSearchedCol %in% colnames(data_SS)){
    stop("date searched column name provided not present in data.")
  }
  SS <- data_SS
  SS[ , datesSearchedCol] <- yyyymmdd(SS[ , datesSearchedCol])
  date1 <- min(SS[ , datesSearchedCol])
  SS[ , datesSearchedCol] <- dateToDay(SS[ , datesSearchedCol], date1)
  SScols <- colnames(SS)
  prefixLength <- nchar(unitPrefix)
  SScolPrefixes <- substr(SScols, 1, prefixLength)
  
  units <- colnames(SS)[which(SScolPrefixes == unitPrefix)]
  nunits <- length(units)
  
  if (nunits == 0){
    stop(paste0("No columns in data_SS include ", unitPrefix, " in the name"))
  }
  SSlist <- vector("list", nunits)

  for (uniti in 1:nunits){
    sample10 <- SS[ , units[uniti]]
    sampleday <- SS[ , datesSearchedCol]
    daysUnitSampled <- sampleday[sample10 == 1]
    SSlist[[uniti]] <- daysUnitSampled - min(daysUnitSampled)
  }
    
  avgInterval <- round(mean(unlist(lapply(lapply(SSlist, diff), mean))))
  maxDay <- max(unlist(lapply(SSlist, max)))
  SS <- seq(0, maxDay, by = avgInterval)
  if (max(SS) != maxDay){
    SS <- c(SS, maxDay)
  }
  return(SS)
}
  
#' Summarize the gGeneric list to a simple table
#'
#' @param object gGeneric output list (each element is a named vector of 
#'   gGeneric values for a cell in the model combinations)
#' @param ... arguments to be passed down
#' @param CL confidence level
#'
#' @return a summary table of g values (medians and confidence bounds) for 
#'   each cell combination within the gGeneric list
#' @export
#'
summary.gGeneric <- function(object, ..., CL = 0.9){

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

  colnames(predsTab) <- predNames

  gTab <- matrix(NA, ncell, 2)
  for (celli in 1:ncell){
    gspec <- ghats[[celli]]
    gmedian <- round(median(gspec), 3)
    gCLlow <- round(quantile(gspec, prob = (1 - CL) / 2), 3)
    gCLup <- round(quantile(gspec, prob = 1 - (1 - CL) / 2), 3)
    gsummary <- paste0(gmedian, " [", gCLlow, " - ", gCLup, "]")
    gTabi <- c(cells[celli], gsummary)
    gTab[celli, ] <- gTabi
  }
  out <- data.frame(predsTab, gTab)

  DPheader <- paste0("Detection Probability (Median [", 
                (1 - CL) / 2 * 100, "% - ", 
                (1 - (1 - CL) / 2) * 100, "%])"
              )
  colnames(out)[npred + (1:2)] <- c("Cell", DPheader)
  return(out)

}

#' Summarize the gGenericSize list to a list of simple tables
#'
#' @param object gGenericSize output list (each element is a size-named 
#'   list of named vectors of gGeneric values for a cell in the model 
#'   combinations)
#' @param ... arguments to be passed down
#' @param CL confidence level
#'
#' @return a list of summary tables of g values (medians and confidence 
#'   bounds) for each cell combination within the gGeneric list
#' @export
#'
summary.gGenericSize <- function(object, ..., CL = 0.9){

  nsizeclass <- length(object)
  out <- vector("list", length = nsizeclass)
  names(out) <- names(object)
  for (sci in 1:nsizeclass){
    out[[sci]] <- summary(object[[sci]], ..., CL = CL)
  }
  return(out)
}
