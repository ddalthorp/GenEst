#' Simulate ghat values and arrival intervals for a set of carcasses 
#'   from fitted pk and cp models and search data
#'
#' @param n the number of simulation draws
#' @param data_CO Carcass Observation data
#' @param data_SS Search Schedule data
#' @param model_SE Searcher Efficiency model
#' @param model_CP Carcass Persistence model
#' @param seed_SE seed for random draws of the SE model
#' @param seed_CP seed for random draws of the CP model
#' @param seed_ghat seed for random draws of the ghats
#' @param kFill value to fill in for missing k when not existing in the model
#' @param unitCol Column name for the unit indicator
#' @param removeCleanout logical indicating if cleanout searches need to be
#'   removed from \code{data_carc}
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @return list of [1] matrix of n ghat estimates for each carcass and [2]
#'   matrix of n arrival intervals (Aj) for each carcass. The row names of the
#'   Aj matrix are the names of the units where each carcass was found.
#' @examples NA
#' @export
#'
rghat <- function(n = 1, data_CO, data_SS, model_SE, model_CP, 
                  seed_SE = NULL, seed_CP = NULL, seed_ghat = NULL, 
                  kFill = NULL, unitCol = "Unit", dateFoundCol = "DateFound", 
                  dateSearchedCol = "DateSearched", removeCleanout = FALSE){

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
  preds <- unique(c(preds_SE, preds_CP))
  data_SE <- model_SE$data
  data_CP <- model_CP$data
  dist <- model_CP$dist
  preds_static <- preds[which(preds %in% names(data_CO))]
  preds_dynamic <- preds[which(preds %in% names(data_SS))]

  if (!all(preds %in% c(preds_static, preds_dynamic))){
    stop("Some model predictors required not in carcass or search data.")
  }
  if (any(preds %in% preds_static & preds %in% preds_dynamic)) {
    msg <- paste("Predictor(s) duplicated across both carcass and search ",
             "data. Each predictor should be in only one data set.", sep = "") 
    stop(msg)
  }

  sim_SE <- rpk(n, model_SE, seed_SE, kFill)
  sim_CP <- rcp(n, model_CP, seed_CP, type = "ppersist") 

  data_SS[ , dateSearchedCol] <- yyyymmdd(data_SS[ , dateSearchedCol])
  data_CO[ , dateFoundCol] <- yyyymmdd(data_CO[ , dateFoundCol])

  date1 <- min(data_SS[ , dateSearchedCol])
  data_CO[ , dateFoundCol] <- dateToDay(data_CO[ , dateFoundCol], date1)
  data_SS[ , dateSearchedCol] <- dateToDay(data_SS[ , dateSearchedCol], date1)

  if (removeCleanout){
    cleanout <- whichCleanout(data_CO, data_SS, unitCol, dateFoundCol,
                  dateSearchedCol
                )  
    if (length(cleanout) > 0){
      data_CO <- data_CO[-cleanout, ]
    }
  }
  ncarc <- nrow(data_CO)
  ghat <- matrix(NA, nrow = ncarc, ncol = n)
  Aj <- matrix(NA, nrow = ncarc, ncol = n)
  for (carci in 1:ncarc){
    ghatAndA <- rghatCarcass(n, data_carc = data_CO[carci, ], dist,  
                  data_SS, preds_SE, preds_CP, sim_SE, sim_CP,
                  preds_static, preds_dynamic, unitCol, dateFoundCol, 
                  dateSearchedCol
                )
    ghat[carci, ] <- ghatAndA$ghat
    Aj[carci, ] <- ghatAndA$Aj
  }
  rownames(Aj) <- data_CO[ , unitCol]
  out <- list("ghat" = ghat, "Aj" = Aj)
  return(out)
}

#' Simulate ghat values and arrival intervals for a carcass from fitted pk 
#'   and cp models and search data
#'
#' @param n the number of simulation draws
#' @param data_carc Carcass Observation data for the carcass
#' @param dist distribution for the CP model
#' @param data_SS Search Schedule data
#' @param preds_SE Names of Searcher Efficiency predictors
#' @param preds_CP Names of Carcass Persistence predictors
#' @param sim_SE simulated Search Efficiency parameters
#' @param sim_CP simulated Carcass Persistence parameters
#' @param preds_static Names of predictors that don't change over time
#' @param preds_dynamic Names of predictors that change over time
#' @param unitCol Column name for the unit indicator
#' @param timeFoundCol Column name for the time found data
#' @param timeSearchedCol Column name for the time searched data
#' @return list of [1] n ghat estimates for a carcass and [2] n arrival 
#'   interval (Aj) estimates for a carcass
#' @examples NA
#' @export
#'
rghatCarcass <- function(n = 1, data_carc, dist, data_SS, preds_SE, preds_CP,
                         sim_SE, sim_CP, preds_static, preds_dynamic,
                         unitCol, timeFoundCol, timeSearchedCol){

  unit <- data_carc[, unitCol]
  carc <- data_carc[, preds_static]
  dateFound <- data_carc[ , timeFoundCol]
  SS_unit <- data_SS[, c(timeSearchedCol, preds_dynamic, unit)]
  SS_unit <- SS_unit[which(SS_unit[ , unit] == 1), ]
  data_carc <- data.frame(SS_unit, carc)
  colnames(data_carc) <- c(colnames(SS_unit), preds_static)
  data_carc <- data_carc[data_carc[ , timeSearchedCol] <= dateFound, ]
  data_SE <- data.frame(data_carc[ , preds_SE])
  data_CP <- data.frame(data_carc[ , preds_CP])
  names(data_SE) <- preds_SE
  names(data_CP) <- preds_CP

  cell_SE <- apply(data_SE, 1, paste, collapse = ".")
  cell_CP <- apply(data_CP, 1, paste, collapse = ".")
  if (all(cell_SE == "")){
    cell_SE <- rep("all", length(cell_SE))
  }
  if (all(cell_CP == "")){
    cell_CP <- rep("all", length(cell_CP))
  }
  params_SE <- sim_SE[cell_SE] 
  params_CP <- sim_CP[cell_CP] 

  t0 <- data_carc[-nrow(data_carc), timeSearchedCol]
  t1 <- data_carc[-1, timeSearchedCol]
  tS <- data_carc[-1, timeSearchedCol]

  Aj <- matrix(NA, nrow = length(tS), ncol = n)

  for (oi in 1:length(tS)){
    pOigAj <- matrix(NA, nrow = oi, ncol = n)

    for (aj in 1:oi){
      pda <- params_CP[[aj]][ , "pda"]
      pdb <- params_CP[[aj]][ , "pdb"]
      persist <- ppersist(pda, pdb, dist, t0[aj], t1[aj], tS[oi])
      p <- params_SE[[aj]][ , "p"]
      k <- params_SE[[aj]][ , "k"]
      kMat <- matrix(c(1 + numeric(n), rep(k, oi - aj)), nrow = n)
      powk <- rowCumprods(kMat)
      pRowProds <- rowProds(1 - p * powk)
      pfind <-  pRowProds * p * k * powk[ , dim(powk)[2]]
      if (aj == oi){
        pfind <- p
      }
      pOigAj[aj, ] <- as.vector(persist) * pfind
    }
    parrive <- diff(c(t0[1], t1[1:oi])) / t1[oi]
    pAjgOi <- pOigAj * parrive
    pAjgOi <- t(t(pAjgOi) / colSums(pAjgOi))
    Aj[oi, ] <- rowSums(rowCumsums(t(pAjgOi)) < runif(n)) + 1
  }

  ghat <- rep(NA, length = n)
  Aj_specific <- Aj[nrow(Aj), ]
  Aj_unique <- unique(Aj_specific)
  for (aj in Aj_unique){
    included <- which(Aj_specific == aj)
    pda <- params_CP[[aj]][included, "pda"]
    pdb <- params_CP[[aj]][included, "pdb"]
    t0_j <- rep(t0[aj], length(tS) - aj + 1)
    t1_j <- rep(t1[aj], length(tS) - aj + 1)
    tS_j <- tS[aj:length(tS)]
    est_CP <- ppersist(pda, pdb, dist, t0_j, t1_j, tS_j)
    days_j <- c(t0[aj:length(t0)], tS[length(tS)])
    est_SE <- SEsi(days_j, params_SE[[aj]][included, ])

    if (aj < length(t0)){
      ghat[included] <- colSums(est_SE * est_CP)
    } else{
      ghat[included] <- as.vector(est_SE) * as.vector(est_CP)
    }
  }  
  Aj <- Aj[nrow(Aj), ]
  out <- list("ghat" = ghat, "Aj" = Aj)
  return(out)
}

#' Determine which carcasses were from cleanout searches
#'
#' @param data_CO Carcass observation data
#' @param data_SS Search schedule data
#' @param unitCol Column name for the unit indicator
#' @param timeFoundCol Column name for the time found data
#' @param timeSearchedCol Column name for the time searched data
#' @return index values of which carcasses were taken on the first search
#' @examples NA
#' @export 
#'
whichCleanout <- function(data_CO, data_SS, unitCol, timeFoundCol,
                          timeSearchedCol){
  ncarc <- nrow(data_CO)
  cleanoutTF <- rep(NA, ncarc)
  for (carci in 1:ncarc){
    specificUnit <- data_CO[carci, unitCol]
    times <- data_SS[data_SS[, specificUnit] == 1, timeSearchedCol]
    time_cleanout <- min(times)
    cleanoutTF[carci] <- data_CO[carci, timeFoundCol] == time_cleanout
  }
  return(which(cleanoutTF))
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

  proposal <- as.character(model$call["kFixed"])
  if (proposal == "NULL"){
    proposal <- NA
  } else{
    proposal <- alogit(as.numeric(proposal))
  }
  return(proposal)
}


#' Generic ghat estimation for a combination of SE model and CP model 
#'   under a given search schedule
#'
#' The g estimated by \code{rghatGeneric} is a generic aggregate detection 
#'   probability and represents the probability of detecting a carcass that 
#'   arrives at a (uniform) random time during the period monitored, for each
#'   of the possible cell combinations, given the SE and CP models. This 
#'   is somethat different from the GenEst estimation of g when the purpose 
#'   is to estimate total mortality (M), in which case the detection 
#'   probability varies with carcass arrival interval and is difficult to 
#'   summarize statistically. The \code{rghatGeneric} estimate is a useful 
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
#' @return ghatGeneric object that is a list of [1] a list of ghat estimates,
#'    with one element in the list corresponding to each of the cells from the 
#'    cross-model combination and [2] a table of predictors and cell names 
#'    associated with the ghats
#' @export
#'
rghatGeneric <- function(n = 1, data_SS, model_SE, model_CP, seed_SE = NULL,
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
    ghat[[celli]] <- ghatGenericCell(SS, param_SE, param_CP, dist, kFill)
  }  
  names(ghat) <- preds$CellNames
  out <- list("ghat" = ghat, "predictors" = preds)
  class(out) <- c("ghatGeneric", "list")
  return(out)
}

#' Generic ghat estimation for a single cell of a combination of SE model and 
#'   CP model under a given search schedule
#'
#' The g estimated by \code{ghatGenericCell} is a generic aggregate detection 
#'   probability and represents the probability of detecting a carcass that 
#'   arrives at a (uniform) random time during the period monitored, for one 
#'   cell across both the SE and CP models. This is somethat different from 
#'   the GenEst estimation of g when the purpose is to estimate total
#'   mortality (M), in which case the detection probability varies with 
#'   carcass arrival interval and is difficult to summarize statistically.
#'   The \code{ghatGenericCell} estimate is a useful "big picture" summary
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
ghatGenericCell <- function(SS, param_SE, param_CP, dist, kFill){

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

#' Generic ghat estimation for a combination of SE model and CP model 
#'   under a given search schedule
#'
#' The g estimated by \code{rghatGeneric} is a generic aggregate detection 
#'   probability and represents the probability of detecting a carcass that 
#'   arrives at a (uniform) random time during the period monitored, for each
#'   of the possible cell combinations, given the SE and CP models. This 
#'   is somethat different from the GenEst estimation of g when the purpose 
#'   is to estimate total mortality (M), in which case the detection 
#'   probability varies with carcass arrival interval and is difficult to 
#'   summarize statistically. The \code{rghatGeneric} estimate is a useful 
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
#' @param kFill value to fill in for missing k when not existing in the model
#' @return list of ghat estimates, with one element in the list corresponding
#'   to each of the cells from the cross-model combination
#' @export
#'
rghatGenericSize <- function(n = 1, data_SS, modelSetSize_SE, modelSetSize_CP,
                             modelSizeSelections_SE, modelSizeSelections_CP,  
                             seed_SE = NULL, seed_CP = NULL, kFill = NULL){

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
    model_SEsci <- modelSizeSelections_SE[[sci]]
    model_SE <- modelSetSize_SE[[sizeclasses[sci]]][[model_SEsci]]
    model_CPsci <- modelSizeSelections_CP[[sci]]
    model_CP <- modelSetSize_CP[[sizeclasses[sci]]][[model_CPsci]]
    ghats[[sci]] <- rghatGeneric(n, data_SS, model_SE, model_CP, 
                      seed_SE, seed_CP, kFill
                    )
  }
  names(ghats) <- sizeclasses
  return(ghats)
}

#' Tabulate an average search schedule from a multi-unit SS data table
#'
#' @param data_SS a multi-unit SS data table, for which the average interval 
#'   will be tabulated
#' @param dateSearchedCol Column name for the date searched data
#' @return vector of the average search schedule
#' @export
#'
averageSS <- function(data_SS, dateSearchedCol = "DateSearched"){
  SS <- data_SS
  SS[ , dateSearchedCol] <- yyyymmdd(SS[ , dateSearchedCol])
  date1 <- min(SS[ , dateSearchedCol])
  SS[ , "DateSearched"] <- dateToDay(SS[ , dateSearchedCol], date1)
  units <- colnames(SS)[grep("unit", tolower(colnames(SS)))]
  nunits <- length(units)
  
  if (nunits == 0){
    stop("No columns in data_SS include unit in the name")
  }
  SSlist <- vector("list", nunits)

  for (uniti in 1:nunits){
    sample10 <- SS[ , units[uniti]]
    sampleday <- SS[ , dateSearchedCol]
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
  
#' Summarize the ghatGeneric list to a simple table
#'
#' @param object ghatGeneric output list (each element is a named vector of 
#'   ghatGeneric values for a cell in the model combinations)
#' @param ... arguments to be passed down
#' @param CL confidence level
#'
#' @return a summary table of ghat values (medians and confidence bounds) for 
#'   each cell combination within the ghatGeneric list
#' @export
#'
summary.ghatGeneric <- function(object, ..., CL = 0.9){

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
  colnames(gTab) <- c("CellNames", "DetectionProbability")
  out <- data.frame(predsTab, gTab)
  return(out)

}
