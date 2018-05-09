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
      powk <- matrixStats::rowCumprods(kMat)
      pRowProds <- matrixStats::rowProds(1 - p * powk)
      pfind <-  pRowProds * p * k * powk[ , dim(powk)[2]]
      if (aj == oi){
        pfind <- p
      }
      pOigAj[aj, ] <- as.vector(persist) * pfind
    }
    parrive <- diff(c(t0[1], t1[1:oi])) / t1[oi]
    pAjgOi <- pOigAj * parrive
    pAjgOi <- t(t(pAjgOi) / colSums(pAjgOi))
    Aj[oi, ] <- rowSums(matrixStats::rowCumsums(t(pAjgOi)) < runif(n)) + 1
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
