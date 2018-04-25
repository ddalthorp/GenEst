#' @title Simulate ghat values and arrival intervals for a set of carcasses 
#'   from fitted pk and cp models and search data
#'
#' @param n the number of simulation draws
#' @param data_carc Carcass Observation data
#' @param data_ss Search Schedule data
#' @param model_SE Searcher Efficiency model
#' @param model_CP Carcass Persistence model
#' @param seed_SE seed for random draws of the SE model
#' @param seed_CP seed for random draws of the CP model
#' @param kFill value to fill in for missing k when not existing in the model
#' @param unitCol Column name for the unit indicator
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @return list of [1] matrix of n ghat estimates for each carcass and [2]
#'   matrix of n arrival intervals (A) for each carcass
#' @examples NA
#' @export
#'
rghat <- function(n = 1, data_carc, data_ss, model_SE, model_CP, seed_SE = 1, 
                  seed_CP = 1, kFill = NULL, unitCol = "Unit",
                  dateFoundCol = "DateFound", 
                  dateSearchedCol = "DateSearched"){

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
  preds_static <- preds[which(preds %in% names(data_carc))]
  preds_dynamic <- preds[which(preds %in% names(data_ss))]

  if (!all(preds %in% c(preds_static, preds_dynamic))){
    stop("Some model predictors required not in carcass or search data.")
  }
  if (any(preds %in% preds_static & preds %in% preds_dynamic)) {
    msg <- paste("Predictor(s) duplicated across both carcass and search ",
             "data. Each predictor should be in only one data set.", sep = "") 
    stop(msg)
  }

  pkSim <- rpk(n, model_SE, seed_SE, kFill)
  cpSim <- rcp(n, model_CP, seed_CP, type = "ppersist") 

  data_ss[ , dateSearchedCol] <- yyyymmdd(data_ss[ , dateSearchedCol])
  data_carc[ , dateFoundCol] <- yyyymmdd(data_carc[ , dateFoundCol])


  date1 <- min(data_ss[ , dateSearchedCol])
  data_carc[ , dateFoundCol] <- dateToDay(data_carc[ , dateFoundCol], date1)
  data_ss[ , dateSearchedCol] <- dateToDay(data_ss[ , dateSearchedCol], date1)

  cleanout <- whichCleanout(data_carc, data_ss, unitCol, dateFoundCol,
                dateSearchedCol
              )  
  if (length(cleanout) > 0){
    data_carc <- data_carc[-cleanout, ]
  }

  ncarc <- nrow(data_carc)
  ghat <- matrix(NA, nrow = ncarc, ncol = n)
  A <- matrix(NA, nrow = ncarc, ncol = n)
  for (carci in 1:ncarc){
    carciEst <- rghatCarcass(n, data_carc = data_carc[carci, ], dist,  
                  data_ss, preds_SE, preds_CP, pkSim, cpSim,
                  preds_static, preds_dynamic, unitCol, dateFoundCol, 
                  dateSearchedCol
                )
    ghat[carci, ] <- carciEst$ghat
    A[carci, ] <- carciEst$A
  }
  out <- list("ghat" = ghat, "A" = A)
  return(out)
}

#' Simulate ghat values and arrival intervals for a carcasses from fitted pk 
#'   and cp models and search data
#'
#' @param n the number of simulation draws
#' @param data_carc Carcass Observation data for the carcass
#' @param dist distribution for the CP model
#' @param data_ss Search Schedule data
#' @param preds_SE Names of Searcher Efficiency predictors
#' @param preds_CP Names of Carcass Persistence predictors
#' @param pkSim simulated pk parameters
#' @param cpSim simulated cp parameters
#' @param preds_static Names of predictors that don't change over time
#' @param preds_dynamic Names of predictors that change over time
#' @param unitCol Column name for the unit indicator
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @return list of [1] n ghat estimates for a carcass and [2] n arrival 
#'   interval (A) estimates for a carcass
#' @examples NA
#' @export
#'
rghatCarcass <- function(n = 1, data_carc, dist, data_ss, preds_SE, preds_CP,
                         pkSim, cpSim, preds_static, preds_dynamic,
                         unitCol, dateFoundCol, dateSearchedCol){

  Unit <- data_carc[, unitCol]
  Carc <- data_carc[, preds_static]
  DateFound <- data_carc[ , dateFoundCol]
  SS <- data_ss[, c(dateSearchedCol, preds_dynamic, Unit)]
  SS <- SS[which(SS[ , Unit] == 1), ]
  Data <- data.frame(SS, Carc)
  colnames(Data) <- c(colnames(SS), preds_static)
  Data <- Data[Data[ , dateSearchedCol] <= DateFound, ]
  SE <- data.frame(Data[ , preds_SE])
  CP <- data.frame(Data[ , preds_CP])
  names(SE) <- preds_SE
  names(CP) <- preds_CP

  SEcell <- apply(SE, 1, paste, collapse = ".")
  CPcell <- apply(CP, 1, paste, collapse = ".")
  if (all(SEcell == "")){
    SEcell <- rep("all", length(SEcell))
  }
  if (all(CPcell == "")){
    CPcell <- rep("all", length(CPcell))
  }
  SEParams <- pkSim[SEcell] 
  CPParams <- cpSim[CPcell] 

  Date0 <- Data[-nrow(Data), dateSearchedCol]
  Date1 <- Data[-1, dateSearchedCol]
  DateS <- Data[-1, dateSearchedCol]

  A <- matrix(NA, nrow = length(DateS), ncol = n)

  for (oi in 1:length(DateS)){
    pOigAj <- matrix(NA, nrow = oi, ncol = n)

    for (aj in 1:oi){
      pda <- CPParams[[aj]][ , "pda"]
      pdb <- CPParams[[aj]][ , "pdb"]
      persist <- ppersist(pda, pdb, dist, Date0[aj], Date1[aj], DateS[oi])
      p <- SEParams[[aj]][ , "p"]
      k <- SEParams[[aj]][ , "k"]
      kMat <- matrix(c(1 + numeric(n), rep(k, oi - aj)), nrow = n)
      powk <- matrixStats::rowCumprods(kMat)
      pRowProds <- matrixStats::rowProds(1 - p * powk)
      pfind <-  pRowProds * p * k * powk[ , dim(powk)[2]]
      if (aj == oi){
        pfind <- p
      }
      pOigAj[aj, ] <- as.vector(persist) * pfind
    }
    parrive <- diff(c(Date0[1], Date1[1:oi])) / Date1[oi]
    pAjgOi <- pOigAj * parrive
    pAjgOi <- t(t(pAjgOi)/colSums(pAjgOi))
    A[oi, ] <- rowSums(matrixStats::rowCumsums(t(pAjgOi)) < runif(n)) + 1
  }

  ghat <- rep(NA, length = n)
  specificA <- A[nrow(A), ]
  uniA <- unique(specificA)
  for (aj in uniA){
    simInd <- which(specificA == aj)
    pda <- CPParams[[aj]][simInd, "pda"]
    pdb <- CPParams[[aj]][simInd, "pdb"]
    t0 <- rep(Date0[aj], length(DateS) - aj + 1)
    t1 <- rep(Date1[aj], length(DateS) - aj + 1)
    tS <- DateS[aj:length(DateS)]
    persist <- ppersist(pda, pdb, dist, t0, t1, tS)
    se <- SEsi(c(Date0[aj:length(Date0)], DateS[length(DateS)]), 
            SEParams[[aj]][simInd, ]
          )
    if (aj < length(Date0)){
      ghat[simInd] <- colSums(se * persist)
    } else{
      ghat[simInd] <- as.vector(se) * as.vector(persist)
    }
  }  
  A <- A[nrow(A), ]
  out <- list("ghat" = ghat, "A" = A)
  return(out)
}

#' Determine which carcasses were from cleanout searches
#'
#' @param data_carc Carcass observation data
#' @param data_ss Search schedule data
#' @param unitCol Column name for the unit indicator
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @return index values of which carcasses were taken on the first search
#' @examples NA
#' @export 
#'
whichCleanout <- function(data_carc, data_ss, unitCol, dateFoundCol,
                          dateSearchedCol){

  ncarc <- nrow(data_carc)
  cleanoutTF <- rep(NA, ncarc)
  for (carci in 1:ncarc){
    specificUnit <- data_carc[carci, unitCol]
    cleanoutDay <- min(data_ss[data_ss[, specificUnit] == 1, dateSearchedCol])
    cleanoutTF[carci] <- data_carc[carci, dateFoundCol] == cleanoutDay
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
