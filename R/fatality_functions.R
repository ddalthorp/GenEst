#' Draw values for Xtilde given a ghat
#'
#' @param n number of random draws
#' @param ghat estimated detection probability
#' @param seed seed used to set the random number generator
#' @return Xtilde 
#' @examples NA
#' @export 
#'
rXtilde <- function(n = 1, ghat, seed = 1){
  set.seed(seed)
  Xtilde <- rcbinom(n, 1 / ghat, ghat)
  return(Xtilde)
}

#' Draw values for Mtilde given a ghat
#'
#' @param n number of random draws
#' @param ghat estimated detection probability
#' @param seed seed used to set the random number generator
#' @return Mtilde 
#' @examples NA
#' @export 
#'
rMtilde <- function(n = 1, ghat, seed = 1){
  
  Xtilde <- rXtilde(n, ghat, seed)
  MtildeVec <- (Xtilde - (Ecbinom(ghat) - 1)) / ghat
  Mtilde <- matrix(MtildeVec, ncol = ncol(ghat))
  return(Mtilde)
}

#' Draw values for Mhat given a ghat and DWP
#'
#' @param n number of random draws per carcass
#' @param ghat estimated detection probability
#' @param DWP Density weighted proportion associated with Mtilde
#' @param seed seed used to set the random number generator
#' @return Mhat 
#' @examples NA
#' @export 
#'
rMhat <- function(n = 1, ghat, DWP = 1, seed = 1){
  
  if (n != 1){
    stop("multiple draws per ghat is not currently available at this time.")
  }
  Mtilde <- rMtilde(n = length(ghat), ghat, seed)
  Mhat <- calcMhat(Mtilde, DWP)
  return(Mhat)
}

#' Calculate Mhat for a given Mtilde and DWP
#'
#' @description If Mtilde is a matrix, DWP is expadanded by columns to match
#'   to facilitate division
#'
#' @param Mtilde Mtilde value
#' @param DWP Density weighted proportion associated with Mtilde
#' @return Mhat 
#' @examples NA
#' @export 
#'
calcMhat <- function(Mtilde, DWP = 1){

  if (!is.null(dim(Mtilde))){
    ncarc <- dim(Mtilde)[1]
    n <- dim(Mtilde)[2]
    if (length(DWP) == 1){
      DWP <- rep(DWP, ncarc)
    } 
    if (dim(Mtilde)[1] != length(DWP)){
      stop("Mtilde and DWP are of different sizes")
    }
    tempDWP <- matrix(NA, nrow = ncarc, ncol = n)
    for (carci in 1:ncarc){
      tempDWP[carci, ] <- rep(DWP[carci], n)
    }
    DWP <- tempDWP
  }
  if (is.vector(Mtilde) & is.vector(DWP)){
    ncarc <- length(Mtilde)
    if (length(DWP) == 1){
      DWP <- rep(DWP, ncarc)
    } 
    if (length(Mtilde) != length(DWP)){
      stop("Mtilde and DWP are of different sizes")
    }
  }

  Mhat <- Mtilde / DWP
  return(Mhat)
}


#' Expand the density weighted proportion table to a value for each carcass
#'
#' @param data_DWP Survey unit (rows) by size (columns) density weighted 
#'   proportion table 
#' @param data_carc Carcass observation data
#' @param removeCleanout logical indicating if cleanout searches need to be
#'   removed from \code{data_carc}
#' @param data_SS Search Schedule data (if cleanout searches are to be 
#'   removed)
#' @param unitCol Column name for the unit indicator
#' @param sizeclassCol Name of colum in \code{data_carc} where the size 
#'   classes are recorded
#' @param dateFoundCol Column name for the date found data (if cleanout 
#'   searches are to be removed)
#' @param dateSearchedCol Column name for the date searched data (if cleanout 
#'   searches are to be removed)
#' @return DWP value for each carcass 
#' @examples NA
#' @export 
#'
DWPbyCarcass <- function(data_DWP, data_carc, unitCol = "Unit",
                         sizeclassCol = NULL, removeCleanout = FALSE,
                         data_SS = NULL, dateFoundCol = "DateFound",
                         dateSearchedCol = "DateSearched"){

  if (!(unitCol %in% colnames(data_DWP) & unitCol %in% colnames(data_carc))){
    stop("Unit column not in both DWP and carcass tables")
  }
  units_carc <- unique(data_carc[ , unitCol])
  units_DWP <- data_DWP[ , unitCol]
  nunits <- nrow(data_DWP)
  if (!all(units_carc %in% units_DWP)){
    stop("Units present in carcass table not in DWP table")
  }

  if (removeCleanout){
    data_SS[ , dateSearchedCol] <- yyyymmdd(data_SS[ , dateSearchedCol])
    data_carc[ , dateFoundCol] <- yyyymmdd(data_carc[ , dateFoundCol])
    date1 <- min(data_SS[ , dateSearchedCol])
    data_carc[ , dateFoundCol] <- dateToDay(data_carc[ , dateFoundCol], date1)
    data_SS[ , dateSearchedCol] <- dateToDay(data_SS[ , dateSearchedCol], 
                                     date1
                                   )
    cleanout <- whichCleanout(data_carc, data_SS, unitCol, dateFoundCol,
                  dateSearchedCol
                )  
    if (length(cleanout) > 0){
      data_carc <- data_carc[-cleanout, ]
    }
  }

  DWPcols <- which(grepl("DWP", colnames(data_DWP)))
  nDWPcols <- length(DWPcols)

  if (is.null(sizeclassCol)){
    sizeclassCol <- "scc"
    data_carc$scc <- "all"
    DWPmat <- as.matrix(data_DWP[ , DWPcols], nrow = nunits, ncol = nDWPcols)
    DWPs <- apply(DWPmat, 1, mean)

    data_DWP <- data.frame(data_DWP[ , unitCol], DWPs)
    colnames(data_DWP) <- c(unitCol, "DWP_all")
    DWPcols <- which(grepl("DWP", colnames(data_DWP)))
    nDWPcols <- length(DWPcols)
  }
  if (!(sizeclassCol %in% colnames(data_carc))){
    stop("Provided size class column not present in carcass table")
  }
  sizeclasses <- gsub("DWP_", "", colnames(data_DWP)[DWPcols])
  carcassSizeclasses <- unique(data_carc[ , sizeclassCol])
  if (!all(carcassSizeclasses %in% sizeclasses)){
    stop("Size classes present in carcass table not in DWP table")
  }
  DWPonly <- as.matrix(data_DWP[ , DWPcols], nrow = nunits, ncol = nDWPcols)

  DWPexp <- numeric(0)
  unitexp <- character(0)
  scexp <- character(0)
  for (uniti in 1:nunits){
    DWPexp <- c(DWPexp, DWPonly[uniti, ])
    unitexp <- c(unitexp, rep(units_DWP[uniti], nDWPcols))
    scexp <- c(scexp, sizeclasses)
  }
  data_DWPtall <- data.frame(DWPexp, unitexp, scexp, stringsAsFactors = FALSE)
  colnames(data_DWPtall) <- c("DWP", unitCol, sizeclassCol)

  ncarc <- nrow(data_carc)
  DWPbyCarc <- numeric(ncarc)
  for (carci in 1:ncarc){
    unitOfInterest <- data_carc[carci, unitCol]
    matchUnit <- data_DWPtall[ , unitCol] == unitOfInterest
    sizeOfInterest <- data_carc[carci, sizeclassCol]
    matchSize <- data_DWPtall[ , sizeclassCol] == sizeOfInterest
    matchBoth <- matchUnit & matchSize
    DWPbyCarc[carci] <- data_DWPtall[which(matchBoth), "DWP"]
  }
  return(DWPbyCarc)
}

