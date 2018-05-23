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
#'   (within a size class) based on the unit where they were found
#'
#' @param data_DWP Survey unit (rows) by size (columns) density weighted 
#'   proportion table 
#' @param data_CO Carcass observation data
#' @param data_SS Search Schedule data 
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @param DWPCol Column name for the DWP values in the DWP table when no
#'   size class is used
#' @param unitCol Column name for the unit indicator
#' @return DWP value for each carcass 
#' @examples NA
#' @export 
#'
DWPbyCarcass <- function(data_DWP, data_CO, data_SS, 
                         dateFoundCol = "DateFound",
                         dateSearchedCol = "DateSearched",
                         DWPCol = NULL, unitCol = "Unit"){

  if (!(unitCol %in% colnames(data_DWP) & unitCol %in% colnames(data_CO))){
    stop("Unit column not in both DWP and carcass tables")
  }
  units_carc <- unique(data_CO[ , unitCol])
  units_DWP <- data_DWP[ , unitCol]
  nunits <- nrow(data_DWP)
  if (!all(units_carc %in% units_DWP)){
    stop("Units present in carcass table not in DWP table")
  }

  if (is.null(DWPCol)){
    whichDWPCol <- grep("dwp", colnames(data_DWP), ignore.case = TRUE)
    if (length(whichDWPCol) == 0){
      stop("No DWP data indicated and no obvious choice for DWP column.")
    }     
    DWPCol <- colnames(data_DWP)[whichDWPCol[1]]
  }

  ncarc <- nrow(data_CO)
  DWPbyCarc <- numeric(ncarc)
  for (carci in 1:ncarc){
    unitOfInterest <- data_CO[carci, unitCol]
    matchUnit <- data_DWP[ , unitCol] == unitOfInterest
    DWPbyCarc[carci] <- data_DWP[matchUnit, DWPCol]
  }
  return(DWPbyCarc)
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
cleanouts <- function(data_CO, data_SS, unitCol, timeFoundCol, 
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

#' Prepare the SS and CO data
#'
#' @description Turns dates into days from the first day (cleanout search, set
#'   as time 0) and removes any carcasses observed on the first days
#'
#' @param data_CO Carcass observation data
#' @param data_SS Search Schedule data 
#' @param unitCol Column name for the unit indicator
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#'
#' @return list of [1] SS with days (starting with t = 0), [2] CO data with 
#'   days (starting with t = 0), and [3] a vector of the CO_data rows 
#'   indicating the carcasses found during cleanout searches. 
#' @examples NA
#' @export 
#'
prepSSCO <- function(data_SS, data_CO, dateSearchedCol = "DateSearched",
                     dateFoundCol = "DateFound", unitCol = "Unit"){

  data_SS[ , dateSearchedCol] <- yyyymmdd(data_SS[ , dateSearchedCol])
  data_CO[ , dateFoundCol] <- yyyymmdd(data_CO[ , dateFoundCol])
  date0 <- min(data_SS[ , dateSearchedCol])
  data_CO[ , dateFoundCol] <- dateToDay(data_CO[ , dateFoundCol], date0)
  data_SS[ , dateSearchedCol] <- dateToDay(data_SS[ , dateSearchedCol], date0)

  which_day0 <- which(data_SS[ , dateSearchedCol] == 0)
  SSunitCols <- which(colnames(data_SS) %in% unique(data_CO[ , unitCol]))
  data_SS[which_day0, SSunitCols] <- 1

  c_out <- cleanouts(data_CO, data_SS, unitCol, dateFoundCol, dateSearchedCol)

  out <- list(data_SS, data_CO, c_out)
  names(out) <- c("data_SS", "data_CO", "cleanout_carcasses")
  return(out)
}
