#' @title Estimate mortality 
#'
#' @description Given given fitted Searcher Efficiency and Carcass 
#'   Persistence models; Search Schedule, Density Weighted Proportion,
#'   and Carcass Observation data; and information about the fraction of the
#'   the facility that was surveyed. This version of the function works for a
#'   single size class
#'
#' @param nsim the number of simulation draws
#' @param data_CO Carcass Observation data
#' @param data_SS Search Schedule data
#' @param data_DWP Survey unit (rows) by size (columns) density weighted 
#'   proportion table 
#' @param frac fraction of facility (by units or by area) surveyed
#' @param model_SE Searcher Efficiency model (or list of models if there are 
#'   multiple size classes)
#' @param model_CP Carcass Persistence model (or list of models if there are 
#'   multiple size classes)
#' @param seed_SE seed for random draws of the SE model
#' @param seed_CP seed for random draws of the CP model
#' @param seed_g seed for random draws of the gs
#' @param seed_M seed for the random draws of the Mhats
#' @param kFill value to fill in for missing k when not existing in the model
#' @param unitCol Column name for the unit indicator
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @param DWPCol Column name for the DWP values in the DWP table when no
#'   size class is used
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'  are recorded. Optional.
#' @return list of SE parameters (pk), CP parameters (ab), g, Aj, and Mhat 
#' @examples NA
#' @export 
#'
estM <- function(nsim = 1, data_CO, data_SS, data_DWP, frac = 1,  
                 model_SE, model_CP, 
                 seed_SE = NULL, seed_CP = NULL, seed_g = NULL, 
                 seed_M = NULL, kFill = NULL,  
                 unitCol = "Unit", dateFoundCol = "DateFound", 
                 dateSearchedCol = "DateSearched", DWPCol = NULL,
                 sizeclassCol = NULL){

  SSCO <- prepSSCO(data_SS, data_CO, dateSearchedCol, dateFoundCol, unitCol)  
  data_CO <- SSCO$data_CO
  data_SS <- SSCO$data_SS
  c_outs <- SSCO$cleanout_carcasses

  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not in carcass data.")
    }
    sizeclasses <- as.character(unique(data_CO[ , sizeclassCol]))
    nsizeclass <- length(sizeclasses)

    if (!all(sizeclasses %in% DWPCol)){
      stop("not all size classes are present in the DWP columns provided.")
    }
  }
  if (is.null(sizeclassCol) & length(DWPCol) > 1){
    stop("multiple DWP columns provided, but no sizeclass column to use.")
  }

  DWP <- DWPbyCarcass(data_DWP, data_CO, data_SS, dateFoundCol, 
           dateSearchedCol, DWPCol, unitCol, sizeclassCol
         )

  est <- estg(nsim, data_CO, data_SS, model_SE, model_CP, seed_SE,  
           seed_CP, seed_g, kFill, unitCol, dateFoundCol, 
           dateSearchedCol, sizeclassCol, cleanoutCarcs = c_outs
         )

  gDWPf <- est$g * DWP * frac
  c_out <- which(apply(gDWPf, 1, sum) == 0)
  if (length(c_out) > 0){
    gDWPf <- gDWPf[-c_out, ]
  }
  n <- length(gDWPf)
  set.seed(seed_M)
  Mhat <- ((rcbinom(n, 1 / gDWPf, gDWPf)) - (Ecbinom(gDWPf) - 1))/(gDWPf)
  if (length(c_out) > 0){
    zeroes <- matrix(0, nrow = length(c_out), ncol = ncol(est$g))
    Mhat <- rbind(zeroes, Mhat)
  }
  out <- list(est$pk, est$ab, est$g, est$Aj, Mhat)
  names(out) <- c("pk", "ab", "g", "Aj", "M")
  class(out) <- c("estM", "list")
  return(out)
}
#' @title Assign DWP Value to Each Carcass
#'
#' @description Expand the density weighted proportion table to a value for 
#'   each carcass (across multiple classes if desired) based on the unit where 
#'   they were found
#'
#' @param data_DWP Survey unit (rows) by size (columns) density weighted 
#'   proportion table 
#' @param data_CO Carcass observation data
#' @param data_SS Search Schedule data 
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @param DWPCol Column name(s) for the DWP values in the DWP table when no
#'   size class is used
#' @param unitCol Column name for the unit indicator
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'  are recorded. Optional.
#' @return DWP value for each carcass 
#' @examples NA
#' @export 
#'
DWPbyCarcass <- function(data_DWP, data_CO, data_SS, 
                         dateFoundCol = "DateFound",
                         dateSearchedCol = "DateSearched",
                         DWPCol = NULL, unitCol = "Unit",
                         sizeclassCol = NULL){

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
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not in carcass data.")
    }
    sizeclass <- as.character(data_CO[ , sizeclassCol])
    sizeclasses <- unique(sizeclass)
    nsizeclass <- length(sizeclasses)

    if (!all(sizeclasses %in% DWPCol)){
      stop("not all size classes are present in the DWP columns provided.")
    }
  } else{

    sizeclass <- rep(DWPCol, nrow(data_CO))
    sizeclasses <- DWPCol
    nsizeclass <- 1
  }


  ncarc <- nrow(data_CO)
  DWPbyCarc <- numeric(ncarc)
  for (carci in 1:ncarc){
    unitOfInterest <- data_CO[carci, unitCol]
    matchUnit <- data_DWP[ , unitCol] == unitOfInterest
    sizeclassOfInterest <- sizeclass[carci]
    DWPbyCarc[carci] <- data_DWP[matchUnit, sizeclassOfInterest]
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

#' @title Summarize total fatality estimation
#' @description \code{summary} defined for class \code{estM} objects
#' @param object \code{estM} object
#' @param ... arguments to pass down
#' @param CL confidence level
#' @export
#'
summary.estM <- function(object, ..., CL = 0.9){
  M <- object$M
  Mtot <- apply(M, 2, sum)
  minMtot <- min(Mtot)
  maxMtot <- max(Mtot)
  pl <- (1 - CL)/2
  ph <- 1 - (1 - CL)/2
  MCLlow <- round(quantile(Mtot, pl), 2)
  MCLhi <- round(quantile(Mtot, ph), 2) 
  Mmed <- round(median(Mtot), 2)

  out <- paste0(Mmed, " [", MCLlow, " - ", MCLhi, "]")
  names(out) <- paste0("Median [", names(MCLlow), " - " , names(MCLhi), "]")
  return(out)         
}
