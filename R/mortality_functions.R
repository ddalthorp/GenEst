#' @title Estimate mortality 
#'
#' @description Given given fitted Searcher Efficiency and Carcass 
#'   Persistence models; Search Schedule, Density Weighted Proportion,
#'   and Carcass Observation data; and information about the fraction of the
#'   the facility that was surveyed. 
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
#' @param unitCol Column name for the unit indicator (optional)
#' @param dateFoundCol Column name for the date found data
#' @param datesSearchedCol Column name for the date searched data
#' @param DWPCol Column name for the DWP values in the DWP table when no
#'   size class is used and there is more than one column in \code{data_DWP}
#'   that could be interpreted as DWP.
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'  are recorded. Optional. If none provided, it is assumed there is no
#'  distinctions among size classes.
#' @param max_intervals maximum number of arrival intervals to consider
#'  for each carcass
#' @return list of Mhat, Aj, ghat, SE parameters (pk), CP parameters (ab)
#' @examples NA
#' @export 
#'
estM <- function(data_CO, data_SS, data_DWP, frac = 1,  
    model_SE, model_CP, dateFoundCol = "DateFound", kFill = NULL,
    seed_SE = NULL, seed_CP = NULL, seed_g = NULL, seed_M = NULL,
    unitCol = NULL, datesSearchedCol = NULL, sizeclassCol = NULL, DWPcol = NULL,
    nsim = 1, max_intervals = 8){

  if (!(dateFoundCol %in% colnames(data_CO))){
    stop("dateFoundCol not found in data_CO")
  }
  if (is.null(unitCol)){ # then find common cols in CO and DWP
    unitCol <- intersect(colnames(data_CO), colnames(data_DWP))
    if (length(unitCol) == 0){
      stop(
        "no columns in data_CO and data_DWP share a common name ",
        "to use as a unit column."
      )
    }
    if (length(unitCol) > 1){
      stop(
        "multiple matching column names in data_CO and data_DWP. ",
        "Provide a value for unitCol."
      )
    }
  }
#  SSdat <- SS(data_SS, datesSearchedCol = datesSearchedCol) # not needed?

  # error-checking for match b/t DWP and CO data is done in DWPbyCarcass
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not in carcass data.")
    }
    sizeclasses <- as.character(unique(data_CO[ , sizeclassCol]))
    nsizeclass <- length(sizeclasses)
  }

  DWP <- DWPbyCarcass(data_DWP = data_DWP, data_CO = data_CO,
    sizeclassCol = sizeclassCol, unitCol = unitCol, DWPCol = DWPCol)


  est <- estg(nsim = nsim, data_CO = data_CO, data_SS = data_SS,
    model_SE = model_SE, model_CP = model_CP,
    seed_SE = seed_SE, seed_CP = seed_CP, seed_g = seed_g,
    kFill = kFill, unitCol = unitCol, dateFoundCol = dateFoundCol,
    datesSearchedCol = datesSearchedCol, sizeclassCol = sizeclassCol,
    max_intervals = max_intervals)

  gDWPf <- est$ghat * DWP * frac
  set.seed(seed_M)
  c_out <- which(rowSums(gDWPf) == 0)
  if (length(c_out) == 0){
    n <- length(gDWPf)
    Mhat <- ((rcbinom(n, 1/gDWPf, gDWPf)) - (Ecbinom(gDWPf) - 1))/gDWPf
  } else {
    Mhat <- array(0, dim = c(dim(data_CO)[1], nsim))
    gDWPf <- gDWPf[-c_out, ]
    n <- length(gDWPf)
    Mhat[-c_out,] <- ((rcbinom(n, 1/gDWPf, gDWPf)) - (Ecbinom(gDWPf) - 1))/gDWPf
  }
  out <- list(Mhat, est$Aj, est$ghat, est$pk, est$ab)
  names(out) <- c("M", "Aj", "ghat", "pk", "ab")
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
#' @param unitCol Column name for the unit indicator (optional)
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'  are recorded. Optional.
#' @param DWPcol Name of column where DWP values are stored (optional). Used
#'  when there is more than one DWP column in \code{data_DWP} but analysis is
#'  intended for a single class (i.e., no "size" is specified in data_CO).
#' @return DWP value for each carcass 
#' @examples NA
#' @export 
#'
DWPbyCarcass <- function(data_DWP, data_CO, sizeclassCol = NULL, unitCol = NULL,
  DWPCol = NULL){
  # unitCol is assumed to be the column that the two data sets share.
  # if none are in common, error is thrown with no remedy.
  # if data sets share more than one column, user is asked to input unitCol.
  if (is.null(unitCol)){
    unitCol <- colnames(data_DWP)[colnames(data_DWP) %in% colnames(data_CO)]
    if (length(unitCol) == 0){
      stop("data_DWP and data_CO must have identically-named unit columns")
    }
    if (length(unitCol) > 1){
      stop(paste("more than one possibility for unitCol (", unitCol, ")",
        collapse = " "))
    }
  } else {
    if (length(unitCol) > 1) stop("length(unitCol) must be 1 if provided")
    if (!(unitCol %in% colnames(data_DWP))){
      stop(unitCol, " not found in data_DWP")
    }
  }
  if (!all(data_CO[, unitCol] %in% data_DWP[ , unitCol])){
    stop("Some units present in carcass table not in DWP table")
  }
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not found in carcass data.")
    }
    if (!all(unique(data_CO[,sizeclassCol]) %in% colnames(data_DWP))){
      stop("some sizes represented in data_CO are not represented in data_DWP.")
    }
    rowind <- match(data_CO[, unitCol], data_DWP[, unitCol])
    colind <- match(data_CO[ , sizeclassCol], names(data_DWP))
    DWPbyCarc <- as.numeric(data_DWP[cbind(rowind, colind)])
  } else { # assume same DWP for all sizes, so matching is by unit only
    # which column is DWP?
    # if there's one numeric column with values in (0, 1], that the dwp column
    if (is.null(DWPCol)) {
      possibleNames <- colnames(data_DWP)
    } else {
      possibleNames <- DWPCol
    }
    for (coli in possibleNames){
      DWPcolumn <- NULL
      if (is.numeric(data_DWP[ , coli]) &&
          all(data_DWP[ , coli] <= 1) && all(data_DWP[ , coli] > 0)){
        # candidate DWP has been discovered
        if (is.null(tmpname)){
          DWPcolumn <- coli
        } else { # it is the second one found => conflict
          stop(
            "data_DWP improperly formatted. Must have columns corresponding ",
            "to data_CO sizes or have a single column of DWP's to associate ",
            "with units."
          )
        }
      }
    }
    rowind <- match(data_CO[, unitCol], data_DWP[, unitCol])
    DWPbyCarc <- data_DWP[rowind , DWPColumn]
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
#' @param datesSearchedCol Column name for the date searched data
#'
#' @return list of [1] SS with days (starting with t = 0), [2] CO data with 
#'   days (starting with t = 0), and [3] a vector of the CO_data rows 
#'   indicating the carcasses found during cleanout searches. 
#' @examples NA
#' @export 
#'
prepSSCO <- function(data_SS, data_CO, datesSearchedCol = "DateSearched",
                     dateFoundCol = "DateFound", unitCol = "Unit"){

  data_SS[ , datesSearchedCol] <- yyyymmdd(data_SS[ , datesSearchedCol])
  data_CO[ , dateFoundCol] <- yyyymmdd(data_CO[ , dateFoundCol])
  date0 <- min(data_SS[ , datesSearchedCol])
  data_CO[ , dateFoundCol] <- dateToDay(data_CO[ , dateFoundCol], date0)
  data_SS[, datesSearchedCol] <- dateToDay(data_SS[, datesSearchedCol], date0)

  which_day0 <- which(data_SS[ , datesSearchedCol] == 0)
  SSunitCols <- which(colnames(data_SS) %in% unique(data_CO[ , unitCol]))
  data_SS[which_day0, SSunitCols] <- 1

  c_out <- cleanouts(data_CO, data_SS, unitCol, dateFoundCol,datesSearchedCol)

  out <- list(data_SS, data_CO, c_out)
  names(out) <- c("data_SS", "data_CO", "cleanout_carcasses")
  return(out)
}

#' @title Summarize total mortality estimation
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

  out <- paste0("Median: ", Mmed, "; ", CL * 100, "% CI: [",
           MCLlow, ", ", MCLhi, "]"
         )
  return(out)         
}
