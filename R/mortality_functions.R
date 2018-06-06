#' @title Estimate mortality 
#'
#' @description Given given fitted Searcher Efficiency and Carcass 
#'   Persistence models; Search Schedule, Density Weighted Proportion,
#'   and Carcass Observation data; and information about the fraction of the
#'   the facility that was surveyed. 
#'
#' @param data_CO Carcass Observation data
#' @param data_SS Search Schedule data
#' @param data_DWP Survey unit (rows) by size (columns) density weighted
#'   proportion table
#' @param frac fraction of facility (by units or by area) surveyed
#' @param dateFoundCol Column name for the date found data
#' @param model_SE Searcher Efficiency model (or list of models if there are
#'   multiple size classes)
#' @param model_CP Carcass Persistence model (or list of models if there are
#'   multiple size classes)
#' @param kFill value to fill in for missing k when not existing in the model
#' @param unitCol Column name for the unit indicator (optional)
#' @param datesSearchedCol Column name for the date searched data
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'  are recorded. Optional. If none provided, it is assumed there is no
#'  distinctions among size classes.
#' @param DWPCol Column name for the DWP values in the DWP table when no
#'   size class is used and there is more than one column in \code{data_DWP}
#'   that could be interpreted as DWP.
#' @param seed_SE seed for random draws of the SE model
#' @param seed_CP seed for random draws of the CP model
#' @param seed_g seed for random draws of the gs
#' @param seed_M seed for the random draws of the Mhats
#' @param nsim the number of simulation draws
#' @param max_intervals maximum number of arrival intervals to consider
#'  for each carcass
#' @return list of Mhat, Aj, ghat
#' @examples NA
#' @export 
#'
estM <- function(data_CO, data_SS, data_DWP, frac = 1,
  dateFoundCol = "DateFound", model_SE, model_CP, kFill = NULL,
  unitCol = NULL, datesSearchedCol = NULL, sizeclassCol = NULL, DWPCol = NULL,
  seed_SE = NULL, seed_CP = NULL, seed_g = NULL, seed_M = NULL,
  nsim = 1, max_intervals = 8){


  if (!(dateFoundCol %in% colnames(data_CO))){
    stop("dateFoundCol not found in data_CO")
  }
  # attempted auto-parsing for unitCol:
  #  find common cols in CO and DWP as the candidate unitCol
  #  if unique, then use that as unitCol
  #  if not present or not unique, then error
  if (is.null(unitCol)){ 
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
  # if no sizeclassCol is provided, then the later analysis is done without
  #   making distinctions between sizes; no error-checking here
  # if sizeclassCol is provided, it must be present in CO. It's levels must also
  #  all be present in DWP, but the check is done in the DWPbyCarcass function,
  #  which allow DWPbyCarcass to more readily be used as a standalone function
  #  if user wishes.
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not in carcass data.")
    } else {
      sizeclasses <- as.character(unique(data_CO[ , sizeclassCol]))
      nsizeclass <- length(sizeclasses)
    }
  }

  # error-checking for match b/t DWP and CO data is done in DWPbyCarcass
  DWP <- DWPbyCarcass(data_DWP = data_DWP, data_CO = data_CO,
    sizeclassCol = sizeclassCol, unitCol = unitCol, DWPCol = DWPCol)

  est <- estg(data_CO = data_CO, data_SS = data_SS, dateFoundCol = dateFoundCol,
    model_SE = model_SE, model_CP = model_CP, kFill = kFill, unitCol = unitCol,
    datesSearchedCol = datesSearchedCol, sizeclassCol = sizeclassCol,
    seed_SE = seed_SE, seed_CP = seed_CP, seed_g = seed_g,
    nsim = nsim, max_intervals = max_intervals)

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
  out <- list(Mhat = Mhat, Aj = est$Aj, ghat = est$ghat, pk = est$pk, ab = est$ab)
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
#' @param DWPCol Name of column where DWP values are stored (optional). Used
#'  when there is more than one DWP column in \code{data_DWP} but analysis is
#'  intended for a single class (i.e., no "size" is specified in data_CO).
#' @return DWP value for each carcass 
#' @examples NA
#' @export 
#'
DWPbyCarcass <- function(data_DWP, data_CO,
  unitCol = NULL, sizeclassCol = NULL, DWPCol = NULL){
  # if called by estM, unitCol is provided; if called from elsewhere, the
  #   column can be autoparsed to find unitCol as is done in estM. [Extract
  #   this into a standalone parsing function that both DWPbyCarcass and
  #   and estM use? lower priority at this point]
  # if unitCol = NULL, then is assumed to be the column that the two data sets
  #   share.
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
  # length(unitCol) = 1 and has been found in both CO and DWP;
  # are all units in data_CO also found in DWP?
  if (!all(data_CO[, unitCol] %in% data_DWP[ , unitCol])){
    stop("Some units present in carcass table not in DWP table")
  } # error-checking on units is complete

  # parse w.r.t. sizeclassCol arg and assign DWP to carcasses
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not found in carcass data.")
    }
    if (!all(unique(data_CO[,sizeclassCol]) %in% colnames(data_DWP))){
      stop("some sizes represented in data_CO are not represented in data_DWP.")
    }
    # size classes and units have been error-checked, and assigning DWP to
    #  carcasses is a simple extraction of DWP from the appropriate row and
    #  column for each carcass in CO.
    # unit in CO defines the desired row in DWP:
    rowind <- match(data_CO[, unitCol], data_DWP[, unitCol])
    # size in CO defines the desired column in DWP:
    colind <- match(data_CO[ , sizeclassCol], names(data_DWP))
    # and DWPbyCarc falls out naturally
    DWPbyCarc <- as.numeric(data_DWP[cbind(rowind, colind)])

  } else { # assume same DWP for all sizes, so matching is by unit only
    # which column has the DWP data?
    # if DWPCol is provided, then that is the candidate DWP column (but still
    #   needs to be error-checked)
    # if no DWPCol is provided, then check for possible DWPCol's
    #   if there is only one column with values in (0, 1], that's DWPCol
    #   if there is not a unique column with values in (0, 1], error
    if (is.null(DWPCol)) {
      possibleNames <- colnames(data_DWP)
    } else { # DWPCol has been provided, but must be error-checked (as below)
      possibleNames <- DWPCol 
    }
    for (coli in possibleNames){
      DWPcolumn <- NULL
      if (is.numeric(data_DWP[ , coli]) &&
          all(data_DWP[ , coli] <= 1) && all(data_DWP[ , coli] > 0)){
        # candidate DWP has been discovered
        if (is.null(DWPcolumn)){
          DWPcolumn <- coli
        } else { # it is the second one found => conflict
          stop(
            "data_DWP improperly formatted. Must have columns corresponding ",
            "to data_CO sizes or have a single column of DWP's to associate ",
            "with units. Alternatively, specify DWP column via DWPCol arg"
          )
        }
      }
    }
    # DWPCol and unitCol have been error-checked, so
    rowind <- match(data_CO[, unitCol], data_DWP[, unitCol])
    DWPbyCarc <- as.numeric(data_DWP[rowind , DWPcolumn])
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

### cleanouts is simply a matter of ignoring carcasses that are found on the
### very first search of the season. This is done in the newer versions of the
### code.

### If users want to clean something else out, that's their prerogative, but
### they then step out of our model, and they will need to accomplish their
### custom cleanouts by editing their input files.

### possible to delete this function?
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

### parsing and formatting of SS data is done in SS(). data_CO parsing is simple
### enough to do on demand without needing to separate it out from the "live"
### code (which makes it easier to follow)

### possible to delete this?

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

### the previous version returned a character string with summary statistics.
### That format is not easy for command line users to use. I changed it to
###   a numeric vector, which users can use without having to parse.

### S3's for print() can profitably use text output because it allows
###   some nice formatting for viewing, but summary() should have readily
###   extractable data.


summary.estM <- function(object, ..., CL = 0.9){
  alpha <- 1 - CL
  Mtot <- colSums(object$Mhat) # vectorized, and more transparent than "apply"
  out <- c(
    "Median" = round(median(Mtot), 2),
    "lwr" = round(quantile(Mtot, alpha/2), 2), 
    "upr" = round(quantile(Mtot, 1 - alpha/2), 2),
    "CL" = CL
   )
  return(out)
}
