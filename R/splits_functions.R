#' Estimate the number of fatalities in each search interval throughout the
#' monitoring period.
#'
#' A carcass that is observed in a given search may have arrived at any time
#' prior to that search, so carcass discovery time is often not a reliable
#' estimate of carcass arrival time. For each observed carcass, 
#' \code{calcRate} takes into account the estimated probability of arrival
#' in each possible arrival interval, adjusts by detection probability, and 
#' sums to estimate the estimated number of carcass arrivals in every search
#' interval.
#'
#' @param M Numeric array (ncarc x nsim) of estimated number of fatalities
#'  by observed carcass and simulation rep
#' @param Aj Integer array (ncarc x nsim) of simulated arrival intervals for
#'  each observed carcass. Arrival intervals are given as integers j, 
#'  indicating that the given carcass (indexed by row) arrived in the jth
#'  search interval in the given simulation rep (indexed by column). Arrival 
#'  interval indices (j) are relative to indexed carcasses' search schedules.
#' @param data_SS \code{\link{SS}} object that contains formatted data for 
#'  calculating splits. Optional argument. Alternatively, user may provide 
#'  \code{days} and \code{searches_carcass}.
#' @param days Vector of all dates that at least one unit was searched. Format
#'  is the number of days since the first search. For example, days = c(0, 7,
#'  14, 28, 35) for a simple 7-day search schedule in which searches were
#'  conducted every once per week on the same day for 5 weeks. Not all units
#'  need be searched on every search date.
#' @param searches_carcass An ncarc x length(days) array of 0s and 1s to 
#'  indicate searches in which the indexed carcass could have been found. 
#'  For example, row i = \code{c(1, 0, 1, 0, 1)} indicates that the search 
#'  schedule for the location (unit) where carcass i was found would be 
#'  \code{days[c(1, 3, 5)]}.
#' @return Numeric array (nsim x nsearch) of estimated fatalities in each
#'  search interval. NOTE: The search at time t = 0 does not correspond to an
#'  interval, and all carcasses found at that time are assumed to have arrived
#'  prior to the monitoring period and are not included in mortality estimates
#'  so \code{nsearch = length(days) - 1}.
#'
#' @export
#'
calcRate <- function(M, Aj, days = NULL, searches_carcass = NULL, 
                     data_SS = NULL){
 if (!is.null(data_SS) && ("SS" %in% class(data_SS))){
    days <- data_SS$days
    unit <- rownames(Aj)
    x <- nrow(Aj)
    searches_carcass <- array(0, dim = c(x, length(days)))
    for (xi in 1:x){
       searches_carcass[xi, ] <- data_SS$searches_unit[unit[xi], ]
    }
  } else if (!is.null(data_SS)){
    stop(
      "In arg list, data_SS must be an SS object. Alternatively, user may ",
      "provide 'days' and 'carcass_searches'."
    )
  }
  if (is.vector(M)){
    M <- matrix(M, nrow = 1)
    Aj <- matrix(Aj, nrow = 1)
    searches_carcass <- matrix(searches_carcass, nrow = 1)
  }
  calcRateC(M, Aj, days, searches_carcass)
}

#' Estimate the number of fatalities by time interval
#'
#' \code{calcTsplit()} is a lower-level function that requires the output
#' of \code{calcRate} as input. See \code{\link{calcSplits}} for a more 
#' powerful, convenient, and flexible alternative.
#'
#' @param rate Array (nsim x nsearch) of arrival rates as number of fatalities

#'  per search interval. Typically, \code{rate} will be the return value of 
#'  the \code{calcRate} function. 
#' @param days A vector of times representing search dates when at least one
#'  unit was searched. Times are formatted as number of days since the first
#'  search, e.g., c(0, 7, 14, 28, 35) would indicate a schedule in at least 
#'  one unit was searched every 7 days.
#' @param tsplit A vector of times that splits the monitoring period into a
#'  set of time intervals for which \code{calcTsplit} will estimate the number
#'  of fatalities. For example, if \code{tsplit = c(0, 14, 19, 35)}, then
#'  \code{calcTsplit} estimates the number of fatalities occuring in intervals
#'  (0, 14], (14, 19], and (19, 35]. Times in \code{tsplit} must be increasing
#'  and between 0 and max(days), inclusive.
#' @return A numeric array with dimensions
#'  \code{dim = c(length(tsplit) - 1, nsim)} giving the estimated number of
#'  fatalities that occured in each time interval.
#'
#' @export
#'
calcTsplit <- function(rate, days, tsplit){
  if (is.vector(rate)){
    rate <- matrix(rate, nrow = 1)
  }
  calcTsplitC(rate, days, tsplit)
}

#' Estimate the number of fatalities by up to two splitting covariates
#'
#' Total mortality can be split into sub-categories, according to various
#' splitting covariates such as species, visibility class, season, site, unit,
#' etc. Given the carcass search data, estimated mortalities, and splitting
#' covariates, \code{calcSplits()} gives the 'splits' or summaries the 
#' estimated mortalities by levels of the splitting covariates. For example,
#' user may specify \code{"season"} and \code{"species"} as splitting 
#' variables to see estimated mortalities by season and species. Input would
#' be arrays of estimated mortalities and arrival intervals when \code{ncarc}
#' carcass have been discovered and uncertainty in mortality estimates is 
#' captured via simulation with \code{nsim} simulation draws.
#'
#' Arrival intervals (\code{Aj}) are given as integers, j, that indicate which
#' search interval the given carcass (indexed by row) arrived in in the given
#' simulation draw (indexed by column). Arrival interval indices (j) are
#' relative to indexed carcasses' search schedules.
#'
#' No more than two splitting variables (\code{split_CO}, \code{split_SS}, and
#' \code{split_time}) in total may be used. \code{split_CO} variables describe
#' qualitative characteristics of the observed carcasses or where they were
#' found. Some examples include searcher (DHD, JPS, MMH), carcass size 
#' (S, M, L), species, age (fresh/dry or immature/mature), unit, visibility 
#' class (easy, moderate, difficult), etc.
#'
#' \code{split_SS} variables describe characteristics of the search intervals,
#' such as season (spring, summer, fall, winter) or treatment
#' (pre- or post-minimization). Each search interval is assigned a level of 
#' the \code{split_SS} variable. For example, for a search schedule with
#' 5 searches (including a search at t = 0), and the \code{split_SS} variable
#' would have values for each of the 4 search intervals. The levels of the
#' \code{split_SS} must be in contiguous blocks. For example,
#' \code{season = c("S", "S", "F", "F")} would be acceptable, but
#' \code{season = c("S", "F", "S", "F")} would not be.
#'
#' \code{split_time} variables are numeric vectors that split the monitoring
#' period into distinct time intervals. For example,
#' \code{split_time = c(0, 30, 60, 90, 120)} would split the 120 monitoring
#' period into 30-day intervals, and \code{calcSplits()} would return 
#' mortality estimates for each of the intervals.
#'
#' @param M Numeric array (ncarc x nsim) of estimated mortalities, such as
#'  those returned by the function xxx.
#' @param Aj Integer array (ncarc x nsim) of simulated arrival intervals for
#'  each observed carcass. Typically, the \code{Aj}
#'  array will be the return value of function xxx.
#' @param split_CO Character vector of names of splitting covariates to be 
#'  found in the \code{data_CO} data frame. No more than two \code{split_CO} 
#'  variables are allowed. Use \code{split_CO = NULL} if no CO splits are 
#'  desired. 
#' @param data_CO data frame that summarizes the carcass search data and must
#'  include columns specified by the \code{split_CO} arg. Each row includes
#'  search and discovery parameters associated with a single observed carcass.
#'  Columns include carcass ID, carcass discovery date, unit, and any number 
#'  of covariates. \code{data_CO} is required if and only if \code{split_CO} 
#'  is non-NULL.
#' @param split_SS Character string giving the name of a splitting covariate 
#'  in the \code{data_SS} list, with \code{data_SS[[split_SS]]} describing
#'  characteristics of the search intervals (e.g., "season"). Note that
#'  \code{length(data_SS[[split_SS]]} must equal 
#'  \code{length(data_SS$days) - 1} becasue no inference is made about 
#'  carcass arrivals prior to time t = 0, and the "interval" prior to t = 0 
#'  is not taken as a "search interval." If no \code{split_SS} split is 
#'  desired, use \code{split_SS = NULL}.
#' @param data_SS Search schedule data
#' @param split_time Numeric vector that defines time intervals for splits.
#'  Times must be numeric, strictly increasing, and span the monitoring period
#'  [0, \code{max(data_SS$days)}]. If no \code{split_time} is desired, use
#'  \code{split_time = NULL}. If \code{split_time} is non-NULL, \code{data_SS}
#'  is required.
#' @param ... arguments to be passed down
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @param unitCol Column name for the unit indicator
#' @return An object of class \code{splitFull} is returned. If one splitting
#'  covariate is given, then the output will be an array of estimated 
#'  mortality in each level of the splitting covariate, with one row for each
#'  covariate level and one column for each simulation draw. If two splitting
#'  covariates are given, output will be a list of arrays. Each array gives 
#'  the estimated mortalities for one level of the second splitting covariate
#'  and all levels of the first splitting covariate.
#'
#' Objects of class \code{splitFull} have attributes \code{vars} (which gives
#' the name of the splitting covariate(s)) and \code{type} (which specifies
#' whether the covariate(s) are of type \code{split_CO}, \code{split_SS}, or
#' \code{split_time}). A summary of a resulting \code{splitFull} object
#' is returned from the S3 function \code{summary(splits, CL = 0.95, ...)},
#' which gives the mean and a 5-number summary for each level of each 
#' covariate. The 5-number summary includes the alpha/2, 0.25, 0.5, 0.75, and
#' 1 - alpha/2 quantiles, where alpha = 1 - CL. A graph summarizing the
#' results can be drawn using \code{plot(splits, CL, ...)}, which gives
#' a graphical representation of the \code{summary}.
#'
#' @export
#'
calcSplits <- function(M, Aj = NULL, split_CO = NULL, data_CO = NULL,
                       split_SS = NULL, data_SS = NULL, split_time = NULL,
                       unitCol = "Unit", dateFoundCol = "DateFound", 
                       dateSearchedCol = "DateSearched", ...){

  ##### read data and check for errors
  if ((!is.null(split_SS) || !is.null(split_time)) & is.null(data_SS)){
    stop("data_SS must be provided if ",
      ifelse(is.null(split_SS), "split_time ", "split_SS "), "is")
  }
  if (!is.null(split_CO) + !is.null(split_SS) + !is.null(split_time) > 0){
    if (is.null(data_CO)){
      stop("data_CO must be provided to perform non-null splits")
    }
  }
 
  dateSearched <- data_SS[ , dateSearchedCol]
  data_SS[ , dateSearchedCol] <- yyyymmdd(data_SS[ , dateSearchedCol])
  data_CO[ , dateFoundCol] <- yyyymmdd(data_CO[ , dateFoundCol])

  date0 <- min(data_SS[ , dateSearchedCol])
  data_CO[ , dateFoundCol] <- dateToDay(data_CO[ , dateFoundCol], date0)
  data_SS[ , dateSearchedCol] <- dateToDay(data_SS[ , dateSearchedCol], date0)

  which_day0 <- which(data_SS[ , dateSearchedCol] == 0)
  SSunitCols <- which(colnames(data_SS) %in% unique(data_CO[ , unitCol]))
  data_SS[which_day0, SSunitCols] <- 1

  cleanout <- whichCleanout(data_CO, data_SS, unitCol, dateFoundCol,
                dateSearchedCol
              )  
  if (length(cleanout) > 0){
    data_CO <- data_CO[-cleanout, ]
  }
  data_SS[ , dateSearchedCol] <- dateSearched
  data_SS <- SS(data_SS)

  unit <- rownames(Aj)
  ### declare traffic directing variables (updated after parsing inputs)
  # number of valid split variables:
  nvar <- 0
  # characterization of horizontal and vertical split variables as "SS"
  # or "CO":
  vartype <- NULL
  split_h <- NULL
  split_v <- NULL
  ### error-checking the split variables and interpreting inputs:
  if (!is.null(split_time)){
    if (!is.numeric(split_time) || !is.vector(split_time)){
      stop("split_time must be NULL or a numeric vector")
    }
    minspl <- min(split_time)
    if (minspl != 0){
      if (minspl < 0){
        stop("min(split_time) = ", minspl, " but must not be < 0" )
      }
      if (minspl > 0){
        warning(
          "min(split_time) = ", minspl, ". ",
          "Appending 0 to split_time, so first split is [0, ", minspl, "]."
        )
      }
    }
    maxspl <- max(split_time)
    maxdays <- max(data_SS$days)
    if (maxspl != maxdays){
      if (maxspl > maxdays){
        stop(
          "max(split_time) = ", maxspl, " ",
          "but must not be > max(data_SS$days) = ", maxdays
        )
      }
      if (maxspl > maxdays){
        warning(
          "max(split_time) = ", maxspl, " < max(data_SS$days) = ", maxdays, 
          ". ", "Appending max(days) to split_time so last split is ",
          "[", maxspl, ", ", max(data_SS$days),"]."
        )
      }
    }
    if (sum(diff(split_time) <= 0) > 0){
      stop("split_time must be strictly increasing")
    }
    # split_time has passed the initial error-checking, so assign attributes
    # to characterize the data
    split_h <- list()
    split_h[["name"]] <- "time"
    split_h[["vals"]] <- split_time
    if (minspl > min(data_SS$days)) split_h$vals <- c(0, split_h$vals)
    if (maxspl < maxdays) split_h$vals <- c(split_h$vals, maxdays)
    split_h$vals[length(split_h$vals)] <- data_SS$days[length(data_SS$days)]
    split_h[["level"]] <- unique(split_h$vals[-1])
    split_h[["nlev"]] <- length(split_h$level) - 1
    split_h[["type"]] <- "time"
  }
  if (!is.null(split_SS)){ # there is a split_SS variable
    if (!is.null(split_h)){
      stop(
        "Only one temporal split allowed. ",
        "Either split_SS or split_time must be NULL"
      )
    }
    if (!is.character(split_SS)){
      stop("split_SS must be NULL or the name of an element in data_SS")
    }
    if (length(split_SS) > 1){
      stop("At most 1 split_SS variable is allowed")
    }
    if (!(split_SS %in% names(data_SS))){
      stop(split_SS, " not found in ", data_SS)
    }
    split_h <- list()
    split_h[["name"]] <- split_SS
    tmp <- 
      cumsum(table(data_SS[[split_h$name]])[unique(data_SS[[split_h$name]])])
    split_h[["vals"]] <- c(0, data_SS$days[tmp])
    split_h[["level"]] <- unique(data_SS[[split_h$name]])
    split_h[["nlev"]] <- length(split_h$level)
    split_h[["type"]] <- "SS"
  }
  if (!is.null(split_CO)){
    if (length(split_CO) > 2 | (length(split_CO) > 1 & !is.null(split_h))){
      stop(
        "At most two split variables are allowed in total, i.e. ",
        "length(split_CO) + length(split_SS) + length(split_time) ",
        "must be <= 2"
      )
    }
    if (!is.character(split_CO)){
      stop("split_CO must be a name of a selected column in data_CO")
    }
    if (sum(split_CO %in% names(data_CO)) != length(split_CO)){
      stop(split_CO[which(!(split_CO %in% names(data_CO)))], "not in data_SS")
    }
    if (length(split_CO) == 2 | !is.null(split_h)){
      split_v <- list()
      split_v[["name"]] <- ifelse(!is.null(split_h), split_CO[1], split_CO[2])
      split_v[["vals"]] <- data_CO[[split_v$name]]
      split_v[["level"]] <- unique(split_v$vals)
      split_v[["nlev"]] <- length(split_v$level)
      split_v[["type"]] <- "CO"
    }
    if (is.null(split_h)){
      split_h <- list()
      split_h[["name"]] <- split_CO[1]
      split_h[["vals"]] <- data_CO[[split_h$name]]
      split_h[["level"]] <- unique(split_h$vals)
      split_h[["nlev"]] <- length(split_h$level)
      split_h[["type"]] <- "CO"
    }
  }
  nvar <- sum(c(!is.null(split_h), !is.null(split_v)))

  # additional preprocessing
  if (is.vector(M)) M <- matrix(M, nrow = 1)
  x <- dim(M)[1] # total observed carcasses (assumes data_CO is error-checked)
  nsim <- dim(M)[2] # number of simulation draws (columns in M)
  if (split_h$type %in% c("time", "SS")){
      days <- data_SS$days
      searches_carcass <- array(0, dim = c(x, length(days)))
      for (xi in 1:x){
        searches_carcass[xi, ] <- data_SS$searches_unit[unit[xi], ]
      }
  }
  # calculate splits
  if (nvar == 0){ # no splits...just calculate total M
    splits <- colSums(M)
  } else if (nvar == 1){ # just one split variable: split_h
    if (split_h$type == "CO"){
      splits <- array(dim = c(split_h$nlev, nsim))
      for (li in 1:split_h$nlev) {
        lind <- which(data_CO[, split_h$name] == split_h$level[li])
        if (length(lind) == 1){
          splits[li, ] <- M[lind, ]
        } else {
          splits[li, ] <- colSums(M[lind, ])
        }
      }
    } else if (split_h$type %in% c("time", "SS")){
      days <- data_SS$days
      rate <- calcRate(M, Aj, days = days, 
                searches_carcass = searches_carcass
              )
      splits <- calcTsplit(rate, data_SS$days, split_h$vals)
    }
  } else if (nvar == 2){ # two split variables: split_h and split_v
    splits <- list()
    if (split_h$type == "CO"){
      for (vi in 1:split_v$nlev){
        splits[[vi]] <- array(0, dim = c(split_h$nlev, nsim))
        for (li in 1:split_h$nlev) {
          lind <- which(
            split_h$vals == split_h$level[li] &
            split_v$vals == split_v$level[vi]
          )
          if (length(lind) > 1){
            splits[[vi]][li, ] <- colSums(M[lind, ])
          } else if (length(lind) == 1){
            splits[[vi]][li, ] <- M[lind, ]
          }
        }
      }
    } else if (split_h$type %in% c("SS", "time")){
      for (vi in 1:split_v$nlev){
        lind <- which(split_v$vals == split_v$level[vi])
        rate <- calcRate(M = M[lind, ], Aj = Aj[lind, ], days = data_SS$days,
          searches_carcass = searches_carcass[lind,])
        splits[[vi]] <- calcTsplit(rate, data_SS$days, split_h$vals)
      }
    }
  }
  #protection against unintended loss of attr's
  splits <- sticky::sticky(splits) 
  attr(splits, "vars") <- c(split_h$name, split_v$name)
  attr(splits, "type") <- c(split_h$type, split_v$type)
  if (split_h$type %in% c("time", "SS")){
    attr(splits, "times") <- split_h$vals
  }
  if (nvar == 1){
    rownames(splits) <- split_h$level
  }
  if (nvar == 2){
    names(splits) <- split_v$level
    for (i in 1:length(splits)){
      rownames(splits[[i]]) <- split_h$level
    }
  }
  class(splits) <- "splitFull"
  return(splits)
}

#' Summarize results of mortality estimate splits
#'
#' Mortality estimates can be calculated for the various levels of splitting
#' covariates such as season, species, or visibility class using
#' \code{\link{calcSplits}}, which gives full arrays of simulated M estimates
#' (i.e., for each level of each splitting covariate, each discovered carcass,
#' and each simulation draw). summary(splits, CL = 0.95, ...) gives
#' summary statistics of the estimates.
#'
#' @param object A \code{splitFull} object (\code{\link{calcSplits}}) that 
#'  gives simulated mortality estimates for all combinations of levels of 1 
#'  or 2 splitting covariates.
#' @param CL desired confidence level for summary CIs (numeric scalar in 
#'  (0, 1))
#' @param ... to be passed down
#' @return an object of class \code{splitSummary}, which gives 5-number
#'  summaries for all combinations of levels among the splitting covariates 
#'  in the \code{splits}. The 5-number summaries include the mean and alpha/2,
#'  0.25, 0.5, 0.75, and 1 - alpha/2 quantiles of mortality estimates, where
#'  alpha = 1 - CL. A graphical representation of the results can be
#'  produced using \code{plot(splits, CL, ...)}.
#' @export
#'
summary.splitFull <- function(object, CL = 0.95, ...){
  splits <- object
  alpha <- 1 - CL
  probs <- c(alpha / 2, 0.25, 0.5, 0.75, 1 - alpha / 2)
  if (is.null(attr(splits, "vars"))){
    sumry <- c(mean = mean(splits), quantile(splits, probs = probs))
  } else if (length(attr(splits, "vars")) == 1){
    if (is.vector(splits)) splits <- matrix(splits, nrow = 1)
    sumry <- cbind(mean = rowMeans(splits), 
               rowQuantiles(splits, probs = probs)
             )
  } else if (length(attr(splits, "vars")) == 2){
    if (is.vector(splits[[1]])){
      splits <- lapply(splits, function(x){
        matrix(x, nrow = 1)
      })
    }
    sumry <- lapply(splits, function(x){
      cbind(mean = rowMeans(x), rowQuantiles(x, probs = probs))
    })
  } else {
    stop(
      "length(attr(splits, 'vars')) > 2.",
      "At most two split variables are allowed."
    )
  }
  sumry <- sticky(sumry)
  attr(sumry, "CL") <- CL
  attr(sumry, "vars") <- attr(splits, "vars")
  attr(sumry, "type") <- attr(splits, "type")
  attr(sumry, "times") <- attr(splits, "times")
  class(sumry) <- "splitSummary"
  return(sumry)
}

#' Plot summary statistics for splits of mortality estimates
#'
#' The S3 \code{plot} method for \code{splitSummary} objects constructs 
#'  boxplots of the mortality estimates for all combinations of splitting 
#'  covariates summarized in the \code{splits} variable.
#'
#' For 1-covariate splits, box plots showing median, IQR, and confidence
#' intervals (for the \code{CL} attribute for the splits object). For
#' 2-covariate splits, the box plots are in an array with levels of
#' the temporal split (\code{split_SS} or \code{split_time}) arranged
#' horizontally (if present) and the levels of the \code{split_CO} variable
#' arranged vertically. If no temporal splits are present, then the box plots
#' along the levels of the first \code{split_CO} variable are arranged
#' horizontally and the levels of the second variable are are arranged
#' vertically.
#'
#' @param x A \code{splitSummary} object (result of \code{\link{calcSplits}})
#'  that includes summary statistics for simulated mortality estimates for all
#'  combinations of levels of 1 or 2 splitting covariates.
#' @param rate \code{logical} scalar indicating whether the figures should be
#'  plotted as number of fatalities per split category (\code{rate = TRUE}) or
#'  fatality rates per unit time (\code{rate = TRUE}). If the splits do not
#'  include either a \code{split_SS} or \code{split_time} variable, the
#'  \code{rate} arg is ignored.
#' @param ... additional arguments to be passed down
#' @export
#'
plot.splitSummary <- function(x, rate = FALSE, ...){
  splits <- x
  nvar <- length(attr(splits, "vars"))
  vartype <- attr(splits, "type")
  if (vartype[1] == "CO") rate <- FALSE
  vars <- attr(splits, "vars")
  alpha <- 1 - attr(splits, "CL")
  times <- attr(splits, "times")
  probs <- c(alpha/2, 0.25, 0.5, 0.75, 1 - alpha/2)
  deltaT <- diff(times)
  if (nvar == 0){
    # split is for total...what kind of figure? Need to show Mtilde & Mhat
  } else if (nvar == 1 & !is.list(splits)){
    splits <- list(splits)
    vnames <- NULL
  } else {
    vnames <- names(splits)
  }
  hnames <- rownames(splits[[1]])
  par(mar = c(0, 0, 0.5, 0.5), oma = c(6, 5.5, 4, 4))
  nlevel_h <- nrow(splits[[1]])
  nlevel_v <- length(splits)
  par(mfrow = c(nlevel_v, 1))
  cex.axis <- 1*(nlevel_v == 1) + (nlevel_v == 2)/0.83 + (nlevel_v > 2)/0.66
  for (vi in 1:nlevel_v){
    if ((vartype[1] %in% c("time", "SS")) & rate) {
      hwid <- deltaT/2
      xlim <- range(times)
      ylim <- range(
                rowQuantiles(splits[[vi]], probs = c(alpha/2, 1 - alpha/2)
                )/deltaT
              )
    } else {
      hwid <- rep(0.45, nlevel_h) # half-width of boxes
      xlim <- c(1, nlevel_h) + hwid[1] * c(-1, 1)
      ylim <- range(
                rowQuantiles(splits[[vi]], probs = c(alpha/2, 1 - alpha/2))
              )
    }
    plot(0, xlim = xlim, ylim = ylim, type = "n", axes = F, xlab = "", 
      ylab = ""
    )
    if (vartype[1] == "CO" | !rate){
      xx <- 1:nlevel_h 
    } else {
      xx <- times[-1] - hwid
    }
    for (hi in 1:nlevel_h){
      ratebars <- !(vartype[1] == "CO" || !rate)
      deno <- ifelse(ratebars, deltaT[hi], 1)
      qtls <- quantile(splits[[vi]][hi, ], prob = probs)/deno
      polygon(xx[hi] + hwid[hi] * c(1, 1, -1, -1), qtls[c(2, 4, 4, 2)])
      lines(xx[hi] + hwid[hi] * c(1, -1), rep(qtls[3], 2), lwd = 3)
      if (alpha >= 0.5) yst <- c(3, 3) else yst <- c(2, 4)
      lines(rep(xx[hi], 2), qtls[c(1, yst[1])])
      lines(rep(xx[hi], 2), qtls[c(yst[2], 5)])
      lines(xx[hi] + hwid[hi]/(2 - ratebars) * c(1, -1), rep(qtls[1], 2))
      lines(xx[hi] + hwid[hi]/(2 - ratebars) * c(1, -1), rep(qtls[5], 2))
    }
    axis(2, las = 1)
    
    ymid <- mean(par("usr")[3:4])
    if (!rate | vartype[1] == "CO"){
      if (vartype[1] == "time"){
        at <- 0.5 + 0:nlevel_h
        lab <- c(0, rownames(splits[[vi]]))
      } else {
        at <- 1:nlevel_h
        lab <- rownames(splits[[vi]])
      }
    } else {
      at <- times
      lab <- times
    }
    if (vi == nlevel_v){
      axis(1, at = at, labels = lab, cex.axis = cex.axis)
    }
    if (nlevel_v > 1){
      axis(4, at = mean(par("usr")[c(3, 4)]), labels = vnames[vi],
        tck = 0, mgp = c(3, 0.5, 0), cex.axis = cex.axis)
    }
  }
  mtext(side = 1, vars[1], line = 4.6, cex = 1.2)
  if (vartype[1] == "SS" & rate) {
    mtext(side = 1, line = 3, text = hnames, at = times[-1] - diff(times)/2)
  }
  ylab <- ifelse(vartype[1] == "CO" | !rate,
    paste0("Estimated M"),
    paste0("Estimated M/day")
  )
  mtext(side = 2, text = ylab, outer = T, line = 3.5, cex = 1.2)

  title <- ifelse(vartype[1] == "CO" | !rate,
    paste0("Estimated mortality by ", vars[1]),
    paste0("Estimated daily mortality rate")
  )
  mtext(text = title, side = 3, line = 2, cex = 1.3, outer = T)
  mtext(side = 3, line = 0.5, cex = 0.9, outer = T,
    text = paste0("Median, IQR, and ", 100*(1 - alpha), 
             "% confidence intervals")
           )
  mtext(side = 4, text = vars[2], outer = T, line = 2.5, cex = 1.2)
}

#' Plot summary statistics for splits of mortality estimates
#'
#' The S3 \code{plot} method for \code{splitFull} objects constructs boxplots
#' of the mortality estimates for all combinations of splitting covariates
#' summarized in the \code{splits} variable. This is a simple wrapper function
#' for creating a \code{splitSummary} object by calling
#' \code{\link{summary.splitFull}} and plotting the result via
#' \code{\link{plot.splitSummary}}.
#'
#' @param x A \code{splitSummary} object (result of \code{\link{calcSplits}})
#'  that includes summary statistics for simulated mortality estimates for all
#'  combinations of levels of 1 or 2 splitting covariates.
#' @param rate \code{logical} scalar indicating whether the figures should be
#'  plotted as number of fatalities per split category (\code{rate = TRUE}) or
#'  fatality rates per unit time (\code{rate = TRUE}). If the splits do not
#'  include either a \code{split_SS} or \code{split_time} variable, the
#'  \code{rate} arg is ignored.
#' @param CL desired confidence level to show in box plots
#' @param ... to be passed down
#'
#' @export
#'
plot.splitFull <- function(x, rate = FALSE, CL = 0.95, ...){
  plot(summary(x, CL), rate)
}

#' Create search schedule data into an SS object for convenient splits 
#'  analyses
#'
#' @description Since data_SS columns largely have a specific, required
#'   format, the \code{SS} function can often decipher the data automatically,
#'   but the user may specify explicit instructions for parsing the data for
#'   safety if desired. If the data are formatted properly, the automatic 
#'   parsing is reliable in most cases. There are two exceptions. (1) If 
#'   there is more than one column with possible dates (formatted as formal
#'   dates (as class \code{Date}, \code{POSIXlt} or \code{POSIXct}) or 
#'   character strings or factors that can be unambiguously interpreted as 
#'   dates (with assumed format "2018-05-15" or "2018/5/15"). In that case, 
#'   the user must specify the desired dates as \code{dateColumn}. (2) If
#'   there is a covariate column consisting entirely of 0s and 1s. In that
#'   case, the user must specify the column(s) in \code{covars}.
#' If no \code{dateColumn} is given, \code{SS} will attempt to find the date
#'   column based on data formats. If there is exactly one column that can be
#'   interpreted as dates, that column will be taken as the dates. Column 
#'   names that are not listed as \code{covars} will be interpreted as 
#'   "units" if their data consists solely of 0s and 1s. Otherwise, they will
#'   be included in the catch-all category of "covariates".
#'
#' @param data_SS data frame or matrix with search schedule parameters, 
#'  including columns for search dates, covariates (describing characteristics
#'  of the search intervals), and each unit (with 0s and 1s in rows to 
#'  indicate whether the given unit was searched on the given date)
#' @param dateCol name of the column with the search dates in it
#' @param preds vector of character strings giving the names of columns to be
#'  interpreted as potential covariates.
#' @return \code{SS} object that can be conveniently used in the splitting
#'  functions.
#'
#' @export
#'
SS <- function(data_SS, dateCol = NULL, preds = NULL){
  if (! (class(data_SS) %in% c("data.frame", "matrix"))){
    stop("data_SS must be a data frame or matrix")
  } else if (is.null(colnames(data_SS))){
    stop("data_SS columns must be named")
  }
  # extract search dates (if possible)
  if (is.null(dateCol)){
    for (coli in colnames(data_SS)){
      tmp <- try(as.Date(data_SS[, coli]), silent = T)
      if (class(tmp) != "try-error"){
        if (!is.null(dateCol)){
          stop("more than 1 date column in data_SS")
        } else {
          dateCol <- coli
        }
      }
    }
    if (is.null(dateCol)){
      stop("no columns can be interpreted as dates")
    }
  } else {
    tmp <- try(as.Date(data_SS[, coli]), silent = T)
    if (class(tmp) == "try-error"){
      stop(paste(dateCol, "is not properly formatted as dates"))
    }
  }
  # extract units
  unitNames <- NULL
  for (coli in colnames(data_SS)){
    if (coli %in% c(preds, dateCol)) next
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
  dates <- as.Date(data_SS[, dateCol])
  date0 <- min(dates)
  ans <- list()
  ans$date0 <- date0
  ans$days <- as.numeric(difftime(dates, date0, units = "days"))
  for (i in 1:length(preds)){
    if (is.factor(data_SS[, preds[i]])){
      ans[[preds[i]]] <- as.character(data_SS[,preds[i]])
    } else {
      ans[[preds[i]]] <- data_SS[,preds[i]]
    }
  }
  ans$searches_unit <- t(as.matrix(data_SS[, unitNames]))
  ans$unit <- unitNames
  class(ans) <- "SS"
  return(ans)
}
#' Transpose a list of arrays
#'
#' A list of \code{n} arrays, each with dimension \code{m} x \code{k} is
#' redimensioned to a list of \code{m} arrays, each with dimension \code{m} x
#' \code{k}. NOTE: Attributes are not preserved.
#'
#' @param M a list of \code{n} \code{m} x \code{k} arrays
#' @return a list of \code{m} \code{n} x \code{k} arrays
#'
#' @export
#'
ltranspose <- function(M){
  if (!is.list(M))
    stop(substitute(M), " must be a list")
  ans <- list()
  if (!is.matrix(M[[1]]))
    stop("elements of ", substitute(M), " must be arrays.")
  adim <- dim(M[[1]])
  for (i in 1:length(M)){
    if (!isTRUE(all.equal(dim(M[[i]]), adim))){
      stop(
        "elements of ", substitute(M),
        " must be arrays with the same dimensions"
      )
    }
    ans[[i]] <- do.call("rbind", lapply(M, function(x) x[i, ]))
  }
  return(ans)
}

#' Transpose a \code{splitFull} array (preserving attributes)
#'
#' @param splits a \code{splitFull} object, which is a list of \code{n}
#'  \code{m} x \code{k} arrays with attributes describing characteristics of the
#'  splits
#' @return a list of \code{m} \code{n} x \code{k} arrays as a \code{splitFull}
#'  object
#' @export

transposeSplits <- function(splits){
  if (!(class(splits) %in% "splitFull")){
    stop(
      substitute(splits), " is not a splitFull object. ",
      "Only splitFull objects can be transposed using transposeSplits()"
    )
  }
  ans <- ltranspose(splits)
  names(ans) <- rownames(splits[[1]])
  class(ans) <- "splitFull"
  attr(ans, "vars") <- attr(splits, "vars")[2:1]
  attr(ans, "type") <- attr(splits, "type")[2:1]
  attr(ans, "times") <- attr(splits, "times")
  return(ans)
}

