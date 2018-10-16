#' @title Compute the logit or anti-logit
#' 
#' @param x A number. For  \code{logit}, a probability (between 0 and 1, 
#'   inclusive). For  \code{alogit}, any real number.
#'
#' @return \code{logit}: The logit of \code{x}.
#'
#' @examples
#'   logit(0.5)
#'
#' @export 
#'
logit <- function(x) {
  log(x / (1 - x))
}

#' @rdname logit
#' 
#' @return \code{alogit}:  The anti-logit of \code{x}.
#'
#' @examples
#'   alogit(0)
#'
#' @export 
#'
alogit <- function(x) {
  1 / (1 + exp(-x))
}

#' @title Get the length of real (non-NA) things
#'
#' @description Length of non-missing (non-\code{NA}) values in a vector.
#'
#' @param x Vector of values, some of which many be \code{NA}.
#'
#' @return Integer count of how many non-\code{NA} values in \code{x}.
#'
#' @examples
#'   x <- c(1, 2, NA, 3)
#'   length(x)
#'   trueLength(x)
#'
#' @export
#'
trueLength <- function(x){
 length(which(!is.na(x)))
}

#' @title Is a vector never decreasing?
#'
#' @description Check if a vector is never decreasing.
#'
#' @param x Vector of numeric values.
#'
#' @param tiesOK Logical indicator if ties are ok or not.
#'
#' @param na.rm Logical indicator if NAs are to be removed or not.
#'
#' @return Logical value.
#'
#' @export
#'
isNeverDecreasing <- function(x, tiesOK = TRUE, na.rm = TRUE){
  if (na.rm == TRUE){
    x <- na.omit(x)
  }
  diffs <- diff(x)
  if (tiesOK == TRUE){
    out <- all(diffs >= 0)
  }
  if (tiesOK == FALSE){
    out <- all(diffs > 0)
  }
  return(out)
}

#' @title Calculate day of study from calendar date
#'
#' @description Convert calendar date to integer day from a reference date
#'   (\code{ref}).
#'
#' @param date A date or vector of dates to convert to days.
#'
#' @param ref Reference date.
#'
#' @return Numeric value(s) of days from \code{ref}.
#'
#' @examples 
#'   x <- c("2018-01-01", "2018-02-01")
#'   dateToDay(x, x[1])
#'
#' @export 
#'
dateToDay <- function(date, ref = NULL){
  if (is.numeric(date)){
    return(date)
  }
  if (is.null(ref)){
    ref <- date
  }
  day <- as.numeric(difftime(date, ref, units = "days"))
  return(day)
}

#' Checks whether a vector of data can be interpreted as dates
#'
#' @description Checks whether the dates are in a standard format and 
#'  sensible. If so, function returns the dates converted to ISO 8601 
#'  yyyy-mm-dd format. Acceptable formats are yyyy-mm-dd, yyyy/mm/dd, 
#'  mm/dd/yyyy, and dd/mm/yyyy. If format is mm/dd/yyyy or dd/mm/yyyy, the 
#'  dates must be interpretable unambiguously. Also, dates must be later than 
#'  1900-01-01. This additional check provides some protection against common 
#'  data entry errors like entering a year as 0217 or 1017 instead of 2017.
#'
#' @param testdate Date(s) to check and format.
#'
#' @return dates formatted as yyyy-mm-dd (if possible) or NULL (if some value
#'  is not interpretable as a date after 1900-01-01).
#'
#' @examples
#'  checkDate("02/20/2018")
#'  checkDate("10/08/2018")
#'
#' @export
#'
checkDate <- function(testdate){
  beginningOfTime <- as.Date("1900-01-01")
  canDate <- try(as.Date(testdate), silent = TRUE)
  if (!("try-error" %in% class(canDate)) &&
      !anyNA(canDate) &&
      all(canDate > beginningOfTime)) return (canDate)

  formats <- list("%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")
  canDate <- lapply(formats, 
               function(x) try(as.Date(testdate, tryFormats = x), 
                               silent = TRUE))
  canForm <- which(lapply(canDate, class) != "try-error")
  if (length(canForm) == 0) return (NULL)
  for (i in 1:length(canForm)){
    if (anyNA(canDate[[canForm[i]]])){
      canForm[i] <- NA
    } else if (any(canDate[[canForm[i]]] < beginningOfTime)) {
      canForm[i] <- NA
    }
  }
  if (all(is.na(canForm))) return(NULL)
  canForm <- canForm[!is.na(canForm)]
  if (length(canForm) > 1) return(NULL)
  return(canDate[[canForm]])
}


#' @title Expected value of a continuous binomial with size = 1/g
#'
#' @description Calculates the expected value of a continuous binomial random
#'  variable with size = 1/g. Uses internal-only data.
#'
#' @param prob Vector of probabilities.
#'
#' @return Mean value of the probabilities.
#'
#' @export 
#'
Ecbinom <- function(prob){
  X <- EcbinomXY$X
  Y <- EcbinomXY$Y
  interp <- approxfun(x = X, y = Y)
  interp(prob)
}

#' @title Generic S3 function for summarizing AICc
#'
#' @description Extract AICc values from \code{pkm}, \code{pkmSet},
#'  \code{pkmSetSize}, \code{cpm}, \code{cpmSet}, and \code{cpmSetSize}.
#'
#' @param x Model or list of models to extract AICc values from.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @return list of models sorted by AICc
#'
#' @export
aicc <- function(x, ... ){
  UseMethod("aicc", x)
}


#' @title Generic S3 function for printing corpus_frame
#'
#' @param x data frame to print
#'
#' @param ... other arguments
#'
#' @return prints data frame
#'
#' @export
print.corpus_frame <- function(x, ...){
  corpus::print.corpus_frame(x, rows = 80)
}
