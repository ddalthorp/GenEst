#' Compute the logit
#' 
#' @param x A probability (between 0 and 1, inclusive).
#'
#' @return The logit of \code{x}.
#'
#' @examples
#'   logit(0.5)
#'
#' @export 
#'
logit <- function(x) {
  log(x / (1 - x))
}

#' Compute the anti-logit.
#' 
#' @param x A number.
#'
#' @return The anti-logit of \code{x}.
#'
#' @examples
#'   alogit(0)
#'
#' @export 
#'
alogit <- function(x) {
  1 / (1 + exp(-x))
}

#' @title Get the length of real things
#'
#' @description Length of non-missing values in a vector
#'
#' @param x vector of values
#'
#' @return integer of how many non-NA values in x
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
#' @description Check if a vector is never decreasing
#'
#' @param x vector of values
#'
#' @param tiesOK logical if ties are ok or not
#'
#' @param na.rm logical if NAs are to be removed or not
#'
#' @return logical value
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
#' @description Convert calendar date to day from reference
#'
#' @param date date to convert
#'
#' @param ref reference date
#'
#' @return converted days from reference
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

#' Converts the standard Excel date format (m/d/YYYY) to YYYY-mm-dd
#'
#' @description only transforms m/d/Y dates, returns untransformed dates that
#'   were already YYYY-mm-dd
#'
#' @param x date(s) to convert
#'
#' @return converted dates
#'
#' @examples
#'   yyyymmdd("02/20/2018") 
#'
#' @export 
#'
yyyymmdd <- function(x){
 
  slashCheck <- all(grepl("\\/", x))
  numerals <- strsplit(as.character(x), "/")[[1]]
  numeralsCheck <- nchar(numerals[1]) %in% 1:2 & nchar(numerals[2]) %in% 1:2 & 
                     nchar(numerals[3]) == 4  
  if (slashCheck & numeralsCheck ){
    x <- as.Date(as.character(x), format = "%m/%d/%Y")
  }
  return(x)
}

#' Checks whether a vector of data can be interpreted as dates
#'
#' @description Checks whether the dates are in a standard format and sensible.
#'  If so, function returns the dates converted to R standard yyyy-mm-dd format;
#'  Acceptable formats are yyyy-mm-dd, yyyy/mm/dd, mm/dd/yyyy, and dd/mm/yyyy.
#'  If format is mm/dd/yyyy or dd/mm/yyyy, the dates must be interpretable
#'  unambiguously. Also, dates must be later than 1900-01-01. This additional
#'  check provides some protection against common data entry errors like
#'  entering a year as 0217 or 1017 instead of 2017.
#'
#' @param testdate date(s) to check and format
#'
#' @return dates formatted as yyyy-mm-dd (if possible) or NULL (if some value is
#'  not interpretable as a date after 1900-01-01).
#'
#' @examples
#'   yyyymmdd("02/20/2018")
#'
#' @export
#'
checkDate <- function(testdate){
  if (is.null(testdate) || anyNA(testdate) || is.numeric(testdate))
    return(NULL)
  dateFormats <- c("%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%Y-%m-%d")
  beginningOfTime <- as.Date("1900-01-01")
  tmp <- as.Date(testdate, format = dateFormats[1])
  if (!anyNA(tmp)){
    tmp1 <- as.Date(testdate, format = dateFormats[2])
    if (anyNA(tmp1) && !any(tmp - beginningOfTime < 0)) return(tmp)
  }
  for (i in 2:length(dateFormats)){
    tmp <- as.Date(testdate, format = dateFormats[i])
    if (!anyNA(tmp) && !any(tmp - beginningOfTime < 0)) return(tmp)
  }
  return(NULL)
}


#' @title Expected value of a continuous binomial with size = 1/g
#'
#' @description Calculates the expected value of a continuous binomial random
#'  variable with size = 1/g. Uses internal-only data
#'
#' @param prob Vector of probabilities
#'
#' @return mean 
#'
#' @export 
#'
Ecbinom <- function(prob){
  X <- EcbinomXY$X
  Y <- EcbinomXY$Y
  interp <- approxfun(x = X, y = Y)
  interp(prob)
}

#' @title Expected value of a truncated continuous binomial with size = 1/g 
#'  and truncated below at g.
#'
#' @description Calculates the expected value of a truncated continuous
#'  binomial. Uses internal-only data
#'
#' @param prob Vector of probabilities
#'
#' @return mean
#'
#' @export
#'
Etcbinom <- function(prob){
  X <- EtcbinomXY$X
  Y <- EtcbinomXY$Y
  interp <- approxfun(x = X, y = Y)
  interp(prob)
}

#' @title Random draw from a continuous binomial random variable with
#'  size = 1/g and truncated at g
#'
#' @description Random draw from a continuous binomial random variable with
#'  size = 1/g and truncated at g = prob
#'
#' @param n number of random draws
#' @param prob Vector of probabilities
#'
#' @return mean
#'
#' @export
#'
rtcbinom1 <- function(n, prob){
  tmp <- rcbinom(n, 1/prob, prob)
  ind <- which(tmp < prob)
  while (length(ind) > 0) {
    tmp[ind] <- rcbinom(length(ind), 1/prob[ind], prob[ind])
    ind <- ind[which(tmp[ind] < prob[ind])]
  }
  tmp
}

#' @title extract AIC value from pkm object
#'
#' @description extract AIC value from pkm object
#'
#' @param x object of class \code{pkm}
#'
#' @return AIC and AICc of the model
#'
#' @export
#'
AIC.pkm <- function(x){
  return(list(AIC = x$AIC, AICc = x$AICc))
}