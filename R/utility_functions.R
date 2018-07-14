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

#' @title Expected value of a continuous binomial
#'
#' @description Calculates the expected value of a continuous binomial. Uses 
#'   internal-only data
#'
#' @param prob probability
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

