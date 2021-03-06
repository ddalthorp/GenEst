% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/carcass_persistence_functions.R
\name{aicc.cpmSet}
\alias{aicc.cpmSet}
\title{Create the AICc tables for a set of carcass persistence models}
\usage{
\method{aicc}{cpmSet}(x, ..., quiet = FALSE, app = FALSE)
}
\arguments{
\item{x}{Set of carcass persistence models fit to the same
observations}

\item{...}{further arguments passed to or from other methods}

\item{quiet}{Logical indicating if messages should be printed}

\item{app}{Logical indicating if the table should have the app model names}
}
\value{
AICc table
}
\description{
S3 function to generate model comparison tables based on AICc
 values for a set of CP models generated by \code{\link{cpmSet}}
}
\examples{
  data(wind_RP)
  mod <- cpmSet(formula_l = l ~ Season * Visibility, formula_s = s ~ Season,
           data = wind_RP$CP, left = "LastPresent", right = "FirstAbsent")
 aicc(mod)

}
