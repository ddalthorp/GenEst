% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{pllogis}
\alias{pllogis}
\title{The CDF of the loglogistic distribution}
\usage{
pllogis(q, pda, pdb)
}
\arguments{
\item{q}{a numeric vector of quantiles}

\item{pda}{the \eqn{\alpha} parameter}

\item{pdb}{the \eqn{\beta} parameter}
}
\value{
\code{Pr(X <= q | pda, pdb)}
}
\description{
The CDF of the loglogistic distribution
}
\details{
There are several common parameterizations of the loglogistic
 distribution. The one used here gives the following:
 \describe{
   \item{CDF}{\code{Pr(X <= x) = 1/(1 + (x/}\eqn{\beta})^-\eqn{\alpha}\code{)}}
   \item{PDF}{\code{Pr(X = x) = (}\eqn{\alpha}/\eqn{\beta}\code{) * (x/}\eqn{\beta})^(\eqn{\alpha}\code{ - 1)/(1 + (x/}\eqn{\beta})^\eqn{\alpha}\code{)^2}}
  }
}
