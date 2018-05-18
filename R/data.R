#' Completely mock example data set
#'
#' A dataset containing SE, CP, SS, DWP, and CO data.
#'
#' @format A list with 5 items:
#' \describe{
#'   \item{SearcherEfficiencyData}{Searcher efficiency trial data}
#'   \item{CarcassPersistenceData}{Carcass persistence trial data}
#'   \item{SearchData}{Search schedule data}
#'   \item{DensityWeightedProportionData}{Density weighted proportion of area 
#'     searched data}
#'   \item{CarcassObservationData}{Carcass observations}  
#' }
#' @source Mock
"mockData"

#' Synthetic data set based on 120 m radius road and pad searches of all 100
#' turbines at a theoretical site.
#'
#' Complete data set for fatality estimation at a wind power facility. Included
#' are SE, CP, SS, DWP, and CO data.
#'
#' @format A list with 5 elements:
#' \describe{
#'   \item{SE}{Searcher efficiency trial data}
#'   \item{CP}{Carcass persistence trial data}
#'   \item{SS}{Search schedule parameters}
#'   \item{DWP}{Density weighted proportion of area searched}
#'   \item{CO}{Carcass observations}
#' }
#' @source RP
"wind_RP"

#' Synthetic data set based on searches of 60 m cleared plots of 23 out of 100
#' turbines at a theoretical site.
#'
#' Complete data set for fatality estimation at a wind power facility. Included
#' are SE, CP, SS, DWP, and CO data.
#'
#' @format A list with 5 elements:
#' \describe{
#'   \item{SE}{Searcher efficiency trial data}
#'   \item{CP}{Carcass persistence trial data}
#'   \item{SS}{Search schedule parameters}
#'   \item{DWP}{Density weighted proportion of area searched}
#'   \item{CO}{Carcass observations}
#' }
#' @source cleared
"wind_cleared"
