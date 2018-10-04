#' @useDynLib GenEst, .registration = TRUE
#' @importFrom cbinom rcbinom
#' @importFrom corpus print.corpus_frame
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom graphics axis box hist lines mtext par plot plot.new points
#'   polygon rect text
#' @importFrom grDevices dev.off devAskNewPage png rgb
#' @importFrom gsl hyperg_2F1
#' @importFrom gtools mixedsort
#' @importFrom htmltools a br code div em h3 h4 HTML img p tags
#' @importFrom lubridate is.Date
#' @importFrom matrixStats colCounts colMaxs rowCumprods rowMaxs rowProds
#'   rowCumsums rowDiffs rowQuantiles rowCounts
#' @importFrom mvtnorm rmvnorm
#' @importFrom Rcpp sourceCpp
#' @importFrom shiny actionButton checkboxInput checkboxGroupInput column 
#'   conditionalPanel downloadButton downloadHandler fileInput fluidRow 
#'   h3 h4 h5 htmlOutput isolate mainPanel navbarPage numericInput 
#'   observeEvent outputOptions p plotOutput radioButtons reactiveValues 
#'   removeNotification renderPlot renderText renderUI runApp 
#'   selectizeInput shinyApp shinyAppDir showNotification sidebarLayout
#'   sidebarPanel tabPanel tabsetPanel textOutput updateNumericInput
#'   updateSelectizeInput updateTabsetPanel 
#' @importFrom shinyjs inlineCSS reset useShinyjs 
#' @importFrom stats .getXlevels approxfun as.formula delete.response density
#'   formula median model.matrix na.omit optim pgamma pnorm qnorm quantile 
#'   reformulate rnorm runif terms update.formula weighted.mean  
#' @importFrom sticky sticky
#' @importFrom survival dsurvreg psurvreg strata Surv survfit survreg
#' @importFrom utils combn packageDescription read.csv read.csv2 write.csv
#'   zip
#'

#' @title Generalized estimation of mortality
#'
#' @description This package is designed to analyze searcher efficiency,
#'   carcass persistence, search schedule, and carcass observation data for
#'   the estimation of mortality.
#'
#' @name GenEst
#'
#' @section Data sets:
#' \code{\link{mock}}\cr
#' \code{\link{wind_cleared}}\cr
#' \code{\link{wind_RP}}\cr
#' \code{\link{wind_RPbat}}\cr
#' \code{\link{solar_powerTower}}\cr
#' \code{\link{solar_PV}}\cr
#' \code{\link{solar_trough}}\cr
#'
#' @section Useful command-line functions:
#' \code{\link{pkm}}\cr
#' \code{\link[=plot.pkm]{plot}} S3 function for \code{pkm} object\cr
#' \code{\link{pkmSet}}\cr
#' \code{\link{AIC}}\cr
#' \code{\link[=plot.pkmSet]{plot}} S3 function for \code{pkm.Set} object\cr
#' \code{\link{pkmSetSize}}\cr
#' \code{\link{rpk}}\cr
#' \code{\link{SEsi}}\cr
#'
#' \code{\link{cpm}}\cr
#' \code{\link[=plot.cpm]{plot}} S3 function for \code{cpm} object\cr
#' \code{\link{cpmSet}}\cr
#' \code{\link{AIC}}\cr
#' \code{\link[=plot.cpmSet]{plot}} S3 function for \code{pkmSet} object\cr
#' \code{\link{cpmSetSize}}\cr
#' \code{\link{rcp}}\cr
#' \code{\link{ppersist}}\cr
#'
#' \code{\link{prepSS}}\cr
#' \code{\link{estM}}\cr
#' \code{\link[=summary.estM]{summary}} S3 function for \code{estM} object\cr
#' \code{\link[=plot.estM]{plot}} S3 function\cr
#' \code{\link{calcSplits}} returns \code{splitFull} object\cr
#' \code{\link[=summary.splitFull]{summary}}
#'   S3 function for \code{pkm} object\cr
#' \code{\link[=plot.splitFull]{plot}}
#'   S3 function for \code{splitFull} object\cr
#' \code{\link{transposeSplits}}\cr
#'
#' \code{\link{estg}}\cr
#' \code{\link{averageSS}}\cr
#' \code{\link{estgGeneric}}\cr
#' \code{\link[=summary.gGeneric]{summary}}
#'   S3 function for \code{gGeneric} object\cr
#' \code{\link[=plot.gGeneric]{plot}}
#'   S3 function for \code{gGeneric} object\cr
#' \code{\link{estgGenericSize}}\cr
#' \code{\link[=summary.gGenericSize]{summary}}
#'   S3 function for \code{gGenericSize} object\cr
#' \code{\link[=plot.gGenericSize]{plot}}
#'   S3 function for \code{gGenericSize} object\cr
#'
#' @section Potentially useful calculation functions:
#' \code{\link{pkLogLik}}\cr
#' \code{\link{cpLogLik}}\cr
#' \code{\link{alogit}}\cr
#' \code{\link{logit}}\cr
#' \code{\link{calcg}}\cr
#' \code{\link{Ecbinom}}\cr
#' \code{\link{Etcbinom}}\cr
#' \code{\link{calcRate}}\cr
#' \code{\link{calcTsplit}}\cr
#' \code{\link{ltranspose}}\cr
#'
#' @section Potentially useful editing functions:
#' \code{\link{combinePreds}}\cr
#' \code{\link{combinePredsAcrossModels}}\cr
#' \code{\link{DWPbyCarcass}}\cr
#' \code{\link{pkmSetSizeFailRemove}}\cr
#' \code{\link{pkmSetFailRemove}}\cr
#' \code{\link{cpmSetSizeFailRemove}}\cr
#' \code{\link{cpmSetFailRemove}}\cr
#' \code{\link{trimSetSize}}\cr
#' \code{\link{tidyModelSetCP}}\cr
#' \code{\link{tidyModelSetSE}}\cr
#' \code{\link{yyyymmdd}}\cr
#'
#' @section Other functions:
#' \code{\link{checkComponents}}\cr
#' \code{\link{checkSpecificModelCP}}\cr
#' \code{\link{checkSpecificModelSE}}\cr
#' \code{\link{countCarcs}}\cr
#' \code{\link{CPcols}}\cr
#' \code{\link{cpmSetFail}}\cr
#' \code{\link{cpmSetSizeFail}}\cr
#' \code{\link{cpmSetSpecCPCellPlot}}\cr
#' \code{\link{isNeverDecreasing}}\cr
#' \code{\link{modelSetCells}}\cr
#' \code{\link{modelSetModelCells}}\cr
#' \code{\link{modelSetModelPredictors}}\cr
#' \code{\link{modelSetPredictors}}\cr
#' \code{\link{pkmSetAllFail}}\cr
#' \code{\link{pkmSetFail}}\cr
#' \code{\link{pkmSetSizeFail}}\cr
#' \code{\link{pkmSetSpecParamPlot}}\cr
#' \code{\link{pkmSetSpecSECellPlot}}\cr
#' \code{\link{pkmFail}}\cr
#' \code{\link{pkmParamPlot}}\cr
#' \code{\link{pkmSECellPlot}}\cr
#' \code{\link{plotCPCells}}\cr
#' \code{\link{plotCPFigure}}\cr
#' \code{\link{plotCPHeader}}\cr
#' \code{\link{plotSEBoxPlots}}\cr
#' \code{\link{plotSEBoxTemplate}}\cr
#' \code{\link{plotSECells}}\cr
#' \code{\link{plotSEFigure}}\cr
#' \code{\link{plotSEHeader}}\cr
#' \code{\link{print.cpm}}\cr
#' \code{\link{print.pkm}}\cr
#' \code{\link{refMod}}\cr
#' \code{\link{rtcbinom1}}\cr
#' \code{\link{SEcols}}\cr
#' \code{\link{simpleMplot}}\cr
#' \code{\link{trueLength}}\cr
#' \code{\link{SEsi_left}}\cr
#' \code{\link{SEsi_right}}\cr
#' \code{\link{SEsi0}}\cr

#' @docType package
#'
#' @keywords package
#'
NULL