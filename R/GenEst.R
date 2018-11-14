#' @useDynLib GenEst, .registration = TRUE
#' @importFrom corpus print.corpus_frame
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom graphics axis box hist lines mtext par plot plot.new points
#'   polygon rect text
#' @importFrom grDevices dev.off devAskNewPage png rgb
#' @importFrom htmltools a br code div em h3 h4 HTML img p tags
#' @importFrom lubridate is.Date
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
#' @importFrom survival strata
#' @importFrom utils combn packageDescription read.csv read.csv2 write.csv
#'   zip
#'

#' @title Generalized estimation of mortality
#'
#' @description This package is designed to analyze searcher efficiency,
#'   carcass persistence, search schedule, and carcass observation data for
#'   the estimation of bird and bat mortality at wind and solar power 
#'   facilities.
#'
#' @name GenEst
#'
#' @section Information:
#' \code{browseVignettes("GenEst")}\cr
#' \code{packageDescription("GenEst")}\cr
#' \code{disclaimers()}\cr
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
#' @section Main command-line functions:
#'  \describe{
#'   \item{\code{\link{pkm}, \link{cpm}}}{estimate searcher efficiency
#'    (\code{pk}) and carcass persistence (\code{cp}) parameters}
#'   \item{\code{\link{estM}}}{estimate mortality given \code{pkm}, \code{cpm}
#'    and data}
#'   \item{\code{\link{calcSplits}}}{split mortality estimates by 
#'      subcategories}
#'   \item{\code{plot}}{S3 function for \code{\link[=plot.pkm]{pkm}},
#'    \code{\link[=plot.pkmSet]{pkmSet}}, \code{\link[=plot.cpm]{cpm}},
#'    \code{\link[=plot.cpmSet]{cpmSet}}, \code{\link[=plot.estM]{estM}},
#'    \code{\link[=plot.splitFull]{splitFull}},
#'    \code{\link[=plot.splitSummary]{splitSummary}},
#'    \code{\link[=plot.gGeneric]{gGeneric}}, and
#'    \code{\link[=plot.gGenericSize]{gGenericSize}} objects}
#'   \item{\code{\link{transposeSplits}}}{transpose 2-d splits}
#'   \item{\code{summary}}{S3 function for \code{\link[=summary.estM]{estM}},
#'    \code{\link[=summary.splitFull]{splitFull}},
#'    \code{\link[=summary.gGeneric]{gGeneric}},
#'    \code{\link[=summary.gGenericSize]{gGenericSize}} objects}
#'   \item{\code{\link{aicc}}}{S3 function for extracting models' AICc values
#'    from \code{\link{pkm}}, \code{\link[=pkm]{pkmSet}}, 
#'    \code{\link[=pkm]{pkmSize}}, \code{\link[=pkm]{pkmSetSize}}, 
#'    \code{\link{cpm}}, \code{\link[=cpm]{cpmSet}},
#'    \code{\link[=cpm]{cpmSize}}, and \code{\link[=cpm]{cpmSetSize}} objects}
#'   \item{\code{\link{estgGeneric}}, \code{\link{estgGenericSize}}}{estimate
#'    detection probability (g) for given searcher efficiency and carcass
#'    persistence model}
#'   \item{\code{runGenEst()}}{start the GUI}
#' }
#' @section Potentially useful calculation functions:
#' \code{\link{rpk}}, \code{\link{rcp}}\cr
#' \code{\link{estg}}, \code{\link{calcg}}\cr
#' \code{\link{ppersist}}, \code{\link{SEsi}}\cr
#' \code{\link{alogit}}, \code{\link{logit}}\cr
#' \code{\link{Ecbinom}}\cr
#' \code{\link{pkLogLik}}, \code{\link{cpLogLik}}\cr
#' \code{\link{calcRate}}, \code{\link{calcTsplit}}, 
#' \code{\link{ltranspose}}\cr
#'
#' @section Potentially useful editing functions:
#' \code{\link{prepSS}}\cr
#' \code{\link{averageSS}}\cr
#' \code{\link{trimSetSize}}\cr
#' \code{\link{combinePreds}}\cr
#' \code{\link{combinePredsAcrossModels}}\cr
#' \code{\link{DWPbyCarcass}}\cr
#' \code{\link{pkmSetSizeFailRemove}}\cr
#' \code{\link{pkmSetFailRemove}}\cr
#' \code{\link{cpmSetSizeFailRemove}}\cr
#' \code{\link{cpmSetFailRemove}}\cr
#' \code{\link{tidyModelSetCP}}\cr
#' \code{\link{tidyModelSetSE}}\cr
#'
#' @section Other functions (primarily associated with the GUI):
#' \code{\link{aboutContent}}\cr
#' \code{\link{aboutPanel}}\cr
#' \code{\link{analysisPanel}}\cr
#' \code{\link{b}}\cr
#' \code{\link{big}}\cr
#' \code{\link{cButtonStyle}}\cr
#' \code{\link{center}}\cr
#' \code{\link{checkComponents}}\cr
#' \code{\link{checkDate}}\cr
#' \code{\link{checkSpecificModelCP}}\cr
#' \code{\link{checkSpecificModelSE}}\cr
#' \code{\link{clearNotifications}}\cr
#' \code{\link{combinePredsAcrossModels}}\cr
#' \code{\link{countCarcs}}\cr
#' \code{\link{CPcols}}\cr
#' \code{\link{CPdistOptions}}\cr
#' \code{\link{CPMainPanel}}\cr
#' \code{\link{cpmCPCellPlot}}\cr
#' \code{\link{cpmFail}}\cr
#' \code{\link{cpmSetFail}}\cr
#' \code{\link{cpmSetSpecCPCellPlot}}\cr
#' \code{\link{CPPanel}}\cr
#' \code{\link{CPSidebar}}\cr
#' \code{\link{initialReactiveValues}}\cr
#' \code{\link{createvtext}}\cr
#' \code{\link{dataDownloadWidget}}\cr
#' \code{\link{dataInputPanel}}\cr
#' \code{\link{dataInputSidebar}}\cr
#' \code{\link{dataInputWidget}}\cr
#' \code{\link{dataTabPanel}}\cr
#' \code{\link{dateCols}}\cr
#' \code{\link{dateToDay}}\cr
#' \code{\link{disclaimersContent}}\cr
#' \code{\link{disclaimersPanel}}\cr
#' \code{\link{disclaimerUSGS}}\cr
#' \code{\link{disclaimerWEST}}\cr
#' \code{\link{dlModTabCP}}\cr
#' \code{\link{dlModTabSE}}\cr
#' \code{\link{downloadCPFig}}\cr
#' \code{\link{downloadData}}\cr
#' \code{\link{downloadgFig}}\cr
#' \code{\link{downloadMFig}}\cr
#' \code{\link{downloadSEFig}}\cr
#' \code{\link{downloadsPanel}}\cr
#' \code{\link{downloadTable}}\cr
#' \code{\link{DWPCols}}\cr
#' \code{\link{expandModelSetCP}}\cr
#' \code{\link{GeneralInputSidebar}}\cr
#' \code{\link{GeneralInputsPanel}}\cr
#' \code{\link{GenEstAcknowledgements}}\cr
#' \code{\link{GenEstAuthors}}\cr
#' \code{\link{GenEstGUIauthors}}\cr
#' \code{\link{GenEstInlineCSS}}\cr
#' \code{\link{GenEstLicense}}\cr
#' \code{\link{GenEstLogos}}\cr
#' \code{\link{GenEstShinyJS}}\cr
#' \code{\link{GenEstUI}}\cr
#' \code{\link{gettingStartedContent}}\cr
#' \code{\link{gettingStartedPanel}}\cr
#' \code{\link{gMainPanel}}\cr
#' \code{\link{gPanel}}\cr
#' \code{\link{gSidebar}}\cr
#' \code{\link{helpPanel}}\cr
#' \code{\link{initialOutput}}\cr
#' \code{\link{isNeverDecreasing}}\cr
#' \code{\link{kFixedWidget}}\cr
#' \code{\link{kFixedWidgetHeader}}\cr
#' \code{\link{kFixedWidgetRow}}\cr
#' \code{\link{li}}\cr
#' \code{\link{loadedDataPanel}}\cr
#' \code{\link{matchCells}}\cr
#' \code{\link{MMainPanel}}\cr
#' \code{\link{modelInputWidget}}\cr
#' \code{\link{modelOutputPanel}}\cr
#' \code{\link{modelOutputWidget}}\cr
#' \code{\link{modelRunWidget}}\cr
#' \code{\link{modelSelectionWidget}}\cr
#' \code{\link{modelSelectionWidgetHeader}}\cr
#' \code{\link{modelSelectionWidgetRow}}\cr
#' \code{\link{modelSetCells}}\cr
#' \code{\link{modelSetModelCells}}\cr
#' \code{\link{modelSetModelPredictors}}\cr
#' \code{\link{modelSetPredictors}}\cr
#' \code{\link{modNamePaste}}\cr
#' \code{\link{modNameSplit}}\cr
#' \code{\link{MPanel}}\cr
#' \code{\link{msgFracNote}}\cr
#' \code{\link{msgList}}\cr
#' \code{\link{msgModDone}}\cr
#' \code{\link{msgModFail}}\cr
#' \code{\link{msgModPartialFail}}\cr
#' \code{\link{msgModRun}}\cr
#' \code{\link{msgModSENobs}}\cr
#' \code{\link{msgModWarning}}\cr
#' \code{\link{msgSampleSize}}\cr
#' \code{\link{msgSplitFail}}\cr
#' \code{\link{msgSSavgFail}}\cr
#' \code{\link{msgSSinputFail}}\cr
#' \code{\link{MSidebar}}\cr
#' \code{\link{navbar}}\cr
#' \code{\link{obsCols_fta}}\cr
#' \code{\link{obsCols_ltp}}\cr
#' \code{\link{obsCols_SE}}\cr
#' \code{\link{ol}}\cr
#' \code{\link{pickSizeclass}}\cr
#' \code{\link{pkmFail}}\cr
#' \code{\link{pkmParamPlot}}\cr
#' \code{\link{pkmSECellPlot}}\cr
#' \code{\link{pkmSet}}\cr
#' \code{\link{pkmSetAllFail}}\cr
#' \code{\link{pkmSetFail}}\cr
#' \code{\link{pkmSetSizeFail}}\cr
#' \code{\link{pkmSetSpecParamPlot}}\cr
#' \code{\link{pkmSetSpecSECellPlot}}\cr
#' \code{\link{plotCPCells}}\cr
#' \code{\link{plotCPFigure}}\cr
#' \code{\link{plotCPHeader}}\cr
#' \code{\link{plotNA}}\cr
#' \code{\link{plotSEBoxPlots}}\cr
#' \code{\link{plotSEBoxTemplate}}\cr
#' \code{\link{plotSECells}}\cr
#' \code{\link{plotSEFigure}}\cr
#' \code{\link{plotSEHeader}}\cr
#' \code{\link{predsCols}}\cr
#' \code{\link{prepPredictors}}\cr
#' \code{\link{prepSizeclassText}}\cr
#' \code{\link{preTextMaker}}\cr
#' \code{\link{prettyModTabCP}}\cr
#' \code{\link{prettyModTabSE}}\cr
#' \code{\link{prettySplitTab}}\cr
#' \code{\link{readCSV}}\cr
#' \code{\link{refMod}}\cr
#' \code{\link{removeCols}}\cr
#' \code{\link{SEcols}}\cr
#' \code{\link{selectData}}\cr
#' \code{\link{selectedDataPanel}}\cr
#' \code{\link{SEMainPanel}}\cr
#' \code{\link{SEPanel}}\cr
#' \code{\link{SEsi_left}}\cr
#' \code{\link{SEsi_right}}\cr
#' \code{\link{SEsi0}}\cr
#' \code{\link{SESidebar}}\cr
#' \code{\link{setFigH}}\cr
#' \code{\link{setFigW}}\cr
#' \code{\link{setkNeed}}\cr
#' \code{\link{setkFix}}\cr
#' \code{\link{simpleMplot}}\cr
#' \code{\link{sizeCols}}\cr
#' \code{\link{small}}\cr
#' \code{\link{splitButtonWidget}}\cr
#' \code{\link{style}}\cr
#' \code{\link{trimSetSize}}\cr
#' \code{\link{trueLength}}\cr
#' \code{\link{u}}\cr
#' \code{\link{ul}}\cr
#' \code{\link{update_input}}\cr
#' \code{\link{update_output}}\cr
#' \code{\link{update_rv}}\cr
#' \code{\link{updateColNames_size}}\cr
#' \code{\link{updateSizeclasses}}\cr
#' \code{\link{updatesizeCol}}\cr
#' \code{\link{widgetMaker}}\cr

#' @docType package
#'
#' @keywords package
#'
NULL