#' @useDynLib GenEst, .registration = TRUE
#' @importFrom cbinom rcbinom
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
#' @importFrom zip zip
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
#' \code{\link{pkmSetAICcTab}}\cr
#' \code{\link[=plot.pkmSet]{plot}} S3 function for \code{pkm.Set} object\cr
#' \code{\link{pkmSetSize}}\cr
#' \code{\link{rpk}}\cr
#' \code{\link{SEsi}}\cr
#'
#' \code{\link{cpm}}\cr
#' \code{\link[=plot.cpm]{plot}} S3 function for \code{cpm} object\cr
#' \code{\link{cpmSet}}\cr
#' \code{\link{cpmSetAICcTab}}\cr
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

#' @section GUI-specific commands:
#' \code{\link{runGenEst}}\cr
#'
#' \code{\link{aboutMainPanel}}\cr
#' \code{\link{aboutPanel}}\cr
#' \code{\link{analysisPanel}}\cr
#' \code{\link{b}}\cr
#' \code{\link{cButtonStyle}}\cr
#' \code{\link{clearNotifications}}\cr
#' \code{\link{cButtonStyle}}\cr
#' \code{\link{CPdistOptions}}\cr
#' \code{\link{CPEstimatesPanel}}\cr
#' \code{\link{CPFiguresPanel}}\cr
#' \code{\link{CPMainPanel}}\cr
#' \code{\link{cpmCPCellPlot}}\cr
#' \code{\link{cpmFail}}\cr
#' \code{\link{CPModComparisonPanel}}\cr
#' \code{\link{CPModSelectionPanel}}\cr
#' \code{\link{CPPanel}}\cr
#' \code{\link{CPSelectedDataPanel}}\cr
#' \code{\link{CPSidebar}}\cr
#' \code{\link{createReactiveValues}}\cr
#' \code{\link{createvtext}}\cr
#' \code{\link{dataInputPanel}}\cr
#' \code{\link{dataInputSidebar}}\cr
#' \code{\link{dataTabPanel}}\cr
#' \code{\link{dateCols}}\cr
#' \code{\link{dateToDay}}\cr
#' \code{\link{disclaimerDeploy}}\cr
#' \code{\link{disclaimerUSGS}}\cr
#' \code{\link{disclaimerWEST}}\cr
#' \code{\link{disclaimers}}\cr
#' \code{\link{disclaimersMainPanel}}\cr
#' \code{\link{disclaimersPanel}}\cr
#' \code{\link{dlModTabCP}}\cr
#' \code{\link{dlModTabSE}}\cr
#' \code{\link{downloadCPFig}}\cr
#' \code{\link{downloadData}}\cr
#' \code{\link{downloadgFig}}\cr
#' \code{\link{downloadMFig}}\cr
#' \code{\link{downloadSEFig}}\cr
#' \code{\link{downloadTable}}\cr
#' \code{\link{downloadsMainPanel}}\cr
#' \code{\link{downloadsPanel}}\cr
#' \code{\link{downloadsRow}}\cr
#' \code{\link{DWPCols}}\cr
#' \code{\link{expandModelSetCP}}\cr
#' \code{\link{ftpLink}}\cr
#' \code{\link{GenEstAcknowledgements}}\cr
#' \code{\link{GenEstAuthors}}\cr
#' \code{\link{GenEstGUIauthors}}\cr
#' \code{\link{GenEstInlineCSS}}\cr
#' \code{\link{GenEstLicense}}\cr
#' \code{\link{GenEstLogos}}\cr
#' \code{\link{GenEstShinyJS}}\cr
#' \code{\link{GeneralInputSidebar}}\cr
#' \code{\link{GeneralInputsPanel}}\cr
#' \code{\link{gFigurePanel}}\cr
#' \code{\link{gMainPanel}}\cr
#' \code{\link{gPanel}}\cr
#' \code{\link{gSchedulePanel}}\cr
#' \code{\link{gSidebar}}\cr
#' \code{\link{gSummaryPanel}}\cr
#' \code{\link{li}}\cr
#' \code{\link{loadedDataPanel}}\cr
#' \code{\link{makekFixedInput}}\cr
#' \code{\link{makekFillInput}}\cr
#' \code{\link{makeEstText}}\cr
#' \code{\link{makeMenu}}\cr
#' \code{\link{matchCells}}\cr
#' \code{\link{MFigurePanel}}\cr
#' \code{\link{MMainPanel}}\cr
#' \code{\link{modelMenuHeader}}\cr
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
#' \code{\link{MSummaryPanel}}\cr
#' \code{\link{navbar}}\cr
#' \code{\link{obsCols_SE}}\cr
#' \code{\link{obsCols_fta}}\cr
#' \code{\link{obsCols_ltp}}\cr
#' \code{\link{ol}}\cr
#' \code{\link{pickSizeclass}}\cr
#' \code{\link{plotNA}}\cr
#' \code{\link{predsCols}}\cr
#' \code{\link{prepPredictors}}\cr
#' \code{\link{prepSizeclassText}}\cr
#' \code{\link{prettyModTabCP}}\cr
#' \code{\link{prettyModTabSE}}\cr
#' \code{\link{prettySplitSpecTab}}\cr
#' \code{\link{prettySplitTab}}\cr
#' \code{\link{readCSVs}}\cr
#' \code{\link{removeCols}}\cr
#' \code{\link{SEEstimatesPanel}}\cr
#' \code{\link{SEFiguresPanel}}\cr
#' \code{\link{selectData}}\cr
#' \code{\link{SEMainPanel}}\cr
#' \code{\link{SEModComparisonPanel}}\cr
#' \code{\link{SEModSelectionPanel}}\cr
#' \code{\link{SEPanel}}\cr
#' \code{\link{SESelectedDataPanel}}\cr
#' \code{\link{SESidebar}}\cr
#' \code{\link{setFigH}}\cr
#' \code{\link{setFigW}}\cr
#' \code{\link{setkFillNeed}}\cr
#' \code{\link{setkFix}}\cr
#' \code{\link{small}}\cr
#' \code{\link{style}}\cr
#' \code{\link{ul}}\cr
#' \code{\link{u}}\cr
#' \code{\link{update_input_clear_all}}\cr
#' \code{\link{update_input_cols_CP_preds}}\cr
#' \code{\link{update_input_cols_fta}}\cr
#' \code{\link{update_input_cols_ltp}}\cr
#' \code{\link{update_input_cols_SE_preds}}\cr
#' \code{\link{update_input_cols_SE_obs}}\cr
#' \code{\link{update_input_data_CO}}\cr
#' \code{\link{update_input_data_CO_clear}}\cr
#' \code{\link{update_input_data_CP}}\cr
#' \code{\link{update_input_data_CP_clear}}\cr
#' \code{\link{update_input_data_DWP}}\cr
#' \code{\link{update_input_data_DWP_clear}}\cr
#' \code{\link{update_input_data_SE}}\cr
#' \code{\link{update_input_data_SE_clear}}\cr
#' \code{\link{update_input_data_SS}}\cr
#' \code{\link{update_input_data_SS_clear}}\cr
#' \code{\link{update_input_outsc_CP}}\cr
#' \code{\link{update_input_outsc_SE}}\cr
#' \code{\link{update_input_run_CP}}\cr
#' \code{\link{update_input_run_CP_clear}}\cr
#' \code{\link{update_input_run_g}}\cr
#' \code{\link{update_input_run_g_clear}}\cr
#' \code{\link{update_input_run_M}}\cr
#' \code{\link{update_input_run_M_clear}}\cr
#' \code{\link{update_input_split_M_clear}}\cr
#' \code{\link{update_input_run_SE}}\cr
#' \code{\link{update_input_run_SE_clear}}\cr
#' \code{\link{update_input_sizeclassCol}}\cr
#' \code{\link{update_input_useSSdata}}\cr
#' \code{\link{update_output_clear_all}}\cr
#' \code{\link{update_output_cols_CP}}\cr
#' \code{\link{update_output_cols_SE}}\cr
#' \code{\link{update_output_data_SE_clear}}\cr
#' \code{\link{update_output_data_CO}}\cr
#' \code{\link{update_output_data_CO_clear}}\cr
#' \code{\link{update_output_data_CP}}\cr
#' \code{\link{update_output_data_CP_clear}}\cr
#' \code{\link{update_output_data_DWP}}\cr
#' \code{\link{update_output_data_DWP_clear}}\cr
#' \code{\link{update_output_data_SE}}\cr
#' \code{\link{update_output_data_SS}}\cr
#' \code{\link{update_output_data_SS_clear}}\cr
#' \code{\link{update_output_outdls_CP}}\cr
#' \code{\link{update_output_outpk_SE}}\cr
#' \code{\link{update_output_outsc_CP}}\cr
#' \code{\link{update_output_outsc_g}}\cr
#' \code{\link{update_output_outsc_SE}}\cr
#' \code{\link{update_output_run_CP}}\cr
#' \code{\link{update_output_run_CP_clear}}\cr
#' \code{\link{update_output_run_g}}\cr
#' \code{\link{update_output_run_g_clear}}\cr
#' \code{\link{update_output_run_M}}\cr
#' \code{\link{update_output_run_M_clear}}\cr
#' \code{\link{update_output_run_SE}}\cr
#' \code{\link{update_output_run_SE_clear}}\cr
#' \code{\link{update_output_sizeclassCol}}\cr
#' \code{\link{update_output_split_M}}\cr
#' \code{\link{update_output_split_M_clear}}\cr
#' \code{\link{update_output_SS}}\cr
#' \code{\link{update_output_transpose_split}}\cr
#' \code{\link{update_rv_clear_all}}\cr
#' \code{\link{update_rv_cols_CP_preds}}\cr
#' \code{\link{update_rv_cols_fta}}\cr
#' \code{\link{update_rv_cols_ltp}}\cr
#' \code{\link{update_rv_cols_SE_obs}}\cr
#' \code{\link{update_rv_cols_SE_preds}}\cr
#' \code{\link{update_rv_data_CO}}\cr
#' \code{\link{update_rv_data_CO_clear}}\cr
#' \code{\link{update_rv_data_CP}}\cr
#' \code{\link{update_rv_data_CP_clear}}\cr
#' \code{\link{update_rv_data_DWP}}\cr
#' \code{\link{update_rv_data_DWP_clear}}\cr
#' \code{\link{update_rv_data_SE}}\cr
#' \code{\link{update_rv_data_SE_clear}}\cr
#' \code{\link{update_rv_data_SS}}\cr
#' \code{\link{update_rv_data_SS_clear}}\cr
#' \code{\link{update_rv_outdls_CP}}\cr
#' \code{\link{update_rv_outpk_SE}}\cr
#' \code{\link{update_rv_outsc_CP}}\cr
#' \code{\link{update_rv_outsc_g}}\cr
#' \code{\link{update_rv_outsc_SE}}\cr
#' \code{\link{update_rv_run_CP}}\cr
#' \code{\link{update_rv_run_CP_clear}}\cr
#' \code{\link{update_rv_run_g}}\cr
#' \code{\link{update_rv_run_g_clear}}\cr
#' \code{\link{update_rv_run_M}}\cr
#' \code{\link{update_rv_run_M_clear}}\cr
#' \code{\link{update_rv_run_SE}}\cr
#' \code{\link{update_rv_run_SE_clear}}\cr
#' \code{\link{update_rv_split_M}}\cr
#' \code{\link{update_rv_split_M_clear}}\cr
#' \code{\link{update_rv_transpose_split}}\cr
#' \code{\link{update_rv_useSSdata}}\cr
#' \code{\link{update_rv_useSSinputs}}\cr
#' \code{\link{update_rv_sizeclassCol}}\cr
#' \code{\link{updateColNames_size}}\cr
#' \code{\link{updateSizeclassCol}}\cr
#' \code{\link{updateSizeclasses}}\cr
#' @docType package
#'
#' @keywords package
#'
NULL