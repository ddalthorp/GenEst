#' @title Create the GenEst User Interface HTML
#'
#' @description This suite of functions create the HTML code underlying the
#'   GenEst user interface (UI). See the "GenEst Graphic User Interface"
#'   vignette for a more complete detailing of the codebase underlying
#'   the GenEst UI. \cr \cr \code{GenEstUI}: whole application. Calls 
#'   \code{dataInputPanel}, \code{analysisPanel}, and \code{helpPanel}.
#'
#' @details Currently there are few differences between the local and deployed
#'   versions of GenEst, and the \code{appType} toggle is only included as an 
#'   argument for functions that can produce different versions of the HTML.
#'   At this point, the only content that is different is the disclaimer text
#'   on the Help panel. 
#'
#' @param appType Toggle control for the app, \code{"base"} for local versions
#'   or \code{"deploy"} for hosted version. Currently only differentiates the
#'   disclaimer text. 
#'
#' @return Each function returns a string of HTML code, either as a
#'   \code{"shiny.tag.list"} object (in the case of \code{GenEstUI}) or a
#'   \code{"shiny.tag"} object (in the case of the other functions). \cr \cr 
#'   \code{GenEstUI}: Full GenEst user interface.
#'
#' @export
#'
GenEstUI <- function(appType = "base"){
  navbarPage(navbar(), collapsible = TRUE, windowTitle = createvtext("Name"),
    selected = "Help", id = "GenEstApp", 
    dataInputPanel(),
    analysisPanel(),
    helpPanel(appType),
    GenEstShinyJS(), GenEstInlineCSS()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{dataInputPanel}: Data Input panel. Calls 
#'   \code{dataInputSidebar} and \code{loadedDataPanel}.
#'
#' @return \code{dataInputPanel}: Data Input panel.
#'
#' @export
#'
dataInputPanel <- function(){
  tabPanel("Data Input", 
    sidebarLayout(
      dataInputSidebar(), 
      loadedDataPanel())
  )
}

#' @rdname GenEstUI
#'
#' @description \code{dataInputSidebar}: Data Input panel's sidebar (where the
#'   data files are selected). Calls \code{\link{dataInputWidget}} for each 
#'   data file type. 
#'
#' @return \code{dataInputSidebar}: Data Input sidebar.
#'
#' @export
#'
dataInputSidebar <- function(){
  sidebarPanel(width = 3, 
    h4(b(u("Select Data Files:")), style = "margin-bottom: 20px"),
    dataInputWidget("SE"), 
    dataInputWidget("CP"), 
    dataInputWidget("SS"), 
    dataInputWidget("DWP"), 
    dataInputWidget("CO"),
    br(),
    actionButton("clear_all", "Clear All", style = cButtonStyle("all")),
    br()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{loadedDataPanel}: Data Input panel's main page (where 
#'   the data files are displayed). Calls \code{\link{dataTabPanel}} for each 
#'   data file type. 
#'
#' @return \code{loadedDataPanel}: Data Input data panel.
#'
#' @export
#'
loadedDataPanel <- function(){
  mainPanel(
    tabsetPanel(id = "LoadedDataViz",
      dataTabPanel("SE"), 
      dataTabPanel("CP"), 
      dataTabPanel("SS"), 
      dataTabPanel("DWP"), 
      dataTabPanel("CO")
    )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{analysisPanel}: Analysis panel. Calls
#'   \code{GeneralInputsPanel}, \code{SEPanel}, \code{CPPanel}, 
#'   \code{MPanel}, and \code{gPanel}.
#'
#' @return \code{AnalysisPanel}: Analysis panel.
#'
#' @export
#'
analysisPanel <- function(){
  tabPanel("Analyses", 
    tabsetPanel(
      GeneralInputsPanel(),
      SEPanel(),
      CPPanel(),
      MPanel(),
      gPanel()
    )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{GeneralInputsPanel}: Analysis panel's General Inputs 
#'   panel. Calls \code{GeneralInputSidebar}.
#'
#' @return \code{GeneralInputsPanel}: Analysis -> General Inputs panel.
#'
#' @export
#'
GeneralInputsPanel <- function(){
  tabPanel("General Inputs", br(), br(), 
    GeneralInputSidebar()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{GeneralInputSidebar}: 
#'   Analysis panel's General Inputs sidebar (where the Number of Iterations,
#'   Confidence Level, and Size Class Column are selected). Calls 
#'   \code{\link{modelInputWidget}} for each input. 
#'
#' @return \code{GeneralInputSidebar}: Analysis -> General Inputs sidebar.
#'
#' @export
#'
GeneralInputSidebar <- function(){
  sidebarPanel(width = 3,
    modelInputWidget("nsim"),
    modelInputWidget("CL"),
    modelInputWidget("sizeclassCol")
  )
}

#' @rdname GenEstUI
#'
#' @description \code{SEPanel}: Analysis panel's Searcher Efficiency panel.
#'   Calls \code{SESidebar} and \code{SEMainPanel}.
#'
#' @return \code{SEPanel}: Analysis -> Searcher Efficiency panel.
#'
#' @export
#'
SEPanel <- function(){
  tabPanel("Searcher Efficiency", br(), br(), 
    SESidebar(), 
    SEMainPanel()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{SESidebar}: Analysis panel's Searcher Efficiency panel's
#'   sidebar (where the Observation Columns, Predictor Columns, and fixed-k 
#'   values are input and where the Size Class, p formula, and k formula are 
#'   selected for the outputs). Calls \code{\link{modelInputWidget}} for each 
#'   input, \code{\link{modelRunWidget}} for running the model button, and 
#'   \code{\link{modelOutputWidget}} for output controls. 
#'
#' @return \code{SESidebar}: Analysis -> Searcher Efficiency sidebar.
#'
#' @export
#'
SESidebar <- function(){
  sidebarPanel(width = 3,
    b(u(big("Model Inputs:"))),
    br(), br(),
    modelInputWidget("obsCols_SE"),
    modelInputWidget("preds_SE"),
    modelInputWidget("kFixedInput"),
    modelRunWidget("SE"),
    modelOutputWidget("SE")
  )
}

#' @rdname GenEstUI
#'
#' @description \code{SEMainPanel}: Analysis panel's Searcher Efficiency 
#'   panel's main panel (where the Selected Data, Model Comparison, Figures,
#'    Model Estimates, and Model Selection are displayed). Calls
#'   \code{selectedDataPanel} for the selected data and then 
#'    \code{modelOutputPanel} for each output.
#'
#' @return \code{SEMainPanel}: Analysis -> Searcher Efficiency main panel.
#'
#' @export
#'
SEMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_SE",
      selectedDataPanel("SE"),
      modelOutputPanel("SEModComparison"),
      modelOutputPanel("SEFigures"),
      modelOutputPanel("SEEstimates"),
      modelOutputPanel("SEModSelection")
    )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{CPPanel}: Analysis panel's Carcass Persistence panel. 
#'   Calls \code{CPSidebar} and \code{CPMainPanel}.
#'
#' @return \code{CPPanel}: Analysis -> Carcass Persistence panel.
#'
#' @export
#'
CPPanel <- function(){
  tabPanel("Carcass Persistence", br(), br(), 
    CPSidebar(), 
    CPMainPanel()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{CPSidebar}: Analysis panel's Carcass Persistence
#'   panel's sidebar (where the Observation Columns, Predictor Columns, and
#'   Distributions are input and where the Size Class, location formula,
#'   and scale formula are selected for the outputs). Calls
#'   \code{\link{modelInputWidget}} for each input, 
#'   \code{\link{modelRunWidget}} for running the model button, and 
#'   \code{\link{modelOutputWidget}} for the output controls. 
#'
#' @return \code{CPSidebar}: Analysis -> Carcass Persistence sidebar.
#'
#' @export
#'
CPSidebar <- function(){
  sidebarPanel(width = 3,
    b(u(big("Model Inputs:"))),
    br(), br(),
    modelInputWidget("ltp"),
    modelInputWidget("fta"),
    modelInputWidget("preds_CP"),
    modelInputWidget("dists"),
    modelRunWidget("CP"),
    modelOutputWidget("CP")
  )
}

#' @rdname GenEstUI
#'
#' @description \code{CPMainPanel}: Analysis panel's Carcass Persistence
#'   panel's main panel (where the Selected Data, Model Estimates, Model
#'   Comparison, Figures, and Model Selection are displayed). Calls
#'   \code{selectedDataPanel} for the selected data and 
#'   \code{modelOutputPanel} for each of the outputs.
#'
#' @return \code{CPMainPanel}: Analysis -> Carcass Persistence main panel.
#'
#' @export
#'
CPMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_CP",
      selectedDataPanel("CP"),
      modelOutputPanel("CPModComparison"),
      modelOutputPanel("CPFigures"),
      modelOutputPanel("CPEstimates"),
      modelOutputPanel("CPModSelection")
    )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{MPanel}: Analysis panel's Mortality Estimation panel. 
#'   Calls \code{MSidebar} and \code{MMainPanel}.
#'
#' @return \code{MPanel}: Analysis -> Mortality Estimation panel.
#'
#' @export
#'
MPanel <- function(){
  tabPanel("Mortality Estimation", br(), br(), 
    MSidebar(),
    MMainPanel()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{MSidebar}: Analysis panel's Mortality Estimation panel's
#'   sidebar (where the assummed k (if needed), Fraction of Facility Sampled,
#'   DWP Column, and Date Found Column are input and the Size Class is
#'   selected for the outputs). Calls 
#'   \code{\link{modelInputWidget}} for each input,
#'   \code{\link{modelRunWidget}} for running the model button, and 
#'   \code{\link{modelOutputWidget}} for the output controls. 
#'
#' @return \code{MSidebar}: Analysis -> Mortality Estimation 
#'   sidebar.
#'
#' @export
#'
MSidebar <- function(){
  sidebarPanel(width = 3,
    b(u(big("Model Inputs:"))),
    br(), br(),
    modelInputWidget("frac"),
    modelInputWidget("DWPCol"),
    modelInputWidget("dateFoundCol"),
    modelRunWidget("M"),
    modelOutputWidget("M")
  )
}

#' @rdname GenEstUI
#'
#' @description \code{MMainPanel}: Analysis panel's Mortality Estimation 
#'   panel's main panel (where Figures, and Summary are displayed). Calls 
#'   \code{modelOutputPanel} for each output.
#'
#' @return \code{MMainPanel}: Analysis -> Mortality Estimation main panel.
#'
#' @export
#'
MMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_M",
      modelOutputPanel("MFigures"),
      modelOutputPanel("MSummary")
    )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{gPanel}: Analysis panel's Detection Probability panel. 
#'   Calls \code{gSidebar} and \code{gMainPanel}.
#'
#' @return \code{gPanel}: Analysis -> Detection Probability panel.
#'
#' @export
#'
gPanel <- function(){
  tabPanel("Detection Probability", br(), br(),
    gSidebar(),
    gMainPanel()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{gSidebar}: Analysis panel's Detection Probability 
#'   panel's sidebar (where the Search Schedule and assummed k (if needed) are
#'   input and the Size Class, is selected for the outputs). Calls 
#'   \code{\link{modelInputWidget}} for each input, 
#'   \code{\link{modelRunWidget}} for running the model button, and 
#'   \code{\link{modelOutputWidget}} for the output controls. 
#'
#' @return \code{gSidebar}: Analysis -> Detection Probability sidebar.
#'
#' @export
#'
gSidebar <- function(){
  sidebarPanel(width = 3,
    b(u(big("Model Inputs:"))),
    br(), br(),
    modelInputWidget("gSearchInterval"),
    modelInputWidget("gSearchMax"),
    modelInputWidget("useSSinputs"),
    modelInputWidget("useSSdata"),
    modelRunWidget("g"),
    modelOutputWidget("g")
  )
}

#' @rdname GenEstUI
#'
#' @description \code{gMainPanel}: Analysis panel's Detection Probability 
#'   panel's main panel (where the Search Schedule, Figures, and Summary are 
#'   displayed). Calls \code{modelOutputPanel} for each output.
#'
#' @return \code{gMainPanel}: Analysis -> Detection Probability main panel.
#'
#' @export
#'
gMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_g",
      selectedDataPanel("g"),
      modelOutputPanel("gFigures"),
      modelOutputPanel("gSummary")
    )
  )
}


#' @rdname GenEstUI
#'
#' @description \code{helpPanel}: Help panel. Calls 
#'   \code{gettingStartedPanel}, \code{downloadsPanel}, \code{aboutPanel}, 
#'   and \code{disclaimersPanel}.
#'
#' @return \code{helpPanel}: Help panel.
#'
#' @export
#'
helpPanel <- function(appType = "base"){
  tabPanel("Help", 
    tabsetPanel(
      gettingStartedPanel(),
      downloadsPanel(),
      aboutPanel(),
      disclaimersPanel(appType)
    )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{gettingStartedPanel}: Help panel's Getting Started 
#'   panel. calls \code{\link{gettingStartedContent}}, the function containing
#'   the raw content for the page (which is text heavy and so moved to its own 
#'   function).
#'
#' @return \code{gettingStartedPanel}: Help -> Getting Started panel.
#'
#' @export
#'
gettingStartedPanel <- function(){
  tabPanel("Getting Started", 
    gettingStartedContent()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{downloadsPanel}: Help panel's Downloads panel. Calls 
#'   \code{\link{dataDownloadWidget}} for each data set.
#'
#' @return \code{downloadsPanel}: Help -> Downloads panel.
#'
#' @export
#'
downloadsPanel<- function(){
  tabPanel("Downloads", 
      mainPanel(
    column(10, offset = 0,
      br(), 
      h3("Example data sets"),
      br(), 
      dataDownloadWidget("RP"),
      dataDownloadWidget("RPbat"),
      dataDownloadWidget("cleared"),
      dataDownloadWidget("powerTower"),
      dataDownloadWidget("PV"),
      dataDownloadWidget("trough"),
      dataDownloadWidget("mock")
    )
  )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{aboutPanel}: Help panel's About panel. Calls 
#'   \code{\link{aboutContent}}, the function containing the raw
#'   content for the page (which is text heavy and so moved to its own 
#'   function).
#'
#' @return \code{aboutPanel}: Help -> About panel.
#'
#' @export
#'
aboutPanel <- function(){
  tabPanel("About", 
    aboutContent()
  )
}

#' @rdname GenEstUI
#'
#' @description \code{disclaimersPanel}: Help panel's Disclaimers panel. 
#'   Calls \code{\link{disclaimersContent}}, the function containing the raw
#'   content for the page (which is text heavy and so moved to its own 
#'   function).
#'
#' @return \code{aboutPanel}: Help -> Disclaimers panel.
#'
#' @export
#'
disclaimersPanel <- function(appType = "base"){
  tabPanel("Disclaimers", 
    disclaimersContent(appType)
  )
}