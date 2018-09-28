#' @title Create the GenEst User Interface HTML
#'
#' @description This suite of functions create the HTML code underlying the
#'   GenEst user interface (UI). \cr \cr 
#'   \code{GenEstUI} creates the HTML code for the whole page by calling 
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
#' @return Each function returns a string of HTML code: \cr \cr 
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
#' @description \code{dataInputPanel} creates the HTML code for the Data Input
#'   panel in the GenEst UI by calling \code{dataInputSidebar} and 
#'   \code{loadedDataPanel}.
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
#' @description \code{dataInputSidebar} creates the HTML code for the Data
#'   Input panel's sidebar (where the data files are selected) by calling 
#'   \code{\link{dataInputWidget}} for each of the data file types. 
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
#' @description \code{loadedDataPanel} creates the HTML code for the Data
#'   Input panel's main page (where the data files are displayed) by calling 
#'   \code{\link{dataTabPanel}} for each of the data file types. 
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
#' @description \code{analysisPanel} creates the HTML code for the Analysis
#'   panel in the GenEst UI by calling \code{GeneralInputsPanel},
#'   \code{SEPanel}, \code{CPPanel}, \code{MPanel}, and \code{gPanel}.
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
#' @description \code{GeneralInputsPanel} creates the HTML code for the 
#'   Analysis panel's General Inputs panel by calling 
#'   \code{GeneralInputSidebar}.
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
#' @description \code{GeneralInputSidebar} creates the HTML code for the 
#'   Analysis panel's General Inputs sidebar (where the Number of Iterations,
#'   Confidence Level, and Size Class Column are selected) by calling 
#'   \code{\link{modelInputWidget}} for each of the inputs. 
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
#' @description \code{SEPanel} creates the HTML code for the Analysis
#'   panel's Searcher Efficiency panel by calling 
#'   \code{SESidebar} and \code{SEMainPanel}.
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
#' @description \code{SESidebar} creates the HTML code for the Analysis 
#'   panel's Searcher Efficiency panel's sidebar (where the Observation 
#'   Columns, Predictor Columns, and fixed-k values are input and where the 
#'   Size Class, p formula, and k formula are selected for the outputs) by
#'   calling \code{\link{modelInputWidget}} for each of the inputs,
#'   \code{\link{modelRunWidget}} for running the model, and then 
#'   \code{\link{modelOutputWidget}} for the output controls. 
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
    modelInputWidget("kFixed"),
    modelRunWidget("SE"),
    modelOutputWidget("SE")
  )
}

#' @rdname GenEstUI
#'
#' @description \code{SEMainPanel} creates the HTML code for the Analysis 
#'   panel's Searcher Efficiency panel's main panel (where the Selected Data,
#'   Model Estimates, Figures, Model Comparison, and Model Selection are 
#'   displayed) by calling \code{selectedDataPanel} for the selected
#'   data and then \code{modelOutputPanel} for each of the outputs.
#'
#' @return \code{SEMainPanel}: Analysis -> Searcher Efficiency main panel.
#'
#' @export
#'
SEMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_SE",
      selectedDataPanel("SE"),
      modelOutputPanel("SEFigures"),
      modelOutputPanel("SEEstimates"),
      modelOutputPanel("SEModComparison"),
      modelOutputPanel("SEModSelection")
    )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{CPPanel} creates the HTML code for the Analysis
#'   panel's Carcass Persistence panel by calling 
#'   \code{CPSidebar} and \code{CPMainPanel}.
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
#' @description \code{CPSidebar} creates the HTML code for the Analysis 
#'   panel's Carcass Persistence panel's sidebar (where the Observation 
#'   Columns, Predictor Columns, and Distributions are input and where the 
#'   Size Class, location formula, and scale formula are selected for the 
#'   outputs) by calling \code{\link{modelInputWidget}} for each of the 
#'   inputs, \code{\link{modelRunWidget}} for running the model, and then 
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
#' @description \code{CPMainPanel} creates the HTML code for the Analysis 
#'   panel's Carcass Persistence panel's main panel (where the Selected Data,
#'   Model Estimates, Figures, Model Comparison, and Model Selection are 
#'   displayed) by calling \code{selectedDataPanel} for the selected
#'   data and then \code{modelOutputPanel} for each of the outputs.
#'
#' @return \code{CPMainPanel}: Analysis -> Carcass Persistence main panel.
#'
#' @export
#'
CPMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_CP",
      selectedDataPanel("CP"),
      modelOutputPanel("CPFigures"),
      modelOutputPanel("CPEstimates"),
      modelOutputPanel("CPModComparison"),
      modelOutputPanel("CPModSelection")
    )
  )
}


#' @rdname GenEstUI
#'
#' @description \code{MPanel} creates the HTML code for the Analysis
#'   panel's Mortality Estimation panel by calling 
#'   \code{MSidebar} and \code{MMainPanel}.
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
#' @description \code{MSidebar} creates the HTML code for the Analysis 
#'   panel's Mortality Estimation panel's sidebar (where the Search Schedule
#'   and the assummed k (if needed) are input and where the Size Class, 
#'   is selected for the outputs) by calling \code{\link{modelInputWidget}} 
#'   for each of the inputs, \code{\link{modelRunWidget}} for running the 
#'   model, and then \code{\link{modelOutputWidget}} for the output controls. 
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
    modelInputWidget("kFill"),
    modelInputWidget("frac"),
    modelInputWidget("DWPCol"),
    modelInputWidget("dateFoundCol"),
    modelRunWidget("M"),
    modelOutputWidget("M")
  )
}

#' @rdname GenEstUI
#'
#' @description \code{MMainPanel} creates the HTML code for the Analysis 
#'   panel's Mortality Estimation panel's main panel (where the Figures, and
#'   Summary are displayed) by calling \code{modelOutputPanel} for each of 
#'   the outputs.
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
#' @description \code{gPanel} creates the HTML code for the Analysis
#'   panel's Detection Probability panel by calling 
#'   \code{gSidebar} and \code{gMainPanel}.
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
#' @description \code{gSidebar} creates the HTML code for the Analysis 
#'   panel's Detection Probability panel's sidebar (where the Search Schedule
#'   and the assummed k (if needed) are input and where the Size Class, 
#'   is selected for the outputs) by calling \code{\link{modelInputWidget}} 
#'   for each of the inputs, \code{\link{modelRunWidget}} for running the 
#'   model, and then \code{\link{modelOutputWidget}} for the output controls. 
#'
#' @return \code{gSidebar}: Analysis -> Detection Probability sidebar.
#'
#' @export
#'
gSidebar <- function(){
  sidebarPanel(width = 3,
    b(u(big("Model Inputs:"))),
    br(), br(),
    modelInputWidget("kFill_g"),
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
#' @description \code{gMainPanel} creates the HTML code for the Analysis 
#'   panel's Detection Probability panel's main panel (where the Search 
#'   Schedule, Figures, and Summary are displayed) by calling 
#'   \code{modelOutputPanel} for each of the outputs.
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
#' @description \code{helpPanel} creates the HTML code for the Help panel in
#'   the GenEst UI by calling \code{gettingStartedPanel},
#'   \code{downloadsPanel}, \code{aboutPanel}, and \code{disclaimersPanel}.
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
#' @description \code{gettingStartedPanel} creates the HTML code for the Help
#'   panel's Getting Started panel, which calls 
#'   \code{\link{gettingStartedContent}}, the function containing the raw
#'   content for the page (which is text heavy and so moved to its own 
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
#' @description \code{downloadsPanel} creates the HTML code for the Help
#'   panel's Downloads panel, which calls 
#'   \code{\link{dataDownloadWidget}} for each of the data sets.
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
      dataDownloadWidget("mock"),
      dataDownloadWidget("mock2")
    )
  )
  )
}

#' @rdname GenEstUI
#'
#' @description \code{aboutPanel} creates the HTML code for the Help
#'   panel's About panel, which calls 
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
#' @description \code{disclaimersPanel} creates the HTML code for the Help
#'   panel's Disclaimers panel, which calls 
#'   \code{\link{disclaimersContent}}, the function containing the raw
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