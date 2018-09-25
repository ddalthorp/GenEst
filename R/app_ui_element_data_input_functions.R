
#' @title Data Input Main Panel UI element 
#'
#' @description create the HTML code for the Data Input panel
#'
#' @return HTML for the Data Input panel
#'
#' @export
#'
dataInputPanel <- function(){
  sidebarLayout(dataInputSidebar(), loadedDataPanel())
}
#' @title Data Input sidebar panel UI element
#'
#' @description create the HTML code for the Data Input sidebar
#'
#' @return HTML for the Data Input sidebar
#'
#' @export
#'
dataInputSidebar <- function(){
  okft <- c("text/csv", "text/comma-separated-values", ".csv")
  llab <- "Load file"
  clab <- "Clear file"
  cstyle <- cButtonStyle()
  sidebarPanel(width = 3, 
    h4(b(u("Select Data Files:")), style = "margin-bottom: 20px"),
    h5(b("Searcher Efficiency Data")),
    fileInput("file_SE", label = NULL, accept = okft, buttonLabel = llab), 
    conditionalPanel(condition = "output.filename_SE != null",
      actionButton("file_SE_clear", clab, style = cstyle) 
    ), 
    h5(b("Carcass Persistence Data")),
    fileInput("file_CP", label = NULL, accept = okft, buttonLabel = llab), 
    conditionalPanel(condition = "output.filename_CP != null",
      actionButton("file_CP_clear", clab, style = cstyle) 
    ), 
    h5(b("Search Schedule Data")),
    fileInput("file_SS", label = NULL, accept = okft, buttonLabel = llab), 
    conditionalPanel(condition = "output.data_SS != null",
      actionButton("file_SS_clear", clab, style = cstyle) 
    ), 
    h5(b("Density Weighted Proportion Data")),
    fileInput("file_DWP", label = NULL, accept = okft, buttonLabel = llab), 
    conditionalPanel(condition = "output.data_DWP != null",
      actionButton("file_DWP_clear", clab, style = cstyle) 
    ), 
    h5(b("Carcass Observation Data")),
    fileInput("file_CO", label = NULL, accept = okft, buttonLabel = llab), 
    conditionalPanel(condition = "output.data_CO != null",
      actionButton("file_CO_clear", clab, style = cstyle)
    )
  )
}

#' @title Data Input loaded data UI element
#'
#' @description create the HTML code for the Data Input loaded data tables
#'
#' @return HTML for the Data Input data table ouput
#'
#' @export
#'
loadedDataPanel <- function(){
  mainPanel(
    tabsetPanel(id = "LoadedDataViz",
      dataTabPanel("Searcher Efficiency", "data_SE"),
      dataTabPanel("Carcass Persistence", "data_CP"),
      dataTabPanel("Search Schedule", "data_SS"),
      dataTabPanel("Density Weighted Proportion", "data_DWP"),
      dataTabPanel("Carcass Observations", "data_CO")
    )
  )
}

#' @title Data Input loaded data UI element for a single table
#'
#' @description create the HTML code for a single Data Input loaded data table
#'
#' @param tabname shiny ID name of the table 
#'
#' @param table table's name in input object
#'
#' @return HTML for the Data Input data table ouput
#'
#' @export
#'
dataTabPanel <- function(tabname, table){
  tabPanel(tabname, br(), dataTableOutput(table))
}
