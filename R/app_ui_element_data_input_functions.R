
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
  sidebarPanel(width = 3,
    radioButtons("csvType", label = ".csv Field and Decimal Separators:",
      c("Comma ( , ) and Period ( . )" = "csv",
        "Semicolon ( ; ) and Comma( , )" = "csv2")
    ),
    fileInput("file_SE", "Searcher Efficiency Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    ), 
    fileInput("file_CP", "Carcass Persistence Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    ), 
    fileInput("file_SS", "Search Schedule Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    ), 
    fileInput("file_DWP", "Density Weighted Proportion Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    ), 
    fileInput("file_CO", "Carcass Observation Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
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
