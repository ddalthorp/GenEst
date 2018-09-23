#' @title Help Downloads Main Panel UI element 
#'
#' @description create the HTML code for the Help Downloads panel
#'
#' @return HTML for the Help Downloads panel
#'
#' @export
#'
downloadsPanel <- function(){
  tabPanel("Downloads", downloadsMainPanel())
}

#' @title Help Downloads main panel UI element
#'
#' @description create the HTML code for the Help Downloads main panel
#'
#' @return HTML for the Help Downloads main panel
#'
#' @export
#'
downloadsMainPanel <- function(){
  mainPanel(
    column(10, offset = 0,
      br(), 
      h3("Example data sets"),
      downloadsRow("RP"),
      downloadsRow("RPbat"),
      downloadsRow("cleared"),
      downloadsRow("powerTower"),
      downloadsRow("PV"),
      downloadsRow("trough"),
      downloadsRow("mock"),
      downloadsRow("mock2")
    )
  )
}

#' @title Help Downloads specific download UI element
#'
#' @description create the HTML code for a specific download row within 
#'   the Help Downloads main panel
#'
#' @param set name of data set "RP", "RPbat", "cleared", "powerTower", "PV",
#'   "trough", "mock", or "mock2"
#'
#' @return HTML for the Help Downloads specific download row
#'
#' @export
#'
downloadsRow <- function(set){

  setNames <- c("RP" = "Wind---Road and pad searches, bats + birds",
                "RPbat" = "Wind---Road and pad searches, bats",
                "cleared" = "Wind---Cleared plots, bats + birds",
                "powerTower" = "Solar---Power tower",
                "PV" = "Solar---Photovoltaic (PV)",
                "trough" = "Solar---Trough",
                "mock" = "Mock data",
                "mock2" = "Mock data with European-style csvs")

  setName <- setNames[set]
  setButtonName <- paste0("download_", set)

  fluidRow(
    column(6, h4(setName)), 
    column(1, downloadButton(setButtonName, "Download"))
  )
}
