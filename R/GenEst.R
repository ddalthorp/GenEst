#' @useDynLib GenEst, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom shiny shinyApp shinyAppDir navbarPage tabPanel tabsetPanel
#'   selectizeInput renderUI showNotification renderText showModal modalDialog
#'   modalButton updateTabsetPanel updateSelectizeInput observeEvent
#'   reactiveValues fileInput numericInput radioButtons conditionalPanel
#'   plotOutput htmlOutput sidebarPanel sidebarLayout mainPanel
#'   removeNotification outputOptions isolate updateTabsetPanel renderPlot
#'   updateNumericInput actionButton checkboxGroupInput
#'   column fluidRow textOutput
#' @importFrom htmltools div img br HTML
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom graphics axis mtext par plot points rect text box lines polygon
#' @importFrom grDevices rgb devAskNewPage
#' @importFrom stats .getXlevels approxfun delete.response formula median
#'   model.matrix na.omit optim pgamma pnorm qnorm quantile reformulate
#'   runif terms update.formula weighted.mean as.formula
#' @importFrom utils combn packageDescription read.csv

#' @title Generalized estimation of fatalities
#'
#' @description This package is designed to analyze searcher efficiency, 
#'   carcass persistence, and carcass observation data for the estimation of
#'   fatalities.
#' @name GenEst
#' @docType package
#' @keywords package
#'
NULL