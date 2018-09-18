#' @title Analysis Main Panel UI element
#'
#' @description create the HTML code for the Analysis panel
#'
#' @return HTML for the Analysis panel
#'
#' @export
#'
analysisPanel <- function(){
  tabsetPanel(
    GeneralInputsPanel(),
    SEPanel(),
    CPPanel(),
    MPanel(),
    gPanel()
  )
}

#' @title Analysis General Inputs Main Panel UI element 
#'
#' @description create the HTML code for the Analysis General Inputs panel
#'
#' @return HTML for the Analysis General Inputs panel
#'
#' @export
#'
GeneralInputsPanel <- function(){
  tabPanel("General Inputs", br(), br(), GeneralInputSidebar())
}

#' @title Analysis General Inputs sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis General Inputs sidebar
#'
#' @return HTML for the Analysis General Inputs sidebar
#'
#' @export
#'
GeneralInputSidebar <- function(){
  sidebarPanel(width = 3,
    numericInput("nsim", "Number of Iterations:", value = 1000,
      min = 1, max = 10000, step = 1
    ),
    numericInput("CL", "Confidence Level:", value = 0.95, min = 0, 
      max = 1, step = 0.001
    ),
    selectizeInput("sizeclassCol", "Size Class Column (optional):", 
      c("No data input yet"), multiple = TRUE, 
      options = list(maxItems = 1)
    )
  )
}
