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
