#' @title Resources Main Panel UI element
#'
#' @description create the HTML code for the Resources panel
#'
#' @param type "base" (for local versions) or "deploy" (for hosted version)
#'
#' @return HTML for the Analysis panel
#'
#' @export
#'
helpPanel <- function(type = "base"){
  tabsetPanel(
    gettingStartedPanel(),
    downloadsPanel(),
    aboutPanel(),
    disclaimersPanel(type)
  )
}








