#' @title Help Main Panel UI element
#'
#' @description create the HTML code for the Help panel
#'
#' @param type "base" (for local versions) or "deploy" (for hosted version)
#'
#' @return HTML for the Help panel
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
