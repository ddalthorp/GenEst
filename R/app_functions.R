#' Launch the app
#' @export
#'
runGenEst <- function(){

  appDir <- system.file("app", package = "GenEst")
  shinyAppDir(appDir, options = list())
  
}


