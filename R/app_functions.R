#' @title Launch the app
#' 
#' @description Launches a local version of the GenEst application by running
#'   \code{shinyAppDir} pointed to the \code{app} subdirectory in the 
#'   local \code{GenEst} package folder.
#'
#' @export
#'
runGenEst <- function(){

  appDir <- system.file("app", package = "GenEst")
  runApp(appDir)
  
}


