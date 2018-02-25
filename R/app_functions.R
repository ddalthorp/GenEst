#' Launches GenEst app.
#' 
#' @export
#'
runGenEst <- function(){

  appDir <- system.file("application", "GenEst", package = "GenEst")
  if(appDir == ""){
    return("Could not find directory.")
  }
  shiny::runApp(appDir, display.mode = "normal")
}

