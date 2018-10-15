resetNotSuspending <- function(output, dontSuspend){
for(i in 1:length(dontSuspend)){
outputOptions(output, dontSuspend[i], suspendWhenHidden = FALSE)
}
}

#' @title Reset values of a list to NULL
#'
#' @description Utility function for clearing purposes.
#'
#' @param x \code{list} object to have elements \code{toNULL} reset to 
#'   \code{NULL}.
#'
#' @param toNULL Names of elements in \code{x} to reset to \code{NULL}.
#'
#' @return Updated \code{x}.
#'
#' @export
#'
reNULL <- function(x, toNULL){
  for(i in 1:length(toNULL)){
    x[[toNULL[i]]] <- NULL
  }
  x
}

#' @title Update the output list upon initiation of the app
#'
#' @description Update the output list when the app is initialized.
#'
#' @param rv Reactive values list for the GenEst GUI.
#'
#' @param output \code{output} list for the GenEst GUI.
#'
#' @return Updated \code{output} list.
#'
#' @export
#'
initialOutput <- function(rv, output){
  output$SStext <- renderText(rv$SStext)

  output$download_RP <- downloadData("RP")
  output$download_RPbat <- downloadData("RPbat")
  output$download_cleared <- downloadData("cleared")
  output$download_powerTower <- downloadData("powerTower")
  output$download_PV <- downloadData("PV")
  output$download_trough <- downloadData("trough")
  output$download_mock <- downloadData("mock")
  output$kFillNeed <- renderText("no")
  outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)

  return(output)
}
#' @title Set Figure width and height based on the number of cells
#'
#' @description Convenience functions for determining the needed figure sizes.
#'
#' @param modelSet Model set of class \code{cpmSet} or \code{pkmSet}.
#'
#' @return \code{setFigW}: Figure width.
#'
#' @export
#'
setFigW <- function(modelSet){
  if (!any(attr(modelSet, "class") %in% c("cpmSet", "pkmSet"))){
    stop("modelSet must be a cpmSet or pkmSet object")
  }
  ncell <- nrow(modelSetCells(modelSet))
  if (ncell > 6){
    return(1200)
  } else{
    return(800)
  }
}

#' @rdname setFigW
#'
#' @param modType "SE" or "CP"
#'
#' @return \code{setFigH}: Figure height.
#'
#' @export
#'
setFigH <- function(modelSet, modType = "SE"){
  if (!any(attr(modelSet, "class") %in% c("cpmSet", "pkmSet"))){
    stop("modelSet must be a cpmSet or pkmSet object")
  }
  if (!modType %in% c("SE", "CP")){
    stop(paste0("input modType (", modType, ") not supported"))
  }

  ncell <- nrow(modelSetCells(modelSet))
  nRow <- ceiling(ncell / 3)
  mult <- 200
  if (ncell > 6){
    mult <- 300
  }
  proposed <- nRow * mult + 400
  out <- max(c(proposed, 800))
  if (modType == "CP"){
    out <- out - 100
  }
  out
}

