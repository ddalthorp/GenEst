#' @title Prepare class text header
#'
#' @description Depending on the classes, prepare the header text. 
#'
#' @param rv Reactive values list for the GenEst GUI.
#'
#' @param type Model type, either "SE" or "CP" or "g".
#'
#' @return Rendered text ready for export to \code{output} list.
#'
#' @export
#'
classText <- function(rv, type = "SE"){
  out <- ""
  if (type == "SE"){
    if (length(rv$sizeclasses_SE) > 1){
      out <- paste0("Size class: ", rv$sizeclass_SE)
    }        
  }
  if (type == "CP"){
    if (length(rv$sizeclasses_CP) > 1){
      out <- paste0("Size class: ", rv$sizeclass_CP)
    }    
  }
  if (type == "g"){
    if (length(rv$sizeclasses_g) > 1){
      out <- paste0("Size class: ", rv$sizeclass_g)
    }
  }
  renderText(out)
}

#' @title Prepare the text for the estimation table
#'
#' @description Depending on the model type and CL, prepare the header text.
#'
#' @param rv Reactive values list for the GenEst GUI.
#'
#' @param type Model type, either "SE" or "CP".
#'
#' @return Rendered text ready for export to \code{output} list.
#'
#' @export
#'
estText <- function(rv, type = "SE"){
  out <- NULL
  if (type == "SE"){
    out <- paste0("Table shows median estimates and ", 100 * rv$CL,  
             "% confidence intervals")
  }
  if (type == "CP"){
    out <- paste0("Table shows median estimates and ", 100 * rv$CL,  
             "% confidence intervals for location and scale")
  }
  renderText(out)
}

#' @title Render a data table without server-side processing
#'
#' @description Simply render the data table without server-side processing.
#'
#' @param x HTML \code{datatable} widget output from 
#'   \code{\link[DT]{datatable}}.
#'
#' @return Rendered \code{x}.
#'
#' @export
#'
renderDTns <- function(x){
  renderDataTable(x, server = FALSE)
}

#' @title (Re)set outputOptions to not suspending for given elements
#'
#' @description Utility function for clearing and setting purposes.
#'
#' @param output \code{output} list to have elements \code{dontSuspend} 
#'   (re)set to having \code{suspendWhenHidden = FALSE}.
#'
#' @param dontSuspend Names of elements in \code{output} to (re)set to 
#'   having \code{suspendWhenHidden = FALSE}.
#'
#' @export
#'
setNotSuspending <- function(output, dontSuspend){
  for(i in 1:length(dontSuspend)){
    outputOptions(output, dontSuspend[i], suspendWhenHidden = FALSE)
  }
}

#' @title Reset values of a list to NULL
#'
#' @description Utility function for clearing and setting purposes.
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

