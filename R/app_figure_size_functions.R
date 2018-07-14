#' @title Set the figure width based on the number of cells
#'
#' @description Convenience function for determining the needed figure width
#'
#' @param modelSet model set
#'
#' @return figure width
#'
#' @export
#'
setFigW <- function(modelSet){
  ncell <- nrow(modelSetCells(modelSet))
  if (ncell > 6){
    return(1200)
  } else{
    return(800)
  }
}

#' @title Set the figure height based on the number of cells
#'
#' @description Convenience function for determining the needed figure height
#'
#' @param modelSet model set
#'
#' @param type "SE" or "CP"
#'
#' @return figure height
#'
#' @export
#'
setFigH <- function(modelSet, type = "SE"){
  ncell <- nrow(modelSetCells(modelSet))
  nRow <- ceiling(ncell / 3)
  mult <- 200
  if (ncell > 6){
    mult <- 300
  }
  proposed <- nRow * mult + 400
  out <- max(c(proposed, 800))
  if (type == "CP"){
    out <- out - 100
  }
  out
}