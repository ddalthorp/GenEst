#' @title Set the figure width based on the number of cells
#'
#' @description Convenience function for determining the needed figure width
#'
#' @param modSet model set
#'
#' @return figure width
#'
#' @export
#'
setFigW <- function(modSet){
  if (modSet[[1]]$ncell > 6){
    return(1200)
  } else{
    return(800)
  }
}

#' @title Set the figure height based on the number of cells
#'
#' @description Convenience function for determining the needed figure height
#'
#' @param modSet model set
#'
#' @param minH minimum height
#'
#' @param type "SE" or "CP"
#'
#' @return figure height
#'
#' @export
#'
setFigH <- function(modSet, minH, type = "SE"){
  nRow <- ceiling(modSet[[1]]$ncell / 2 )
  proposed <- c(nRow * 200 + 400)
  if (type == "CP"){
    proposed <- proposed - 100
  }
  max(proposed, minH)
}