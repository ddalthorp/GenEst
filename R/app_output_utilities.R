
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

