#' @title Update the size classes
#'
#' @description Determine the options for size classes, based on a data table
#'   and column name, returning \code{NULL} if no size class column is
#'   provided
#'
#' @param data data table to draw sizes from
#'
#' @param sizeclassCol size class column name 
#'
#' @return unique size classes
#'
#' @export
#'
updateSizeclasses <- function(data, sizeclassCol){
  if (is.null(sizeclassCol)){
    return("all")
  }
  return(as.character(unique(data[ , sizeclassCol])))
}

#' @title Locate the sizeclass selected by the inputs
#'
#' @description Locate the selection of a size class from the size class 
#'   column, retuning the first option from the size classes if the selection
#'   is not available. 
#'
#' @param sizeclasses size class options
#'
#' @param choice size class chosen
#'
#' @return location of the size class chosen
#'
#' @export
#'
pickSizeclass <- function(sizeclasses, choice){

  sizeclass <- NULL
  if (!(choice %in% sizeclasses)){
    choice <- sizeclasses[1]
  }
  sizeclass <- sizeclasses[which(sizeclasses == choice)]
  return(sizeclass)
}

#' @title Update the name of the size class column based on available names
#'
#' @description Update the size class column name based on the available
#'   options. If the existing size class column name is no longer in the
#'   set of available names, a NULL is returned to reset the column name.
#'
#' @param sizeclassCol current size class column name
#'
#' @param colNames_all updated vector of column names in all needed tables
#'
#' @return updated sizeclassCol
#'
#' @export
#'
updateSizeclassCol <- function(sizeclassCol, colNames_all){
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colNames_all)){
      NULL
    } else{
      sizeclassCol
    }
  } else{
    NULL
  }
}
