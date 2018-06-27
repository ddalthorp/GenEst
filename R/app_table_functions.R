#' @title Create the pretty version of the Searcher Efficiency model table
#'
#' @description Format a reader-friendly version of the parameter table from
#'   a Searcher Efficiency model, based on confidence level of interest
#'
#' @param modTab model table
#'
#' @param CL Confidence level
#'
#' @return pretty version of the SE model table
#'
#' @export
#'
prettyModTabSE <- function(modTab, CL = 0.95){

  out <- modTab[ , c("cell", "p_median", "k_median")]
  ncell <- nrow(out)

  for (celli in 1:ncell){
    p_m <- modTab[celli, "p_median"]
    p_l <- modTab[celli, "p_lower"]
    p_u <- modTab[celli, "p_upper"]
    k_m <- modTab[celli, "k_median"]
    k_l <- modTab[celli, "k_lower"]
    k_u <- modTab[celli, "k_upper"]
    out[celli, "p_median"] <- paste0(p_m, " [", p_l, " - ", p_u, "]")

    if (is.na(k_m)){
      out[celli, "k_median"] <- ""
    } else{
      out[celli, "k_median"] <- paste0(k_m, " [", k_l, " - ", k_u, "]")
    }
  }

  coltxt <- paste0(" (Median [", 100 * (1 - CL) / 2, "% - ", 
              100 - 100 * (1 - CL) / 2, "%])"
            )
  colnames(out) <- c("Cell", paste0(c("p", "k"), coltxt))
  return(out)
}

#' @title Create the download version of the Searcher Efficiency model table
#'
#' @description Format a user-friendly version of the parameter table from
#'   a Searcher Efficiency model, based on confidence level of interest
#'
#' @param modTab model table
#'
#' @param CL Confidence level
#'
#' @return download version of the SE model table
#'
#' @export
#'
dlModTabSE <- function(modTab, CL = 0.95){

  out <- modTab
  lo <- 100 * (1 - CL) / 2
  up <- 100 - 100 * (1 - CL) / 2
  coltypes <- c("Median", paste0(lo, "%"), paste0(up, "%"))
  colnames(out) <- c("Cell", paste0("p ", coltypes), paste0("k ", coltypes))
  return(out)
}