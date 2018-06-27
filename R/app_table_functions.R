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

  kFit <- any(grepl("k_median", colnames(modTab)))
  
  if (!kFit){
    modTab$k_median <- NA
    modTab$k_lower <- NA
    modTab$k_upper <- NA
  }

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

  kFit <- any(grepl("k_median", colnames(modTab)))
  if (!kFit){
    modTab$k_median <- NA
    modTab$k_lower <- NA
    modTab$k_upper <- NA
  }

  out <- modTab
  lo <- 100 * (1 - CL) / 2
  up <- 100 - 100 * (1 - CL) / 2
  coltypes <- c("Median", paste0(lo, "%"), paste0(up, "%"))
  colnames(out) <- c("Cell", paste0("p ", coltypes), paste0("k ", coltypes))
  return(out)
}

#' @title Create the pretty version of the Carcass Persistence model table
#'
#' @description Format a reader-friendly version of the parameter table from
#'   a Carcass Persistence model, based on confidence level of interest
#'
#' @param modTab model table
#'
#' @param CL Confidence level
#'
#' @return pretty version of the CP model table
#'
#' @export
#'
prettyModTabCP <- function(modTab, CL = 0.95){
  out <- modTab[ , c("cell", "l_median", "s_median")]
  ncell <- nrow(out)

  for (celli in 1:ncell){
    l_m <- modTab[celli, "l_median"]
    l_l <- modTab[celli, "l_lower"]
    l_u <- modTab[celli, "l_upper"]
    s_m <- modTab[celli, "s_median"]
    s_l <- modTab[celli, "s_lower"]
    s_u <- modTab[celli, "s_upper"]
    out[celli, "l_median"] <- paste0(l_m, " [", l_l, " - ", l_u, "]")

    if (s_m == s_l & s_m == s_u & s_m == 1){
      out[celli, "s_median"] <- "1"
    } else{
      out[celli, "s_median"] <- paste0(s_m, " [", s_l, " - ", s_u, "]")
    }
  }

  coltxt <- paste0(" (Median [", 100 * (1 - CL) / 2, "% - ", 
              100 - 100 * (1 - CL) / 2, "%])"
            )
  colnames(out) <- c("Cell", paste0(c("Location", "Scale"), coltxt))
  return(out)
}

#' @title Create the download version of the Carcass Persistence model table
#'
#' @description Format a user-friendly version of the parameter table from
#'   a Carcass Persistence model, based on confidence level of interest
#'
#' @param modTab model table
#'
#' @param CL Confidence level
#'
#' @return download version of the SE model table
#'
#' @export
#'
dlModTabCP <- function(modTab, CL = 0.95){

  out <- modTab
  lo <- 100 * (1 - CL) / 2
  up <- 100 - 100 * (1 - CL) / 2
  coltypes <- c("Median", paste0(lo, "%"), paste0(up, "%"))
  colnames(out) <- c("Cell", paste0("Location ", coltypes), 
                     paste0("Scale ", coltypes))
  return(out)
}