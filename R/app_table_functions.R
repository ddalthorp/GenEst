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
prettyModTabSE <- function(modTab, CL = 0.90){

  kFit <- any(grepl("k_median", colnames(modTab)))
  
  if (!kFit){
    modTab$k_median <- NA
    modTab$k_lower <- NA
    modTab$k_upper <- NA
  }

  out <- modTab[ , c("cell", "n", "p_median", "k_median")]
  ncell <- nrow(out)

  for (celli in 1:ncell){
    p_m <- round(modTab[celli, "p_median"], 3)
    p_l <- round(modTab[celli, "p_lower"], 3)
    p_u <- round(modTab[celli, "p_upper"], 3)
    k_m <- round(modTab[celli, "k_median"], 3)
    k_l <- round(modTab[celli, "k_lower"], 3)
    k_u <- round(modTab[celli, "k_upper"], 3)
    out[celli, "p_median"] <- paste0(p_m, " [", p_l, ", ", p_u, "]")

    if (is.na(k_m)){
      out[celli, "k_median"] <- ""
    } else{
      out[celli, "k_median"] <- paste0(k_m, " [", k_l, ", ", k_u, "]")
    }
  }

  colnames(out) <- c("Cell", "n", "p", "k")
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
dlModTabSE <- function(modTab, CL = 0.90){

  kFit <- any(grepl("k_median", colnames(modTab)))
  if (!kFit){
    modTab$k_median <- NA
    modTab$k_lower <- NA
    modTab$k_upper <- NA
  }

  out <- modTab
  coltypes <- c("Median", (1 - CL)/2, 1 - (1 - CL)/2)
  colnames(out) <- c("Cell", "n", paste0("p_", coltypes),
                     paste0("k_", coltypes))
  return(out)
}

#' @title Create the pretty version of the Carcass Persistence model table
#'
#' @description Format a reader-friendly version of the parameter table from
#'  a carcass persistence model showing CIs for medianCP and for rI's for
#'  intervals of Ir
#'
#' @param table_CP \code{descCP} object or NULL
#'
#' @return pretty version of the CP model table in a data frame with point
#'  and interval estimates for medianCP and rI statistics. Output table is
#'  ready for rendering in shiny and posting in the GUI
#'
#' @export
#'

prettyModTabCP <- function(table_CP){
  if(is.null(table_CP))
    return(data.frame(msg = "Selected model was not successfully fit."))
  if (!"descCP" %in% class(table_CP)) stop("table_CP must be a descCP object")
  # descCP objects are matrices with have 3n named columns and ncell named rows

  modTab <- round(table_CP, 3)
  out <- data.frame(array(dim = 0:1 + (dim(table_CP) - 0:1)/c(1, 3)))
  names(out) <- c("n", colnames(table_CP)[which((1:dim(table_CP)[2])%%3 == 2)])
  rownames(out) <- row.names(modTab)
  out$n <- table_CP[ , "n"]
  for (i in 2:ncol(out)){
    out[ , i] <- paste0(modTab[, 2 + 3*(i - 2)],
      "  [", modTab[ , 3 + 3*(i - 2)], ", ", modTab[ , 4 + 3*(i - 2)], "]")
  }
  return(out)
}


#' @title Create the pretty version of the split summary table
#'
#' @description Format a reader-friendly version of the split summary table
#'   a mortality estimation
#'
#' @param splitSummary a split summary
#'
#' @return split pretty table 
#'
#' @export
#'
prettySplitTab <- function(splitSummary){
  if (!("splitSummary" %in% class(splitSummary))){
    out <- as.matrix(splitSummary, nrow = 1)
  } else if (is.list(splitSummary)){
    out <- NULL
    for (i in 1:length(splitSummary)){
      out <- round(rbind(out, splitSummary[[i]]), 2)
    }
    out <- cbind(rep(names(splitSummary), each = dim(splitSummary[[1]])[1]),
      rownames(out), out)
    rownames(out) <- NULL
    colnames(out) <- c(attr(splitSummary, "vars")[2:1], 
                       colnames(splitSummary[[1]]))
  } else {
    if (!is.matrix(splitSummary)){
      splitSummary <- t(as.matrix(splitSummary))
    }
    out <- cbind(rownames(splitSummary), round(splitSummary, 2))
    rownames(out) <- NULL
    colnames(out) <- c(attr(splitSummary, "vars"), colnames(splitSummary))
  }
  out
}