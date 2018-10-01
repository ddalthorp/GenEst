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

  out <- modTab[ , c("cell", "p_median", "k_median")]
  ncell <- nrow(out)

  for (celli in 1:ncell){
    p_m <- modTab[celli, "p_median"]
    p_l <- modTab[celli, "p_lower"]
    p_u <- modTab[celli, "p_upper"]
    k_m <- modTab[celli, "k_median"]
    k_l <- modTab[celli, "k_lower"]
    k_u <- modTab[celli, "k_upper"]
    out[celli, "p_median"] <- paste0(p_m, " [", p_l, ", ", p_u, "]")

    if (is.na(k_m)){
      out[celli, "k_median"] <- ""
    } else{
      out[celli, "k_median"] <- paste0(k_m, " [", k_l, ", ", k_u, "]")
    }
  }

  colnames(out) <- c("Cell", "p", "k")
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
#' @param modTabs model tables
#'
#' @param CL Confidence level
#'
#' @return pretty version of the CP model table
#'
#' @export
#'
prettyModTabCP <- function(modTabs, CL = 0.90){
  modTab <- modTabs[["ls"]]
  modTab_d <- modTabs[["desc"]]
  out <- modTab[ , c("cell", "l_median", "s_median")]
  ncell <- nrow(out)

  for (celli in 1:ncell){
    l_m <- modTab[celli, "l_median"]
    l_l <- modTab[celli, "l_lower"]
    l_u <- modTab[celli, "l_upper"]
    s_m <- modTab[celli, "s_median"]
    s_l <- modTab[celli, "s_lower"]
    s_u <- modTab[celli, "s_upper"]
    out[celli, "l_median"] <- paste0(l_m, " [", l_l, ", ", l_u, "]")

    if (s_m == s_l & s_m == s_u & s_m == 1){
      out[celli, "s_median"] <- "1"
    } else{
      out[celli, "s_median"] <- paste0(s_m, " [", s_l, ", ", s_u, "]")
    }
  }

  colnames(out) <- c("Cell", c("Location", "Scale"))
  cellCol <- which(colnames(modTab_d) == "cell")
  out_d <- modTab_d[ , -cellCol]
  for (celli in 1:ncell){
    cellMatch <- which(out$Cell == modTab_d$cell[celli])
    out_d[celli, ] <- round(modTab_d[cellMatch, -cellCol], 2)
  }
  colnames(out_d)[which(colnames(out_d) == "median")] <- "Median CP"
  out <- cbind(out, out_d)
  return(out)
}

#' @title Create the download version of the Carcass Persistence model table
#'
#' @description Format a user-friendly version of the parameter table from
#'   a Carcass Persistence model, based on confidence level of interest
#'
#' @param modTabs model tables
#'
#' @param CL Confidence level
#'
#' @return download version of the SE model table
#'
#' @export
#'
dlModTabCP <- function(modTabs, CL = 0.90){
  modTab <- modTabs[["ls"]]
  modTab_d <- modTabs[["desc"]]
  ncell <- nrow(modTab)
  out <- modTab
  lo <- 100 * (1 - CL) / 2
  up <- 100 - 100 * (1 - CL) / 2
  coltypes <- c("Median", paste0(lo, "%"), paste0(up, "%"))
  colnames(out) <- c("Cell", paste0("Location ", coltypes), 
                     paste0("Scale ", coltypes))
  cellCol <- which(colnames(modTab_d) == "cell")
  out_d <- modTab_d[ , -cellCol]
  for (celli in 1:ncell){
    cellMatch <- which(out$Cell == modTab_d$cell[celli])
    out_d[celli, ] <- round(modTab_d[cellMatch, -cellCol], 2)
  }
  colnames(out_d)[which(colnames(out_d) == "median")] <- "Median CP"
  out <- cbind(out, out_d)
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

  vectored <- as.vector(splitSummary)

  if ("splitSummary" %in% attr(vectored, "class")){

    nspec <- length(splitSummary)
    Out <- prettySplitSpecTab(splitSummary[[1]])
    Out[, 1] <- row.names(splitSummary[[1]])
    colnames(Out)[1] <- attr(splitSummary, "vars")[1]
    extralev <- rep(names(splitSummary)[1], nrow(Out))
    Out <- cbind(extralev, Out)    
    colnames(Out)[1] <- attr(splitSummary, "vars")[2]

    for (speci in 2:nspec){
      specOut <- prettySplitSpecTab(splitSummary[[speci]])
      colnames(specOut)[1] <- attr(splitSummary, "vars")[1]
      specOut[, 1] <- row.names(splitSummary[[speci]])
      colnames(specOut)[1] <- attr(splitSummary, "vars")[1]
      extralev <- rep(names(splitSummary)[speci], nrow(specOut))
      specOut <- cbind(extralev, specOut)    
      colnames(specOut)[1] <- attr(splitSummary, "vars")[2]
      Out <- rbind(Out, specOut)
    }
    return(Out)
  } else{
    return(prettySplitSpecTab(splitSummary))
  }
}

#' @title Create the pretty version of a specific split summary table
#'
#' @description Format a reader-friendly version of a specific split summary 
#'   table from a mortality estimation
#'
#' @param splitSummarySpec a specific split summary
#'
#' @return specific split pretty table  
#'
#' @export
#'
prettySplitSpecTab <- function(splitSummarySpec){
  vectored <- as.vector(splitSummarySpec)
  nrows <- nrow(splitSummarySpec)
  ncols <- ncol(splitSummarySpec)
  if (is.null(nrows)){
    nrows <- 1
    ncols <- length(splitSummarySpec)
    rnames <- "all"
    cnames <- names(splitSummarySpec)
  } else{
    rnames <- rownames(splitSummarySpec)
    cnames <- colnames(splitSummarySpec)
  }
  prettyTab <- matrix(round(vectored, 2), nrow = nrows, ncol = ncols)
    
  colnames(prettyTab) <- cnames
  varname <- attr(splitSummarySpec, "vars")
  if (is.null(varname)){
    return(prettyTab)
  }
  prettyTab <- cbind(rnames, prettyTab)
  colnames(prettyTab)[1] <- varname
  return(prettyTab)
}
