#' @title Prepare predictors based on inputs
#'
#' @description Prepare predictor inputs from the app for use in the model
#'   function
#'
#' @param preds predictors, as input to the app
#'
#' @return prepared predictors (or 1 if no predictors)
#'
#' @export
#'
prepPredictors <- function(preds = NULL){

 out <- paste(preds, collapse = "*")
 if (is.null(preds)){
   out <- 1
 }
 return(out)
}

#' @title Create the kFillNeed text
#'
#' @description Based on the number of observation columns, create text output
#'   of "yes" or "no"
#'
#' @param rv reactive value list
#'
#' @return kFillNeed character of "yes" or "no"
#'
#' @export
#'
setkFillNeed <- function(rv){
  textout <- "no"
  if(length(rv$obsCols_SE) == 1 & length(rv$kFixed) == 0){
    textout <- "yes"
  }
  return(renderText(textout))
}

#' @title Update the fixed k value
#' 
#' @description Update the value for \code{kFixed} is chosen and available
#'
#' @param kFixedChoice choice to fix k (1) or not (anything else)
#'
#' @param kFixed existing kFixed value
#'
#' @return new kFixed value
#'
#' @export
#'
setkFix <- function(kFixedChoice, kFixed){
  if (kFixedChoice == 1 & is.numeric(kFixed)){
    return(kFixed)
  }else{
    return(NULL)
  }
}

#' @title Select the date columns from a data table
#'
#' @description Simple function to facilitate selection of date columns from
#'   a data table
#'
#' @param data data table potentially containing columns that could be 
#'   coerced (via \code{as.Date(yyyymmmdd())}) into a date
#'
#' @return column names of columns that can be coerced to dates
#'
#' @export
#'
dateCols <- function(data){

  ncols <- ncol(data)
  dateTF <- rep(NA, ncols)
  for (coli in 1:ncols){
    temp <- tryCatch(
              as.Date(yyyymmdd(data[ , coli])),
              error = function(x){FALSE}
            )
    dateTF[coli] <- lubridate::is.Date(temp)
  }
  out <- colnames(data)[dateTF]
  return(out)
}

#' @title Remove selected columns from column names
#'
#' @description Simple function to facilitate removal of columns selected
#'
#' @param colNames column names from which some could be removed
#'
#' @param selCols selected columns to be removed
#'
#' @return column names without selected columns
#'
#' @export
#'
removeSelCols <- function(colNames, selCols){
  which_sel <- which(colNames %in% selCols)
  if (length(which_sel) > 0){
    out <- colNames[-which_sel]
  } else{
    out <- colNames
  }
  return(out)
}

                                              #' @title Update the string of column names that are in all the needed tables
#'
#' @description Determine the overlap between the column names in the SE, CP,
#'    and CO data tables.
#'
#' @param rv reactive values list with elements named \code{colnames_SE},
#'    \code{colnames_CP}, and \code{colnames_CO}
#'
#' @return possible column names
#'
#' @export
#'
updateColNames_all <- function(rv){

  SECPCO <- NULL
  SE <- rv$colNames_SE
  CP <- rv$colNames_CP
  CO <- rv$colNames_CO

  SECP <- which(SE %in% CP)
  SECO <- which(SE %in% CO)
  CPSE <- which(CP %in% SE)
  CPCO <- which(CP %in% CO)
  COSE <- which(CO %in% SE)
  COCP <- which(CO %in% CP)
  alltogether <- c(SECP, SECO, CPSE, CPCO, COSE, COCP)

  if (length(alltogether) == 0){
    if (is.null(SE) + is.null(CP) + is.null(CO) == 2){
      SECPCO <- unique(c(SE, CP, CO))
    }
  } else{
    if (is.null(SE) + is.null(CP) + is.null(CO) == 1){
      SECPCOa <- c(SE[SECP], SE[SECO], CP[CPSE], CP[CPCO], CO[COSE], CO[COCP])
      SECPCO <- unique(SECPCOa)
    } else{
      SECP <- SE[SE %in% CP]
      SECPCO <- CO[CO %in% SECP]
    }
  }

  return(SECPCO)
}

#' @title Update the string of column names that are in all the needed tables
#'
#' @description Determine the overlap between the column names in the SE, CP,
#'    and CO data tables among columns with unique(length(x)) < length(x)
#'
#' @param rv reactive values list with elements named \code{data_SE},
#'    \code{data_CP}, and \code{data_CO}
#'
#' @return possible column names
#'
#' @export
#'
updateColNames_size <- function(rv){
  # columns with length(x) == length(unique(x)) in SE or CP are not eligible for
  # size class column
  SECPCO <- NULL
  goodInd <- function(x){
    which(apply(x, FUN = function(x) length(unique(x)) < length(x), MAR = 2))
  }
  if (!is.null(rv$data_SE)){
    SECPCO <- names(goodInd(rv$data_SE))
  }
  if (!is.null(rv$data_CP)){
    tmp <- names(goodInd(rv$data_CP))
    if (!is.null(SECPCO)) SECPCO <- intersect(SECPCO, tmp) else SECPCO <- tmp
  }
  if (!is.null(rv$data_CO)){
    tmp <- colnames(rv$data_CO)
    if (!is.null(SECPCO)) SECPCO <- intersect(SECPCO, tmp) else SECPCO <- tmp
  }
  return(SECPCO)
}

#' @title Select particular columns from a data set
#'
#' @description Convenience function for selecting specific columns from a 
#'   data table
#'
#' @param data data table to select from
#'
#' @param cols column names to select
#'
#' @return selected data
#'
#' @export
#'
selectData <- function(data, cols){
  colNames <- colnames(data)
  selectedTab <- data[ , which(colNames %in% cols)]
  selectedDF <- data.frame(selectedTab)
  if (length(cols) == 1){
    colnames(selectedDF) <- cols
  }
  return(selectedDF)
}

#' @title Split model names into their components and remove only a desired 
#'   one
#'
#' @description Split a model name to return a specific component. Splitting 
#'   is done based on the semicolon in the name
#'
#' @param modNames names of the model to be split off
#'
#' @param pos position in the name to split off
#'
#' @return vector of split-off model names
#'
#' @export
#'
modNameSplit <- function(modNames, pos){
  modNames_split <- modNames
  nmod <- length(modNames)
  if (nmod > 0){
    for (modi in 1:nmod){
      modNames_split[modi] <- strsplit(modNames[modi], "; ")[[1]][pos]
    }
  }
  modNames_split <- gsub("NULL", "s ~ 1", modNames_split)
  modNames_split <- gsub("~ 1", "~ constant", modNames_split)
  modNames_split <- gsub("dist:", "", modNames_split)
  return(modNames_split)
}

#' @title Count the minimum number of carcasses in the cells
#'
#' @description Count the minimum number of carcasses in all of the cells
#'   within a \code{_SetSize} model complex
#'
#' @param mods model output from the \code{_SetSize} version of a function
#'
#' @return the minimum number of carcasses in the cells
#'
#' @export
#'
countCarcs <- function(mods){
  nsizeclasses <- length(mods)
  nmods <- length(mods[[1]])
  if (nsizeclasses > 0 & nmods > 0){
    ncarc <- rep(NA, nsizeclasses * nmods)
    counter <- 0
    for (sci in 1:nsizeclasses){
      for (modi in 1:nmods){
        counter <- counter + 1
        if (!grepl("Failed model fit", mods[[sci]][[modi]][1])){
          ncarc[counter] <- min(table(mods[[sci]][[modi]]$carcCell))
        }
      }
    }
    ncarc <- min(na.omit(ncarc))
  }else{
    ncarc <- Inf
  }
  return(ncarc)
}

#' @title Prepare text for size classes 
#'
#' @description Prepare and render text of the size class names
#'
#' @param sizeclasses names of the size classes
#'
#' @return prepared and render name text
#'
#' @export
#'
prepSizeclassText <- function(sizeclasses){
  return(renderText(paste(sizeclasses, collapse = " ")))
}

#' @title Paste the parts of a model's name back together
#'
#' @description Paste the component parts of a model's name back together
#'   for presentation
#'
#' @param parts the component parts of the model's name
#'
#' @param type "SE" or "CP"
#'
#' @param tab logical for if it's the table output for CP
#'
#' @return the pasted name
#'
#' @export
#'
modNamePaste <- function(parts, type = "SE", tab = FALSE){
  if (tab & parts[1] == " exponential"){
    out <- paste(c(parts[1:2], "NULL"), collapse = "; ")
  } else{
    out <- paste(parts, collapse = "; ")
  }
  if (type == "CP"){
    out <- paste("dist:", out, sep = "")
  }
  out <- gsub("~ constant", "~ 1", out)
  return(out)
}
  
#' @title Produce the options for the distributions in the CP model
#'
#' @description Simply make the named list for the disributions in the CP
#'   model
#'
#' @return list with named elements of the distributions 
#'
#' @export
#'
CPdistOptions <- function(){
  list("exponential" = "exponential", "weibull" = "weibull",
    "lognormal" = "lognormal", "loglogistic" = "loglogistic"
  )
}


#' @title Produce a blank plot for unsucessful fits
#'
#' @description Simply make a blank plot with descriptive text
#'
#' @return dummy plot
#'
#' @export
#'
plotNA <- function(){
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", 
    ylab = "", ylim = c(0, 1), xlim = c(0,1))
  text(0.01, 0.9, "Selected model was not fit successfully.", adj = 0)
}

