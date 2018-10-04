#' @title Create the version text for GenEst 
#'
#' @description Create a text string of the version number and date
#'
#' @param type "Full" or "Short" or "Name" or "NameDate"
#'
#' @return version text
#'
#' @export
#'
createvtext <- function(type = "Full"){
  vnumber <- packageDescription("GenEst", fields = "Version")
  vdate <- packageDescription("GenEst", fields = "Date")
  if (type == "Full"){
    vtext <- paste0("This is version ", vnumber, " (", vdate, ")")
  }
  if (type == "Short"){
    vtext <- paste0("v", vnumber)
  }
  if (type == "Name"){
    vtext <- paste0("GenEst ", "v", vnumber)
  }
  if (type == "NameDate"){
    vtext <- paste0("GenEst ", "v", vnumber, " (", vdate, ")")
  }
  return(vtext)
}

#' @title Read in csv files in either format
#'
#' @description Handle reading in of a csv that is either comma-decimal or
#'   semicolon-comma separation style
#'
#' @param path file path
#'
#' @return read in data table
#'
#' @export
#'
readCSV <- function(path){
  ef <- function(x){"_BAD_READ_"}
  out1 <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
            error = ef, warning = ef)

  out2 <- tryCatch(read.csv2(path, stringsAsFactors = FALSE),
            error = ef, warning = ef)
  if (is.null(attr(out1, "class")) & is.null(attr(out2, "class"))){
   stop("File not found or not formatted as a .csv")
  }
  if ("data.frame" %in% attr(out1, "class")){
    if (is.null(attr(out2, "class"))){
      return(out1)
    }
    if ("data.frame" %in% attr(out2, "class")){
      if (ncol(out2) == 1){
        return(out1)
      }
    }
  }
  if ("data.frame" %in% attr(out2, "class")){
    if (is.null(attr(out1, "class"))){
      return(out2)
    }
    if ("data.frame" %in% attr(out1, "class")){
      if (ncol(out1) == 1){
        return(out2)
      }
    }
  }
  return(out1)
}

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
  if(length(rv$obsCols_SE) == 1 & any(is.na(rv$kFixed))){
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
  nkFix <- length(kFixed)
  out <- rep(NA, nkFix)
  for (i in 1:nkFix){
    if (kFixedChoice[i] == 1 & is.numeric(kFixed[i])){
      out[i] <- kFixed[i]
    }
  }
  names(out) <- names(kFixed)
  out
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
    dateTF[coli] <- is.Date(temp)
  }
  out <- colnames(data)[dateTF]
  return(out)
}

#' @title Select the potential size class columns from a data table
#'
#' @description Simple function to facilitate selection of columns that could
#'   be size class values from a data table
#'
#' @param data data table
#'
#' @return column names of columns that can be size class values
#'
#' @export
#'
sizeclassCols <- function(data){

  if (is.null(data)){
    return(NULL)
  }
  ncols <- ncol(data)
  scTF <- rep(NA, ncols)
  for (coli in 1:ncols){
    tmp <- data[ , coli]
    if (length(unique(tmp)) < nrow(data)){
      scTF[coli] <- TRUE
    } else{
      scTF[coli] <- FALSE
    }
  }
  out <- colnames(data)[scTF]
  return(out)
}


#' @title Select the DWP-ok columns from a data table
#'
#' @description Simple function to facilitate selection of columns that could
#'   be DWP values from a data table
#'
#' @param data data table
#'
#' @return column names of columns that can be DWP values
#'
#' @export
#'
DWPCols <- function(data){
  ncols <- ncol(data)
  dwpTF <- rep(NA, ncols)
  for (coli in 1:ncols){
    tmp <- data[ , coli]
    if (!is.factor(tmp) && is.numeric(tmp) &&( all(tmp > 0) & all(tmp <= 1))){
      dwpTF[coli] <- TRUE
    } else{
      dwpTF[coli] <- FALSE
    }
  }
  out <- colnames(data)[dwpTF]
  return(out)
}

#' @title Select the predictor-ok columns from a data table
#'
#' @description Simple function to facilitate selection of columns that could
#'   be predictors for SE or CP models from a data table
#'
#' @param data data table
#'
#' @return column names of columns that can be predictors
#'
#' @export
#'
predsCols <- function(data){
  ncols <- ncol(data)
  predTF <- rep(NA, ncols)
  for (coli in 1:ncols){
    tmp <- data[ , coli]
    cont <- FALSE
    if (is.numeric(tmp) && any(na.omit(tmp %% 1 != 0))){
      cont <- TRUE
    }
    if (length(unique(tmp)) == nrow(data)){
      reps <- FALSE
    } else{
      reps <- TRUE
    }
    if (grepl("[-.]", colnames(data)[coli])){
      okName <- FALSE
    } else{
      okName <- TRUE
    }    
    if (!any(is.na(tmp)) & !cont & reps & okName){
      predTF[coli] <- TRUE
    } else{
      predTF[coli] <- FALSE
    }
  }
  out <- colnames(data)[predTF]
  return(out)
}

#' @title Select the columns from a data table that could be SE observations
#'
#' @description Simple function to facilitate selection of columns that could
#'   be observations for an SE model
#'
#' @param data data table
#'
#' @return column names of columns that can be observations
#'
#' @export
#'
obsCols_SE <- function(data){
  ncols <- ncol(data)
  obsTF <- rep(NA, ncols)
  for (coli in 1:ncols){
    tmp <- na.omit(data[ , coli])
    if (any(tmp == 0 | tmp == 1) & all(tmp == 0 | tmp == 1)){
      obsTF[coli] <- TRUE
    } else{
      obsTF[coli] <- FALSE
    }
  }
  out <- colnames(data)[obsTF]
  return(out)
}

#' @title Select the columns from a data table that could be CP Last Time
#'   Present observations
#'
#' @description Simple function to facilitate selection of columns that could
#'   be Last Time Present observations for a CP model
#'
#' @param data data table
#'
#' @return column names of columns that can be observations
#'
#' @export
#'
obsCols_ltp <- function(data){
  ncols <- ncol(data)
  obsTF <- rep(NA, ncols)
  for (coli in 1:ncols){
    tmp <- data[ , coli]
    if (is.numeric(tmp) && is.finite(tmp) && all(na.omit(tmp) >= 0)){
      obsTF[coli] <- TRUE
    } else{
      obsTF[coli] <- FALSE
    }
  }
  out <- colnames(data)[obsTF]
  return(out)
}


#' @title Select the columns from a data table that could be CP First Time
#'   Absent observations
#'
#' @description Simple function to facilitate selection of columns that could
#'   be First Time Absent observations for a CP model
#'
#' @param data data table
#'
#' @return column names of columns that can be observations
#'
#' @export
#'
obsCols_fta <- function(data){
  ncols <- ncol(data)
  obsTF <- rep(NA, ncols)
  for (coli in 1:ncols){
    tmp <- data[ , coli]
    if (is.numeric(tmp) && all(na.omit(tmp) > 0)){
      obsTF[coli] <- TRUE
    } else{
      obsTF[coli] <- FALSE
    }
  }
  out <- colnames(data)[obsTF]
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
removeCols <- function(colNames, selCols){
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
#' @param rv reactive values list
#'
#' @return possible column names
#'
#' @export
#'
updateColNames_size <- function(rv){

  SECPCO <- NULL
  SE <- sizeclassCols(rv$data_SE)
  CP <- sizeclassCols(rv$data_CP)
  CO <- sizeclassCols(rv$data_CO)

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
  if (is.null(data)){
    return(NULL)
  }

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
#' @param type "model" or "split"
#'
#' @return dummy plot
#'
#' @export
#'
plotNA <- function(type = "model"){
  if (type == "model"){
    badText <- "Selected model was not fit successfully."
  } 
  if (type == "split"){
    badText <- "Second split too fine for plotting. Consider transposing."
  }
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", 
    ylab = "", ylim = c(0, 1), xlim = c(0,1))
  text(0.01, 0.9, badText, adj = 0)
}

#' @title Create the main reactive value list for GenEst 
#'
#' @description Create a list of reactive values as used across the components
#'   of the GenEst application
#'
#' @return a reactive values list
#'
#' @export
#'
createReactiveValues <- function(){
  reactiveValues(
    data_SE = NULL, data_CP = NULL, data_SS = NULL, data_DWP = NULL, 
    data_CO = NULL,
    filename_SE = NULL, filename_CP = NULL,
    colNames_SE = NULL, colNames_SE_preds = NULL, colNames_SE_preds0 = NULL,
    toRemove_SE_obs = NULL, toRemove_SE_preds = NULL,
    colNames_SE_obs = NULL, colNames_SE_obs0 = NULL, 
    colNames_CP = NULL, colNames_CP_preds = NULL, colNames_CP_preds0 = NULL, 
    toRemove_CP_preds = NULL,
    colNames_ltp = NULL, colNames_ltp0 = NULL, toRemove_ltp = NULL,   
    colNames_fta = NULL, colNames_fta0 = NULL, toRemove_fta = NULL,

    colNames_SS = NULL, colNames_SS_sel = NULL, colNames_SS_nosel = NULL, 
    colNames_DWP = NULL,
    colNames_CO = NULL, colNames_COdates = NULL,
    colNames_size = NULL,

    nsim = 1000, CL = 0.90,

    sizeclassCol = NULL, sizeClassCol0 = NULL, toRemove_sizeClassCol = NULL,
    sizeclasses = NULL, sizeclass = NULL, sizeclass_SE = NULL,
    sizeclass_CP = NULL, sizeclass_g = NULL, sizeclass_M = NULL, 
    nsizeclasses = NULL,

    obsCols_SE = NULL, preds_SE = NULL, predictors_SE = NULL, 
    formula_p = NULL, formula_k = NULL, kFixedChoice = NULL, kFixed = NULL, 
    mods_SE = NULL, mods_SE_og = NULL, sizeclasses_SE = NULL, 
    outSEpk = NULL, AICcTab_SE = NULL, modOrder_SE = NULL, modNames_SE = NULL,
    modNames_SEp = NULL, modNames_SEk = NULL, modSet_SE = NULL,
    best_SE = NULL, modTab_SE = NULL, modTabPretty_SE = NULL,
    modTabDL_SE = NULL, figH_SE = 800, figW_SE = 800,
    kFill = NULL, sizeclasses_k = NULL, nsizeclasses_k = NULL, 

    ltp = NULL, fta = NULL, preds_CP = NULL, dists = NULL, 
    predictors_CP = NULL, formula_l = NULL, formula_s = NULL, 
    mods_CP = NULL, mods_CP_og = NULL, CPdls = NULL, outCPdlsfig = NULL, 
    outCPdlstab = NULL, sizeclasses_CP = NULL, AICcTab_CP = NULL, 
    modOrder_CP = NULL, modNames_CP = NULL, modNames_CPdist = NULL, 
    modNames_CPl = NULL, modNames_CPs = NULL, modSet_CP = NULL, 
    best_CP = NULL, modTab_CP = NULL, modTabPretty_CP = NULL, 
    modTabDL_CP = NULL, figH_CP = 700, figW_CP = 800,

    M = NULL, Msplit = NULL, unitCol = NULL, frac = 1, 
    sizeclassCol_M = NULL, DWPCol = NULL, dateFoundCol = NULL, 
    SEmodToUse = NULL, CPmodToUse = NULL,
    split_CO = NULL, split_SS = NULL, nsplit_CO = 0, nsplit_SS = 0, 
    figH_M = 600, figW_M = 800,

    SS = seq(0, 364, 7), SStext = paste(seq(0, 364, 7), collapse = ", "),
    avgSI = NULL, SStemp = NULL, gSearchInterval = 7, gSearchMax = 364,
    kFill_g = NULL, sizeclasses_g = NULL, nsizeclasses_g = NULL,
    gGeneric = NULL, SEmodToUse_g = NULL, CPmodToUse_g = NULL,
    figH_g = 400, figW_g = 800,

    kCheck = NA, kCheck_g = NA, csvformat = ""
  )
}

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
#' @param colNames_size updated vector of size column names in all needed 
#'   tables
#'
#' @return updated sizeclassCol
#'
#' @export
#'
updateSizeclassCol <- function(sizeclassCol, colNames_size){
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colNames_size)){
      NULL
    } else{
      sizeclassCol
    }
  } else{
    NULL
  }
}

#' @title Generic S3 function for summarizing AICc
#'
#' @description Extract AICc values from \code{pkm}, \code{pkmSet},
#'  \code{pkmSetSize}, \code{cpm}, \code{cpmSet}, and \code{cpmSetSize}
#'
#' @param x is the model or list of models to extract AICc values from
#'
#' @return list of models sorted by AICc
#'
#' @export
aicc <- function(x, ... ){
  UseMethod("aicc", x)
}