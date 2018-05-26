#' Creates the pretty version of the Searcher Efficiency model table
#'
#' @description Based on confidence level of interest
#' @param modTab model table
#' @param CL Confidence level
#' @return column names
#' @export
#'
prettyModTabSE <- function(modTab, CL = 0.9){

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

#' Creates the pretty Carcass Persistence model table
#'
#' @description Based on confidence level of interest
#' @param modTab model table
#' @param CL Confidence level
#' @return column names
#' @export
#'
prettyModTabCP <- function(modTab, CL = 0.9){
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

#' Prepares predictors based on inputs
#'
#' @param preds input predictors
#' @return prepared predictors (or 1 if no predictors)
#' @export
#'
prepPredictors <- function(preds = NULL){

 out <- paste(preds, collapse = "*")
 if (is.null(preds)){
   out <- 1
 }
 return(out)
}

#' Creates the small sample size warning 
#'
#' @param mods models to check for sample sizes
#' @return a small sample size message
#' @export
#'
msgSampleSize <- function(mods){
  minCellCount <- countCarcs(mods)
  if (any(minCellCount < 10)){
    msg <- paste("Small (< 10) sample sizes per cell. Consider a simpler ",
             "model. Parameter estimates may be unstable.", sep = ""
           )
    return(showNotification(msg, type = "warning", duration = NULL))
  }
}

#' Creates the error message for when no models are fit successfully
#' @param mods (fully failed) model object
#' @return a model fail error message
#' @export
#'
msgModFail <- function(mods){
  msg <- paste(
           "No models were successfully fit.", 
            gsub("Failed model fit: ", "", unique(unlist(mods))),
            sep = " "
         )
  if(!is.null(msg)){
    return(showNotification(msg, type = "error", duration = NULL))
  }
}

#' Creates the warning message for when only some models are fit successfully
#' @return a partial model fail warning
#' @export
#'
msgModPartialFail <- function(){
  msg <- paste(
           "Some models were not successfully fit. Failed models were ",
           "removed. Check inputs.", sep = " "
         )
  return(showNotification(msg, type = "warning", duration = NULL))
}

#' Creates the warning message for when the SS average doesn't work
#' @return an average SS fail warning
#' @export
#'
msgSSavgFail <- function(){
  msg <- paste(
           "Search Schedule cannot be averaged using given date searched ",
           "column", sep = ""
         )
  return(showNotification(msg, type = "warning", duration = NULL))
}

#' Creates a model running message
#' @param modelType "SE", "CP", "g", or "M"
#' @return a model running message
#' @export
#'
msgModRun <- function(modelType){
  msg <- NULL
  if (modelType == "SE"){
    msg <- ("Running Searcher Efficiency Model")
  }
  if (modelType == "CP"){
    msg <- ("Running Carcass Persistence Model")
  }
  if (modelType == "g"){
    msg <- ("Running Detection Probability Model")
  }
  if (modelType == "M"){
    msg <- ("Estimating Fatalities")
  }
  if(!is.null(msg)){
    return(showNotification(msg, duration = NULL))
  }
}

#' Creates the M finished message
#'
#' @param M output from estM
#' @return M done message
#' @export
#'
msgModDoneM <- function(M){

  if (any(is.na(M))){
    if (all(is.na(M))){
      msg <- "All M estimations failed, check input"
      return(showNotification(msg, type = "error", duration = NULL))
    } else{
      msg <- "Some M estimations failed, check input"
      return(showNotification(msg, type = "warning", duration = NULL))
    }
  } else{
    msg  <- "M estimation done."
    return(showNotification(msg, type = "message", duration = NULL))
  }
}

#' Creates the SE data size notification 
#'
#' @param formula_k formula for k
#' @param kFixed fixed value for k if used
#' @param obsCols vector of observation column names
#' @return data size message
#' @export
#'
msgNobsSE <- function(formula_k, kFixed, obsCols){
  msg <- NULL
  if (length(obsCols) == 1){
    if(length(formula_k) > 0 & length(kFixed) == 0){
      msg <- ("Only one observation, k not estimated.")
    }
    if (length(kFixed) == 1){
      msg <- ("Only one observation, fix k input ignored.")
    }
  }
  if(!is.null(msg)){
    return(showNotification(msg, type = "warning", duration = NULL))
  }
}


#' Creates the kFillNeed text
#'
#' @param obsCols vector of observation column names
#' @return kFillNeed
#' @export
#'
setkFillNeed <- function(obsCols){
  if(length(obsCols) == 1){
    return(renderText("yes"))
  }
  if(length(obsCols) > 1){
    return(renderText("no"))
  }
}

#' @title Clear specific notifications
#' @description Clear notifications within the app. 
#'
#' @param msg_NobsSE Number of search occasions message for SE
#' @param msg_SampleSizeSE Sample size message for SE
#' @param msg_ModFailSE Fail message for SE
#' @param msg_ModFailCP  Fail message for CP
#' @param msg_SampleSizeCP Sample size message for CP 
#' @param msg_ModFailM Fail message for mortality estimation
#' @param msg_ModFailg Fail message for generic g estimation
#' @param msg_avgSSfail Fail message for averaging search schedule
#' @return Nothing
#' @export
#'
clearNotifications <- function(msg_NobsSE = NULL, msg_SampleSizeSE = NULL,
                               msg_ModFailSE = NULL,
                               msg_ModFailCP = NULL, msg_SampleSizeCP = NULL,
                               msg_ModFailM = NULL, msg_ModFailg = NULL,
                               msg_avgSSfail = NULL){

  if(!is.null(msg_ModFailSE)){
    removeNotification(msg_ModFailSE)
  }
  if(!is.null(msg_SampleSizeSE)){
    removeNotification(msg_SampleSizeSE)
  }
  if(!is.null(msg_NobsSE)){
    removeNotification(msg_NobsSE)
  }
  if(!is.null(msg_ModFailCP)){
    removeNotification(msg_ModFailCP)
  }
  if(!is.null(msg_SampleSizeCP)){
    removeNotification(msg_SampleSizeCP)
  }
  if(!is.null(msg_ModFailM)){
    removeNotification(msg_ModFailM)
  }
  if(!is.null(msg_ModFailg)){
    removeNotification(msg_ModFailg)
  }
  if(!is.null(msg_avgSSfail)){
    removeNotification(msg_avgSSfail)
  }
 
}

#' Updates the string of column names that are in all the needed tables
#'
#' @param colnames_SE column names for the searcher efficiency
#' @param colnames_CP column names for the carcass persistence
#' @param colnames_CO column names for the carcass observations
#' @return possible column names
#' @export
#'
updateColNames_all <- function(colnames_SE, colnames_CP, colnames_CO){

  SECPCO <- NULL
  SE <- colnames_SE
  CP <- colnames_CP
  CO <- colnames_CO

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

#' Updates the name of the size class column based on available names
#'
#' @description if the existing size class column name is no longer in the
#'   set of available names, a NULL is returned to reset the column name
#'
#' @param sizeclassCol current size class column name
#' @param colNames_all updated vector of column names in all needed tables
#' @return updated sizeclassCol
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

#' Select particular columns from a data set
#'
#' @param data data table to selec from
#' @param cols column names to select
#' @return selected data
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

#' Update the fixed k value
#'
#' @param kFixedChoice choice to fix k (1) or not (anything else)
#' @param kFixed existing kFixed value
#' @return new kFixed value
#' @export
#'
setkFix <- function(kFixedChoice, kFixed){
  if (kFixedChoice == 1 & is.numeric(kFixed)){
    return(kFixed)
  }else{
    return(NULL)
  }
}

#' Update the size classes
#'
#' @param data data table to draw sizes from
#' @param sizeclassCol size class column name 
#' @return unique size classes
#' @export
#'
updateSizeclasses <- function(data, sizeclassCol){
  if (is.null(sizeclassCol)){
    return("all")
  }
  return(as.character(unique(data[ , sizeclassCol])))
}


#' Locate the sizeclass selected by the inputs
#'
#' @param sizeclasses size class options
#' @param choice size class chosen
#' @return location of the size class chosen
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
#' Split model names into their components and remove only a desired one
#' @description Splitting is done based on the semicolon
#' @param modNames names of the model to be split off
#' @param pos position in the name to split off
#' @return vector of split-off model names
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
  modNames_split <- gsub("dist:", "", modNames_split)
  return(modNames_split)
}

#' Count the minimum number of carcasses in the cells
#'
#' @param mods model output from the _SetSize version of a function
#' @return the minimum number of carcasses in the cells
#' @export
#'
countCarcs <- function(mods){
  ncarc <- NULL
  nsizeclasses <- length(mods)
  nmods <- length(mods[[1]])
  if (nsizeclasses > 0 & nmods > 0){
    for (sci in 1:nsizeclasses){
      for (modi in 1:nmods){
        ncarc <- c(NULL, min(table(mods[[sci]][[modi]]$carcCell)))
      }
    }
  }else{
    ncarc <- Inf
  }
  return(ncarc)
}

#' Set the figure width based on the number of cells
#'
#' @param modSet model set
#' @return figure width
#' @export
#'
setFigW <- function(modSet){
  if (modSet[[1]]$ncell > 6){
    return(1200)
  } else{
    return(800)
  }
}

#' Set the figure height based on the number of cells
#'
#' @param modSet model set
#' @param minH minimum height
#' @param type "SE" or "CP"
#' @return figure height
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

#' Prepare text for size classes 
#'
#' @param sizeclasses names of the size classes
#' @return prepared and render name text
#' @export
#'
prepSizeclassText <- function(sizeclasses){
  return(renderText(paste(sizeclasses, collapse = " ")))
}

#' Make a model menu
#'
#' @param mods models
#' @param sizeclasses size class names
#' @param type "SE" or "CP"
#' @return rendered model menu object
#' @export
#'
makeMenu <- function(mods, sizeclasses, type){

  modelMenu <- ""
  nsizeclasses <- length(sizeclasses)
  if (nsizeclasses > 0){
    for(sci in 1:nsizeclasses){
      AICcTab <- pkmSetAICcTab(mods[[sci]], quiet = TRUE)
      modOrder <- as.numeric(row.names(AICcTab))
      modNames <- names(mods[[sci]])[modOrder]
      modNames <- gsub("; NULL", "", modNames)
      modNames <- gsub("dist: ", "", modNames)

      modNames_nchar <- nchar(modNames)
      modNames_maxchar <- max(modNames_nchar)
      modNames_nspaces <- modNames_maxchar - modNames_nchar + 10
      modSpaces <- sapply(modNames_nspaces, 
                     function(x){paste(rep(" ", x), collapse = "")}
                   )

      modAICcs <- AICcTab[ , "AICc"]
      modLabels <- paste0(modNames, " (AICc: ", modAICcs, ")")
      names(modNames) <- modLabels
      labels_nchar <- nchar(modLabels)
      labels_maxchar <- max(labels_nchar)
      widthval <- max(c(400, labels_maxchar * 7 + 20))
      widthtxt <- paste0(widthval, "px")
      mtuText <- paste("modelChoices_", type, sci, sep = "") 
      scText <- paste("Model for ", sizeclasses[sci], sep = "")
      modSelect <- selectizeInput(mtuText, scText, modNames, width = widthtxt)
      modelMenu <- paste(modelMenu, modSelect)  
    }
  }
          
  return(renderUI({HTML(modelMenu)}))
}

#' Paste the parts of a model's name back together
#' @param parts the parts
#' @param type "SE" or "CP"
#' @param tab logical for if it's the table output for CP
#' @return the pasted name
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
  return(out)
}
  
#' Produce the options for the distributions in the CP model
#' @return list with named elements of the distributions 
#' @export
#'
CPdistOptions <- function(){
  list("exponential" = "exponential", "weibull" = "weibull",
    "lognormal" = "lognormal", "loglogistic" = "loglogistic"
  )
}