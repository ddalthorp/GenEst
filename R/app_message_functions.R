#' @title Create the small sample size warning 
#'
#' @description Produces a notification for small sample sizes
#'
#' @param mods models to check for sample sizes
#'
#' @return a small sample size message
#'
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

#' @title Create the error message for when no models are fit successfully
#'
#' @description Produces a notification for complete model failure
#'
#' @param mods (fully failed) model object
#'
#' @return a model fail error message
#'
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

#' @title Create the warning message for when only some models are fit 
#'   successfully
#'
#' @description Produces a notification for partial model failure
#'
#' @return a partial model fail warning
#'
#' @export
#'
msgModPartialFail <- function(){
  msg <- paste(
           "Some models were not successfully fit. Failed models were ",
           "removed.", sep = " "
         )
  return(showNotification(msg, type = "warning", duration = NULL))
}

#' @title Create the warning message for when the SS average doesn't work
#'
#' @description Produces a notification for when an average search schedule
#'   can't be created
#'
#' @return an average SS fail warning
#'
#' @export
#'
msgSSavgFail <- function(){
  msg <- paste(
           "Search Schedule cannot be averaged using given date searched ",
           "column", sep = ""
         )
  return(showNotification(msg, type = "warning", duration = NULL))
}

#' @title Create a model running message
#'
#' @description Produces a model-running notification
#'
#' @param modelType "SE", "CP", "g", or "M"
#'
#' @return a model running message
#'
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
    msg <- ("Estimating Mortality")
  }
  if(!is.null(msg)){
    return(showNotification(msg, duration = NULL))
  }
}

#' @title Create the SE data size notification 
#'
#' @description Produces a notification for SE data sizes (associated with k)
#'
#' @param formula_k formula for k
#'
#' @param kFixed fixed value for k if used
#'
#' @param obsCols vector of observation column names
#'
#' @return data size message
#'
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

#' @title Create the fail message for when splits aren't done correctly
#'
#' @description Produces a notification for failed mortality splits
#'
#' @param type "setup" or "run"
#'
#' @return a split fail warning
#'
#' @export
#'
msgsplitFail <- function(type = NULL){

  if (is.null(type)){
    return(NULL)
  }
  if (type == "setup"){
    msg <- paste0(
             "Improper splits setup. Maximum two splits, only one of which ",
             "can be associated with the search schedule."
           )
  }
  if (type == "run"){
    msg <- "Splits calculation failed. Check split selections."
  }
  return(showNotification(msg, type = "error", duration = NULL))
}

#' @title Clear specific notifications
#'
#' @description Clear specific notifications within the app. 
#'
#' @param msg_NobsSE Number of search occasions message for SE
#'
#' @param msg_SampleSizeSE Sample size message for SE
#'
#' @param msg_ModFailSE Fail message for SE
#'
#' @param msg_ModFailCP  Fail message for CP
#'
#' @param msg_SampleSizeCP Sample size message for CP 
#'
#' @param msg_ModFailM Fail message for mortality estimation
#'
#' @param msg_ModFailg Fail message for generic g estimation
#'
#' @param msg_avgSSfail Fail message for averaging search schedule
#'
#' @param msg_splitFail Fail message for splitting M
#'
#' @return Nothing
#'
#' @export
#'
clearNotifications <- function(msg_NobsSE = NULL, msg_SampleSizeSE = NULL,
                               msg_ModFailSE = NULL,
                               msg_ModFailCP = NULL, msg_SampleSizeCP = NULL,
                               msg_ModFailM = NULL, msg_ModFailg = NULL,
                               msg_avgSSfail = NULL, msg_splitFail = NULL){

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
  if(!is.null(msg_splitFail)){
    removeNotification(msg_splitFail)
  }
  if(!is.null(msg_ModFailg)){
    removeNotification(msg_ModFailg)
  }
  if(!is.null(msg_avgSSfail)){
    removeNotification(msg_avgSSfail)
  }
 
}