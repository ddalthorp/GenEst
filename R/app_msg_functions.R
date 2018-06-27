#' @title Produce the message list
#'
#' @description Produce the message list, currently restricted to a NULL value
#'   starting for each value
#'
#' @return list of message elements
#'
#' @export
#'
msgList <- function(){
  list(
    "ModSE" = NULL, "ModCP" = NULL
  )
}

#' @title Clear all notifications
#' 
#' @description Clear all messages in the message list
#'
#' @param msgs message list
#'
#' @export
#'
#'
clearNotifications <- function(msgs = msgList()){
  if (!is.null(msgs$ModSE)){
    removeNotification(msgs$ModSE)
  }
  if (!is.null(msgs$ModCP)){
    removeNotification(msgs$ModCP)
  }
}

#' @title Create a model running message
#'
#' @description Produces a model-running notification
#'
#' @param msgs message list
#'
#' @param modelType "SE", "CP", "g", or "M"
#'
#' @param clear if all notifications should be cleared or not
#'
#' @return a model running message
#'
#' @export
#'
msgModRun <- function(msgs, modelType, clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
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



#' @title Create a model done message
#'
#' @description Produces a model-done notification
#'
#' @param msgs message list
#'
#' @param modelType "SE", "CP", "g", or "M"
#'
#' @param rv reactive values list
#'
#' @param clear if all notifications should be cleared or not
#'
#' @return a model running message
#'
#' @export
#'
msgModDone <- function(msgs, modelType, rv, clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
  if (modelType == "SE"){
    if (all(unlist(pkmSetSizeFail(rv$mods_SE_og)))){
      return(msgModFail(rv$mod_SE))
    } else{
      if (any(unlist(pkmSetSizeFail(rv$mods_SE_og)))){
      }
    }
  }

  if (modelType == "CP"){
    return(NULL)
  }
}


#' @title Create the warning message text for when only some models are fit 
#'   successfully
#'
#' @description Produces text for a notification for partial model failure
#'
#' @return a partial model fail warning text
#'
#' @export
#'
msgModPartialFail <- function(){
paste(
  "Some models were not successfully fit. Failed models were ",
  "removed.", sep = " "
)
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