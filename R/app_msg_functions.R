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
    "ModSE" = NULL
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

#' @title Create the error message for when no models are fit successfully
#'
#' @description Produces a notification for complete model failure
#'
#' @param msgs message list
#'
#' @param clear if all notifications should be cleared or not
#'
#' @return a model fail error message
#'
#' @export
#'
msgModFail <- function(msgs, clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
  msg <- paste(
           "No models were successfully fit."
         )
  if(!is.null(msg)){
    return(showNotification(msg, type = "error", duration = NULL))
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
msgModDone <- function(msgs, modelType, clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
  NULL
}
