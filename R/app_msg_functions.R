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
  list("ModSE" = NULL, "ModCP" = NULL)
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
#' @description Produces a SE or CP model-done notification
#'
#' @param msgs message list
#'
#' @param rv reactive values list
#'
#' @param type "SE" or "CP"
#'
#' @param clear if all notifications should be cleared or not
#'
#' @return an SE model done message
#'
#' @export
#'
msgModDone <- function(msgs, rv, type = "SE", clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
  if (type == "SE"){
    if (all(unlist(pkmSetSizeFail(rv$mods_SE_og)))){
      return(msgModFail(rv$mods_SE_og))
    } else{
      return(msgModWarning(rv$mods_SE_og, "SE"))
    }
  }
  if (type == "CP"){
    if (all(unlist(cpmSetSizeFail(rv$mods_CP_og)))){
      return(msgModFail(rv$mods_CP_og))
    } else{
      return(msgModWarning(rv$mods_CP_og, "CP"))
    }
  }  
  NULL
}

#' @title Create the warning message text for when only some models are fit 
#'   successfully
#'
#' @description Produces text for a notification for partial model failure
#'
#' @param mods Set Size list of models
#'
#' @param type "SE" or "CP"
#'
#' @return a partial model fail warning text
#'
#' @export
#'
msgModPartialFail <- function(mods, type = "SE"){

  anyFail <- FALSE
  if (type == "SE"){
    if (any(unlist(pkmSetSizeFail(mods)))){
      anyFail <- TRUE
    }
  }
  if (type == "CP"){
    if (any(unlist(cpmSetSizeFail(mods)))){
      anyFail <- TRUE
    }
  }
  if (!anyFail){
    return(NULL)
  }

  nsizeclass <- length(mods)
  uniquemsgs <- NULL
  for (sci in 1:nsizeclass){
    newmsgs <- NULL
    if (type == "SE"){
      failedmods <- which(pkmSetFail(mods[[sci]]))
    }
    if (type == "CP"){
      failedmods <- which(cpmSetFail(mods[[sci]]))
    }
    nfailedmods <- length(failedmods)
    if (nfailedmods > 0){
      for(fmodi in 1:nfailedmods){
        newmsg <- mods[[sci]][[names(failedmods)[fmodi]]]
        newmsg <- gsub("Failed model fit: ", "", newmsg)
        newmsgs <- unique(c(newmsgs, newmsg))
      }
    }
    uniquemsgs <- unique(c(uniquemsgs, newmsgs))
  }
  paste0(
    "Some models were not successfully fit. Failed models were removed. ",
    paste(uniquemsgs, collapse = " ")
  )
}

#' @title Create the warning message text for small sample sizes
#'
#' @description Produces text for a notification for small sample sizes
#'
#' @param mods Set Size list of models
#'
#' @return small sample sizes warning text (if needed)
#'
#' @export
#'
msgSampleSize <- function(mods){
  cellCounts <- countCarcs(mods)
  minCellCount <- min(na.omit(cellCounts))
  if (minCellCount < 10){
    return(
      paste0("Small (< 10) sample sizes in some cells. ",
             "Consider simplifying the model; ",
             "parameter estimates may be unstable."
      )
    )
  }
  NULL
}

#' @title Create the warning message for a model run (if needed)
#'
#' @description Produces a notification for partial model failures if needed
#'
#' @param mods Set Size list of models
#'
#' @param type "SE" or "CP"
#'
#' @return a partial model warning (if needed)
#'
#' @export
#'
msgModWarning <- function(mods, type = "SE"){
  msg <- paste(msgModPartialFail(mods, type), msgSampleSize(mods), sep = " ")
  if (length(msg) > 0){
    return(showNotification(msg, type = "warning", duration = NULL))    
  }
  NULL
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