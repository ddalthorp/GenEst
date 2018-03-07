#' Compute the logit.
#' 
#' @param x A probability (between 0 and 1, inclusive).
#' @return The logit of \code{x}.
#' @examples
#' logit(0.5)
#' @export 
#'
logit <- function(x) {
  log(x / (1 - x))
}

#' Compute the anti-logit.
#' 
#' @param x A number.
#' @return The anti-logit of \code{x}.
#' @examples
#' alogit(0)
#' @export 
#'
alogit <- function(x) {
  1 / (1 + exp(-x))
}
  
#' Create the predictor combination table for a searcher efficiency or carcass
#'  persistence analysis.
#' 
#' @param preds Names of predictor variables to include.
#' @param data Searcher efficiency or carcass persistence data.
#' @return Predictor combination table.
#' @examples
#' NA
#' @export 
#'
combinePreds <- function(preds, data){

  npred <- length(preds)
  if (npred == 0){
    return(data.frame(group = "all", CellNames = "all"))
  }else{
    if(any(is.na(match(preds, names(data))))) {
        stop("At least one predictor missing from data.")
    }
    predNames <- preds
    predLevels <- list()
    npredLevels <- list()
    for(predi in 1:npred){
      predLevels[[predi]] <- levels(as.factor(data[[predNames[predi]]]))
      npredLevels[[predi]] <- length(predLevels[[predi]])
    }
    reps <- cumprod(npredLevels)[npred:1]
    npredLevels_1 <- npredLevels[[1]]
    predLevels_1 <- predLevels[[1]]
    pred_1 <- gl(npredLevels_1, 1, length = reps[1], labels = predLevels_1)
    output <- data.frame(pred_1)
    if (npred > 1){
      for (predi in 2:npred){
        npredLevels_i <- npredLevels[[predi]]
        predLevels_i <- predLevels[[predi]]
        pred_i <- gl(npredLevels_i, reps[predi], labels = predLevels_i) 
        output[[predi]] <- pred_i
      }
    }
  }
  names(output) <- predNames
  output$CellNames <- apply(output, 1, paste0, collapse = ".")
  return(output)
}


#' Check if all component terms and interactions are included in a formula
#'
#'
#' @param formula_in A formula object
#'
#' Terms are automatically alphabatized within interactions.
#'
#' @export 
#'
checkComponents <- function(formula){
 
  termOrders <- attr(terms(formula), "order")
  termNames <- attr(terms(formula), "term.labels")
  nterm <- length(termNames)

  if(nterm == 0){
    return(TRUE)
  }
  if(max(termOrders) == 1){
    return(TRUE)
  }

  termNamesAlph <- rep(NA, nterm)
  for (termi in 1:nterm){
    temp <- strsplit(termNames[termi], ":")[[1]]
    temp <- sort(temp)
    termNamesAlph[termi] <- paste(temp, collapse = ":")
  }
  termOrdersSort <- termOrders[order(termOrders, termNamesAlph)]
  termNamesSort <- termNamesAlph[order(termOrders, termNamesAlph)]

  ixns <- which(termOrdersSort > 1)
  nixn <- length(ixns)
  ixnOrders <- termOrdersSort[ixns]
  allThere <- rep(FALSE, nixn)

  for (ixni in 1:nixn){
    ixnName <- termNamesSort[ixns[ixni]]
    ixnParts <- strsplit(ixnName, ":")[[1]]
    ixnSubOrder <- ixnOrders[ixni] - 1
    toCheck <- NULL
   
    for (orderi in 1:ixnSubOrder){
      combPartsOptions <- combn(ixnParts, orderi)
      addToCheck <- apply(combPartsOptions, 2, paste, collapse = ":")
      toCheck <- c(toCheck, addToCheck)

    }

    allThere[ixni] <- all(toCheck %in% termNamesAlph)
  }
  output <- all(allThere)
  return(output)
}



