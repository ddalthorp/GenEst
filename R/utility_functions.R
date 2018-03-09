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
#' Terms are automatically alphabatized within interactions.
#'
#' @param formula_in A formula object
#'
#' @return a logical regarding complete set of terms and interactions
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

#' Match cells between a (potentially) reduced and full model
#'
#' Terms are automatically alphabatized to insure matching across models.
#'
#' @param model_spec specific model compared against the full
#' @param model_full full model to compare to the specific
#'
#' @return vector of length equal to the number of cells in the full model, 
#'  each element containing the related cell name in the reduced model
#' @export 
#'
matchCells <- function(model_spec, model_full){

  cells_spec <- model_spec$cells
  cells_full <- model_full$cells
  ncell_full <- model_full$ncell

  predictors_spec <- model_spec$predictors
  predictors_full <- model_full$predictors
  predictorsMatch_spec <- which(predictors_spec %in% predictors_full)
  predictorsMatch_full <- which(predictors_full %in% predictors_spec)
 
  if (!all(predictors_spec %in% predictors_full)){
    stop("Full model does not include specific model's terms.")
  }

  if (length(predictorsMatch_spec) == 0){
    out <- as.character(rep(cells_spec$CellNames, ncell_full))
  }else{

    matchedPredictors_full <- sort(colnames(cells_full)[predictorsMatch_full])
    matchedPredictors_spec <- sort(colnames(cells_spec)[predictorsMatch_spec])
    cellSubSet_spec <- data.frame(cells_spec[ , matchedPredictors_spec])
    cellSubSet_full <- data.frame(cells_full[ , matchedPredictors_full])
    pasteCellNames_spec <- apply(cellSubSet_spec, 1, paste, collapse = ".")   
    pasteCellNames_full <- apply(cellSubSet_full, 1, paste, collapse = ".")   

    out <- rep(NA, ncell_full)
    for (celli in 1:ncell_full){
      cellMatch <- which(pasteCellNames_spec == pasteCellNames_full[celli])
      out[celli] <- cells_spec$CellNames[cellMatch]
    }
  }
  return(out)
}

