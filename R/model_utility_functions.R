#' @title Combine predictors 
#'
#' @description Create the predictor combination table for a searcher 
#'  efficiency or carcass persistence analysis.
#' 
#' @param preds Names of predictor variables to include.
#' 
#' @param data Searcher efficiency or carcass persistence data.
#' 
#' @return Predictor combination table.
#' 
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
        pred_i <- gl(npredLevels_i, reps[predi], length = reps[1],
                    labels = predLevels_i
                  ) 
        output[[predi]] <- pred_i
      }
    }
  }
  names(output) <- predNames
  output$CellNames <- apply(output, 1, paste0, collapse = ".")
  return(output)
}

#' @title Check for model components
#'
#' @description Check if all component terms and interactions are 
#'   included in a formula. Terms are automatically alphabatized within 
#'   interactions.
#'
#' @param formula A formula object
#'
#' @return a logical regarding complete set of terms and interactions
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


#' @title Match cells between models.
#'
#' @description Match cells between a (potentially) reduced and the full model
#'   set. Terms are automatically alphabatized to insure matching across 
#'   models.
#'
#' @param specific specific model compared against the full set
#'
#' @param modelSet full model set to compare to the specific
#'
#' @return vector of length equal to the number of cells in the full model, 
#'   each element containing the related cell name in the reduced model
#'
#' @export 
#'
matchCells <- function(specific, modelSet){

  cells_spec <- specific$cells
  cells_set <- modelSetCells(modelSet)
  ncell_set <- nrow(cells_set)

  predictors_spec <- specific$predictors
  predictors_set <- modelSetPredictors(modelSet)

  predictorsMatch_spec <- which(predictors_spec %in% predictors_set)
  predictorsMatch_set <- which(predictors_set %in% predictors_spec)
 
  if (!all(predictors_spec %in% predictors_set)){
    stop("Complete model set does not include specific model's terms.")
  }

  if (length(predictorsMatch_spec) == 0){
    out <- as.character(rep(cells_spec$CellNames, ncell_set))
  } else{

    matchedPredictors_set <- sort(colnames(cells_set)[predictorsMatch_set])
    matchedPredictors_spec <- sort(colnames(cells_spec)[predictorsMatch_spec])
    cellSubSet_spec <- data.frame(cells_spec[ , matchedPredictors_spec])
    cellSubSet_set <- data.frame(cells_set[ , matchedPredictors_set])
    pasteCellNames_spec <- apply(cellSubSet_spec, 1, paste, collapse = ".")   
    pasteCellNames_set <- apply(cellSubSet_set, 1, paste, collapse = ".")   

    out <- rep(NA, ncell_set)
    for (celli in 1:ncell_set){
      cellMatch <- which(pasteCellNames_spec == pasteCellNames_set[celli])
      out[celli] <- cells_spec$CellNames[cellMatch]
    }
  }
  return(out)
}

#' @title Combine predictors across models
#'
#' @description Create the factor combination table for a set of searcher 
#'  efficiency and carcass persistence analyses.
#'
#' @param data_CP Carcass persistence data.
#'
#' @param data_SE Searcher efficiency data.
#'
#' @param preds_CP Carcass persistence predictor variables.
#'
#' @param preds_SE Searcher efficiency predictor variables.
#'
#' @return Factor combination table.
#' 
#' @export 
#'
combinePredsAcrossModels <- function(preds_CP, preds_SE, data_CP, data_SE){

  preds <- unique(c(preds_SE, preds_CP))
  npred <- length(preds)
  if (npred == 0){
    preds <- data.frame(group = "all", CellNames = "all", 
               CellNames_SE = "all", CellNames_CP = "all"
             )
    return(preds)
  }
  if(any(is.na(match(preds_SE, names(data_SE))))) {
    stop("At least one Searcher Efficiency predictor missing from data.")
  }
  if(any(is.na(match(preds_CP, names(data_CP))))) {
    stop("At least one Carcass Persistence predictor missing from data.")
  }
  
  predNames <- preds
  predLevels <- list()
  npredLevels <- list()
  for(predi in 1:npred){
    predLevels_SE <- levels(as.factor(data_SE[[predNames[predi]]]))
    predLevels_CP <- levels(as.factor(data_CP[[predNames[predi]]]))
    npredLevels_SE <- length(predLevels_SE)
    npredLevels_CP <- length(predLevels_CP)
    if (npredLevels_SE > 0 & npredLevels_CP > 0){
      if (!(all(predLevels_SE %in% predLevels_CP) & 
            all(predLevels_CP %in% predLevels_SE))){
        stop("Identical factor has different levels in SE and CP data.")
      }
    }

    predLevels[[predi]] <- unique(c(predLevels_SE, predLevels_CP))
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
      pred_i <- gl(npredLevels_i, reps[predi], length = reps[1],
                  labels = predLevels_i
                ) 
      output[ , predi] <- pred_i
    }
  }
  names(output) <- predNames
  CellNames <- apply(output, 1, paste0, collapse = ".")
  output_SE <- data.frame(output[ , preds_SE])
  CellNames_SE <- apply(output_SE, 1, paste0, collapse = ".")
  output_CP <- data.frame(output[ , preds_CP])
  CellNames_CP <- apply(output_CP, 1, paste0, collapse = ".")
  output$CellNames <- CellNames
  if (all(CellNames_SE == "")){
    CellNames_SE <- "all"
  }
  if (all(CellNames_CP == "")){
    CellNames_CP <- "all"
  }
  output$CellNames_SE <- CellNames_SE
  output$CellNames_CP <- CellNames_CP
  return(output)
}

#' @title Trim a Model-Set-Size Complex to a Single Model Per Size
#'
#' @description Select a single model from each size class (based on the model
#'   names).
#'
#' @param modSetSize modSetSize complex (cpm or pkm)
#'
#' @param mods named (according to size classes) vector of model names to use
#'
#' @return modSetSize reduced to a single model per size class
#'
#' @export
#'
trimSetSize <- function(modSetSize, mods){
  sizeclasses <- names(modSetSize)
  nsizeclass <- length(sizeclasses)
  models <- list()
  for (sz in sizeclasses){
    if (! (mods[[sz]] %in% names(modSetSize[[sz]]))){
      stop("requested model for ", sz, " (",  mods[[sz]] , ") not found")
    }
    models[[sz]] <- modSetSize[[sz]][[mods[[sz]]]]
  }
  return(models)
}


#' @title Determine the predictors from each model in a model set
#'
#' @description Determine the predictors from each model in a model set
#'
#' @param modelSet model set
#'
#' @return List of the predictors from each model in a model set
#'
#' @export
#'
modelSetModelPredictors <- function(modelSet){
  nmod <- length(modelSet)
  out <- vector("list", length = nmod)
  for (modi in 1:nmod){
     if (!(grepl("Failed model fit", modelSet[[modi]][1]))){
       if ( length(modelSet[[modi]]$predictors) > 0){
         out[[modi]] <- modelSet[[modi]]$predictors
       }
     }
  }
  names(out) <- names(modelSet)
  return(out)
}

#' @title Determine the predictors for a whole model set
#'
#' @description Determine the predictors for a whole model set
#'
#' @param modelSet model set
#'
#' @return vector of the predictors from a model set
#'
#' @export
#'
modelSetPredictors <- function(modelSet){
  unique(unlist(modelSetModelPredictors(modelSet)))
}

#' @title Determine the cells from each model in a model set
#'
#' @description Determine the cells from each model in a model set
#'
#' @param modelSet model set
#'
#' @return List of the cells from each model in a model set
#'
#' @export
#'
modelSetModelCells <- function(modelSet){
  nmod <- length(modelSet)
  out <- vector("list", length = nmod)
  for (modi in 1:nmod){
    if (!(grepl("Failed model fit", modelSet[[modi]][1]))){
      out[[modi]] <- modelSet[[modi]]$cells
    }
  }
  names(out) <- names(modelSet)
  return(out)
}

#' @title Determine the cell table for a full model set
#'
#' @description Determine the cell table for a full model set
#'
#' @param modelSet model set
#'
#' @return cell table for the model set
#'
#' @export
#'
modelSetCells <- function(modelSet){
  modelCells <- modelSetModelCells(modelSet)
  modelPreds <- modelSetPredictors(modelSet)

  if (is.null(modelPreds)){
    out <- data.frame("group" = "all", "CellNames" = "all")
    return(out)
  }

  nmod <- length(modelSet)
  npred <- length(modelPreds)
  predLevels <- vector("list", npred)

  for (modi in 1:nmod){
    modiCells <- modelCells[[modi]]
    for (predi in 1:npred){
      if (modelPreds[predi] %in% colnames(modiCells)){
        existing <- predLevels[[predi]]
        new <- as.character(modiCells[ , modelPreds[predi]])
        predLevels[[predi]] <- unique(c(existing, new))
      }
    }
  }
  out <- expand.grid(predLevels)
  colnames(out) <- modelPreds
  out$CellNames <- apply(out, 1, paste, collapse = ".")
  return(out)
}


#' @title Return the model with the greatest log-likelihood
#'
#' @description  Compares all fitted models in a list and returns the model
#'  with the greatest log-likelihood
#'
#' @param modelSet a list of fitted models with a \code{loglik} element. 
#'  Models may be \code{pkm}, \code{cpm}, \code{survreg} objects or any 
#'  objects with a \code{loglik} component.
#'
#' @return The model object with the greatest log-likelihood among
#'  the models in \code{modelSet}
#'
#' @export
#'
refMod <- function(modelSet){
  llvec <- sapply(modelSet, "[[", "loglik")
  out <- modelSet[[which(llvec == max(llvec))]]
  return(out)
}

