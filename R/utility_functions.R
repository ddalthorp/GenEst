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

#' Check if all component terms and interactions are included in a formula
#'
#' Terms are automatically alphabatized within interactions.
#'
#' @param formula A formula object
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

#' Length of non-missing values in a vector
#'
#' @param x vector of values
#' @return integer of how many non-NA values in x
#'
#' @export
#'
trueLength <- function(x){
 length(which(!is.na(x)))
}

#' Check if a vector is never decreasing
#'
#' @param x vector of values
#' @param tiesOK logical if ties are ok or not
#' @param na.rm logical if NAs are to be removed or not
#' @return logical value
#'
#' @export
#'
isNeverDecreasing <- function(x, tiesOK = TRUE, na.rm = TRUE){
  if (na.rm == TRUE){
    x <- na.omit(x)
  }
  diffs <- diff(x)
  if (tiesOK == TRUE){
    out <- all(diffs >= 0)
  }
  if (tiesOK == FALSE){
    out <- all(diffs > 0)
  }
  return(out)
}

#' Convert calendar date to day from reference
#'
#' @param date date to convert
#' @param ref reference date
#' @return converted days from reference
#' @examples NA
#' @export 
#'
dateToDay <- function(date, ref = NULL){
  if (is.numeric(date)){
    return(date)
  }
  if (is.null(ref)){
    ref <- date
  }
  day <- as.numeric(difftime(date, ref, units = "days"))
  return(day)
}

#' Converts the standard Excel date format (m/d/YYYY) to YYYY-mm-dd
#'
#' @description only transforms m/d/Y dates, returns untransformed dates that
#'   were already YYYY-mm-dd
#'
#' @param x date(s) to convert
#' @return converted dates
#' @examples NA
#' @export 
#'
yyyymmdd <- function(x){
 
  slashCheck <- all(grepl("\\/", x))
  numerals <- strsplit(as.character(x), "/")[[1]]
  numeralsCheck <- nchar(numerals[1]) %in% 1:2 & nchar(numerals[2]) %in% 1:2 & 
                     nchar(numerals[3]) == 4  
  if (slashCheck & numeralsCheck ){
    x <- as.Date(as.character(x), format = "%m/%d/%Y")
  }
  return(x)
}

#' Calculates the expected value of a continuous binomial
#'
#' @description uses internal-only data
#'
#' @param prob probability
#' @return mean 
#' @examples NA
#' @export 
#'
Ecbinom <- function(prob){
  X <- EcbinomXY$X
  Y <- EcbinomXY$Y
  interp <- approxfun(x = X, y = Y)
  interp(prob)
}

#' Create the factor combination table for a set of searcher efficiency 
#'  and carcass persistence analyses.
#' @param data_CP Carcass persistence data.
#' @param data_SE Seacher efficiency data.
#' @param preds_CP Carcass persistence predictor variables.
#' @param preds_SE Seacher efficiency predictor variables.
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

#' Trim a Model-Set-Size Complex to a Single Model Per Size
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
