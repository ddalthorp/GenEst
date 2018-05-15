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
    return(data.frame(group = "all", CellNames = "all"))
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
      pred_i <- gl(npredLevels_i, reps[predi], labels = predLevels_i) 
      output[[predi]] <- pred_i
    }
  }
  names(output) <- predNames
  output$CellNames <- apply(output, 1, paste0, collapse = ".")
  return(output)
}