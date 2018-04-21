# maybe not needed?

  predsTable <- combinePredsAcrossModels(preds_SE, preds_CP, data_SE, data_CP)



# combine the predictors across the SE and CP models

combinePredsAcrossModels <- function(preds_SE, preds_CP, data_SE, data_CP){

  preds <- sort(unique(c(preds_SE, preds_CP)), decreasing = TRUE)
  npred <- length(preds)
  if (npred == 0){
    return(data.frame(group = "all", CellNames = "all"))
  }else{
    dataCols <- unique(c(names(data_SE), names(data_CP)))
    if (any(is.na(match(preds, dataCols)))) {
        stop("At least one predictor missing from data.")
    }
    predNames <- preds
    predLevels <- list()
    npredLevels <- list()
    for(predi in 1:npred){
      levels_SE <- levels(as.factor(data_SE[[predNames[predi]]]))
      levels_CP <- levels(as.factor(data_CP[[predNames[predi]]]))
      predLevels[[predi]] <- unique(c(levels_SE, levels_CP))
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
