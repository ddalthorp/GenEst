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
#' @param predictors Names of predictor variables to include.
#' @param data Searcher efficiency or carcass persistence data.
#' @return Factor combination table.
#' @examples
#' NA
#' @export 
#'
combine_preds <- function(preds, data){

  n_preds <- length(preds)
  if(n_preds == 0){
    return(data.frame(group = "all", CellNames = "all"))
  }else{
      if(any(is.na(match(preds, names(data))))) {
        stop("At least one predictor missing from data.")
      }
      varNames <- preds
      varLabels <- list()
      varNlevels <- list()
      for(i in 1:n_preds){
        varLabels[[i]] <- levels(as.factor(data[[varNames[i]]]))
        varNlevels[[i]] <- length(varLabels[[i]])
      }
      reps <- cumprod(varNlevels)[n_preds:1] 
      egDat <- data.frame(var1 = gl(varNlevels[[1]], 1, length = reps[1], 
                          labels = varLabels[[1]]))
      if(n_preds > 1){
        for(j in 2:n_preds){
          egDat[[paste("var", j, collapse = NULL)]] <- 
             gl(varNlevels[[j]], reps[j], length = reps[1], 
                labels = varLabels[[j]])
        }
      }
    }
  names(egDat) <- varNames
  egDat$CellNames <- apply(egDat, 1, paste0, collapse=".")
  return(egDat)
}

