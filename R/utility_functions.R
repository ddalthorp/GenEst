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
combine_preds <- function(preds, data){

  n_preds <- length(preds)
  if(n_preds == 0){
    return(data.frame(group = "all", CellNames = "all"))
  }else{
    if(any(is.na(match(preds, names(data))))) {
        stop("At least one predictor missing from data.")
    }
    var_names <- preds
    var_labels <- list()
    var_n_levels <- list()
    for(i in 1:n_preds){
      var_labels[[i]] <- levels(as.factor(data[[var_names[i]]]))
      var_n_levels[[i]] <- length(var_labels[[i]])
    }
    reps <- cumprod(var_n_levels)[n_preds:1]
    reps_1 <- reps[1]
    var_1_n_levels <- var_n_levels[[1]]
    var_1_labels <- var_labels[[1]]
    var_1 <- gl(var_1_n_levels, 1, length = reps_1, labels = var_1_labels)
    output <- data.frame(var_1)
    if(n_preds > 1){
      for(j in 2:n_preds){
        var_j_n_levels <- var_n_levels[[j]]
        var_j_labels <- var_labels[[j]]
        var_j <- gl(var_j_n_levels, 1, length = reps_1, labels = var_j_labels) 
        output[[j]] <- var_j

      }
    }
  }
  names(output) <- var_names
  output$CellNames <- apply(output, 1, paste0, collapse = ".")
  return(output)
}

