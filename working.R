
#' Fit all possible searcher efficiency models across all size classes.
#'
#' Function inputs generally follow \code{pkm_set} and \code{pkm} but with an 
#'  additional size column input and calculation of the set of pkm models for
#'  each of the size classes
#'
#' @param formula_p Formula for p; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 
#' 'Details'.
#'
#' @param data Dataframe with results from searcher efficiency trials and any
#' covariates included in \code{formula_p} or {formula_k} (required).
#'
#' @param formula_k Formula for k; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 
#; 'Details'.
#'
#' @param obsCols Vector of names of columns in \code{data} where results 
#' for each search occasion are stored (optional). If no \code{obsCols} are 
#' provided, \code{pkm} uses as \code{obsCols} all columns with names that 
#' begin with an \code{'s'} or \code{'S'} and end with a number, e.g., 's1',
#' 's2', 's3', etc. This option is included as a convenience for the user, 
#' but care must be taken that other data are not stored in columns with 
#' names matching that pattern. Alternatively, \code{obsCols} may be 
#' entered as a vector of names, like \code{c('s1', 's2', 's3')}, 
#' \code{paste0('s', 1:3)}, or \code{c('initialSearch', 'anotherSearch', 
#' 'lastSearch')}.
#'
#' @param kFixed Parameter for user-specified \code{k} value (optional). If a
#' value is provided, \code{formula_k} is ignored and the model is fit under 
#' the assumption that the \code{k} parameter is fixed and known to be
#' \code{fix_k}.
#'
#' @param init_k Initial value used for \code{k} in the optimization.
#'
#' @param sizeclassCol Name of colum in \code{data} where the size classes
#'  are recorded
#'
#' @return \code{pkm_set} returns a list of objects, each of which is a list
#'  of \code{pkm}" outputs (each corresponding to the fit of a specific model
#'  within the set of \code{pkm} models fit for the given size class), that is
#'  of length equal to the total number of size classes
#'
pkmSetSize <- function(formula_p, formula_k = NULL, data, obsCols = NULL, 
                sizeclassCol = NULL, kFixed = NULL, kInit = 0.7){

  if (length(sizeclassCol) == 0){
    message("No size class provided, function run as if pkm_set")
    output <- pkm_set(formula_p, formula_k, data, obsCols, kFixed, kInit)
    return(output)
  }

  sizeclassData <- as.character(data[ , sizeclassCol])
  sizeclasses <- unique(sizeclassData)
  nsizeclasses <- length(sizeclasses)

  out <- vector("list", nsizeclasses)
  names(out) <- sizeclasses
  for (sci in 1:nsizeclasses){
    sizeclassMatch <- which(sizeclassData == sizeclasses[sci])
    data_i <- data[sizeclassMatch, ]
    out[[sci]] <- pkmSet(formula_p, formula_k, data_i, obsCols, kFixed, kInit)
  }

  return(out)
}

