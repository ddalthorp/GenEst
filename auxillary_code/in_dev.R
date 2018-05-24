#' @title Estimate mortality 
#'
#' @description Given given fitted Searcher Efficiency and Carcass 
#'   Persistence models; Search Schedule, Density Weighted Proportion,
#'   and Carcass Observation data; and information about the fraction of the
#'   the facility that was surveyed. This version of the function works for a
#'   single size class
#'
#' @param nsim the number of simulation draws
#' @param data_CO Carcass Observation data
#' @param data_SS Search Schedule data
#' @param data_DWP Survey unit (rows) by size (columns) density weighted 
#'   proportion table 
#' @param frac fraction of facility (by units or by area) surveyed
#' @param model_SE Searcher Efficiency model (or list of models if there are 
#'   multiple size classes)
#' @param model_CP Carcass Persistence model (or list of models if there are 
#'   multiple size classes)
#' @param seed_SE seed for random draws of the SE model
#' @param seed_CP seed for random draws of the CP model
#' @param seed_ghat seed for random draws of the ghats
#' @param seed_M seed for the random draws of the Mhats
#' @param kFill value to fill in for missing k when not existing in the model
#' @param unitCol Column name for the unit indicator
#' @param dateFoundCol Column name for the date found data
#' @param dateSearchedCol Column name for the date searched data
#' @param DWPCol Column name for the DWP values in the DWP table when no
#'   size class is used
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'  are recorded. Optional.
#' @return list of SE parameters (pk), CP parameters (ab), ghat, Aj, and Mhat 
#' @examples NA
#' @export 
#'
estM <- function(nsim = 1, data_CO, data_SS, data_DWP, frac = 1,  
                 model_SE, model_CP, 
                 seed_SE = NULL, seed_CP = NULL, seed_ghat = NULL, 
                 seed_M = NULL, kFill = NULL,  
                 unitCol = "Unit", dateFoundCol = "DateFound", 
                 dateSearchedCol = "DateSearched", DWPCol = NULL,
                 sizeclassCol = NULL){

  SSCO <- prepSSCO(data_SS, data_CO, dateSearchedCol, dateFoundCol, unitCol)  
  data_CO <- SSCO$data_CO
  data_SS <- SSCO$data_SS
  c_outs <- SSCO$cleanout_carcasses

  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not in carcass data.")
    }
    sizeclasses <- as.character(unique(data_CO[ , sizeclassCol]))
    nsizeclass <- length(sizeclasses)

    if (!all(sizeclasses %in% DWPCol)){
      stop("not all size classes are present in the DWP columns provided.")
    }
  }

  DWP <- DWPbyCarcass(data_DWP, data_CO, data_SS, dateFoundCol, 
           dateSearchedCol, DWPCol, unitCol, sizeclassCol
         )

  est <- estghat(nsim, data_CO, data_SS, model_SE, model_CP, seed_SE,  
           seed_CP, seed_ghat, kFill, unitCol, dateFoundCol, 
           dateSearchedCol, sizeclassCol
         )

  gDWPf <- est$ghat * DWP * frac
  c_out <- which(apply(gDWPf, 1, sum) == 0)
  if (length(c_out) > 0){
    gDWPf <- gDWPf[-c_out, ]
  }
  n <- length(gDWPf)
  set.seed(seed_M)
  Mhat <- sum(rcbinom(n, 1 / gDWPf, gDWPf) - (Ecbinom(gDWPf) - 1))/(gDWPf)

  if (length(c_out) > 0){
    zeroes <- matrix(0, nrow = length(c_out), ncol = ncol(est$ghat))
    Mhat <- rbind(zeroes, Mhat)
  }
  out <- list(ghatAj$pk, ghatAj$ab, ghatAj$ghat, ghatAj$Aj, Mhat)
  names(out) <- c("pk", "ab", "ghat", "Aj", "Mhat")
  return(out)
}