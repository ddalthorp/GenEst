#' @title Create the main reactive value list for GenEst 
#'
#' @description Create a list of reactive values as used across the components
#'   of the GenEst application
#'
#' @return a reactive values list
#'
#' @export
#'
createReactiveValues <- function(){
  reactiveValues(
    data_SE = NULL, data_CP = NULL, data_SS = NULL, data_DWP = NULL, 
    data_CO = NULL,
    colNames_SE = NULL, colNames_SE_sel = NULL, colNames_SE_nosel = NULL, 
    colNames_CP = NULL, colNames_CP_sel = NULL, colNames_CP_nosel = NULL,    
    colNames_SS = NULL,
    colNames_DWP = NULL,
    colNames_CO = NULL, colNames_COdates = NULL,
    colNames_all = NULL, 

    nsim = 1000, CL = 0.95, sizeclassCol = NULL,

    obsCols_SE = NULL, preds_SE = NULL, predictors_SE = NULL, 
    formula_p = NULL, formula_k = NULL, kFixedChoice = NULL, kFixed = NULL 
    

  )
}