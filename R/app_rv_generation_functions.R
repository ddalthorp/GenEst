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

    nsim = 1000, CL = 0.95, sizeclassCol = NULL, sizeclasses = NULL,
    sizeclass = NULL, 

    obsCols_SE = NULL, preds_SE = NULL, predictors_SE = NULL, 
    formula_p = NULL, formula_k = NULL, kFixedChoice = NULL, kFixed = NULL, 
    mods_SE = NULL, sizeclasses_SE = NULL, outSEpk = NULL, 
    AICcTab_SE = NULL, modOrder_SE = NULL, modNames_SE = NULL,
    modNames_SEp = NULL, modNames_SEk = NULL, modSet_SE = NULL,
    best_SE = NULL, modTab_SE = NULL, modTabPretty_SE = NULL,
    modTabDL_SE = NULL, figH_SE = 800, figW_SE = 800,

    ltp = NULL, fta = NULL, preds_CP = NULL, dists = NULL, 
    predictors_CP = NULL, formula_l = NULL, formula_s = NULL, 
    mods_CP = NULL, CPdls = NULL, outCPdlsfig = NULL, outCPdlstab = NULL, 
    sizeclasses_CP = NULL, AICcTab_CP = NULL, modOrder_CP = NULL, 
    modNames_CP = NULL, modNames_CPdist = NULL, modNames_CPl = NULL, 
    modNames_CPs = NULL, modSet_CP = NULL, best_CP = NULL, 
    modTab_CP = NULL, modTabPretty_CP = NULL, modTabDL_CP = NULL, 
    figH_CP = 700, figW_CP = 800,

    SS = seq(0, 364, 7), SStext = paste(seq(0, 364, 7), collapse = ", ")

  )
}