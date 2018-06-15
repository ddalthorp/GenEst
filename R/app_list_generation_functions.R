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

  rv <- reactiveValues(

        data_SE = NULL, colNames_SE = NULL, colNames_SE_sel = NULL, 
        colNames_SE_nosel = NULL, obsCols_SE = NULL, 
        preds_SE = NULL, kFixedChoice = 0, kFixed = NULL,  
        predictors_SE = NULL, formula_p = NULL, formula_k = NULL, 
        mods_SE = NULL, modNames_SE = NULL, modNames_SEp = NULL, 
        modNames_SEk = NULL, modTab_SE = NULL, AICcTab_SE = NULL,
        modOrder_SE = NULL, modSet_SE = NULL, best_SE = NULL, 
        figH_SE = 800, figW_SE = 800,  
        sizeclasses_SE = NULL, tabfig_SEpk = NULL,

        data_CP = NULL, colNames_CP = NULL, colNames_CP_sel = NULL, 
        colNames_CP_nosel = NULL, ltp = NULL, fta = NULL, 
        preds_CP = NULL, dists = NULL, mods_CP = NULL, predictors_CP = NULL,
        formula_l = NULL, formula_s = NULL, sizeclasses_CP = NULL, 
        AICcTab_CP = NULL,  modOrder_CP = NULL, modNames_CP = NULL, 
        modNames_CPdist = NULL, modNames_CPl = NULL, modNames_CPs = NULL, 
        modSet_CP = NULL, best_CP = NULL, modTab_CP = NULL, 
        figH_CP = 700, figW_CP = 800, sizeclasses_CP = NULL,  
        CPdls = NULL, tabfig_CPdlsfig = NULL, tabfig_CPdlstab = NULL, 

        data_SS = NULL, colNames_SS = NULL, 

        SS = seq(0, 364, 7), SStext = paste(seq(0, 364, 7), collapse = ", "), 
        SStemp = NULL, avgSI = NULL, 
        gSearchInterval = NULL, gSearchMax = NULL, 

        data_DWP = NULL, colNames_DWP = NULL,
        data_CO = NULL, colNames_CO = NULL, colNames_COdates = NULL, 

        sizeclassCol = NULL, sizeclasses = NULL, sizeclass = NULL,
        colNames_all = NULL, nsizeclasses = NULL,

        kFill = NULL, kFill_g = NULL,  
        dateFoundCol = NULL, sizeclassCol_M = NULL,
        SEmodToUse = NULL, CPmodToUse = NULL, M = NULL,
        models_SE = NULL, models_CP = NULL, DWPCol = NULL, 
        CL = 0.9, nsim = 1000, SEmodToUse_g = NULL, CPmodToUse_g = NULL,
        gGeneric = NULL,
        split_CO = NULL, split_SS = NULL,
        nsplit_CO = NULL, nsplit_SS = NULL, Msplit = NULL,
        figH_M = 600, figW_M = 800, figH_g = 600, figW_g = 800
      )
  return(rv)
}
