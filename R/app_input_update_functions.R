#' @title Update the inputs when everything is cleared out
#'
#' @description Update the inputs when everything is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_clear_all <- function(rv, session){
  update_input_data_SE_clear(rv, session)
  update_input_data_CP_clear(rv, session)
  update_input_data_DWP_clear(rv, session)
  update_input_data_SS_clear(rv, session)
  update_input_data_CO_clear(rv, session)
}

#' @title Update the inputs when SE data are read in
#'
#' @description Update the inputs when the SE data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_SE <- function(rv, session){
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE_preds)
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE_obs)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Searcher Efficiency")
}

#' @title Update the inputs when SE data are cleared out
#'
#' @description Update the inputs when the SE data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_SE_clear <- function(rv, session){

  reset("file_SE")
  reset("preds_SE")
  reset("obsCols_SE")
  reset("outSEp")
  reset("outSEk")
  reset("outsizeclassSE")
  reset("kFill")
  reset("DWPCol")
  reset("split_SS")
  reset("split_CO")
  reset("modelChoices_SE1")
  reset("outsizeclassg")

  updateSelectizeInput(session, "preds_SE", choices = "")
  updateSelectizeInput(session, "obsCols_SE", choices = "")
  scc <- rv$colNames_size
  if (is.null(scc)){
    scc <- ""
  }
  scs <- rv$sizeclassCol
  if (is.null(scc)){
    scs <- ""
  }
  updateSelectizeInput(session, "sizeclassCol", choices = scc, selected = scs)
  updateSelectizeInput(session, "modelChoices_SE1", choices = "")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
  updateSelectizeInput(session, "outSEp", choices = "")
  updateSelectizeInput(session, "outSEk", choices = "")
  updateSelectizeInput(session, "outsizeclassSE", choices = "")
  updateSelectizeInput(session, "outsizeclassg", choices = "")

}


#' @title Update the inputs when CP data are read in
#'
#' @description Update the inputs when the CP data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_CP <- function(rv, session){
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_preds)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_CP_ltp)
  updateSelectizeInput(session, "fta", choices = rv$colNames_CP_fta)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Persistence")
}

#' @title Update the inputs when CP data are cleared out
#'
#' @description Update the inputs when the CP data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_CP_clear <- function(rv, session){

  reset("file_CP")
  reset("preds_CP")
  reset("ltp")
  reset("fta")
  reset("modelChoices_CP1")
  reset("outCPl")
  reset("outCPs")
  reset("outCPdist")
  reset("outsizeclassCP")
  updateSelectizeInput(session, "preds_CP", choices = "")
  updateSelectizeInput(session, "ltp", choices = "")
  updateSelectizeInput(session, "fta", choices = "")
  scc <- rv$colNames_size
  if (is.null(scc)){
    scc <- ""
  }
  scs <- rv$sizeclassCol
  if (is.null(scc)){
    scs <- ""
  }
  updateSelectizeInput(session, "sizeclassCol", choices = scc, selected = scs)
  updateSelectizeInput(session, "modelChoices_CP1", choices = "")
  updateSelectizeInput(session, "outCPl", choices = "")
  updateSelectizeInput(session, "outCPs", choices = "")
  updateSelectizeInput(session, "outCPdist", choices = "")
  updateSelectizeInput(session, "outsizeclassCP", choices = "")

  reset("split_SS")
  reset("split_CO")
  reset("outsizeclassg")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
  updateSelectizeInput(session, "outsizeclassg", choices = "")

}

#' @title Update the inputs when SS data are read in
#'
#' @description Update the inputs when the SS data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_SS <- function(rv, session){
  updateTabsetPanel(session, "LoadedDataViz", "Search Schedule")
}

#' @title Update the inputs when SS data are cleared out
#'
#' @description Update the inputs when the SS data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_SS_clear <- function(rv, session){
  reset("file_SS")
  reset("gSearchInterval")
  reset("gSearchMax")
  reset("split_SS")
  reset("split_CO")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
}

#' @title Update the inputs when DWP data are read in
#'
#' @description Update the inputs when the DWP data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_DWP <- function(rv, session){
  updateSelectizeInput(session, "DWPCol", choices = rv$colNames_DWP)
  if (length(rv$colNames_DWP) == 1){
    updateSelectizeInput(session, "DWPCol", selected = rv$colNames_DWP)
  }
  if (rv$nsizeclasses > 1){
    updateSelectizeInput(session, "DWPCol", selected = rv$colNames_DWP[1])
  }
  updateTabsetPanel(session, "LoadedDataViz", "Density Weighted Proportion")
}

#' @title Update the inputs when DWP data are cleared out
#'
#' @description Update the inputs when the DWP data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_DWP_clear <- function(rv, session){
  reset("file_DWP")
  reset("DWPCol")
  updateSelectizeInput(session, "DWPCol", choices = "")
  reset("split_SS")
  reset("split_CO")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
}

#' @title Update the inputs when CO data are read in
#'
#' @description Update the inputs when the CO data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_CO <- function(rv, session){
  updateSelectizeInput(session, "dateFoundCol", choices = rv$colNames_COdates)
  if (length(rv$colNames_COdates) == 1){
    updateSelectizeInput(session, "dateFoundCol", 
      choices = rv$colNames_COdates, selected = rv$colNames_COdates
    )
  }
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")
}

#' @title Update the inputs when CO data are cleared out
#'
#' @description Update the inputs when the CO data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_CO_clear <- function(rv, session){

  reset("file_CO")
  reset("dateFoundCol")

  scc <- rv$colNames_size
  if (is.null(scc)){
    scc <- ""
  }
  scs <- rv$sizeclassCol
  if (is.null(scc)){
    scs <- ""
  }
  updateSelectizeInput(session, "sizeclassCol", choices = scc, selected = scs)
  updateSelectizeInput(session, "dateFoundCol", choices = "")
  reset("split_SS")
  reset("split_CO")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
}

#' @title Update the inputs when the size class column is selected
#'
#' @description Update the inputs when the SE data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param session session
#'
#' @export
#'
update_input_sizeclassCol <- function(rv, input, session){
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE_preds,
    selected = input$preds_SE)
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE_obs,
    selected = input$obsCols_SE)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_CP_nosel,
    selected = input$ltp)
  updateSelectizeInput(session, "fta", choices = rv$colNames_CP_nosel,
    selected = input$fta)
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_preds,
    selected = input$preds_CP)
  updateSelectizeInput(session, "DWPCol", choices = rv$colNames_DWP,
    selected = rv$DWPCol)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
}

#' @title Update the predictor options when SE observation columns are 
#'   selected
#'
#' @description Update the inputs when the SE observation columns are selected
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_cols_SE_obs <- function(rv, session){
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE_preds,
      selected = rv$preds_SE)
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE_obs,
      selected = rv$obsCols_SE)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
}

#' @title Update the observation options when SE predictor columns are 
#'   selected
#'
#' @description Update the inputs when the SE predictor columns are selected
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_cols_SE_preds <- function(rv, session){
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE_obs,
      selected = rv$obsCols_SE)
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE_preds,
      selected = rv$preds_SE)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
}

#' @title Update the predictor options and First Time Absent options when the 
#'   Last Time Present column is selected
#'
#' @description Update the inputs when the Last Time Present column is 
#'   selected
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_cols_ltp <- function(rv, session){
  updateSelectizeInput(session, "fta", choices = rv$colNames_fta,
      selected = rv$fta)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_ltp,
      selected = rv$ltp)
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_preds,
      selected = rv$preds_CP)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
}

#' @title Update the predictor options and Last Time Present options when the 
#'   First Time Absent column is selected
#'
#' @description Update the inputs when the First Time Absent column is 
#'   selected
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_cols_fta <- function(rv, session){
  updateSelectizeInput(session, "fta", choices = rv$colNames_fta,
      selected = rv$fta)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_ltp,
      selected = rv$ltp)
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_preds,
      selected = rv$preds_CP)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
}

#' @title Update the observation options when CP predictor columns are 
#'   selected
#'
#' @description Update the inputs when the CP predictor columns are selected
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_cols_CP_preds <- function(rv, session){
  updateSelectizeInput(session, "fta", choices = rv$colNames_fta,
      selected = rv$fta)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_ltp,
      selected = rv$ltp)
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_preds,
      selected = rv$preds_CP)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_size,
    selected = rv$sizeclassCol
  )
}

#' @title Update the SE output dropdown selections when the model is run
#'
#' @description Update the SE output dropdown selections when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_SE <- function(rv, session){
  updateTabsetPanel(session, "analyses_SE", "Model Comparison")
  updateSelectizeInput(session, "outSEp", choices = rv$modNames_SEp)
  updateSelectizeInput(session, "outSEk", choices = rv$modNames_SEk)
  updateSelectizeInput(session, "outsizeclassSE", choices = rv$sizeclasses)

  updateSelectizeInput(session, "DWPCol", choices = rv$colNames_DWP,
    selected = rv$DWPCol)
  if (length(rv$colNames_DWP) == 1){
    updateSelectizeInput(session, "DWPCol", selected = rv$colNames_DWP)
  }

  reset("outsizeclassg")
  reset("gSearchInterval")
  reset("gSearchMax")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
  updateSelectizeInput(session, "outsizeclassg", choices = "")
}


#' @title Update the inputs when SE model is cleared
#'
#' @description Update the inputs when the SE model is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_SE_clear <- function(rv, session){
  reset("outSEp")
  reset("outSEk")
  reset("outsizeclassSE")
  reset("kFill")
  reset("DWPCol")
  reset("split_SS")
  reset("split_CO")
  reset("modelChoices_SE1")
  reset("outsizeclassg")

  updateSelectizeInput(session, "modelChoices_SE1", choices = "")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
  updateSelectizeInput(session, "outSEp", choices = "")
  updateSelectizeInput(session, "outSEk", choices = "")
  updateSelectizeInput(session, "outsizeclassSE", choices = "")
  updateSelectizeInput(session, "outsizeclassg", choices = "")


}

#' @title Update the SE output dropdown selections when the size class is 
#'   chosen
#'
#' @description Update the SE output dropdown selections when the size 
#'   class is chosen
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_outsc_SE <- function(rv, session){
  updateSelectizeInput(session, "outSEp", choices = rv$modNames_SEp)
  updateSelectizeInput(session, "outSEk", choices = rv$modNames_SEk)
}

#' @title Update the CP output dropdown selections when the model is run
#'
#' @description Update the CP output dropdown selections when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_CP <- function(rv, session){
  updateTabsetPanel(session, "analyses_CP", "Model Comparison")
  updateSelectizeInput(session, "outCPl", choices = rv$modNames_CPl)
  updateSelectizeInput(session, "outCPs", choices = rv$modNames_CPs)
  updateSelectizeInput(session, "outCPdist", choices = rv$modNames_CPdist)
  updateSelectizeInput(session, "outsizeclassCP", choices = rv$sizeclasses)

  reset("outsizeclassg")
  reset("gSearchInterval")
  reset("gSearchMax")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
  updateSelectizeInput(session, "outsizeclassg", choices = "")
}

#' @title Update the inputs when CP model is cleared
#'
#' @description Update the inputs when the CP model is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_CP_clear <- function(rv, session){
  reset("outCPl")
  reset("outCPs")
  reset("outCPdist")
  reset("outsizeclassCP")
  reset("split_SS")
  reset("split_CO")
  reset("modelChoices_CP1")
  reset("outsizeclassg")

  updateSelectizeInput(session, "modelChoices_CP1", choices = "")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
  updateSelectizeInput(session, "outCPl", choices = "")
  updateSelectizeInput(session, "outCPs", choices = "")
  updateSelectizeInput(session, "outCPdist", choices = "")
  updateSelectizeInput(session, "outsizeclassCP", choices = "")
  updateSelectizeInput(session, "outsizeclassg", choices = "")
}

#' @title Update the CP output dropdown selections when the size class is 
#'   chosen
#'
#' @description Update the CP output dropdown selections when the size 
#'   class is chosen
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_outsc_CP <- function(rv, session){
  updateSelectizeInput(session, "outCPl", choices = rv$modNames_CPl)
  updateSelectizeInput(session, "outCPs", choices = rv$modNames_CPs)
  updateSelectizeInput(session, "outCPdist", choices = rv$modNames_CPdist)
}

#' @title Update the SS average dropdown selections when the SS data are used
#'
#' @description Update the SS average dropdown selections when the SS data 
#'   are used
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_useSSdata <- function(rv, session){
  if (!is.na(rv$SStemp[1])){
    updateNumericInput(session, "gSearchInterval", value = rv$avgSI)
    updateNumericInput(session, "gSearchMax", value = max(rv$SS))
  }
}

#' @title Update the g output dropdown selections when the model is run
#'
#' @description Update the g output dropdown selections when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_g <- function(rv, session){
  updateSelectizeInput(session, "outsizeclassg", choices = rv$sizeclasses_g)
  updateTabsetPanel(session, "analyses_g", "Summary")
}


#' @title Update the inputs when g model is cleared
#'
#' @description Update the inputs when the g model is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_g_clear <- function(rv, session){
  reset("outsizeclassg")
  updateSelectizeInput(session, "outsizeclassg", choices = "")

}

#' @title Update the M output dropdown selections and the Fraction Surveyed
#'   when the model is run
#'
#' @description Update the M output dropdown selections and the Fraction 
#'   Surveyed when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_M <- function(rv, session){
  updateNumericInput(session, "frac", value = rv$frac)
  updateSelectizeInput(session, "split_SS", choices = rv$colNames_SS_nosel)
  updateSelectizeInput(session, "split_CO", choices = rv$colNames_CO)
}

#' @title Update the inputs when M model is cleared
#'
#' @description Update the inputs when the M model is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_M_clear <- function(rv, session){
  reset("split_SS")
  reset("split_CO")
  updateSelectizeInput(session, "split_SS", choices = "")
  updateSelectizeInput(session, "split_CO", choices = "")
}

#' @title Update the inputs when M split is cleared
#'
#' @description Update the inputs when the M split is cleared
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_split_M_clear <- function(rv, session){
  reset("split_SS")
  reset("split_CO")
  updateSelectizeInput(session, "split_SS", choices = rv$colNames_SS_nosel)
  updateSelectizeInput(session, "split_CO", choices = rv$colNames_CO)
}
