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
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE_nosel)
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE_nosel)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Search Efficiency")
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
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_nosel)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_CP_nosel)
  updateSelectizeInput(session, "fta", choices = rv$colNames_CP_nosel)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Persistence")
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
  updateTabsetPanel(session, "LoadedDataViz", "Density Weighted Proportion")
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
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")
  updateSelectizeInput(session, "splitCol", choices = rv$colNames_CO)
  updateSelectizeInput(session, "dateFoundCol", choices = rv$colNames_COdates)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")
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
  rv$colNames_SE_sel <- c(input$obsCols_SE, input$sizeclassCol)
  rv$colNames_SE_nosel <- removeSelCols(rv$colNames_SE, rv$colNames_SE_sel)
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE_nosel,
    selected = input$preds_SE)
  rv$colNames_SE_sel <- c(input$preds_SE, input$sizeclassCol)
  rv$colNames_SE_nosel <- removeSelCols(rv$colNames_SE, rv$colNames_SE_sel)
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE_nosel,
    selected = input$obsCols_SE)
  rv$colNames_CP_sel <- c(input$preds_CP, input$fta, input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_CP_nosel,
    selected = input$ltp)
  rv$colNames_CP_sel <- c(input$preds_CP, input$ltp, input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, "fta", choices = rv$colNames_CP_nosel,
    selected = input$fta)
  rv$colNames_CP_sel <- c(input$ltp, input$fta, input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_nosel,
    selected = input$preds_CP)
}

#' @title Update the remaining columns when SE data columns are selected
#'
#' @description Update the inputs when the SE data columns are selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param session session
#'
#' @param x specific column
#'
#' @export
#'
update_input_cols_SE <- function(rv, input, session, x = "obsCols"){

  notx <- switch(x, "obsCols" = "preds_SE", "preds" = "obsCols_SE")
  x <- paste0(x, "_SE")
  rv$colNames_SE_sel <- c(input[[x]], input$sizeclassCol)
  rv$colNames_SE_nosel <- removeSelCols(rv$colNames_SE, rv$colNames_SE_sel)
  updateSelectizeInput(session, notx, choices = rv$colNames_SE_nosel,
    selected = input[[notx]])
}

#' @title Update the remaining columns when CP data columns are selected
#'
#' @description Update the inputs when the CP data columns are selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param session session
#'
#' @param x specific column
#'
#' @export
#'
update_input_cols_CP <- function(rv, input, session, x = "ltp"){

  notx1 <- switch(x, "ltp" = "fta", "fta" = "ltp", "preds" = "ltp")
  notx2 <- switch(x, "ltp" = "preds", "fta" = "preds", "preds" = "fta")

  x <- gsub("preds", "preds_CP", x)
  notx1 <- gsub("preds", "preds_CP", notx1)
  notx2 <- gsub("preds", "preds_CP", notx2)

  rv$colNames_CP_sel <- c(input[[x]], input[[notx1]], input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, notx2, choices = rv$colNames_CP_nosel,
    selected = input[[notx2]])

  rv$colNames_CP_sel <- c(input[[x]], input[[notx2]], input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, notx1, choices = rv$colNames_CP_nosel,
    selected = input[[notx1]])
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

  if (rv$kFixedChoice == 1){
    updateNumericInput(session, "kFill", value = rv$kFixed)
  }
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


#' @title Update the M output dropdown selections when the model is run
#'
#' @description Update the M output dropdown selections when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_M <- function(rv, session){
  updateSelectizeInput(session, "split_SS", choices = rv$colNames_SS_nosel)
  updateSelectizeInput(session, "split_CO", choices = rv$colNames_CO)
}
