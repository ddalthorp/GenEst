#' @title Update the output list when SE data are read in
#'
#' @description Update the output list when the SE data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_SE <- function(rv, output){
  output$data_SE <- renderDataTable(datatable(rv$data_SE))
  return(output)
}

#' @title Update the output list when CP data are read in
#'
#' @description Update the output list when the CP data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_CP <- function(rv, output){
  output$data_CP <- renderDataTable(datatable(rv$data_CP))
  return(output)
}

#' @title Update the output list when SS data are read in
#'
#' @description Update the output list when the SS data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_SS <- function(rv, output){
  output$data_SS <- renderDataTable(datatable(rv$data_SS))
  return(output)
}

#' @title Update the output list when DWP data are read in
#'
#' @description Update the output list when the DWP data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_DWP <- function(rv, output){
  output$data_DWP <- renderDataTable(datatable(rv$data_DWP))
  return(output)
}

#' @title Update the output list when CO data are read in
#'
#' @description Update the output list when the CO data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_CO <- function(rv, output){
  output$data_CO <- renderDataTable(datatable(rv$data_CO))
  return(output)
}

#' @title Update the selected CP and SE data when a size class column is 
#'   selected
#'
#' @description Update the output tables when the size class column is
#'   selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_sizeclassCol <- function(rv, input, output){
  if (!is.null(input$obsCols_SE)){
    selectedCols <- c(input$obsCols_SE, input$sizeclassCol, input$preds_SE)
    selectedData <- selectData(rv$data_SE, selectedCols)
    output$selected_SE <- renderDataTable(datatable(selectedData))
  }
  if (!is.null(c(input$ltp, input$fta))){
    obsColsSelected <- c(input$ltp, input$fta)
    selectedCols <- c(obsColsSelected, input$sizeclassCol, input$preds_CP)
    selectedData <- selectData(rv$data_CP, selectedCols)
    output$selected_CP <- renderDataTable(datatable(selectedData))
  }
  return(output)
}

#' @title Update the selected SE data when a column is selected
#'
#' @description Update the output table when the SE columns are selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_cols_SE <- function(rv, input, output){
  selectedCols <- c(input$obsCols_SE, input$sizeclassCol, input$preds_SE)
  selectedData <- selectData(rv$data_SE, selectedCols)
  output$selected_SE <- renderDataTable(datatable(selectedData))
  return(output)
}

#' @title Update the selected CP data when a column is selected
#'
#' @description Update the output table when the CP columns are selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_cols_CP <- function(rv, input, output){
  obsColsSelected <- c(input$ltp, input$fta)
  selectedCols <- c(obsColsSelected, input$sizeclassCol, input$preds_CP)
  selectedData <- selectData(rv$data_CP, selectedCols)
  output$selected_CP <- renderDataTable(datatable(selectedData))
  return(output)
}

#' @title Update the output when an SE model has been run
#'
#' @description Update the output table when an SE model has been run
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_SE <- function(rv, output, session){

  if (!all(unlist(pkmSetSizeFail(rv$mods_SE)))){

    output$SEModDone <- renderText("OK")
    outputOptions(output, "SEModDone", suspendWhenHidden = FALSE)

    output$kFillNeed <- setkFillNeed(rv$obsCols_SE)
    output$AICcTab_SE <- renderDataTable({rv$AICcTab_SE})    
    output$modTab_SE <- renderDataTable({rv$modTab_SE})
    output$fig_SE <- renderPlot({ 
                       plot(rv$modSet_SE, specificModel = rv$best_SE)
                     }, height = rv$figH_SE, width = rv$figW_SE)
    outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)
    isolate({
      output$sizeclasses_SE <- prepSizeclassText(rv$sizeclasses_SE)
      outputOptions(output, "sizeclasses_SE", suspendWhenHidden = FALSE)
      output$modelMenu_SE <- makeMenu(rv$mods_SE, rv$sizeclasses_SE, "SE")
    })
    if (length(rv$sizeclasses) == 1){
      output$DWPNeed <- renderText("yes")
    } else{
      output$DWPNeed <- renderText("no")
    }
    outputOptions(output, "DWPNeed", suspendWhenHidden = FALSE)
    output$sizeclass_SE1 <- renderText(paste0("Size class: ", rv$sizeclass))
    output$sizeclass_SE2 <- renderText(paste0("Size class: ", rv$sizeclass))

    output$downloadSEest <- downloadTable("SE_estimates.csv", rv$modTab_SE)
    output$downloadSEAICc <- downloadTable("SE_AICc.csv", rv$AICcTab_SE)
    output$downloadSEfig <- downloadSEFig(rv)
  }
  return(output)
}



