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

  if (!all(unlist(pkmSetSizeFail(rv$mods_SE))) &&
      !any(unlist(lapply(rv$mods_SE_og, pkmSetAllFail)))){

    output$SEModDone <- renderText("OK")
    output$kFillNeed <- setkFillNeed(rv$obsCols_SE)
    if (length(rv$sizeclasses) == 1){
      output$DWPNeed <- renderText("yes")
    } else{
      output$DWPNeed <- renderText("no")
    }
    outputOptions(output, "SEModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)
    outputOptions(output, "DWPNeed", suspendWhenHidden = FALSE)

    output$AICcTab_SE <- renderDataTable({rv$AICcTab_SE})    
    output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
    output$fig_SE <- renderPlot({ 
                       plot(rv$modSet_SE, specificModel = rv$best_SE)
                     }, height = rv$figH_SE, width = rv$figW_SE)

    isolate({
      output$sizeclasses_SE <- prepSizeclassText(rv$sizeclasses_SE)
      outputOptions(output, "sizeclasses_SE", suspendWhenHidden = FALSE)
      output$modelMenu_SE <- makeMenu(rv$mods_SE, rv$sizeclasses_SE, "SE")
    })

    scText <- renderText(paste0("Size class: ", rv$sizeclass_SE))
    output$sizeclass_SE1 <- scText
    output$sizeclass_SE2 <- scText
    output$sizeclass_SE3 <- scText

    output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE)
    output$dlSEAICc <- downloadTable("SE_AICc.csv", rv$AICcTab_SE)
    output$dlSEfig <- downloadSEFig(rv)
  }
  return(output)
}

#' @title Update the SE output when a size class is chosen
#'
#' @description Update the SE output when a size class is chosen
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
update_output_outsc_SE <- function(rv, output, session){
  if (length(rv$mods_SE) > 0){
    output$AICcTab_SE <- renderDataTable({rv$AICcTab_SE})    
    output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
    output$fig_SE <- renderPlot({ 
                       plot(rv$modSet_SE, specificModel = rv$best_SE)
                     }, height = rv$figH_SE, width = rv$figW_SE)

    scText <- renderText(paste0("Size class: ", rv$sizeclass_SE))
    output$sizeclass_SE1 <- scText
    output$sizeclass_SE2 <- scText
    output$sizeclass_SE3 <- scText

    output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE)
    output$dlSEAICc <- downloadTable("SE_AICc.csv", rv$AICcTab_SE)
    output$dlSEfig <- downloadSEFig(rv)
  }
  return(output)
}

#' @title Update the SE output when a p or k equation is chosen
#'
#' @description Update the SE output when a p or k equation is chosen
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
update_output_outpk_SE <- function(rv, output, session){
  if (length(rv$mods_SE) > 0){
    output$fig_SE <- renderPlot({ 
                       tryCatch(
                         plot(rv$modSet_SE, specificModel = rv$outSEpk),
                         error = function(x){plotNA()}
                       )
                     }, height = rv$figH_SE, width = rv$figW_SE)
    output$dlSEfig <- downloadSEFig(rv)
    if (!is.null(rv$modTab_SE)){
      output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
      output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE)
    }
  }
  return(output)
}

#' @title Update the output when an CP model has been run
#'
#' @description Update the output table when an CP model has been run
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
update_output_run_CP <- function(rv, output, session){

  if (!all(unlist(cpmSetSizeFail(rv$mods_CP)))){

    output$CPModDone <- renderText("OK")
    outputOptions(output, "CPModDone", suspendWhenHidden = FALSE)

    output$AICcTab_CP <- renderDataTable({rv$AICcTab_CP})    
    output$modTab_CP <- renderDataTable({rv$modTabPretty_CP})
    output$fig_CP <- renderPlot({ 
                       plot(rv$modSet_CP, specificModel = rv$best_CP)
                     }, height = rv$figH_CP, width = rv$figW_CP)

    isolate({
      output$sizeclasses_CP <- prepSizeclassText(rv$sizeclasses_CP)
      outputOptions(output, "sizeclasses_CP", suspendWhenHidden = FALSE)
      output$modelMenu_CP <- makeMenu(rv$mods_CP, rv$sizeclasses_CP, "CP")
    })

    scText <- renderText(paste0("Size class: ", rv$sizeclass_CP))
    output$sizeclass_CP1 <- scText
    output$sizeclass_CP2 <- scText
    output$sizeclass_CP3 <- scText

    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP)
    output$dlCPAICc <- downloadTable("CP_AICc.csv", rv$AICcTab_CP)
    output$dlCPfig <- downloadCPFig(rv)
  }
  return(output)
}

#' @title Update the CP output when a size class is chosen
#'
#' @description Update the CP output when a size class is chosen
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
update_output_outsc_CP <- function(rv, output, session){
  if (length(rv$mods_CP) > 0){
    output$modTab_CP <- renderDataTable(rv$modTabPretty_CP)
    output$fig_CP <- renderPlot({ 
                       plot(rv$modSet_CP, specificModel = rv$best_CP)
                     }, height = rv$figH_CP, width = rv$figW_CP)

    scText <- renderText(paste0("Size class: ", rv$sizeclass_CP))
    output$sizeclass_CP1 <- scText
    output$sizeclass_CP2 <- scText
    output$sizeclass_CP3 <- scText

    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP)
    output$dlCPAICc <- downloadTable("CP_AICc.csv", rv$AICcTab_CP)
    output$dlCPfig <- downloadCPFig(rv)
  }
  return(output)
}

#' @title Update the CP output when a distribuition or l or s equation is 
#'   chosen
#'
#' @description Update the SE output when a distribution or l or s equation is 
#'   chosen
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
update_output_outdls_CP <- function(rv, output, session){
  if (length(rv$mods_CP) > 0){
    output$modTab_CP <- renderDataTable(rv$modTabPretty_CP)
    output$fig_CP <- renderPlot({ 
                       tryCatch(
                       plot(rv$modSet_CP, specificModel = rv$outCPdlsfig),
                         error = function(x){plotNA()}
                       )
                     }, height = rv$figH_CP, width = rv$figW_CP)
    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP)
    output$dlCPfig <- downloadCPFig(rv)

    if (!is.null(rv$modTab_CP)){
      output$modTab_CP <- renderDataTable({rv$modTabPretty_CP})
      output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP)
    }

  }
  return(output)
}

#' @title Update the SS text output when the SS is updated
#'
#' @description Update the SS text output when the SS is updated
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
update_output_SS <- function(rv, output, session){
  output$SStext <- renderText(rv$SStext)
  output
}

#' @title Update the output when a generic g model has been run
#'
#' @description Update the output table when a generic g model has been run
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
update_output_run_g <- function(rv, output, session){

  if (!is.null(rv$gGeneric[[1]])){
    summaryTab <- summary(rv$gGeneric[[1]], CL = rv$CL)
    output$table_g <- renderDataTable(summaryTab)
    output$fig_g <- renderPlot({
                      tryCatch(
                        plot(rv$gGeneric[[1]], CL = rv$CL),
                        error = function(x){plot(1,1)},
                        warning = function(x){plot(1,1)}
                      )
                    }, height = rv$figH_g, width = rv$figW_g)
    output$gModDone <- renderText("OK")
    outputOptions(output, "gModDone", suspendWhenHidden = FALSE)

    scText <- renderText(paste0("Size class: ", rv$sizeclass_g))
    output$sizeclass_g1 <- scText
    output$sizeclass_g2 <- scText

    output$dlgtab <- downloadTable("g_estimates.csv", summaryTab)
    output$dlgfig <- downloadgFig(rv, 1)
  }
  return(output)
}

#' @title Update the output when a generic g size class is chosen
#'
#' @description Update the output table when a generic g size class is chosen
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
update_output_outsc_g <- function(rv, output, session){

  if (class(rv$gGeneric[[rv$sizeclass_g]])[1] == "gGeneric"){
    summaryTab <- summary(rv$gGeneric[[rv$sizeclass_g]], CL = rv$CL)
    output$table_g <- renderDataTable(summaryTab)
    output$fig_g <- renderPlot({
                      tryCatch(
                        plot(rv$gGeneric[[rv$sizeclass_g]], CL = rv$CL),
                        error = function(x){plot(1,1)},
                        warning = function(x){plot(1,1)}
                      )
                    }, height = rv$figH_g, width = rv$figW_g)
    output$gModDone <- renderText("OK")
    outputOptions(output, "gModDone", suspendWhenHidden = FALSE)

    scText <- renderText(paste0("Size class: ", rv$sizeclass_g))
    output$sizeclass_g1 <- scText
    output$sizeclass_g2 <- scText

    output$dlgtab <- downloadTable("g_estimates.csv", summaryTab)
    output$dlgfig <- downloadgFig(rv, rv$sizeclass_g)
  }
  return(output)
}

#' @title Update the output when an M model has been run
#'
#' @description Update the output table when an M model has been run
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
update_output_run_M <- function(rv, output, session){

  if (!is.null(rv$Msplit)){
    output$MModDone <- renderText("OK")
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

    output$fig_M <- renderPlot({plot(rv$Msplit)},
                      height = rv$figH_M, width = rv$figW_M
                    )
    summaryTab <-  prettySplitTab(summary(rv$Msplit, CL = rv$CL))
    output$table_M <- renderDataTable(datatable(summaryTab))
    output$dlMtab <- downloadTable("M_table.csv", summaryTab)
    output$dlMfig <- downloadMFig(rv)
  }
  output
}

#' @title Update the output when M has been split
#'
#' @description Update the output table when M has been split
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
update_output_split_M <- function(rv, output, session){

  if (is.null(rv$Msplit)){
    output$fig_M <- renderPlot({
                      tryCatch(plot(rv$M),
                        error = function(x){plotNA()}
                      )
                    }, height = rv$figH_M, width = rv$figW_M)
    output$dlMfig <- downloadMFig(rv, split = FALSE)

  } else{
    output$fig_M <- renderPlot({plot(rv$Msplit)},
                      height = rv$figH_M, width = rv$figW_M
                    )
    summaryTab <-  prettySplitTab(summary(rv$Msplit, CL = rv$CL))
    output$table_M <- renderDataTable(datatable(summaryTab))
    output$dlMtab <- downloadTable("M_table.csv", summaryTab)
    output$dlMfig <- downloadMFig(rv)
  }
  output
}


#' @title Update the output when the M split is transposed
#'
#' @description Update the output when the M split is transposed
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
update_output_transpose_split <- function(rv, output, session){

  if (!is.null(rv$Msplit)){
      output$fig_M <- renderPlot({plot(rv$Msplit)},
                        height = rv$figH_M, width = rv$figW_M
                      )
      output$dlMfig <- downloadMFig(rv, TRUE, TRUE)

  }
  output
}