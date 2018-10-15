#' @title Update the reactive value list when everything is cleared out
#'
#' @description Update the rv list when everything is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_clear_all <- function(rv, output){
  output <- update_output_file_SE_clear(rv, output)
  output <- update_output_file_CP_clear(rv, output)
  output <- update_output_file_DWP_clear(rv, output)
  output <- update_output_file_SS_clear(rv, output)
  output <- update_output_file_CO_clear(rv, output)
  return(output)
}

#' @title Update the output list upon initiation of the app
#'
#' @description Update the output list when the app is started
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
initialOutput <- function(rv, output){
  output$SStext <- renderText(rv$SStext)

  output$download_RP <- downloadData("RP")
  output$download_RPbat <- downloadData("RPbat")
  output$download_cleared <- downloadData("cleared")
  output$download_powerTower <- downloadData("powerTower")
  output$download_PV <- downloadData("PV")
  output$download_trough <- downloadData("trough")
  output$download_mock <- downloadData("mock")
  output$kFillNeed <- renderText("no")
  outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)

  return(output)
}

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
update_output_file_SE <- function(rv, output){
  output <- update_output_file_SE_clear(rv, output)
  output$data_SE <- renderDataTable(datatable(rv$data_SE), server = FALSE)
  output$filename_SE <- renderText(paste0("File: ", rv$filename_SE))
  outputOptions(output, "filename_SE", suspendWhenHidden = FALSE)
  return(output)
}

#' @title Update the output list when SE data are cleared
#'
#' @description Update the output list when the SE data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_file_SE_clear <- function(rv, output){
  output$data_SE <- NULL
  output$filename_SE <- NULL
  output$selected_SE <- NULL
  output$SEModDone <- NULL
  output$kFillNeed <- setkFillNeed(rv)
  output$DWPNeed <- NULL
  output$AICcTab_SE <- NULL
  output$modTab_SE <- NULL
  output$fig_SE <- NULL
  output$sizeclasses_SE <- NULL
  output$modelMenu_SE <- NULL
  output$sizeclass_SE1 <- NULL
  output$sizeclass_SE2 <- NULL
  output$sizeclass_SE3 <- NULL
  output$sizeclass_SEyn <- NULL
  outputOptions(output, "AICcTab_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "modTab_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "SEModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclasses_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "filename_SE", suspendWhenHidden = FALSE)
  output$text_SE_est <- NULL
  outputOptions(output, "text_SE_est", suspendWhenHidden = FALSE)
  outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)

  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

  output$table_g <- NULL
  output$fig_g <- NULL
  output$gModDone <- NULL
  output$sizeclass_gyn <- NULL
  output$sizeclass_g1 <- NULL
  output$sizeclass_g2 <- NULL

  outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
  outputOptions(output, "table_g", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
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
update_output_file_CP <- function(rv, output){
  output <- update_output_file_CP_clear(rv, output)
  output$data_CP <- renderDataTable(datatable(rv$data_CP), server = FALSE)
  output$filename_CP <- renderText(paste0("File: ", rv$filename_CP))
  outputOptions(output, "filename_CP", suspendWhenHidden = FALSE)
  return(output)
}

#' @title Update the output list when CP data are cleared
#'
#' @description Update the output list when the CP data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_file_CP_clear <- function(rv, output){
  output$data_CP <- NULL
  output$filename_CP <- NULL
  outputOptions(output, "filename_CP", suspendWhenHidden = FALSE)
  output$selected_CP <- NULL
  output$CPModDone <- NULL
  output$AICcTab_CP <- NULL
  output$modTab_CP <- NULL
  output$fig_CP <- NULL
  output$sizeclasses_CP <- NULL
  output$modelMenu_CP <- NULL
  output$sizeclass_CP1 <- NULL
  output$sizeclass_CP2 <- NULL
  output$sizeclass_CP3 <- NULL
  output$sizeclass_CPyn <- NULL
  output$text_CP_est <- NULL
  outputOptions(output, "text_CP_est", suspendWhenHidden = FALSE)

  outputOptions(output, "AICcTab_CP", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_CP", suspendWhenHidden = FALSE)
  outputOptions(output, "modTab_CP", suspendWhenHidden = FALSE)
  outputOptions(output, "CPModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclasses_CP", suspendWhenHidden = FALSE)

  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

  output$table_g <- NULL
  output$fig_g <- NULL
  output$gModDone <- NULL
  output$sizeclass_gyn <- NULL
  output$sizeclass_g1 <- NULL
  output$sizeclass_g2 <- NULL

  outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
  outputOptions(output, "table_g", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)

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
update_output_file_SS <- function(rv, output){
  output <- update_output_file_SS_clear(rv, output)
  output$data_SS <- renderDataTable(datatable(rv$data_SS), server = FALSE)
  outputOptions(output, "data_SS", suspendWhenHidden = FALSE)
  return(output)
}

#' @title Update the output list when SS data are cleared
#'
#' @description Update the output list when the SS data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_file_SS_clear <- function(rv, output){
  output$data_SS <- NULL

  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)

  output$table_g <- NULL
  output$fig_g <- NULL
  output$gModDone <- NULL
  output$sizeclass_gyn <- NULL
  output$sizeclass_g1 <- NULL
  output$sizeclass_g2 <- NULL

  outputOptions(output, "data_SS", suspendWhenHidden = FALSE)
  outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
  outputOptions(output, "table_g", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
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
update_output_file_DWP <- function(rv, output){
  output <- update_output_file_DWP_clear(rv, output)
  output$data_DWP <- renderDataTable(datatable(rv$data_DWP), server = FALSE)
  outputOptions(output, "data_DWP", suspendWhenHidden = FALSE)
  return(output)
}


#' @title Update the output list when DWP data are cleared
#'
#' @description Update the output list when the DWP data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_file_DWP_clear <- function(rv, output){
  output$data_DWP <- NULL
  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "data_DWP", suspendWhenHidden = FALSE)
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
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
update_output_file_CO <- function(rv, output){
  output <- update_output_file_CO_clear(rv, output)
  output$data_CO <- renderDataTable(datatable(rv$data_CO), server = FALSE)
  outputOptions(output, "data_CO", suspendWhenHidden = FALSE)
  return(output)
}

#' @title Update the output list when CO data are cleared
#'
#' @description Update the output list when the CO data file input is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_file_CO_clear <- function(rv, output){
  output$data_CO <- NULL
  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "data_CO", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
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
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_class <- function(rv, output){
  if (!is.null(rv$obsCols_SE)){
    selectedCols <- c(rv$obsCols_SE, rv$sizeCol, rv$preds_SE)
    selectedData <- selectData(rv$data_SE, selectedCols)
    output$selected_SE <- renderDataTable(datatable(selectedData), 
                                          server = FALSE)
  }
  if (!is.null(c(rv$ltp, rv$fta))){
    obsColsSelected <- c(rv$ltp, rv$fta)
    selectedCols <- c(obsColsSelected, rv$sizeCol, rv$preds_CP)
    selectedData <- selectData(rv$data_CP, selectedCols)
    output$selected_CP <- renderDataTable(datatable(selectedData), 
                                          server = FALSE)
  }

#  isolate({
    output$kFixedInput <- kFixedWidget(rv$sizeclasses_k)
#  })

  return(output)
}

#' @title Update the selected SE data when a column is selected
#'
#' @description Update the output table when the SE columns are selected
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_obsSE <- function(rv, output){
  selectedCols <- c(rv$obsCols_SE, rv$sizeCol, rv$preds_SE)
  if (!is.null(rv$data_SE)){
    selectedData <- selectData(rv$data_SE, selectedCols)
    output$selected_SE <- renderDataTable(datatable(selectedData), 
                                          server = FALSE)
  }
  return(output)
}

update_output_predsSE <- function(rv, output){
  selectedCols <- c(rv$obsCols_SE, rv$sizeCol, rv$preds_SE)
  if (!is.null(rv$data_SE)){
    selectedData <- selectData(rv$data_SE, selectedCols)
    output$selected_SE <- renderDataTable(datatable(selectedData), 
                                          server = FALSE)
  }
  return(output)
}

#' @title Update the selected CP data when a column is selected
#'
#' @description Update the output table when the CP columns are selected
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_fta <- function(rv, output){
  selectedCols <- c(rv$ltp, rv$fta, rv$sizeCol, rv$preds_CP)
  selectedData <- selectData(rv$data_CP, selectedCols)
  output$selected_CP <- renderDataTable(datatable(selectedData), 
                                          server = FALSE)
  return(output)
}

update_output_ltp <- function(rv, output){
  selectedCols <- c(rv$ltp, rv$fta, rv$sizeCol, rv$preds_CP)
  selectedData <- selectData(rv$data_CP, selectedCols)
  output$selected_CP <- renderDataTable(datatable(selectedData), 
                                          server = FALSE)
  return(output)
}

update_output_predsCP <- function(rv, output){
  selectedCols <- c(rv$ltp, rv$fta, rv$sizeCol, rv$preds_CP)
  selectedData <- selectData(rv$data_CP, selectedCols)
  output$selected_CP <- renderDataTable(datatable(selectedData), 
                                          server = FALSE)
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
#' @return an updated output list
#'
#' @export
#'
update_output_run_SE <- function(rv, output){
  output$text_SE_est <- NULL

  if (!all(unlist(pkmSetSizeFail(rv$mods_SE))) &&
      !any(unlist(lapply(rv$mods_SE_og, pkmSetAllFail)))){

    output$SEModDone <- renderText("OK")
    output$kFillNeed <- setkFillNeed(rv)
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
                       plot(rv$modSet_SE, specificModel = rv$best_SE, 
                         app = TRUE)
                     }, height = rv$figH_SE, width = rv$figW_SE)

    isolate({
      output$sizeclasses_SE <- prepSizeclassText(rv$sizeclasses_SE)
      outputOptions(output, "sizeclasses_SE", suspendWhenHidden = FALSE)
      output$modelMenu_SE <- modelSelectionWidget(rv$mods_SE, "SE")
    })

    preText <- paste0("Size class: ", rv$sizeclass_SE)
    if (length(rv$sizeclasses_SE) == 1){
      preText <- ""
    }    

    output$text_SE_est <- renderText(paste0(
      "Table shows median estimates and ", 100 * rv$CL,  
           "% confidence intervals"))

    scText <- renderText(preText)
    output$sizeclass_SE1 <- scText
    output$sizeclass_SE2 <- scText
    output$sizeclass_SE3 <- scText

    if (length(rv$sizeclasses_SE) == 1){
      output$sizeclass_SEyn <- renderText("NO")
    } else{
      output$sizeclass_SEyn <- renderText("YES")
    }
    outputOptions(output, "sizeclass_SEyn", suspendWhenHidden = FALSE)

    output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE, 
                                          rv$csvformat)
    output$dlSEAICc <- downloadTable("SE_AICc.csv", rv$AICcTab_SE, 
                                          rv$csvformat)
    output$dlSEfig <- downloadSEFig(rv)

    outputOptions(output, "text_SE_est", suspendWhenHidden = FALSE)

  }


  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

  output$table_g <- NULL
  output$fig_g <- NULL
  output$gModDone <- NULL
  output$sizeclass_gyn <- NULL
  output$sizeclass_g1 <- NULL
  output$sizeclass_g2 <- NULL

  outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
  outputOptions(output, "table_g", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)


  return(output)
}

#' @title Update the output list when SE model is cleared
#'
#' @description Update the output list when the SE model is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_SE_clear <- function(rv, output){
  output$SEModDone <- NULL
  output$kFillNeed <- setkFillNeed(rv)
  output$DWPNeed <- NULL
  output$AICcTab_SE <- NULL
  output$modTab_SE <- NULL
  output$fig_SE <- NULL
  output$sizeclasses_SE <- NULL
  output$modelMenu_SE <- NULL
  output$sizeclass_SE1 <- NULL
  output$sizeclass_SE2 <- NULL
  output$sizeclass_SE3 <- NULL
  output$sizeclass_SEyn <- NULL
  outputOptions(output, "AICcTab_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "modTab_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "SEModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclasses_SE", suspendWhenHidden = FALSE)
  output$text_SE_est <- NULL
  outputOptions(output, "text_SE_est", suspendWhenHidden = FALSE)

  output$fig_M <- NULL
  output$table_M <- NULL

  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

  output$table_g <- NULL
  output$fig_g <- NULL
  output$gModDone <- NULL
  output$sizeclass_gyn <- NULL
  output$sizeclass_g1 <- NULL
  output$sizeclass_g2 <- NULL


  outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
  outputOptions(output, "table_g", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
  outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)

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
#' @return an updated output list
#'
#' @export
#'
update_output_outSEclass <- function(rv, output){
  if (length(rv$mods_SE) > 0){
    output$AICcTab_SE <- renderDataTable({rv$AICcTab_SE})
    output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
    output$fig_SE <- renderPlot({ 
                       plot(rv$modSet_SE, specificModel = rv$best_SE, 
                         app = TRUE)
                     }, height = rv$figH_SE, width = rv$figW_SE)

    preText <- paste0("Size class: ", rv$sizeclass_SE)
    if (length(rv$sizeclasses_SE) == 1){
      preText <- ""
    }    
    scText <- renderText(preText)
    output$sizeclass_SE1 <- scText
    output$sizeclass_SE2 <- scText
    output$sizeclass_SE3 <- scText

    output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE, 
                                          rv$csvformat)
    output$dlSEAICc <- downloadTable("SE_AICc.csv", rv$AICcTab_SE, 
                                          rv$csvformat)
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
#' @return an updated output list
#'
#' @export
#'
update_output_outSEp <- function(rv, output){
  if (length(rv$mods_SE) > 0){
    output$fig_SE <- renderPlot({ 
                       tryCatch(
                         plot(rv$modSet_SE, specificModel = rv$outSEpk, 
                           app = TRUE),
                         error = function(x){plotNA()}
                       )
                     }, height = rv$figH_SE, width = rv$figW_SE)
    output$dlSEfig <- downloadSEFig(rv)
    if (!is.null(rv$modTab_SE)){
      output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
      output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE, 
                                          rv$csvformat)
    }
  }
  return(output)
}

update_output_outSEk <- function(rv, output){
  if (length(rv$mods_SE) > 0){
    output$fig_SE <- renderPlot({ 
                       tryCatch(
                         plot(rv$modSet_SE, specificModel = rv$outSEpk, 
                           app = TRUE),
                         error = function(x){plotNA()}
                       )
                     }, height = rv$figH_SE, width = rv$figW_SE)
    output$dlSEfig <- downloadSEFig(rv)
    if (!is.null(rv$modTab_SE)){
      output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
      output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE, 
                                          rv$csvformat)
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
#' @return an updated output list
#'
#' @export
#'
update_output_run_CP <- function(rv, output){

  output$text_CP_est <- NULL

  if (!all(unlist(cpmSetSizeFail(rv$mods_CP)))){

    output$CPModDone <- renderText("OK")
    outputOptions(output, "CPModDone", suspendWhenHidden = FALSE)

    output$AICcTab_CP <- renderDataTable({rv$AICcTab_CP})    
    output$modTab_CP <- renderDataTable({rv$modTabPretty_CP})
    output$fig_CP <- renderPlot({ 
                       plot(rv$modSet_CP, specificModel = rv$best_CP, 
                         app = TRUE)
                     }, height = rv$figH_CP, width = rv$figW_CP)

    isolate({
      output$sizeclasses_CP <- prepSizeclassText(rv$sizeclasses_CP)
      outputOptions(output, "sizeclasses_CP", suspendWhenHidden = FALSE)
      output$modelMenu_CP <- modelSelectionWidget(rv$mods_CP, "CP")
    })

    output$text_CP_est <- renderText(paste0(
      "Table shows median estimates and  ",
      100 * rv$CL,  "% confidence intervals for location and scale"))

    preText <- paste0("Size class: ", rv$sizeclass_CP)
    if (length(rv$sizeclasses_CP) == 1){
      preText <- ""
    }    
    scText <- renderText(preText)
    output$sizeclass_CP1 <- scText
    output$sizeclass_CP2 <- scText
    output$sizeclass_CP3 <- scText

    if (length(rv$sizeclasses_CP) == 1){
      output$sizeclass_CPyn <- renderText("NO")
    } else{
      output$sizeclass_CPyn <- renderText("YES")
    }
    outputOptions(output, "sizeclass_CPyn", suspendWhenHidden = FALSE)


    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP, 
                                          rv$csvformat)
    output$dlCPAICc <- downloadTable("CP_AICc.csv", rv$AICcTab_CP, 
                                          rv$csvformat)
    output$dlCPfig <- downloadCPFig(rv)
  }

  outputOptions(output, "text_CP_est", suspendWhenHidden = FALSE)

  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

  output$table_g <- NULL
  output$fig_g <- NULL
  output$gModDone <- NULL
  output$sizeclass_gyn <- NULL
  output$sizeclass_g1 <- NULL
  output$sizeclass_g2 <- NULL

  outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
  outputOptions(output, "table_g", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)

  return(output)
}

#' @title Update the output list when CP model is cleared
#'
#' @description Update the output list when the CP model is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_CP_clear <- function(rv, output){
  output$CPModDone <- NULL
  output$AICcTab_CP <- NULL
  output$modTab_CP <- NULL
  output$fig_CP <- NULL
  output$sizeclasses_CP <- NULL
  output$modelMenu_CP <- NULL
  output$sizeclass_CP1 <- NULL
  output$sizeclass_CP2 <- NULL
  output$sizeclass_CP3 <- NULL
  output$sizeclass_CPyn <- NULL
  output$dlCPest <- NULL
  output$dlCPAICc <- NULL
  output$dlCPfig <- NULL
  outputOptions(output, "AICcTab_CP", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_CP", suspendWhenHidden = FALSE)
  outputOptions(output, "modTab_CP", suspendWhenHidden = FALSE)
  outputOptions(output, "CPModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclasses_CP", suspendWhenHidden = FALSE)
  output$text_CP_est <- NULL
  outputOptions(output, "text_CP_est", suspendWhenHidden = FALSE)

  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)

  output$table_g <- NULL
  output$fig_g <- NULL
  output$gModDone <- NULL
  output$sizeclass_gyn <- NULL
  output$sizeclass_g1 <- NULL
  output$sizeclass_g2 <- NULL

  outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
  outputOptions(output, "table_g", suspendWhenHidden = FALSE)

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
#' @return an updated output list
#'
#' @export
#'
update_output_outCPclass <- function(rv, output){
  if (length(rv$mods_CP) > 0){
    output$modTab_CP <- renderDataTable(datatable(rv$modTabPretty_CP),
      server = FALSE)
    output$fig_CP <- renderPlot({ 
                       plot(rv$modSet_CP, specificModel = rv$best_CP, 
                         app = TRUE)
                     }, height = rv$figH_CP, width = rv$figW_CP)

    preText <- paste0("Size class: ", rv$sizeclass_CP)
    if (length(rv$sizeclasses_CP) == 1){
      preText <- ""
    }    
    scText <- renderText(preText)
    output$sizeclass_CP1 <- scText
    output$sizeclass_CP2 <- scText
    output$sizeclass_CP3 <- scText

    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP, 
                                          rv$csvformat)
    output$dlCPAICc <- downloadTable("CP_AICc.csv", rv$AICcTab_CP, 
                                          rv$csvformat)
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
#' @return an updated output list
#'
#' @export
#'
update_output_outCPdist <- function(rv, output){
  if (length(rv$mods_CP) > 0){
    output$modTab_CP <- renderDataTable(datatable(rv$modTabPretty_CP),
      server = FALSE)
    output$fig_CP <- renderPlot({ 
                     tryCatch(
                       plot(rv$modSet_CP, specificModel = rv$outCPdlsfig, 
                         app = TRUE),
                       error = function(x){plotNA()}
                     )
                     }, height = rv$figH_CP, width = rv$figW_CP)
    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP,
                                           rv$csvformat)
    output$dlCPfig <- downloadCPFig(rv)

    if (!is.null(rv$modTab_CP)){
      output$modTab_CP <- renderDataTable({rv$modTabPretty_CP})
      output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP,
                                           rv$csvformat)
    }

  }
  return(output)
}

update_output_outCPl <- function(rv, output){
  if (length(rv$mods_CP) > 0){
    output$modTab_CP <- renderDataTable(datatable(rv$modTabPretty_CP),
      server = FALSE)
    output$fig_CP <- renderPlot({ 
                     tryCatch(
                       plot(rv$modSet_CP, specificModel = rv$outCPdlsfig, 
                         app = TRUE),
                       error = function(x){plotNA()}
                     )
                     }, height = rv$figH_CP, width = rv$figW_CP)
    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP,
                                           rv$csvformat)
    output$dlCPfig <- downloadCPFig(rv)

    if (!is.null(rv$modTab_CP)){
      output$modTab_CP <- renderDataTable({rv$modTabPretty_CP})
      output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP,
                                           rv$csvformat)
    }

  }
  return(output)
}

update_output_outCPs <- function(rv, output){
  if (length(rv$mods_CP) > 0){
    output$modTab_CP <- renderDataTable(datatable(rv$modTabPretty_CP),
      server = FALSE)
    output$fig_CP <- renderPlot({ 
                     tryCatch(
                       plot(rv$modSet_CP, specificModel = rv$outCPdlsfig, 
                         app = TRUE),
                       error = function(x){plotNA()}
                     )
                     }, height = rv$figH_CP, width = rv$figW_CP)
    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP,
                                           rv$csvformat)
    output$dlCPfig <- downloadCPFig(rv)

    if (!is.null(rv$modTab_CP)){
      output$modTab_CP <- renderDataTable({rv$modTabPretty_CP})
      output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP,
                                           rv$csvformat)
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
#' @return an updated output list
#'
#' @export
#'
update_output_useSSdata <- function(rv, output){
  output$SStext <- renderText(rv$SStext)
  return(output)
}

update_output_useSSinputs <- function(rv, output){
  output$SStext <- renderText(rv$SStext)
  return(output)
}

#' @title Update the output when a generic g model has been run
#'
#' @description Update the output table when a generic g model has been run
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_g <- function(rv, output){

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

    if (length(rv$sizeclasses_SE) == 1){
      output$sizeclass_gyn <- renderText("NO")
    } else{
      output$sizeclass_gyn <- renderText("YES")
    }
    outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)

    preText <- paste0("Size class: ", rv$sizeclass_g)
    if (length(rv$sizeclasses_g) == 1){
      preText <- ""
    }    
    scText <- renderText(preText)
    output$sizeclass_g1 <- scText
    output$sizeclass_g2 <- scText

    output$dlgtab <- downloadTable("g_estimates.csv", summaryTab, 
                                          rv$csvformat)
    output$dlgfig <- downloadgFig(rv, 1)
  }
  return(output)
}


#' @title Update the output list when g model is cleared
#'
#' @description Update the output list when the g model is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_g_clear <- function(rv, output){
  output$table_g <- NULL
  output$fig_g <- NULL
  output$gModDone <- NULL
  output$sizeclass_gyn <- NULL
  output$sizeclass_g1 <- NULL
  output$sizeclass_g2 <- NULL
  outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
  outputOptions(output, "table_g", suspendWhenHidden = FALSE)
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
#' @return an updated output list
#'
#' @export
#'
update_output_outgclass <- function(rv, output){

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

    preText <- paste0("Size class: ", rv$sizeclass_g)
    if (length(rv$sizeclasses_g) == 1){
      preText <- ""
    }    
    scText <- renderText(preText)
    output$sizeclass_g1 <- scText
    output$sizeclass_g2 <- scText

    output$dlgtab <- downloadTable("g_estimates.csv", summaryTab,
                                           rv$csvformat)
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
#' @return an updated output list
#'
#' @export
#'
update_output_run_M <- function(rv, output){

  if (!is.null(rv$Msplit)){
    output$MModDone <- renderText("OK")
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

    output$fig_M <- renderPlot({plot(rv$Msplit, CL = rv$CL)},
                      height = rv$figH_M, width = rv$figW_M
                    )
    summaryTab <-  prettySplitTab(summary(rv$Msplit, CL = rv$CL))
    output$table_M <- renderDataTable(datatable(summaryTab))
    output$dlMtab <- downloadTable("M_table.csv", summaryTab, rv$csvformat)
    output$dlMfig <- downloadMFig(rv)
  }
  return(output)
}

#' @title Update the output list when M model is cleared
#'
#' @description Update the output list when the M model is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_M_clear <- function(rv, output){
  output$fig_M <- NULL
  output$table_M <- NULL
  output$MModDone <- NULL
  outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
  outputOptions(output, "fig_M", suspendWhenHidden = FALSE)

  return(output)
}

#' @title Update the output when M has been split
#'
#' @description Update the output table when M has been split
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_split_M <- function(rv, output){

  if (is.null(rv$Msplit)){
    output$fig_M <- renderPlot({
                      tryCatch(plot(rv$M, CL = rv$CL),
                        error = function(x){plotNA()}
                      )
                    }, height = rv$figH_M, width = rv$figW_M)
    output$dlMfig <- downloadMFig(rv, split = FALSE)

  } else{
    output$fig_M <- renderPlot({
                      tryCatch(plot(rv$Msplit, CL = rv$CL),
                        error = function(x){plotNA("split")}
                      )
                    }, height = rv$figH_M, width = rv$figW_M
                    )
    summaryTab <-  prettySplitTab(summary(rv$Msplit, CL = rv$CL))
    output$table_M <- renderDataTable(datatable(summaryTab))
    output$dlMtab <- downloadTable("M_table.csv", summaryTab,
                                           rv$csvformat)
    output$dlMfig <- downloadMFig(rv)
  }
  output$MSplitDone <- renderText("OK")
  outputOptions(output, "MSplitDone", suspendWhenHidden = FALSE)
  output$nMSplits <- renderText(
                       as.character(rv$nsplit_CO + rv$nsplit_SS))
  outputOptions(output, "nMSplits", suspendWhenHidden = FALSE)
  return(output)
}



#' @title Update the output list when M split is cleared
#'
#' @description Update the output list when the M split is cleared
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_split_M_clear <- function(rv, output){

  output <- update_output_run_M(rv, output)
  output$MSplitDone <- NULL
  outputOptions(output, "MSplitDone", suspendWhenHidden = FALSE)
  output$nMSplits <- renderText(as.character(0))
  outputOptions(output, "nMSplits", suspendWhenHidden = FALSE)
  return(output)
}

#' @title Update the output when the M split is transposed
#'
#' @description Update the output when the M split is transposed
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_transpose_split <- function(rv, output){

  if (!is.null(rv$Msplit)){
      output$fig_M <- renderPlot({
                        tryCatch(plot(rv$Msplit, CL = rv$CL),
                          error = function(x){plotNA("split")}
                        )
                      }, height = rv$figH_M, width = rv$figW_M
                      )
      output$dlMfig <- downloadMFig(rv, TRUE, TRUE)

  }
  return(output)
}
