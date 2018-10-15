#' @title Update the outputs when an event occurs
#'
#' @description When an event occurs in the GenEst GUI, the output values may
#'   need to be updated. This function contains all of the possible updates
#'   based on the event options (or lacks any updates if the event doesn't
#'   require any).
#'
#' @param eventName Character name of the event. One of "clear_all",
#'   "file_SE", "file_SE_clear", "file_CP", "file_CP_clear", "file_SS",
#'   "file_SS_clear", "file_DWP", "file_DWP_clear", "file_CO", 
#'   "file_CO_clear", "class", "obsSE", "predsSE", "run_SE", "run_SE_clear",
#'   "outSEclass", "outSEp", "outSEk", "ltp", "fta", "predsCP", "run_CP",
#'   "run_CP_clear", "outCPclass", "outCPdist", "outCPl", "outCPs",
#'   "run_M", "run_M_clear", "split_M", "split_M_clear", "transpose_split",
#'   "useSSdata", "useSSinputs", "run_g", "run_g_clear", or "outgclass".
#'
#' @param rv Reactive values list for the GenEst GUI.
#'
#' @param output \code{output} list for the GenEst GUI.
#'
#' @return Updated \code{output} list.
#'
#' @export
#'
update_output <- function(eventName, rv, output){

  eventOptions <- c("clear_all", "file_SE", "file_SE_clear", "file_CP",
                    "file_CP_clear", "file_SS", "file_SS_clear", "file_DWP",
                    "file_DWP_clear", "file_CO", "file_CO_clear", "class",
                    "obsSE", "predsSE", "run_SE", "run_SE_clear",
                    "outSEclass", "outSEp", "outSEk", "ltp", "fta", "predsCP",
                    "run_CP", "run_CP_clear", "outCPclass", "outCPdist",
                    "outCPl", "outCPs", "run_M", "run_M_clear", "split_M",
                    "split_M_clear", "transpose_split", "useSSdata",
                    "useSSinputs", "run_g", "run_g_clear", "outgclass")

  if (missing(eventName) || (eventName %in% eventOptions) == FALSE){
    stop("eventName missing or not in list of available eventNames")
  }

  if (eventName == "clear_all"){

    toNULL <- c("data_SE", "filename_SE", "selected_SE", "SEModDone", 
                "DWPNeed", "AICcTab_SE", "modTab_SE", "fig_SE", 
                "sizeclasses_SE", "modelMenu_SE", "sizeclass_SE1", 
                "sizeclass_SE2", "sizeclass_SE3", "sizeclass_SEyn",
                "text_SE_est", "data_CP", "filename_CP", "selected_CP", 
                "CPModDone", "AICcTab_CP", "modTab_CP", "fig_CP", 
                "sizeclasses_CP", "modelMenu_CP", "sizeclass_CP1", 
                "sizeclass_CP2", "sizeclass_CP3", "sizeclass_CPyn",
                "text_CP_est", "fig_M", "table_M", "MModDone", "table_g", 
                "fig_g", "gModDone", "sizeclass_gyn", "sizeclass_g1", 
                "sizeclass_g2", "data_SS", "data_DWP", "data_CO")
    output <- reNULL(output, toNULL)
    output$kFillNeed <- setkFillNeed(rv)

    outputOptions(output, "AICcTab_SE", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_SE", suspendWhenHidden = FALSE)
    outputOptions(output, "modTab_SE", suspendWhenHidden = FALSE)
    outputOptions(output, "SEModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "sizeclasses_SE", suspendWhenHidden = FALSE)
    outputOptions(output, "filename_SE", suspendWhenHidden = FALSE)
    outputOptions(output, "text_SE_est", suspendWhenHidden = FALSE)
    outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
    outputOptions(output, "table_g", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
    outputOptions(output, "filename_SE", suspendWhenHidden = FALSE)
    outputOptions(output, "filename_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "text_CP_est", suspendWhenHidden = FALSE)
    outputOptions(output, "AICcTab_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "modTab_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "CPModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "sizeclasses_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "data_SS", suspendWhenHidden = FALSE)
    outputOptions(output, "data_DWP", suspendWhenHidden = FALSE)
    outputOptions(output, "data_CO", suspendWhenHidden = FALSE)
  }


  if (eventName == "file_SE" | eventName == "file_SE_clear"){

    toNULL <- c("data_SE", "filename_SE", "selected_SE", "SEModDone", 
                "DWPNeed", "AICcTab_SE", "modTab_SE", "fig_SE", 
                "sizeclasses_SE", "modelMenu_SE", "sizeclass_SE1", 
                "sizeclass_SE2", "sizeclass_SE3", "sizeclass_SEyn",
                "text_SE_est", "fig_M", "table_M", "MModDone", "table_g", 
                "fig_g", "gModDone", "sizeclass_gyn", "sizeclass_g1", 
                "sizeclass_g2")
    output <- reNULL(output, toNULL)
    output$kFillNeed <- setkFillNeed(rv)
    if (eventName == "file_SE"){
      output$data_SE <- renderDataTable(datatable(rv$data_SE), server = FALSE)
      output$filename_SE <- renderText(paste0("File: ", rv$filename_SE))
    }

    dontSuspend <- c("AICcTab_SE", "fig_SE", "modTab_SE", "SEModDone", 
                     "sizeclasses_SE", "filename_SE", "text_SE_est", 
                     "kFillNeed", "MModDone", "gModDone",
                     "sizeclass_gyn", "fig_g", "table_g", "fig_M", 
                     "filename_SE")
    resetNotSuspending(output, dontSuspend)
  }

  if (eventName == "file_CP" | eventName == "file_CP_clear"){

    toNULL <- c("data_CP", "filename_CP", "selected_CP", "CPModDone", 
                "AICcTab_CP", "modTab_CP", "fig_CP", 
                "sizeclasses_CP", "modelMenu_CP", "sizeclass_CP1", 
                "sizeclass_CP2", "sizeclass_CP3", "sizeclass_CPyn",
                "text_CP_est", "fig_M", "table_M", "MModDone", "table_g", 
                "fig_g", "gModDone", "sizeclass_gyn", "sizeclass_g1", 
                "sizeclass_g2")
    output <- reNULL(output, toNULL)
    if (eventName == "file_CP"){
      output$data_CP <- renderDataTable(datatable(rv$data_CP), server = FALSE)
      output$filename_CP <- renderText(paste0("File: ", rv$filename_CP))
    }

    outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
    outputOptions(output, "table_g", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
    outputOptions(output, "filename_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "text_CP_est", suspendWhenHidden = FALSE)
    outputOptions(output, "AICcTab_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "modTab_CP", suspendWhenHidden = FALSE)
    outputOptions(output, "CPModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "sizeclasses_CP", suspendWhenHidden = FALSE)
  }

  if (eventName == "file_SS" | eventName == "file_SS_clear"){

    toNULL <- c("data_SS", "fig_M", "table_M", "MModDone", "table_g", 
                "fig_g", "gModDone", "sizeclass_gyn", "sizeclass_g1", 
                "sizeclass_g2")
    output <- reNULL(output, toNULL)
    if (eventName == "file_SS"){
      output$data_SS <- renderDataTable(datatable(rv$data_SS), server = FALSE)
    }

    outputOptions(output, "data_SS", suspendWhenHidden = FALSE)
    outputOptions(output, "gModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "sizeclass_gyn", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_g", suspendWhenHidden = FALSE)
    outputOptions(output, "table_g", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

  }

  if (eventName == "file_DWP" | eventName == "file_DWP_clear"){

    toNULL <- c("data_DWP", "fig_M", "table_M", "MModDone")
    output <- reNULL(output, toNULL)
    if (eventName == "file_DWP"){
      output$data_DWP <- renderDataTable(datatable(rv$data_DWP), 
                           server = FALSE)
    }

    outputOptions(output, "data_DWP", suspendWhenHidden = FALSE)
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
    
  }

  if (eventName == "file_CO" | eventName == "file_CO_clear"){

    toNULL <- c("data_CO", "fig_M", "table_M", "MModDone")
    output <- reNULL(output, toNULL)
    if (eventName == "file_CO"){
      output$data_CO <- renderDataTable(datatable(rv$data_CO), server = FALSE)
    }

    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "data_CO", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_M", suspendWhenHidden = FALSE)
  }


  if (eventName == "class"){
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

    output$kFixedInput <- kFixedWidget(rv$sizeclasses_k)
    
  }

  if (eventName == "obsSE"){
    selectedCols <- c(rv$obsCols_SE, rv$sizeCol, rv$preds_SE)
    if (!is.null(rv$data_SE)){
      selectedData <- selectData(rv$data_SE, selectedCols)
      output$selected_SE <- renderDataTable(datatable(selectedData), 
                                            server = FALSE)
    }
    
  }

  if (eventName == "predsSE"){
    selectedCols <- c(rv$obsCols_SE, rv$sizeCol, rv$preds_SE)
    if (!is.null(rv$data_SE)){
      selectedData <- selectData(rv$data_SE, selectedCols)
      output$selected_SE <- renderDataTable(datatable(selectedData), 
                                            server = FALSE)
    }
    
  }

  if (eventName == "fta"){
    selectedCols <- c(rv$ltp, rv$fta, rv$sizeCol, rv$preds_CP)
    selectedData <- selectData(rv$data_CP, selectedCols)
    output$selected_CP <- renderDataTable(datatable(selectedData), 
                                            server = FALSE)
    
  }

  if (eventName == "ltp"){
    selectedCols <- c(rv$ltp, rv$fta, rv$sizeCol, rv$preds_CP)
    selectedData <- selectData(rv$data_CP, selectedCols)
    output$selected_CP <- renderDataTable(datatable(selectedData), 
                                            server = FALSE)
    
  }

  if (eventName == "predsCP"){
    selectedCols <- c(rv$ltp, rv$fta, rv$sizeCol, rv$preds_CP)
    selectedData <- selectData(rv$data_CP, selectedCols)
    output$selected_CP <- renderDataTable(datatable(selectedData), 
                                            server = FALSE)
    
  }

  if (eventName == "run_SE"){
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


    
  }

  if (eventName == "run_SE_clear"){
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

    
  }

  if (eventName == "outSEclass"){
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
    
  }

  if (eventName == "outSEp" | eventName == "outSEk"){
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
    
  }

  if (eventName == "run_CP"){

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

    
  }

  if (eventName == "run_CP_clear"){
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
  
    
  }

  if (eventName == "outCPclass"){
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
    
  }

  if (eventName %in% c("outCPdist", "outCPl", "outCPs")){
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
    
  }

  if (eventName == "useSSdata" | eventName == "useSSinputs"){
    output$SStext <- renderText(rv$SStext)
    
  }

  if (eventName == "run_g"){

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
    
  }

  if (eventName == "run_g_clear"){
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
    
  }

  if (eventName == "outgclass"){

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
    
  }

  if (eventName == "run_M"){

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
    
  }

  if (eventName == "run_M_clear"){
    output$fig_M <- NULL
    output$table_M <- NULL
    output$MModDone <- NULL
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "fig_M", suspendWhenHidden = FALSE)

    
  }

  if (eventName == "split_M"){

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
    
  }

  if (eventName == "split_M_clear"){

# run_M
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

    output$MSplitDone <- NULL
    outputOptions(output, "MSplitDone", suspendWhenHidden = FALSE)
    output$nMSplits <- renderText(as.character(0))
    outputOptions(output, "nMSplits", suspendWhenHidden = FALSE)
    
  }

  if (eventName == "transpose_split"){

    if (!is.null(rv$Msplit)){
        output$fig_M <- renderPlot({
                          tryCatch(plot(rv$Msplit, CL = rv$CL),
                            error = function(x){plotNA("split")}
                          )
                        }, height = rv$figH_M, width = rv$figW_M
                        )
        output$dlMfig <- downloadMFig(rv, TRUE, TRUE)

    }
    
  }
  return(output)
}