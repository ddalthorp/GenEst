#' Create the UI
#' @param input server input
#' @param output server output
#' @param session server session
#' @return server setup
#' @export
#'
server <- function(input, output, session) {

vnumber <- packageDescription("GenEst", fields = "Version")
vdate <- packageDescription("GenEst", fields = "Date")
vtext <- paste("This is version ", vnumber, " (", vdate, ")", sep = "")
output$versionInfo <- renderText(vtext)

disclaimer <- paste("GenEst v", vnumber, " (", vdate, ")", sep = "")
showModal(modalDialog(title = disclaimer, 
  "This software is preliminary or provisional and is subject to revision. 
  It is being provided to meet the need for timely best science. The 
  software has not received final approval by the U.S. Geological Survey 
  (USGS). No warranty, expressed or implied, is made by the USGS or the U.S.
  Government as to the functionality of the software and related material 
  nor shall the fact of release constitute any such warranty. The software 
  is provided on the condition that neither the USGS nor the U.S. 
  Government shall be held liable for any damages resulting from the 
  authorized or unauthorized use of the software.",  
  easyClose = FALSE, footer = modalButton("OK"))
)

rv <- reactiveValues(

        data_SE = NULL, colNames_SE = NULL, obsCols_SE = NULL, 
        preds_SE = NULL, kFixedChoice = 0, kFixed = NULL,  
        predictors_SE = NULL, formula_p = NULL, formula_k = NULL, 
        mods_SE = NULL, modNames_SE = NULL, modNames_SEp = NULL, 
        modNames_SEk = NULL, modTab_SE = NULL, AICcTab_SE = NULL,
        modOrder_SE = NULL, modSet_SE = NULL, best_SE = NULL, 
        figH_SE = 800, figW_SE = 800,  
        sizeclasses_SE = NULL, tabfig_SEpk = NULL,

        data_CP = NULL, colNames_CP = NULL, ltp = NULL, fta = NULL, 
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
        dateSearchedCol_g = NULL, gSearchInterval = NULL, gSearchMax = NULL, 

        data_DWP = NULL, colNames_DWP = NULL,
        data_CO = NULL, colNames_CO = NULL, 


        sizeclassCol = NULL, sizeclasses = NULL, sizeclass = NULL,
        colNames_all = NULL, nsizeclasses = NULL,

        kFill = NULL, kFill_g = NULL, unitCol = NULL, 
        dateFoundCol = NULL, dateSearchedCol = NULL, sizeclassCol_M = NULL,
        SEmodToUse = NULL, CPmodToUse = NULL, M = NULL,
        models_SE = NULL, models_CP = NULL, DWPCol = NULL, 
        CL = 0.9, n = 1000, SEmodToUse_g = NULL, CPmodToUse_g = NULL, 
        rghatsGeneric = NULL

      )

msg_RunModSE <- NULL
msg_NobsSE <- NULL
msg_ModFailSE <- NULL
msg_SampleSizeSE <- NULL
msg_RunModCP <- NULL
msg_ModFailCP <- NULL
msg_SampleSizeCP <- NULL
msg_avgSSfail <- NULL
msg_RunModg <- NULL
msg_RunModM <- NULL
msg_ModFailM <- NULL

output$SStext <- renderText(rv$SStext)

observeEvent(input$file_SE, {
  rv$data_SE <- read.csv(input$file_SE$datapath, stringsAsFactors = FALSE)
  rv$colNames_SE <- colnames(rv$data_SE)
  rv$colNames_all <- updateColNames_all(rv$colNames_SE, rv$colNames_CP,
                       rv$colNames_CO
                     )
  rv$sizeclassCol <- updateSizeclassCol(rv$sizeclassCol, rv$colNames_all)
  output$data_SE <- renderDataTable(datatable(rv$data_SE))
  updateTabsetPanel(session, "LoadedDataViz", "Search Efficiency")
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE)
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
})
observeEvent(input$file_CP, {
  rv$data_CP <- read.csv(input$file_CP$datapath, stringsAsFactors = FALSE)
  rv$colNames_CP <- colnames(rv$data_CP)
  rv$colNames_all <- updateColNames_all(rv$colNames_SE, rv$colNames_CP,
                       rv$colNames_CO
                     )
  rv$sizeclassCol <- updateSizeclassCol(rv$sizeclassCol, rv$colNames_all)
  output$data_CP <- renderDataTable(datatable(rv$data_CP))
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Persistence")
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_CP)
  updateSelectizeInput(session, "fta", choices = rv$colNames_CP)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
})
observeEvent(input$file_SS, {
  rv$data_SS <- read.csv(input$file_SS$datapath, stringsAsFactors = FALSE)
  rv$colNames_SS <- colnames(rv$data_SS)
  output$data_SS <- renderDataTable(datatable(rv$data_SS))
  updateTabsetPanel(session, "LoadedDataViz", "Search Schedule")
  updateSelectizeInput(session, "dateSearchedCol", choices = rv$colNames_SS)
  updateSelectizeInput(session, "dateSearchedCol_g", choices = rv$colNames_SS)
})
observeEvent(input$file_DWP, {
  rv$data_DWP <- read.csv(input$file_DWP$datapath, stringsAsFactors = FALSE)
  rv$colNames_DWP <- colnames(rv$data_DWP)
  output$data_DWP<- renderDataTable(datatable(rv$data_DWP))
  updateTabsetPanel(session, "LoadedDataViz", "Density Weighted Proportion")
  updateSelectizeInput(session, "DWPCol", choices = rv$colNames_DWP)
})
observeEvent(input$file_CO, {
  rv$data_CO <- read.csv(input$file_CO$datapath, stringsAsFactors = FALSE)
  rv$colNames_CO <- colnames(rv$data_CO)
  rv$colNames_all <- updateColNames_all(rv$colNames_SE, rv$colNames_CP,
                       rv$colNames_CO
                     )
  rv$sizeclassCol <- updateSizeclassCol(rv$sizeclassCol, rv$colNames_all)
  output$data_CO <- renderDataTable(datatable(rv$data_CO))
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")
  updateSelectizeInput(session, "splitCol", choices = rv$colNames_CO)
  updateSelectizeInput(session, "unitCol", choices = rv$colNames_CO)
  updateSelectizeInput(session, "dateFoundCol", choices = rv$colNames_CO)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
})

observeEvent(input$obsCols_SE, {
  selectedCols <- c(input$obsCols_SE, input$sizeclassCol, input$preds_SE)
  selectedData <- selectData(rv$data_SE, selectedCols)
  output$selected_SE <- renderDataTable(datatable(selectedData))
})
observeEvent(input$preds_SE, {
  selectedCols <- c(input$obsCols_SE, input$sizeclassCol, input$preds_SE)
  selectedData <- selectData(rv$data_SE, selectedCols)
  output$selected_SE <- renderDataTable(datatable(selectedData))
})

observeEvent(input$runMod_SE, {

  msg_RunModSE <- msgModRun("SE")

  rv$obsCols_SE <- input$obsCols_SE
  rv$preds_SE <- input$preds_SE
  rv$kFixed <- setkFix(input$kFixedChoice, input$kFixed)
  rv$n <- input$n
  rv$CL <- input$CL
  rv$sizeclassCol <- input$sizeclassCol
  rv$kFixedChoice <- input$kFixedChoice
  rv$predictors_SE <- prepPredictors(rv$preds_SE)
  rv$formula_p <- formula(paste("p~", rv$predictors_SE, sep = ""))
  rv$formula_k <- formula(paste("k~", rv$predictors_SE, sep = "")) 

  rv$mods_SE <- suppressWarnings(
                  pkmSetSize(formula_p = rv$formula_p,
                    formula_k = rv$formula_k, data = rv$data_SE, 
                    obsCol = rv$obsCols_SE, sizeclassCol = rv$sizeclassCol,
                    kFixed = rv$kFixed, kInit = 0.7, CL = rv$CL, 
                    quiet = TRUE
                  ) 
                ) 
  if (all(unlist(pkmSetSizeFail(rv$mods_SE)))){

    removeNotification(msg_RunModSE)
    msg_ModFailSE <- msgModFail(rv$mod_SE)

  } else{

    if (any(unlist(pkmSetSizeFail(rv$mods_SE)))){
      msg_ModFailSE <- msgModPartialFail()
      rv$mods_SE <- pkmSetSizeFailRemove(rv$mods_SE)
    }
    removeNotification(msg_RunModSE)
    msg_SampleSizeSE <- msgSampleSize(rv$mod_SE)
    msg_NobsSE <- msgNobsSE(rv$formula_k, rv$kFixed, rv$obsCols_SE)

    rv$sizeclasses <- updateSizeclasses(rv$data_SE, rv$sizeclassCol)
    rv$sizeclasses_SE <- rv$sizeclasses
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$tabfig_sizeclassSE)
    rv$AICcTab_SE <- pkmSetAICcTab(rv$mods_SE[[rv$sizeclass]], TRUE)
    rv$modOrder_SE <- as.numeric(row.names(rv$AICcTab_SE))
    rv$modNames_SE <- names(rv$mods_SE[[rv$sizeclass]])[rv$modOrder_SE]
    rv$modNames_SEp <- modNameSplit(rv$modNames_SE, 1)
    rv$modNames_SEk <- modNameSplit(rv$modNames_SE, 2)
    rv$modSet_SE <- rv$mods_SE[[rv$sizeclass]]
    rv$best_SE <- (names(rv$modSet_SE)[rv$modOrder_SE])[1]
    rv$modTab_SE <- rv$mods_SE[[rv$sizeclass]][[rv$best_SE]]$cellwiseTable
    rv$modTab_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
    rv$figH_SE <- setFigH(rv$modSet_SE, 800)
    rv$figW_SE <- setFigW(rv$modSet_SE)

    output$kFillNeed <- setkFillNeed(rv$obsCols_SE)
    output$AICcTab_SE <- renderDataTable({rv$AICcTab_SE})    
    output$modTab_SE <- renderDataTable({rv$modTab_SE})
    output$fig_SE <- renderPlot({ 
                       plot(rv$modSet_SE, specificModel = rv$best_SE,
                         sizeclassName = rv$sizeclass
                       )},
                       height = rv$figH_SE, width = rv$figW_SE
                     )
    outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)
    isolate({
      output$sizeclasses_SE <- prepSizeclassText(rv$sizeclasses_SE)
      outputOptions(output, "sizeclasses_SE", suspendWhenHidden = FALSE)
      output$modelMenu_SE <- makeMenu(rv$mods_SE, rv$sizeclasses_SE, "SE")
    })

    updateTabsetPanel(session, "analyses_SE", "Model Comparison")
    updateSelectizeInput(session, "tabfig_SEp", choices = rv$modNames_SEp)
    updateSelectizeInput(session, "tabfig_SEk", choices = rv$modNames_SEk)
    updateSelectizeInput(session, "tabfig_sizeclassSE", 
      choices = rv$sizeclasses
    )
    if (rv$kFixedChoice == 1){
      updateNumericInput(session, "kFill", value = rv$kFixed)
    }
    if (length(rv$sizeclasses) == 1){
      output$DWPNeed <- renderText("yes")
    } else{
      output$DWPNeed <- renderText("no")
    }
    outputOptions(output, "DWPNeed", suspendWhenHidden = FALSE)

  }
})

observeEvent(input$tabfig_sizeclassSE, {
  if (length(rv$mods_SE) > 0){
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$tabfig_sizeclassSE)
    rv$AICcTab_SE <- pkmSetAICcTab(rv$mods_SE[[rv$sizeclass]], TRUE)
    rv$modOrder_SE <- as.numeric(row.names(rv$AICcTab_SE))
    rv$modNames_SE <- names(rv$mods_SE[[rv$sizeclass]])[rv$modOrder_SE]
    rv$modNames_SEp <- modNameSplit(rv$modNames_SE, 1)
    rv$modNames_SEk <- modNameSplit(rv$modNames_SE, 2)
    rv$modSet_SE <- rv$mods_SE[[rv$sizeclass]]
    rv$best_SE <- (names(rv$modSet_SE)[rv$modOrder_SE])[1]
    rv$modTab_SE <- rv$mods_SE[[rv$sizeclass]][[rv$best_SE]]$cellwiseTable
    rv$modTab_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
    rv$figH_SE <- setFigH(rv$modSet_SE, 800)
    rv$figW_SE <- setFigW(rv$modSet_SE)

    output$modTab_SE <- renderDataTable(rv$modTab_SE)
    output$fig_SE <- renderPlot({ 
                       plot(rv$modSet_SE, specificModel = rv$best_SE,
                         sizeclassName = rv$sizeclass
                       )},
                       height = rv$figH_SE, width = rv$figW_SE
                     )
    updateSelectizeInput(session, "tabfig_SEp", choices = rv$modNames_SEp)
    updateSelectizeInput(session, "tabfig_SEk", choices = rv$modNames_SEk)

    observeEvent(input$tabfig_SEp, {
      rv$tabfig_SEpk <- modNamePaste(c(input$tabfig_SEp, input$tabfig_SEk))
      rv$modSet_SE <- rv$mods_SE[[rv$sizeclass]]
      rv$modTab_SE <- rv$modSet_SE[[rv$tabfig_SEpk]]$cellwiseTable
      rv$modTab_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
      output$modTab_SE <- renderDataTable(rv$modTab_SE)
      output$fig_SE <- renderPlot({
                         tryCatch(
                           plot(rv$modSet_SE,
                             specificModel = rv$tabfig_SEpk,
                             sizeclassName = rv$sizeclass
                           ), error = function(x){plot(1,1)}
                         )}, 
                       height = rv$figH_SE, width = rv$figW_SE
                       )
    })
    observeEvent(input$tabfig_SEk, {
      rv$tabfig_SEpk <- modNamePaste(c(input$tabfig_SEp, input$tabfig_SEk))
      rv$modSet_SE <- rv$mods_SE[[rv$sizeclass]]
      rv$modTab_SE <- rv$modSet_SE[[rv$tabfig_SEpk]]$cellwiseTable
      rv$modTab_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
      output$modTab_SE <- renderDataTable(rv$modTab_SE)
      output$fig_SE <- renderPlot({
                         tryCatch(
                           plot(rv$modSet_SE, 
                             specificModel = rv$tabfig_SEpk,
                             sizeclassName = rv$sizeclass
                           ), error = function(x){plot(1,1)}
                         )}, 
                       height = rv$figH_SE, width = rv$figW_SE
                       )
    })
    rv$AICcTab_SE <- pkmSetAICcTab(rv$mods_SE[[rv$sizeclass]], TRUE)
    output$AICcTab_SE <- renderDataTable(datatable(rv$AICcTab_SE))
  }
})

observeEvent(input$ltp, {
  obsColsSelected <- c(input$ltp, input$fta)
  selectedCols <- c(obsColsSelected, input$sizeclassCol, input$preds_CP)
  selectedData <- selectData(rv$data_CP, selectedCols)
  output$selected_CP <- renderDataTable(datatable(selectedData))
})
observeEvent(input$fta, {
  obsColsSelected <- c(input$ltp, input$fta)
  selectedCols <- c(obsColsSelected, input$sizeclassCol, input$preds_CP)
  selectedData <- selectData(rv$data_CP, selectedCols)
  output$selected_CP <- renderDataTable(datatable(selectedData))
})
observeEvent(input$preds_CP, {
  obsColsSelected <- c(input$ltp, input$fta)
  selectedCols <- c(obsColsSelected, input$sizeclassCol, input$preds_CP)
  selectedData <- selectData(rv$data_CP, selectedCols)
  output$selected_CP <- renderDataTable(datatable(selectedData))
})

observeEvent(input$runMod_CP, {

  msg_RunModCP <- msgModRun("CP")

  rv$ltp <- input$ltp
  rv$fta <- input$fta
  rv$preds_CP <- input$preds_CP
  rv$dists <- input$dists  
  rv$n <- input$n
  rv$CL <- input$CL
  rv$sizeclassCol <- input$sizeclassCol
  rv$predictors_CP <- prepPredictors(rv$preds_CP)
  rv$formula_l <- formula(paste("l~", rv$predictors_CP, sep = ""))
  rv$formula_s <- formula(paste("s~", rv$predictors_CP, sep = "")) 

  rv$mods_CP <- suppressWarnings(
                  cpmSetSize(formula_l = rv$formula_l,
                    formula_s = rv$formula_s, data = rv$data_CP, 
                    left = rv$ltp, right = rv$fta, dists = rv$dists,
                    sizeclassCol = rv$sizeclassCol, CL = rv$CL, quiet = TRUE
                  )
                )
  if (all(unlist(cpmSetSizeFail(rv$mods_CP)))){

    removeNotification(msg_RunModCP)
    msg_ModFailCP <- msgModFail(rv$mod_CP)

  } else{

    if (any(unlist(pkmSetSizeFail(rv$mods_CP)))){
      msg_ModFailCP <- msgModPartialFail()
      rv$mods_CP <- cpmSetSizeFailRemove(rv$mods_CP)
    }
    removeNotification(msg_RunModCP)
    msg_SampleSizeCP <- msgSampleSize(rv$mod_CP)

    rv$sizeclasses <- updateSizeclasses(rv$data_CP, rv$sizeclassCol)
    rv$sizeclasses_CP <- rv$sizeclasses
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$tabfig_sizeclassCP)
    rv$AICcTab_CP <- cpmSetAICcTab(rv$mods_CP[[rv$sizeclass]], TRUE)
    rv$AICcTab_CP[ , "Scale Formula"] <- gsub("NULL", "", 
                                       rv$AICcTab_CP[ , "Scale Formula"]
                                     )
    rv$modOrder_CP <- as.numeric(row.names(rv$AICcTab_CP))
    rv$modNames_CP <- names(rv$mods_CP[[rv$sizeclass]])[rv$modOrder_CP]
    rv$modNames_CPdist <- modNameSplit(rv$modNames_CP, 1)
    rv$modNames_CPl <- modNameSplit(rv$modNames_CP, 2)
    rv$modNames_CPs <- modNameSplit(rv$modNames_CP, 3)
    rv$modSet_CP <- rv$mods_CP[[rv$sizeclass]]
    rv$best_CP <- (names(rv$modSet_CP)[rv$modOrder_CP])[1]
    rv$modTab_CP <- rv$mods_CP[[rv$sizeclass]][[rv$best_CP]]$cellwiseTable_ls
    rv$modTab_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)

    rv$figH_CP <- setFigH(rv$modSet_CP, 700, "CP")
    rv$figW_CP <- setFigW(rv$modSet_CP)

    output$AICcTab_CP <- renderDataTable({rv$AICcTab_CP})    
    output$modTab_CP <- renderDataTable({rv$modTab_CP})
    rv$best_CP <- gsub("NULL", "s ~ 1", rv$best_CP)
    output$fig_CP <- renderPlot({ 
                       plot(rv$modSet_CP, specificModel = rv$best_CP,
                         sizeclassName = rv$sizeclass
                       )},
                       height = rv$figH_CP, width = rv$figW_CP
                     )
    isolate({
      output$sizeclasses_CP <- prepSizeclassText(rv$sizeclasses_CP)
      outputOptions(output, "sizeclasses_CP", suspendWhenHidden = FALSE)
      output$modelMenu_CP <- makeMenu(rv$mods_CP, rv$sizeclasses_CP, "CP")
    })

    updateTabsetPanel(session, "analyses_CP", "Model Comparison")
    updateSelectizeInput(session, "tabfig_CPl", choices = rv$modNames_CPl)
    updateSelectizeInput(session, "tabfig_CPs", choices = rv$modNames_CPs)
    updateSelectizeInput(session, "tabfig_CPd", 
      choices = rv$modNames_CPdist
    )
    updateSelectizeInput(session, "tabfig_sizeclassCP", 
      choices = rv$sizeclasses
    )

  }
})

observeEvent(input$tabfig_sizeclassCP, {
  if (length(rv$mods_CP) > 0){
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$tabfig_sizeclassCP)
    rv$AICcTab_CP <- cpmSetAICcTab(rv$mods_CP[[rv$sizeclass]], TRUE)
    rv$modOrder_CP <- as.numeric(row.names(rv$AICcTab_CP))
    rv$modNames_CP <- names(rv$mods_CP[[rv$sizeclass]])[rv$modOrder_CP]
    rv$modNames_CPdist <- modNameSplit(rv$modNames_CP, 1)
    rv$modNames_CPl <- modNameSplit(rv$modNames_CP, 2)
    rv$modNames_CPs <- modNameSplit(rv$modNames_CP, 3)
    rv$modSet_CP <- rv$mods_CP[[rv$sizeclass]]
    rv$best_CP <- (names(rv$modSet_CP)[rv$modOrder_CP])[1]
    rv$modTab_CP <- rv$mods_CP[[rv$sizeclass]][[rv$best_CP]]$cellwiseTable_ls
    rv$modTab_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
    rv$figH_CP <- setFigH(rv$modSet_CP, 700, "CP")
    rv$figW_CP <- setFigW(rv$modSet_CP)

    output$modTab_CP <- renderDataTable(rv$modTab_CP)
    rv$best_CP <- gsub("NULL", "s ~ 1", rv$best_CP)

    output$fig_CP <- renderPlot({ 
                       plot(rv$modSet_CP, specificModel = rv$best_CP,
                         sizeclassName = rv$sizeclass
                       )},
                       height = rv$figH_CP, width = rv$figW_CP
                     )

    updateSelectizeInput(session, "tabfig_CPl", choices = rv$modNames_CPl)
    updateSelectizeInput(session, "tabfig_CPs", choices = rv$modNames_CPs)
    updateSelectizeInput(session, "tabfig_CPd", 
      choices = rv$modNames_CPdist
    )

    observeEvent(input$tabfig_CPd,{
      rv$CPdls <- c(input$tabfig_CPd, input$tabfig_CPl, input$tabfig_CPs)
      rv$tabfig_CPdlsfig <- modNamePaste(rv$CPdls, "CP")
      rv$tabfig_CPdlstab <- modNamePaste(rv$CPdls, "CP", tab = TRUE)
      rv$modSet_CP <- rv$mods_CP[[rv$sizeclass]]
      rv$modTab_CP <- rv$modSet_CP[[rv$tabfig_CPdlstab]]$cellwiseTable_ls
      rv$modTab_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
      output$modTab_CP <- renderDataTable(rv$modTab_CP)
      output$fig_CP <- renderPlot({
                         tryCatch(
                           plot(rv$modSet_CP,
                             specificModel = rv$tabfig_CPdlsfig,
                             sizeclassName = rv$sizeclass
                           ), error = function(x){plot(1,1)}
                         )}, height = rv$figH_CP, width = rv$figW_CP
                       )
    })
    observeEvent(input$tabfig_CPl,{
      rv$CPdls <- c(input$tabfig_CPd, input$tabfig_CPl, input$tabfig_CPs)
      rv$tabfig_CPdlsfig <- modNamePaste(rv$CPdls, "CP")
      rv$tabfig_CPdlstab <- modNamePaste(rv$CPdls, "CP", tab = TRUE)
      rv$modSet_CP <- rv$mods_CP[[rv$sizeclass]]
      rv$modTab_CP <- rv$modSet_CP[[rv$tabfig_CPdlstab]]$cellwiseTable_ls
      rv$modTab_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
      output$modTab_CP <- renderDataTable(rv$modTab_CP)
      output$fig_CP <- renderPlot({
                         tryCatch(
                           plot(rv$modSet_CP,
                             specificModel = rv$tabfig_CPdlsfig,
                             sizeclassName = rv$sizeclass
                           ), error = function(x){plot(1,1)}
                         )}, height = rv$figH_CP, width = rv$figW_CP
                       )
    })
    observeEvent(input$tabfig_CPs,{
      rv$CPdls <- c(input$tabfig_CPd, input$tabfig_CPl, input$tabfig_CPs)
      rv$tabfig_CPdlsfig <- modNamePaste(rv$CPdls, "CP")
      rv$tabfig_CPdlstab <- modNamePaste(rv$CPdls, "CP", tab = TRUE)
      rv$modSet_CP <- rv$mods_CP[[rv$sizeclass]]
      rv$modTab_CP <- rv$modSet_CP[[rv$tabfig_CPdlstab]]$cellwiseTable_ls
      rv$modTab_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
      output$modTab_CP <- renderDataTable(rv$modTab_CP)
      output$fig_CP <- renderPlot({
                         tryCatch(
                           plot(rv$modSet_CP,
                             specificModel = rv$tabfig_CPdlsfig,
                             sizeclassName = rv$sizeclass
                           ), error = function(x){plot(1,1)}
                         )}, height = rv$figH_CP, width = rv$figW_CP
                       )
    })

    rv$AICcTab_CP <- cpmSetAICcTab(rv$mods_CP[[rv$sizeclass]], TRUE)
    rv$AICcTab_CP[ , "Scale Formula"] <- gsub("NULL", "", 
                                       rv$AICcTab_CP[ , "Scale Formula"]
                                     )
    output$AICcTab_CP <- renderDataTable(datatable(rv$AICcTab_CP))
  }
})

observeEvent(input$useSSdata, {
  rv$SS <- NULL
  rv$dateSearchedCol_g <- input$dateSearchedCol_g
  rv$SStemp <- tryCatch(
                 averageSS(rv$data_SS, rv$dateSearchedCol_g), 
                 error = function(x){NA}
               )
  if (is.na(rv$SStemp[1])){
    msg_avgSSfail <- msgSSavgFail()
  } else{
    rv$SS <- rv$SStemp
    rv$avgSI <-  mean(diff(rv$SS[-length(rv$SS)]))

    updateNumericInput(session, "gSearchInterval", value = rv$avgSI)
    updateNumericInput(session, "gSearchMax", value = max(rv$SS))
    rv$SStext <- paste(rv$SS, collapse = ", ")
    output$SStext <- renderText(rv$SStext)
  }
}) 
observeEvent(input$useSSinputs, {
  rv$gSearchInterval <- input$gSearchInterval
  rv$gSearchMax <- input$gSearchMax
  rv$SS <- seq(0, rv$gSearchMax, by = rv$gSearchInterval)
  if (max(rv$SS) != rv$gSearchMax){
    rv$SS <- c(rv$SS, rv$gSearchMax)
  }
  rv$SStext <- paste(rv$SS, collapse = ", ")
  output$SStext <- renderText(rv$SStext)
})

observeEvent(input$runModM, {

  msg_RunModM <- msgModRun("M")

  rv$kFill <- NA
  if (length(rv$obsCols_SE) == 1 | rv$kFixedChoice == 1){
    rv$kFill <- input$kFill
  }

  rv$nsizeclasses <- length(rv$sizeclasses)
  if (length(rv$nsizeclasses) == 1){
    if (is.null(rv$sizeclasses)){
      rv$sizeclasses <- "all"
     }
  }

  rv$unitCol <- input$unitCol
  rv$dateFoundCol <- input$dateFoundCol
  rv$dateSearchedCol <- input$dateSearchedCol
  rv$n <- input$n
  rv$frac <- input$frac
  rv$SEmodToUse <- rep(NA, rv$nsizeclasses)
  rv$CPmodToUse <- rep(NA, rv$nsizeclasses)

  for (sci in 1:rv$nsizeclasses){
    rv$SEmodToUse[sci] <- input[[sprintf("modelChoices_SE%d", sci)]]
    rv$CPmodToUse[sci] <- input[[sprintf("modelChoices_CP%d", sci)]]
    if (!grepl("s ~", rv$CPmodToUse[sci])){
      rv$CPmodToUse[sci] <- paste(rv$CPmodToUse[sci], "; NULL", sep = "")
    }
    rv$CPmodToUse[sci] <- paste("dist: ", rv$CPmodToUse[sci], sep = "")
  }
  names(rv$SEmodToUse) <- rv$sizeclasses
  names(rv$CPmodToUse) <- rv$sizeclasses

  rv$models_SE <- trimSetSize(rv$mods_SE, rv$SEmodToUse)
  rv$models_CP <- trimSetSize(rv$mods_CP, rv$CPmodToUse)

  if (rv$nsizeclasses > 1){
    rv$DWPCol <- rv$sizeclasses
    rv$sizeclassCol_M <- rv$sizeclassCol
  } else{
    rv$DWPCol <- input$DWPCol  
    rv$sizeclassCol_M <- NULL
    rv$models_SE <- rv$models_SE[[1]]
    rv$models_CP <- rv$models_CP[[1]]
  }

  rv$M <- tryCatch(
            estM(nsim = rv$n, rv$data_CO, rv$data_SS, rv$data_DWP, 
              frac = rv$frac, model_SE = rv$models_SE, 
              model_CP = rv$models_CP, kFill = rv$kFill, 
              unitCol = rv$unitCol, dateFoundCol = rv$dateFoundCol, 
              dateSearchedCol = rv$dateSearchedCol, DWPCol = rv$DWPCol,
              sizeclassCol = rv$sizeclassCol_M
            ), error = function(x){NA}
          )
  print(names(rv$M))
  removeNotification(msg_RunModM)
  msg_ModFailM <- msgModDoneM(rv$M)

})


observeEvent(input$runMod_g, {

  msg_RunModg <- msgModRun("g")

  rv$CL <- input$CL
  rv$kFill_g <- NA
  if (length(rv$obsCols_SE) == 1 | rv$kFixedChoice == 1){
    rv$kFill_g <- input$kFill_g
  }
  rv$sizeclasses_g <- rv$sizeclasses
  rv$nsizeclasses_g <- length(rv$sizeclasses_g)
  if (length(rv$nsizeclasses_g) == 1){
    if (is.null(rv$sizeclasses_g)){
      rv$sizeclasses_g <- "all"
      rv$nsizeclasses_g <- 1
    }
  }

  rv$n <- input$n
  rv$rghatsGeneric <- vector("list", length = rv$nsizeclasses_g)
  for (sci in 1:rv$nsizeclasses_g){

    rv$SEmodToUse_g <- input[[sprintf("modelChoices_SE%d", sci)]]
    rv$CPmodToUse_g <- input[[sprintf("modelChoices_CP%d", sci)]]
    if (!grepl("s ~", rv$CPmodToUse_g)){
      rv$CPmodToUse_g <- paste(rv$CPmodToUse_g, "; NULL", sep = "")
    }
    rv$CPmodToUse_g <- paste("dist: ", rv$CPmodToUse_g, sep = "")

    rv$rghatsGeneric[[sci]] <- estghatGeneric(rv$n, rv$SS, 
                                 rv$mods_SE[[sci]][[rv$SEmodToUse_g]],
                                 rv$mods_CP[[sci]][[rv$CPmodToUse_g]],
                                 kFill = rv$kFill)
  }
  names(rv$rghatsGeneric) <- rv$sizeclasses_g

  removeNotification(msg_RunModg)

  if (!is.null(rv$rghatsGeneric[[1]])){
    output$tab_g <- renderDataTable(
                      summary(rv$rghatsGeneric[[1]], CL = rv$CL)
                    )
    output$fig_g <- renderPlot(
                      tryCatch(
                        plot(rv$rghatsGeneric[[1]], 
                          sizeclassName = rv$sizeclasses_g[1], CL = rv$CL
                        ), error = function(x){plot(1,1)}
                      )
                    )
  }
  updateSelectizeInput(session, "tabfig_sizeclassg", 
    choices = rv$sizeclasses_g
  )
  updateTabsetPanel(session, "analyses_g", "Table")

})

observeEvent(input$tabfig_sizeclassg, {
  rv$sizeclass_g <- pickSizeclass(rv$sizeclasses_g, input$tabfig_sizeclassg)
  rv$CL <- input$CL
  if (class(rv$rghatsGeneric[[rv$sizeclass_g]])[1] == "ghatGeneric"){
    output$tab_g <- renderDataTable(
                      summary(rv$rghatsGeneric[[rv$sizeclass_g]],
                        CL = rv$CL
                      )
                    )
    output$fig_g <- renderPlot(
                      tryCatch(
                        plot(rv$rghatsGeneric[[rv$sizeclass_g]],
                          sizeclassName = rv$sizeclass_g, CL = rv$CL
                        ), error = function(x){plot(1,1)}
                      )
                    )
  }
})

}