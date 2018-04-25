library(shiny)
library(GenEst)

function(input, output, session){

  vnumber <- packageDescription("GenEst", field = "Version")
  vdate <- packageDescription("GenEst", field = "Date")
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
    easyClose = F, footer = modalButton("OK"))
  )

  msgRunModSE <- NULL
  msgModSizeSE <- NULL
  msgModFailSE <- NULL
  msgSampleSizeSE <- NULL
  msgRunModCP <- NULL
  msgModFailCP <- NULL
  msgSampleSizeCP <- NULL
  msgRunModg <- NULL
  msgModFailg <- NULL

  rv <- reactiveValues(
          dataSE = NULL, colNamesSE = NULL, obsColsSE = NULL, predsSE = NULL, 
          kFixedChoice = 0, kFixed = NULL,  predictors_p = NULL, 
          predictors_k = NULL, formula_p = NULL, formula_k = NULL,
          modsSE = NULL, modNamesSE = NULL, 
          modTabSE = NULL, AICcTabSE = NULL, modOrderSE = NULL,
          modelChoicesSE = NULL, modSetSE_spec = NULL, 
          bestSE = NULL, specSE = NULL, figSEnrow = NULL, 
          figSEht = 600, figSEwh = 800,
          modNamesSEp = NULL, modNamesSEk = NULL, tabfigSEpk = NULL, 
          minCellCountSE = NULL, sizeclassesSE = NULL, 
          
          dataCP = NULL, colNamesCP = NULL, ftp = NULL, lta = NULL, 
          predsCP = NULL, predictors_l = NULL, predictors_s = NULL, 
          formula_l = NULL, formula_s = NULL, dists = NULL,
          modsCP = NULL, AICcTabCP = NULL, 
          modOrderCP = NULL, modNamesCP = NULL, modTabCP = NULL, 
          modSetCP_spec = NULL, figCPnrow = NULL, figCPht = 600, 
          figCPwh = 800, bestCP = NULL,
          modNamesCPdist = NULL, modNamesCPl = NULL, modNamesCPs = NULL,
          tabfigCPdls_fig = NULL, tabfigCPdls_tab = NULL, 
          minCellCountCP = NULL, sizeclassesCP = NULL, 

          dataSS = NULL, colNamesSS = NULL, 

          kFill = NULL, unitCol = NULL, 
          dateFoundCol = NULL, dateSearchedCol = NULL,
          SEmodToUse = NULL, CPmodToUse = NULL, scis = NULL, 

          rghat = NULL,

          dataDWP = NULL, colNamesDWP = NULL,

          dataCO = NULL, colNamesCO = NULL,

          sizeclassChosen = NULL, colNamesAll = NULL,  
          sizeclassCol = NULL, sizeclasses = NULL, nsizeclasses = NULL, 

          CL = 0.9, niterations = 1000)

  observeEvent(input$fileSE, {
    rv$dataSE <- read.csv(input$fileSE$datapath, header = TRUE, 
                   stringsAsFactors = FALSE
                 )
    rv$colNamesSE <- colnames(rv$dataSE)
    if (length(rv$colNamesAll) == 0){
      rv$colNamesAll <- rv$colNamesSE 
    }else{
      rv$colNamesAll <- rv$colNamesSE[rv$colNamesSE %in% rv$colNamesAll]
    }
    if(length(rv$sizeclassCol) > 0){
      if ((rv$sizeclassCol %in% rv$colNamesAll) == FALSE){
        rv$sizeclassCol <- NULL
      }
    }
    output$dataSE <- DT::renderDataTable({
                            DT::datatable(data.frame(rv$dataSE))
                     })
    updateSelectizeInput(session, "predsSE", choices = rv$colNamesSE)
    updateSelectizeInput(session, "obsColsSE", choices = rv$colNamesSE)
    updateSelectizeInput(session, "sizeclassCol", choices = rv$colNamesAll,
      selected = rv$sizeclassCol
    )
    updateTabsetPanel(session, "LoadedDataViz", "Search Efficiency")
  })
  observeEvent(input$fileCP, {
    rv$dataCP <- read.csv(input$fileCP$datapath, header = TRUE, 
                   stringsAsFactors = FALSE
                 )
    rv$colNamesCP <- colnames(rv$dataCP)
    if (length(rv$colNamesAll) == 0){
      rv$colNamesAll <- rv$colNamesCP 
    }else{
      rv$colNamesAll <- rv$colNamesCP[rv$colNamesCP %in% rv$colNamesAll]
    }
    if(length(rv$sizeclassCol) > 0){
      if ((rv$sizeclassCol %in% rv$colNamesAll) == FALSE){
        rv$sizeclassCol <- NULL
      }
    }
    output$dataCP <- DT::renderDataTable(rv$dataCP)
    updateSelectizeInput(session, "predsCP", choices = rv$colNamesCP)
    updateSelectizeInput(session, "sizeclassCol", choices = rv$colNamesAll,
      selected = rv$sizeclassCol
    )
    updateSelectizeInput(session, "ltp", choices = rv$colNamesCP)
    updateSelectizeInput(session, "fta", choices = rv$colNamesCP)
    updateTabsetPanel(session, "LoadedDataViz", "Carcass Persistence")
  })
  observeEvent(input$fileSS, {
    rv$dataSS <- read.csv(input$fileSS$datapath, header = TRUE, 
                   stringsAsFactors = FALSE
                 )
    rv$colNamesSS <- colnames(rv$dataSS)
    output$dataSS <- DT::renderDataTable(rv$dataSS)
    updateTabsetPanel(session, "LoadedDataViz", "Search Schedule")
    updateSelectizeInput(session, "dateSearchedCol", choices = rv$colNamesSS)


  })
  observeEvent(input$fileDWP, {
    rv$dataDWP <- read.csv(input$fileDWP$datapath, header = TRUE, 
                   stringsAsFactors = FALSE
                 )
    output$dataDWP<- DT::renderDataTable(rv$dataDWP)
    updateTabsetPanel(session, "LoadedDataViz", "Density Weighted Proportion")
  })
  observeEvent(input$fileCO, {
    rv$dataCO <- read.csv(input$fileCO$datapath, header = TRUE, 
                   stringsAsFactors = FALSE
                 )
    rv$colNamesCO <- colnames(rv$dataCO)
    if (length(rv$colNamesAll) == 0){
      rv$colNamesAll <- rv$colNamesCO 
    }else{
      rv$colNamesAll <- rv$colNamesCO[rv$colNamesCO %in% rv$colNamesAll]
    }
    if(length(rv$sizeclassCol) > 0){
      if ((rv$sizeclassCol %in% rv$colNamesAll) == FALSE){
        rv$sizeclassCol <- NULL
      }
    }
    output$dataCO <- DT::renderDataTable(rv$dataCO)
    updateSelectizeInput(session, "splitColCO", choices = rv$colNamesCO)
    updateSelectizeInput(session, "sizeclassCol", choices = rv$colNamesAll,
      selected = rv$sizeclassCol
    )
    updateSelectizeInput(session, "unitCol", choices = rv$colNamesCO)
    updateSelectizeInput(session, "dateFoundCol", choices = rv$colNamesCO)
    updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")
  })

  observeEvent(input$obsColsSE, {
    selectedCols <- c(input$obsColsSE, input$sizeclassCol, input$predsSE)
    selectedTab <- rv$dataSE[ , which(rv$colNamesSE %in% selectedCols)]
    selectedDF <- data.frame(selectedTab)
    if (length(selectedCols) == 1){
      colnames(selectedDF) <- selectedCols
    }
    output$selectedSE <- DT::renderDataTable(selectedDF)
  })
  observeEvent(input$predsSE, {
    selectedCols <- c(input$obsColsSE, input$sizeclassCol, input$predsSE)
    selectedTab <- rv$dataSE[ , which(rv$colNamesSE %in% selectedCols)]
    selectedDF <- data.frame(selectedTab)
    if (length(selectedCols) == 1){
      colnames(selectedDF) <- selectedCols
    }
    output$selectedSE <- DT::renderDataTable(selectedDF)
  })
  observeEvent(input$runModSE, {

    msgRunModSE <- showNotification("Running Searcher Efficiency Model",
                     duration = NULL)
    rv$obsColsSE <- input$obsColsSE
    rv$predsSE <- input$predsSE
    if (input$kFixedChoice == 1 & is.numeric(input$kFixed)){
      rv$kFixed <- input$kFixed
    }else{
      rv$kFixed <- NULL
    }
    rv$niterations <- input$niterations
    rv$CL <- input$CL
    rv$sizeclassCol <- input$sizeclassCol
    rv$kFixedChoice <- input$kFixedChoice
 
    rv$sizeclasses <- as.character(unique(rv$dataSE[ , rv$sizeclassCol]))
    if (length(rv$sizeclassCol) == 0){
      rv$sizeclasses <- "all"
    }
    rv$sizeclassesSE <- rv$sizeclasses

    rv$predictors_p <- paste(rv$predsSE, collapse = "*")
    if (length(rv$predsSE) == 0){
      rv$predictors_p <- 1
    }
    rv$formula_p <- formula(paste("p~", rv$predictors_p, sep = ""))
    rv$formula_k <- formula(paste("k~", rv$predictors_p, sep = "")) 

    if (length(rv$obsColsSE) == 1){
      if(length(rv$formula_k) > 0 & length(rv$kFixed) == 0){
        msg <- "Only one observation, k not estimated."
        msgModSizeSE <- showNotification(msg, type = "warning", 
                          duration = NULL
                        )
      }
    
      if (length(rv$kFixed) == 1){
        msg <- "Only one observation, fix k input ignored."
        msgModSizeSE <- showNotification(msg, type = "warning", 
                          duration = NULL
                        )
      }
      output$kFillNeed <- renderText("yes")
    } else{
      output$kFillNeed <- renderText("no")
    }
    outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)

    rv$modsSE <- suppressWarnings(
                   pkmSetSize(formula_p = rv$formula_p,
                     formula_k = rv$formula_k, data = rv$dataSE, 
                     obsCol = rv$obsColsSE, sizeclassCol = rv$sizeclassCol,
                     kFixed = rv$kFixed, kInit = 0.7, CL = rv$CL, 
                     quiet = TRUE
                   )
                 )
    if (all(unlist(pkmSetSizeFail(rv$modsSE)))) {
      removeNotification(msgRunModSE)
      msg <- paste(
               "No models were successfully fit.", 
               gsub("Failed model fit: ", "", unique(unlist(rv$modsSE))),
               sep = " "
             )
      msgModFailSE <- showNotification(msg, type = "error", duration = NULL)
    } else{
      if (any(unlist(pkmSetSizeFail(rv$modsSE)))) {
        removeNotification(msgRunModSE)
        msg <- paste(
                 "Some models were not successfully fit. Failed models were ", 
                 "removed. Check inputs.", sep = " "
               )
        msgModFailSE <- showNotification(msg, type = "warning", 
                          duration = NULL
                        )
        rv$modsSE <- pkmSetSizeFailRemove(rv$modsSE)
      }

      rv$sizeclassChosen <- which(rv$sizeclasses == input$tabfigSizeClassSE)
      if (length(rv$sizeclassChosen) == 0){
        rv$sizeclassChosen <- 1
      }
      rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE[[rv$sizeclassChosen]], TRUE)
                      
      rv$modOrderSE <- as.numeric(row.names(rv$AICcTabSE))
      rv$modNamesSE <- names(rv$modsSE[[rv$sizeclassChosen]])[rv$modOrderSE]
      rv$modTabSE <- rv$modsSE[[1]][[rv$modOrderSE[1]]]$cellwiseTable
      rv$modSetSE_spec <- rv$modsSE[[rv$sizeclassChosen]]

      rv$minCellCountSE <- NULL
      for (sci in 1:length(rv$sizeclasses)){
        for (modi in 1:length(rv$modSetSE_spec)){
          rv$minCellCountSE <- c(rv$minCellCountSE, 
                                min(table(rv$modsSE[[sci]][[modi]]$carcCell)))
        }
      }
      if (any(rv$minCellCountSE < 10)){
        msg <- paste("Small (< 10) sample sizes per cell. Consider a",
                 "simpler model. Parameter estimates may be unstable.",
                 sep = ""
               ) 
        msgSampleSizeSE <- showNotification(msg, type = "warning",
                             duration = NULL
                           )
      }

      if (rv$kFixedChoice == 1){
        updateNumericInput(session, "kFill", value = rv$kFixed)
      }

      rv$modNamesSEp <- rv$modNamesSE
      rv$modNamesSEk <- rv$modNamesSE
      for (modi in 1:length(rv$modNamesSE)){
        rv$modNamesSEp[modi] <- strsplit(rv$modNamesSE[modi], "; ")[[1]][1]
        rv$modNamesSEk[modi] <- strsplit(rv$modNamesSE[modi], "; ")[[1]][2]
      }

      rv$figSEnrow <- ceiling(rv$modSetSE_spec[[1]]$ncell / 2 )
      rv$figSEht <- rv$figSEnrow * 200 + 400
      if (rv$modSetSE_spec[[1]]$ncell > 6){
        rv$figSEwh <- 1200
      }
      rv$bestSE <- (names(rv$modSetSE_spec)[rv$modOrderSE])[1]
      output$figSE <- renderPlot({
                        plot(rv$modSetSE_spec, specificModel = rv$bestSE)},
                        height = rv$figSEht, width = rv$figSEwh
                      )
  
      colnames(rv$modTabSE) <- c("Cell", 
                                 "p Median", 
                      paste("p ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
                      paste("p ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                 "k Median",
                      paste("k ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
                      paste("k ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
                               )
      colnames(rv$AICcTabSE) <- c("p Formula", "k Formula", "AICc", 
                                  "Delta AICc"
                                )
      output$AICcTabSE <- DT::renderDataTable({rv$AICcTabSE})    
      output$modTabSE <- DT::renderDataTable({rv$modTabSE})

      updateSelectizeInput(session, "tabfigSizeClassSE", 
        choices = rv$sizeclasses
      )
      updateSelectizeInput(session, "tabfigSEp", choices = rv$modNamesSEp)
      updateSelectizeInput(session, "tabfigSEk", choices = rv$modNamesSEk)
      updateTabsetPanel(session, "analysesSE", "Model Comparison Tables")

      removeNotification(msgRunModSE)
 
      isolate({
        output$sizeclassesSE <- renderText(
                                  paste(rv$sizeclassesSE, collapse = " ")
                                )
        outputOptions(output, "sizeclassesSE", suspendWhenHidden = FALSE)
        output$modelMenuSE <- renderUI({
          
          modelMenuSE <- ""
          nsizeclasses <- length(rv$sizeclassesSE)
          if (nsizeclasses > 0){
            for(sci in 1:nsizeclasses){
              AICcTab <- pkmSetAICcTab(rv$modsSE[[sci]], quiet = TRUE)
              modOrder <- as.numeric(row.names(AICcTab))
              modNames <- names(rv$modsSE[[sci]])[modOrder]
              mtuText <- paste("modelChoicesSE", sci, sep = "") 
              scText <- paste("Model for ", rv$sizeclasses[sci], sep = "")
              modSelect <- selectizeInput(mtuText, scText, modNames,
                             width = "400px"
                           )
              modelMenuSE <- paste(modelMenuSE, modSelect)  
            }
          }
          HTML(modelMenuSE)
        })
      })
    }
  })
  observeEvent(input$tabfigSizeClassSE, {
    if (length(rv$modsSE) > 0){
      rv$sizeclassChosen <- which(rv$sizeclasses == input$tabfigSizeClassSE)
      if (length(rv$sizeclassChosen) == 0){
        rv$sizeclassChosen <- 1
      }
      rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE[[rv$sizeclassChosen]], TRUE)
                      
      rv$modOrderSE <- as.numeric(row.names(rv$AICcTabSE))
      rv$modNamesSE <- names(rv$modsSE[[1]])[rv$modOrderSE]
      rv$modNamesSEp <- rv$modNamesSE
      rv$modNamesSEk <- rv$modNamesSE
      for (modi in 1:length(rv$modNamesSE)){
        rv$modNamesSEp[modi] <- strsplit(rv$modNamesSE[modi], "; ")[[1]][1]
        rv$modNamesSEk[modi] <- strsplit(rv$modNamesSE[modi], "; ")[[1]][2]
      }
      rv$modTabSE <- 
          rv$modsSE[[rv$sizeclassChosen]][[rv$modOrderSE[1]]]$cellwiseTable
      rv$modSetSE_spec <- rv$modsSE[[rv$sizeclassChosen]]
      rv$bestSE <- (names(rv$modSetSE_spec)[rv$modOrderSE])[1]

      colnames(rv$modTabSE) <- c("Cell", 
                                 "p Median", 
                    paste("p ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
                    paste("p ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                 "k Median",
                    paste("k ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
                    paste("k ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
                                 )
      output$modTabSE <- DT::renderDataTable(rv$modTabSE)
      output$figSE <- renderPlot({ 
                        plot(rv$modSetSE_spec, specificModel = rv$bestSE)},
                        height = rv$figSEht, width = rv$figSEwh
                      )
      updateSelectizeInput(session, "tabfigSEp", choices = rv$modNamesSEp)
      updateSelectizeInput(session, "tabfigSEk", choices = rv$modNamesSEk)

      observeEvent(input$tabfigSEp, {
        rv$tabfigSEpk <- paste(input$tabfigSEp, input$tabfigSEk, sep = "; ")
        rv$modSetSE_spec <- rv$modsSE[[rv$sizeclassChosen]]
        rv$modTabSE <- 
          rv$modsSE[[rv$sizeclassChosen]][[rv$tabfigSEpk]]$cellwiseTable
        colnames(rv$modTabSE) <- c("Cell", 
                                   "p Median", 
                      paste("p ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
                      paste("p ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                   "k Median",
                      paste("k ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
                      paste("k ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
                                   )
        output$modTabSE <- DT::renderDataTable(rv$modTabSE)
        output$figSE <- renderPlot({
                          tryCatch(
                            plot(rv$modSetSE_spec, 
                              specificModel = rv$tabfigSEpk), 
                            error = function(x){plot(1,1)}
                          )
                        }, height = rv$figSEht, width = rv$figSEwh
                        )
      })
      observeEvent(input$tabfigSEk, {
        rv$tabfigSEpk <- paste(input$tabfigSEp, input$tabfigSEk, 
                              sep = "; "
                            )
        rv$modSetSE_spec <- rv$modsSE[[rv$sizeclassChosen]]
        rv$modTabSE <- 
          rv$modsSE[[rv$sizeclassChosen]][[rv$tabfigSEpk ]]$cellwiseTable
        colnames(rv$modTabSE) <- c("Cell", 
                                   "p Median", 
                      paste("p ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
                      paste("p ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                   "k Median",
                      paste("k ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
                      paste("k ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
                                   )
        output$modTabSE <- DT::renderDataTable(rv$modTabSE)
        output$figSE <- renderPlot({
                          tryCatch(
                            plot(rv$modSetSE_spec, 
                              specificModel = rv$tabfigSEpk), 
                            error = function(x){plot(1,1)}
                          )
                        }, height = rv$figSEht, width = rv$figSEwh
                        )
      })
      rv$sizeclassChosen <- which(rv$sizeclasses == input$tabfigSizeClassSE)
      if (length(rv$sizeclassChosen) == 0){
        rv$sizeclassChosen <- 1
      }
      rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE[[rv$sizeclassChosen]], TRUE)
                      

      colnames(rv$AICcTabSE) <- c("p Formula", "k Formula", "AICc", 
                                  "Delta AICc"
                                )
      output$AICcTabSE <- DT::renderDataTable({rv$AICcTabSE})
    }

  })

  observeEvent(input$ltp, {
    obsColsSelected <- c(input$ltp, input$fta)
    selectedCols <- c(obsColsSelected, input$sizeclassCol, input$predsCP)
    selectedTab <- rv$dataCP[ , which(rv$colNamesCP %in% selectedCols)]
    selectedDF <- data.frame(selectedTab)
    if (length(selectedCols) == 1){
      colnames(selectedDF) <- selectedCols
    }
    output$selectedCP <- DT::renderDataTable(selectedDF)
  })
  observeEvent(input$fta, {
    obsColsSelected <- c(input$ltp, input$fta)
    selectedCols <- c(obsColsSelected, input$sizeclassCol, input$predsCP)
    selectedTab <- rv$dataCP[ , which(rv$colNamesCP %in% selectedCols)]
    selectedDF <- data.frame(selectedTab)
    if (length(selectedCols) == 1){
      colnames(selectedDF) <- selectedCols
    }
    output$selectedCP <- DT::renderDataTable(selectedDF)
  })
  observeEvent(input$predsCP, {
    obsColsSelected <- c(input$ltp, input$fta)
    selectedCols <- c(obsColsSelected, input$sizeclassCol, input$predsCP)
    selectedTab <- rv$dataCP[ , which(rv$colNamesCP %in% selectedCols)]
    selectedDF <- data.frame(selectedTab)
    if (length(selectedCols) == 1){
      colnames(selectedDF) <- selectedCols
    }
    output$selectedCP <- DT::renderDataTable(selectedDF)
  })
  observeEvent(input$runModCP, {
    msgRunModCP <- showNotification("Running Carcass Persistence Model",
                     duration = NULL)
    rv$ltp <- input$ltp
    rv$fta <- input$fta
    rv$predsCP <- input$predsCP
    rv$dists <- input$dists

    rv$niterations <- input$niterations
    rv$CL <- input$CL
    rv$sizeclassCol <- input$sizeclassCol
    rv$sizeclasses <- as.character(unique(rv$dataCP[ , rv$sizeclassCol]))
    if (length(rv$sizeclassCol) == 0){
      rv$sizeclasses <- "all"
    }
    rv$sizeclassesCP <- rv$sizeclasses

    rv$predictors_l <- paste(rv$predsCP, collapse = "*")
    if (length(rv$predsCP) == 0){
      rv$predictors_l <- 1
    }
    rv$formula_l <- formula(paste("l~", rv$predictors_l, sep = ""))
    rv$formula_s <- formula(paste("s~", rv$predictors_l, sep = "")) 

    rv$modsCP <- suppressWarnings(
                   cpmSetSize(formula_l = rv$formula_l,
                     formula_s = rv$formula_s, data = rv$dataCP, 
                     left = rv$ltp, right = rv$fta, dists = rv$dists,
                     sizeclassCol = rv$sizeclassCol, CL = rv$CL, quiet = TRUE
                   )
                 )

    if (all(unlist(cpmSetSizeFail(rv$modsCP)))){
      removeNotification(msgRunModCP)
      msg <- paste(
               "No models were successfully fit.", 
               gsub("Failed model fit: ", "", unique(unlist(rv$modsCP))),
               sep = " "
             )
      msgModFailCP <- showNotification(msg, type = "error", duration = NULL)
    } else{
      if (any(unlist(cpmSetSizeFail(rv$modsCP)))){
        removeNotification(msgRunModCP)
        msg <- paste(
                 "Some models were not successfully fit. Failed models were ", 
                 "removed. Check inputs.", sep = " "
               )
        msgModFailCP <- showNotification(msg, type = "warning", 
                          duration = NULL
                        )
        rv$modsCP <- cpmSetSizeFailRemove(rv$modsCP)
      }


      rv$sizeclassChosen <- which(rv$sizeclasses == input$aicTabSizeClassCP)
      if (length(rv$sizeclassChosen) == 0){
        rv$sizeclassChosen <- 1
      }
      rv$AICcTabCP <- cpmSetAICcTab(rv$modsCP[[rv$sizeclassChosen]], TRUE)
      rv$modOrderCP <- as.numeric(row.names(rv$AICcTabCP))
      rv$modNamesCP <- names(rv$modsCP[[rv$sizeclassChosen]])[rv$modOrderCP]
      rv$modTabCP <- rv$modsCP[[1]][[rv$modOrderCP[1]]]$cellwiseTable_ls
      rv$modSetCP_spec <- rv$modsCP[[rv$sizeclassChosen]]

      rv$minCellCountCP <- NULL
      for (sci in 1:length(rv$sizeclasses)){
        for (modi in 1:length(rv$modSetCP_spec)){
          rv$minCellCountCP <- c(rv$minCellCountCP, 
                               min(table(rv$modsCP[[sci]][[modi]]$carcCells)))
        }
      }
      if (any(rv$minCellCountCP < 10)){
        msg <- paste("Small (< 10) sample sizes per cell. Consider a",
                 "simpler model. Parameter estimates may be unstable.",
                 sep = ""
               ) 
        msgSampleSizeCP <- showNotification(msg, type = "warning",
                             duration = NULL
                           )
      }


      rv$modNamesCPdist <- rv$modNamesCP
      rv$modNamesCPl <- rv$modNamesCP
      rv$modNamesCPs <- rv$modNamesCP
      for (modi in 1:length(rv$modNamesCP)){
        rv$modNamesCPdist[modi] <- strsplit(rv$modNamesCP[modi], 
                                     "; ")[[1]][1]
        rv$modNamesCPl[modi] <- strsplit(rv$modNamesCP[modi], "; ")[[1]][2]
        rv$modNamesCPs[modi] <- strsplit(rv$modNamesCP[modi], "; ")[[1]][3]
        rv$modNamesCPdist[modi] <- gsub("dist: ", "",
                                     rv$modNamesCPdist[modi])
        rv$modNamesCPs[modi] <- gsub("NULL", "s ~ 1", rv$modNamesCPs[modi])
      }
      rv$figCPnrow <- ceiling(rv$modSetCP_spec[[1]]$ncell / 2 )
      rv$figCPht <- rv$figCPnrow * 200 + 400
      if (rv$modSetCP_spec[[1]]$ncell > 6){
        rv$figCPwh <- 1200
      }
      rv$bestCP <- (names(rv$modSetCP_spec)[rv$modOrderCP])[1]
      output$figCP <- renderPlot({
                        plot(rv$modSetCP_spec, specificModel = rv$bestCP)},
                        height = rv$figCPht, width = rv$figCPwh
                      )

      rv$AICcTabCP[ , "s formula"] <- gsub("NULL", "", 
                                          rv$AICcTabCP[ , "s formula"]
                                        )
      colnames(rv$AICcTabCP) <- c("Distribution", "Location Formula", 
                                  "Scale Formula", "AICc", "Delta AICc"
                                  )
      colnames(rv$modTabCP) <- c("Cell", 
                                 "Location Median", 
               paste("Location ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
               paste("Location ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                 "Scale Median",
               paste("Scale ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
               paste("Scale ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
                                 )
      output$AICcTabCP <- DT::renderDataTable({rv$AICcTabCP})    
      output$modTabCP <- DT::renderDataTable({rv$modTabCP})
  
      updateSelectizeInput(session, "tabfigSizeClassCP", 
        choices = rv$sizeclasses
      ) 
      updateSelectizeInput(session, "tabfigCPdist", 
        choices = rv$modNamesCPdist)
      updateSelectizeInput(session, "tabfigCPl", choices = rv$modNamesCPl)
      updateSelectizeInput(session, "tabfigCPs", choices = rv$modNamesCPs)
      updateTabsetPanel(session, "analysesCP", "Model Comparison Tables")

      removeNotification(msgRunModCP)

      isolate({

        output$sizeclassesCP <- renderText(
                                  paste(rv$sizeclassesCP, collapse = " ") 
                                )
        outputOptions(output, "sizeclassesCP", suspendWhenHidden = FALSE)

        output$modelMenuCP <- renderUI({
        
          modelMenuCP <- ""
          nsizeclassesCP <- length(rv$sizeclassesCP)
          if (nsizeclassesCP > 0){
            for(sci in 1:nsizeclassesCP){
              AICcTabCP <- cpmSetAICcTab(rv$modsCP[[sci]], quiet = TRUE)
              modOrderCP <- as.numeric(row.names(AICcTabCP))
              modNamesCP <- names(rv$modsCP[[sci]])[modOrderCP]
              modNamesCP <- gsub("; NULL", "", modNamesCP)
              mtuTextCP <- paste("modelChoicesCP", sci, sep = "") 
              scTextCP <- paste("Model for ", rv$sizeclassesCP[sci], sep = "")
              modSelectCP <- selectizeInput(mtuTextCP, scTextCP, modNamesCP,
                               width = "400px"
                             )
              modelMenuCP <- paste(modelMenuCP, modSelectCP)  
            }
          }
          HTML(modelMenuCP)
        })
      })
    }
  })
  observeEvent(input$tabfigSizeClassCP, {
    if (length(rv$modsCP) > 0){
      rv$sizeclassChosen <- which(rv$sizeclasses == input$tabfigSizeClassCP)
      if (length(rv$sizeclassChosen) == 0){
        rv$sizeclassChosen <- 1
      }
      rv$AICcTabCP <- cpmSetAICcTab(rv$modsCP[[rv$sizeclassChosen]], TRUE)
      rv$modOrderCP <- as.numeric(row.names(rv$AICcTabCP))
      rv$modNamesCP <- names(rv$modsCP[[1]])[rv$modOrderCP]
      rv$modNamesCPdist <- rv$modNamesCP
      rv$modNamesCPl <- rv$modNamesCP
      rv$modNamesCPs <- rv$modNamesCP
      for (modi in 1:length(rv$modNamesCP)){
        rv$modNamesCPdist[modi] <- 
            strsplit(rv$modNamesCP[modi], "; ")[[1]][1]
        rv$modNamesCPl[modi] <- strsplit(rv$modNamesCP[modi], "; ")[[1]][2]
        rv$modNamesCPs[modi] <- strsplit(rv$modNamesCP[modi], "; ")[[1]][3]
        rv$modNamesCPdist[modi] <- gsub("dist: ", "", rv$modNamesCPdist[modi])
        rv$modNamesCPs[modi] <- gsub("NULL", "s ~ 1", rv$modNamesCPs[modi])
      }
      rv$modTabCP <- 
          rv$modsCP[[rv$sizeclassChosen]][[rv$modOrderCP[1]]]$cellwiseTable_ls
      rv$modSetCP_spec <- rv$modsCP[[rv$sizeclassChosen]]
      rv$bestCP <- (names(rv$modSetCP_spec)[rv$modOrderCP])[1]

      colnames(rv$modTabCP) <- c("Cell", 
                                 "Location Median", 
             paste("Location ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
             paste("Location ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                 "Scale Median",
             paste("Scale ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
             paste("Scale ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
             )
      output$modTabCP <- DT::renderDataTable(rv$modTabCP)
      output$figCP <- renderPlot({
                        plot(rv$modSetCP_spec, specificModel = rv$bestCP)},
                        height = rv$figCPht, width = rv$figCPwh
                      )
      updateSelectizeInput(session, "tabfigCPdist", 
        choices = rv$modNamesCPdist)
      updateSelectizeInput(session, "tabfigCPl", choices = rv$modNamesCPl)
      updateSelectizeInput(session, "tabfigCPs", choices = rv$modNamesCPs)

      observeEvent(input$tabfigCPdist, {
        rv$tabfigCPdls_fig <- paste("dist: ", input$tabfigCPdist, "; ",
                            input$tabfigCPl, "; ",input$tabfigCPs, sep = ""
                          )
        if (input$tabfigCPdist == "exponential"){
          rv$tabfigCPdls_tab <- paste("dist: ", input$tabfigCPdist, "; ",
                              input$tabfigCPl, "; NULL", sep = ""
                            )
        } else{
          rv$tabfigCPdls_tab <- paste("dist: ", input$tabfigCPdist, "; ",
                              input$tabfigCPl, "; ", input$tabfigCPs, sep = ""
                            )
        }

        rv$modTabCP <- 
       rv$modsCP[[rv$sizeclassChosen]][[rv$tabfigCPdls_tab]]$cellwiseTable_ls
        rv$modSetCP_spec <- rv$modsCP[[rv$sizeclassChosen]]

        colnames(rv$modTabCP) <- c("Cell", 
                                   "Location Median", 
               paste("Location ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
               paste("Location ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                   "Scale Median",
               paste("Scale ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
               paste("Scale ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
                )
        output$modTabCP <- DT::renderDataTable(rv$modTabCP)
        output$figCP <- renderPlot({
                           plot(rv$modSetCP_spec, 
                             specificModel = rv$tabfigCPdls_fig
                           )}, height = rv$figCPht, width = rv$figCPwh
                        )

      })
      observeEvent(input$tabfigCPl, {
        rv$tabfigCPdls_fig <- paste("dist: ", input$tabfigCPdist, "; ",
                            input$tabfigCPl, "; ", input$tabfigCPs, sep = ""
                          )
        if (input$tabfigCPdist == "exponential"){
          rv$tabfigCPdls_tab <- paste("dist: ", input$tabfigCPdist, "; ",
                              input$tabfigCPl, "; NULL", sep = ""
                            )
        } else{
          rv$tabfigCPdls_tab <- paste("dist: ", input$tabfigCPdist, "; ",
                              input$tabfigCPl, "; ", input$tabfigCPs, sep = ""
                            )
        }

        rv$modSetCP_spec <- rv$modsCP[[rv$sizeclassChosen]]
        rv$modTabCP <- 
       rv$modsCP[[rv$sizeclassChosen]][[rv$tabfigCPdls_tab]]$cellwiseTable_ls

        colnames(rv$modTabCP) <- c("Cell", 
                                   "Location Median", 
               paste("Location ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
               paste("Location ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                   "Scale Median",
               paste("Scale ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
               paste("Scale ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
               )
        output$modTabCP <- DT::renderDataTable(rv$modTabCP)
        output$figCP <- renderPlot({
                          tryCatch(
                            plot(rv$modSetCP_spec, 
                              specificModel = rv$tabfigCPdls_fig), 
                            error = function(x){plot(1,1)}
                          )
                        }, height = rv$figCPht, width = rv$figCPwh
                        )
      })
      observeEvent(input$tabfigCPs, {
        rv$tabfigCPdls_fig <- paste("dist: ", input$tabfigCPdist, "; ",
                            input$tabfigCPl, "; ",input$tabfigCPs, sep = ""
                          )
        if (input$tabfigCPdist == "exponential"){
          rv$tabfigCPdls_tab <- paste("dist: ", input$tabfigCPdist, "; ",
                              input$tabfigCPl, "; NULL", sep = ""
                            )
        } else{
          rv$tabfigCPdls_tab <- paste("dist: ", input$tabfigCPdist, "; ",
                              input$tabfigCPl, "; ", input$tabfigCPs, sep = ""
                            )
        }

        rv$modSetCP_spec <- rv$modsCP[[rv$sizeclassChosen]]
        rv$modTabCP <- 
        rv$modsCP[[rv$sizeclassChosen]][[rv$tabfigCPdls_tab]]$cellwiseTable_ls

        colnames(rv$modTabCP) <- c("Cell", 
                                   "Location Median", 
               paste("Location ", 100 * (1 - rv$CL) / 2, "%", sep = ""), 
               paste("Location ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = ""),
                                   "Scale Median",
               paste("Scale ", 100 * (1 - rv$CL) / 2, "%", sep = ""),
               paste("Scale ", 100 - 100 * (1 - rv$CL) / 2, "%", sep = "")
                )
        output$modTabCP <- DT::renderDataTable(rv$modTabCP)
        output$figCP <- renderPlot({
                          tryCatch(
                            plot(rv$modSetCP_spec, 
                              specificModel = rv$tabfigCPdls_fig), 
                            error = function(x){plot(1,1)}
                          )
                        }, height = rv$figCPht, width = rv$figCPwh
                        )
      })
      rv$sizeclassChosen <- which(rv$sizeclasses == input$tabfigSizeClassCP)
      if (length(rv$sizeclassChosen) == 0){
        rv$sizeclassChosen <- 1
      }
      rv$AICcTabCP <- cpmSetAICcTab(rv$modsCP[[rv$sizeclassChosen]], TRUE)
                      

      rv$AICcTabCP[ , "s formula"] <- gsub("NULL", "", 
                                        rv$AICcTabCP[ , "s formula"]
                                      )
      colnames(rv$AICcTabCP) <- c("Distribution", "Location Formula", 
                                  "Scale Formula", "AICc", "Delta AICc"
                                  )
      output$AICcTabCP <- DT::renderDataTable({rv$AICcTabCP})
    }
  })

  observeEvent(input$runModM, {

    rv$kFill <- NA

    if (length(rv$obsColsSE) == 1 | rv$kFixedChoice == 1){
      rv$kFill <- input$kFill
    }

    if (length(rv$nsizeclasses) == 1){
      if (is.null(rv$sizeclasses)){
        rv$sizeclasses <- "all"
      }
    }

    rv$unitCol <- input$unitCol
    rv$dateFoundCol <- input$dateFoundCol
    rv$dateSearchedCol <- input$dateSearchedCol
    rv$niterations <- input$niterations

    msg <- "Estimating detection probabilities"
    msgRunModg <- showNotification(msg, duration = NULL)
    rv$nsizeclasses <- length(rv$sizeclasses)

    rv$rghat <- vector("list", length = rv$nsizeclasses)
    for (sci in 1:rv$nsizeclasses){
      rv$SEmodToUse <- input[[sprintf("modelChoicesSE%d", sci)]]
      rv$CPmodToUse <- input[[sprintf("modelChoicesCP%d", sci)]]
      if (!grepl("s ~", rv$CPmodToUse)){
        rv$CPmodToUse <- paste(rv$CPmodToUse, "; NULL", sep = "")
      }
      if (rv$sizeclasses[1] == "all"){
        rv$scis <- 1:nrow(rv$dataCO)
      } else{
        rv$scis <- which(rv$dataCO[ , rv$sizeclassCol] == rv$sizeclasses[sci])
      }
      rv$rghat[[sci]] <- tryCatch(
                           rghat(rv$niterations, 
                             rv$dataCO[rv$scis, ], 
                             rv$dataSS, 
                             rv$modsSE[[sci]][[rv$SEmodToUse]], 
                             rv$modsCP[[sci]][[rv$CPmodToUse]], 
                             kFill = rv$kFill, unitCol = rv$unitCol, 
                             dateFoundCol = rv$dateFoundCol, 
                             dateSearchedCol = rv$dateSearchedCol
                           ), error = function(x){NA}
                         )

    }

    if (is.list(rv$rghat[[1]]) & is.matrix(rv$rghat[[1]][[1]])){
      msg  <- "ghat estimation done."
      msgModFailg <- showNotification(msg, type = "message", duration = NULL)
    }

    removeNotification(msgRunModg)
    if (any(is.na(rv$rghat))){
      if (all(is.na(rv$rghat))){
        msg <- "All ghat estimations failed, check input"
        msgModFailg <- showNotification(msg, type = "error", duration = NULL)
      } else{
        msg <- "Some ghat estimations failed, check input"
        msgModFailg <- showNotification(msg, type = "warning", 
                         duration = NULL
                       )
      }
    }
  })

# not sure about these with the new estimation set up

  observeEvent(input$unitColCO, {
   selectedCols <- c(input$unitColCO, input$sizeclassCol, input$splitColCO)
    selectedTab <- rv$dataCO[ , which(rv$colNamesCO %in% selectedCols)]
    selectedDF <- data.frame(selectedTab)
    if (length(selectedCols) == 1){
      colnames(selectedDF) <- selectedCols
    }
    output$selectedCO <- DT::renderDataTable(selectedDF)
  })
  observeEvent(input$splitColCO, {
   selectedCols <- c(input$unitColCO, input$sizeclassCol, input$splitColCO)
    selectedTab <- rv$dataCO[ , which(rv$colNamesCO %in% selectedCols)]
    selectedDF <- data.frame(selectedTab)
    if (length(selectedCols) == 1){
      colnames(selectedDF) <- selectedCols
    }
    output$selectedCO <- DT::renderDataTable(selectedDF)
  })
}

