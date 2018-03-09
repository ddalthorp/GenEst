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
  msgModFitSE <- NULL

  rv <- reactiveValues(
          dataSE = NULL, colNamesSE = NULL, obsColsSE = NULL, predsSE = NULL, 
          kFixedChoice = 0, kFixed = NULL,  predictors_p = NULL, 
          predictors_k = NULL, formula_p = NULL, formula_k = NULL,
          modsSE = NULL, modsCheckSE = NULL, modNamesSE = NULL, 
          modTabSE = NULL, AICcTabSE = NULL, modOrderSE = NULL,
          modelChoicesSE = NULL, modSetSE_spec = NULL, 
          bestSE = NULL, specSE = NULL, figSEnrow = NULL, 
          figSEht = 600, figSEwh = 800,
          
          dataCP = NULL, colNamesCP = NULL, 

          dataSS = NULL,  

          dataCO = NULL, colNamesCO = NULL,

          sizeclassChosen = NULL, sizeclasses = NULL, sizeclassCol = NULL, 
          sizeclasses = NULL, nsizeclasses = NULL, 

          CL = 0.9, niterations = 1000)



  observeEvent(input$fileSE, {
    rv$dataSE <- read.csv(input$fileSE$datapath, header = T)
    rv$colNamesSE <- colnames(rv$dataSE)
    if (length(rv$sizeclasses) == 0){
      rv$sizeclasses <- rv$colNamesSE 
    }else{
      rv$sizeclasses <- rv$colNamesSE[rv$colNamesSE %in% rv$sizeclasses]
    }
    output$dataSE <- DT::renderDataTable({
                            DT::datatable(data.frame(rv$dataSE))
                     })
    updateSelectizeInput(session, "predsSE", choices = rv$colNamesSE)
    updateSelectizeInput(session, "obsColsSE", choices = rv$colNamesSE)
    updateSelectizeInput(session, "sizeclassCol", choices = rv$sizeclasses)
    updateTabsetPanel(session, "LoadedDataViz", "Search Efficiency")
  })
  observeEvent(input$fileCP, {
    rv$dataCP <- read.csv(input$fileCP$datapath, header = T)
    rv$colNamesCP <- colnames(rv$dataCP)
    if (length(rv$sizeclasses) == 0){
      rv$sizeclasses <- rv$colNamesCP 
    }else{
      rv$sizeclasses <- rv$colNamesCP[rv$colNamesCP %in% rv$sizeclasses]
    }
    output$dataCP <- DT::renderDataTable(rv$dataCP)
    updateSelectizeInput(session, "predsCP", choices = rv$colNamesCP)
    updateSelectizeInput(session, "sizeclassCol", choices = rv$sizeclasses)
    updateSelectizeInput(session, "ltp", choices = rv$colNamesCP)
    updateSelectizeInput(session, "fta", choices = rv$colNamesCP)
    updateTabsetPanel(session, "LoadedDataViz", "Carcass Persistence")
  })
  observeEvent(input$fileSS, {
    rv$dataSS <- read.csv(input$fileSS$datapath, header = T)
    output$dataSS <- DT::renderDataTable(rv$dataSS)
    updateTabsetPanel(session, "LoadedDataViz", "Search Schedule")
  })
  observeEvent(input$fileCO, {
    rv$dataCO <- read.csv(input$fileCO$datapath, header = T)
    rv$colNamesCO <- colnames(rv$dataCO)
    if(length(rv$sizeclasses) == 0){
      rv$sizeclasses <- rv$colNamesCO 
    }else{
      rv$sizeclasses <- rv$colNamesCO[rv$colNamesCO %in% rv$sizeclasses]
    }
    output$dataCO <- DT::renderDataTable(rv$dataCO)
    updateSelectizeInput(session, "splitColCO", choices = rv$colNamesCO)
    updateSelectizeInput(session, "sizeclassCol", choices = rv$sizeclasses)
    updateSelectizeInput(session, "unitColCO", choices = rv$colNamesCO)
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
    }
    rv$niterations <- input$niterations
    rv$CL <- input$CL
    rv$sizeclassCol <- input$sizeclassCol
    rv$kFixedChoice <- input$kFixedChoice
 
    rv$sizeclasses <- as.character(unique(rv$dataSE[ , rv$sizeclassCol]))
    if (length(rv$sizeclassCol) == 0){
      rv$sizeclasses <- "all"
    }

    rv$predictors_p <- paste(rv$predsSE, collapse = "*")
    if (length(rv$predsSE) == 0){
      rv$predictors_p <- 1
    }
    rv$formula_p <- formula(paste("p~", rv$predictors_p, sep = ""))
    rv$formula_k <- formula(paste("k~", rv$predictors_p, sep = "")) 

    rv$modsSE <- pkmSetSize(formula_p = rv$formula_p,
                   formula_k = rv$formula_k, data = rv$dataSE, 
                   obsCols = rv$obsColsSE, sizeclassCol = rv$sizeclassCol,
                   kFixed = rv$kFixed, kInit = 0.7
                 )
    rv$modsCheckSE <- pkmCheck(rv$modsSE)

    if (length(rv$sizeclasses) == 1){
      rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE) 
      rv$modOrderSE <- as.numeric(row.names(rv$AICcTabSE))
      rv$modNamesSE <- names(rv$modsSE)[rv$modOrderSE]
      rv$modTabSE <- rv$modsSE[[rv$modOrderSE[1]]]$cellwise_pk
      rv$modSetSE_spec <- rv$modsSE
      rv$figSEnrow <- ceiling((rv$modSetSE_spec[[1]])$ncell / 2 )
      rv$figSEht <- rv$figSEnrow * 200 + 200
      if (rv$modSetSE_spec[[1]]$ncell > 6){
        rv$figSEwh <- 1200
      }
      rv$bestSE <- (names(rv$modSetSE_spec)[rv$modOrderSE])[1]
      output$figSE <- renderPlot({
                        plot(rv$modSetSE_spec, specificModel = rv$bestSE)},
                        height = rv$figSEht, width = rv$figSEwh
                      )
    }else{
      rv$sizeclassChosen <- which(rv$sizeclasses == input$aicTabSizeClassSE)
      if (length(rv$sizeclassChosen) == 0){
        rv$sizeclassChosen <- 1
      }
      rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE[[rv$sizeclassChosen]])
      rv$modOrderSE <- as.numeric(row.names(rv$AICcTabSE))
      rv$modNamesSE <- names(rv$modsSE[[1]])[rv$modOrderSE]
      rv$modTabSE <- rv$modsSE[[1]][[rv$modOrderSE[1]]]$cellwiseTable
      rv$modSetSE_spec <- rv$modsSE[[rv$sizeclassChosen]]
      rv$figSEnrow <- ceiling(rv$modSetSE_spec[[1]]$ncell / 2 )
      rv$figSEht <- rv$figSEnrow * 200 + 200
      if (rv$modSetSE_spec[[1]]$ncell > 6){
        rv$figSEwh <- 1200
      }
      rv$bestSE <- (names(rv$modSetSE_spec)[rv$modOrderSE])[1]
      output$figSE <- renderPlot({
                        plot(rv$modSetSE_spec, specificModel = rv$bestSE)},
                        height = rv$figSEht, width = rv$figSEwh
                      )
    }

    output$AICcTabSE <- DT::renderDataTable({rv$AICcTabSE})    
    output$modTabSE <- DT::renderDataTable({rv$modTabSE})

    updateSelectizeInput(session, "figSizeClassSE", choices = rv$sizeclasses)
    updateSelectizeInput(session, "figModSE", choices = rv$modNamesSE)
    updateSelectizeInput(session, "modTabSizeClassSE", 
      choices = rv$sizeclasses
    )
    updateSelectizeInput(session, "modTabModSE", choices = rv$modNamesSE)
    updateSelectizeInput(session, "aicTabSizeClassSE", 
      choices = rv$sizeclasses
    )
    updateTabsetPanel(session, "analysesSE", "Model Comparison Tables")

    removeNotification(msgRunModSE)
    if (rv$modsCheckSE == FALSE){
      msgModFitSE <- showNotification("Not all models were fit properly.",
                       type = "warning", duration = NULL)
    }
    isolate({

      output$modelMenuSE <- renderUI({
        
        modelMenuSE <- ""
        nsizeclasses <- length(rv$sizeclasses)
        if (nsizeclasses > 0){
          if (nsizeclasses == 1){
            AICcTab <- pkmSetAICcTab(rv$modsSE) 
            modOrder <- as.numeric(row.names(AICcTab))
            modNames <- names(rv$modsSE)[modOrder]
            mtuText <- "modelChoicesSE"
            scText <- "Model choice"
            modSelect <- selectizeInput(mtuText, scText, modNames)
            modelMenuSE <- paste(modelMenuSE, modSelect)  
          }else{
            for(sci in 1:nsizeclasses){
              AICcTab <- pkmSetAICcTab(rv$modsSE[[sci]])
              modOrder <- as.numeric(row.names(AICcTab))
              modNames <- names(rv$modsSE[[sci]])[modOrder]
              mtuText <- paste("modelChoicesSE", sci, sep = "") 
              scText <- paste("Model choice, ", rv$sizeclasses[sci], sep = "")
              modSelect <- selectizeInput(mtuText, scText, modNames)
              modelMenuSE <- paste(modelMenuSE, modSelect)  
            }
          }
        }  
        HTML(modelMenuSE)
      })
    })
  })

  observeEvent(input$aicTabSizeClassSE, {
    if (length(rv$modsSE) > 0){
      if (length(rv$sizeclasses) == 1){
        rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE) 
      }else{
        rv$sizeclassChosen <- which(rv$sizeclasses == input$aicTabSizeClassSE)
        if (length(rv$sizeclassChosen) == 0){
          rv$sizeclassChosen <- 1
        }
        rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE[[rv$sizeclassChosen]])
      }
      output$AICcTabSE <- DT::renderDataTable({rv$AICcTabSE})
    }
  })

  observeEvent(input$modTabSizeClassSE, {
    if (length(rv$modsSE) > 0){
      if (length(rv$sizeclasses) == 1){
        rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE) 
        rv$modOrderSE <- as.numeric(row.names(rv$AICcTabSE))
        rv$modNamesSE <- names(rv$modsSE)[rv$modOrderSE]
        rv$modTabSE <- rv$modsSE[[rv$modOrderSE[1]]]$cellwiseTable
      }else{
        rv$sizeclassChosen <- which(rv$sizeclasses == input$modTabSizeClassSE)
        if (length(rv$sizeclassChosen) == 0){
          rv$sizeclassChosen <- 1
        }
        rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE[[rv$sizeclassChosen]])
        rv$modOrderSE <- as.numeric(row.names(rv$AICcTabSE))
        rv$modNamesSE <- names(rv$modsSE[[1]])[rv$modOrderSE]
        rv$modTabSE <- 
          rv$modsSE[[rv$sizeclassChosen]][[rv$modOrderSE[1]]]$cellwiseTable
      }

      output$modTabSE <- DT::renderDataTable(rv$modTabSE)
      updateSelectizeInput(session, "modTabModSE", choices = rv$modNamesSE)

      observeEvent(input$modTabModSE, {
        if (length(rv$sizeclasses) == 1){
          rv$modTabSE <- rv$modsSE[[input$modTabModSE]]$cellwiseTable
        }else{
          rv$modTabSE <- 
            rv$modsSE[[rv$sizeclassChosen]][[input$modTabModSE]]$cellwiseTable
        }
        output$modTabSE <- DT::renderDataTable(rv$modTabSE)
      })
    }
  })

  observeEvent(input$figSizeClassSE, {
    if (length(rv$modsSE) > 0){
      if (length(rv$sizeclasses) == 1){
        rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE) 
        rv$modOrderSE <- as.numeric(row.names(rv$AICcTabSE))
        rv$modNamesSE <- names(rv$modsSE)[rv$modOrderSE]
        rv$modSetSE_spec <- rv$modsSE
        rv$bestSE <- (names(rv$modSetSE_spec)[rv$modOrderSE])[1]
        output$figSE <- renderPlot({ 
                          plot(rv$modSetSE_spec, specificModel = rv$bestSE)},
                          height = rv$figSEht, width = rv$figSEwh
                        )
      }else{
        rv$sizeclassChosen <- which(rv$sizeclasses == input$figSizeClassSE)
        if (length(rv$sizeclassChosen) == 0){
          rv$sizeclassChosen <- 1
        }
        rv$AICcTabSE <- pkmSetAICcTab(rv$modsSE[[rv$sizeclassChosen]])
        rv$modOrderSE <- as.numeric(row.names(rv$AICcTabSE))
        rv$modNamesSE <- names(rv$modsSE[[1]])[rv$modOrderSE]
        rv$modSetSE_spec <- rv$modsSE[[rv$sizeclassChosen]]
        rv$bestSE <- (names(rv$modSetSE_spec)[rv$modOrderSE])[1]
        output$figSE <- renderPlot({ 
                          plot(rv$modSetSE_spec, specificModel = rv$bestSE)},
                          height = rv$figSEht, width = rv$figSEwh
                        )
      }

      updateSelectizeInput(session, "figModSE", choices = rv$modNamesSE)

      observeEvent(input$figModSE, {
        if (length(rv$sizeclasses) == 1){
          rv$modSetSE_spec <- rv$modsSE
          output$figSE <- renderPlot({
                             plot(rv$modSetSE_spec, 
                               specificModel = input$figModSE
                             )}, height = rv$figSEht, width = rv$figSEwh
                          )
        }else{
          rv$modSetSE_spec <- rv$modsSE[[rv$sizeclassChosen]]
          output$figSE <- renderPlot({
                             plot(rv$modSetSE_spec, 
                               specificModel = input$figModSE
                             )}, height = rv$figSEht, width = rv$figSEwh
                          )
        }

      })
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

