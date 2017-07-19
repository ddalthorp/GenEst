###############################################################################
###############################################################################
##
##  This script contains the server code for the the GenEst app
##
##  Coded by Joseph (Josie, they) L. Simonis, DAPPER Stats
##
##  version 0.0.0.1 2017
##
##  Held under GNU GPL v >= 3	
##
###############################################################################
###############################################################################

##############################
#
#  Table of Contents
#
#  1. load packages
#  2. server code
#

#1. load code and packages

  source("genestfunctions.R")
  packageLoad()



#2. server code

shinyServer(function(input, output, session) {

  # reactive values
  #
  #  EF_SE   =  EntryFile_SearcherEfficiency (logical re: its loading)
  #  data_SE =  the Searcher Efficiency data
 
    rv <- reactiveValues(EF = F, data_SE = "NONE", cn_SE = NULL, 
                         CP = F, data_CP = "NONE", 
                         SS = F, data_SS = "NONE", 
                         FO = F, data_FO = "NONE", 
                         MD = F, data_MD = "NONE")

  # Upload data buttons
  
    # Search Efficiency

      observeEvent(input$SEFile, {

        rv$EF <- T
        rv$data_SE <- read.csv(input$SEFile$datapath, header = T)
        rv$cn_SE <- colnames(rv$data_SE)
        output$SEin <- renderTable(rv$data_SE)

        if(rv$EF == F){
          rv$cn_SE <- character(0) 
        }
        output$selected_factors_SE <- renderTable(NULL)

        updateSelectizeInput(session, "SEfactorselect", choices = rv$cn_SE)
        updateSelectizeInput(session, "SEobsselect", choices = rv$cn_SE)

        observeEvent(input$SEfactorselect, {
          observeEvent(input$SEobsselect, {
            output$selected_SE <- renderTable(rv$data_SE[,which(rv$cn_SE %in% c(input$SEfactorselect, input$SEobsselect))])
          })
        })
      })

    # Carcass Persistence

      observeEvent(input$CPFile, {

        rv$CP <- T
        rv$data_CP <- read.csv(input$CPFile$datapath, header = T)

        output$CPin <- renderTable(rv$data_CP)
      })

    # Search Schedule

      observeEvent(input$SSFile, {

        rv$SS <- T
        rv$data_SS <- read.csv(input$SSFile$datapath, header = T)

        output$SSin <- renderTable(rv$data_SS)
      })


    # Fatality Observations

      observeEvent(input$FOFile, {

        rv$FO <- T
        rv$data_FO <- read.csv(input$FOFile$datapath, header = T)

        output$FOin <- renderTable(rv$data_FO)
      })

    # Meta Data

      observeEvent(input$MDFile, {

        rv$MD <- T
        rv$data_MD <- read.csv(input$MDFile$datapath, header = T)

        output$MDin <- renderTable(rv$data_MD)
      })
})

