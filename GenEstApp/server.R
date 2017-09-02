#############################################################################
#
#  This script contains the server code for the GenEst package
#
#  version 0.0.0.2 September 2017
#
#  Held under GNU GPL v >= 3	
#
#############################################################################

shinyServer(function(input, output, session) {

  # reactive values
  #
  #  EF_SE   =  EntryFile_SearcherEfficiency (logical re: its loading)
  #  SEdataIn =  the Searcher Efficiency data
  #  cn_SE = column names of the SE data input file
 
    rv <- reactiveValues(
            EF = F, SEdataIn = "NONE", cn_SE = NULL, SEmods = NULL, 
            thetaSE = NULL, SEmodsAICtab = NULL, SEsizeclasses = NULL,  
            SEmodnames = NULL, SEnmods = NULL, SEtext = NULL, 
            SEobscols = NULL, SEvars = NULL, SEsizeclasscol = NULL,  
            SEmodstouse = NULL, NsizeclassesSE = NULL,  
            CP = F, CPdataIn = "NONE",NsizeclassesCP = NULL,
            CPvars = NULL, CPsizeclasscol = NULL, CPltp = NULL, CPfta = NULL,
            CPmods = NULL, thetaCP = NULL ,CPsizeclasses = NULL,  
            CPmodcomps = NULL, CPdistnames = NULL,
            CPmodnames1 = NULL, CPmodnames = NULL, CPmodstouse = NULL, 
            nCPmodnames1 = NULL,
            SS = F, SSdataIn = "NONE", SSs = NULL, 
            gtable = NULL, garray = NULL,  
            CO = F, COdataIn = "NONE", cn_CO = NULL, 
            DWPdatatab = NULL, 
            Mhatarray = NULL, Mhatsc = NULL, splitcats = NULL, 
            Nsplitcats = NULL, 
            MD = F, data_MD = "NONE")

  # Upload data buttons
  
    # Search Efficiency

      observeEvent(input$SEFile, {

        rv$EF <- T
        rv$SEdataIn <- read.csv(input$SEFile$datapath, header = T)
        rv$cn_SE <- colnames(rv$SEdataIn)
        output$SEdata <- renderDataTable(rv$SEdataIn)

        if(rv$EF == F){
          rv$cn_SE <- character(0) 
        }

        updateSelectizeInput(session, "SEvars", choices = rv$cn_SE)
        updateSelectizeInput(session, "SEobscols", choices = rv$cn_SE)
        updateSelectizeInput(session, "SEsizeclasscol", choices = rv$cn_SE)

        observeEvent(input$SEobscols, {
          output$selected_SE <- renderTable(rv$SEdataIn[,which(rv$cn_SE %in%
                                    c(input$SEvars, input$SEobscols, 
                                         input$SEsizeclasscol))])
        })

        observeEvent(input$SEmodrun, {
          rv$SEvars <- input$SEvars
          rv$SEobscols <- input$SEobscols
          rv$SEsizeclasscol <- input$SEsizeclasscol


           rv$SEmods <- SEmodsacrosssizes(SEdata = rv$SEdataIn, 
                                   obscols = rv$SEobscols,
                                   SEvars = rv$SEvars,
                                   sizeclasscol = rv$SEsizeclasscol,
                                   fixK = input$fixKchoice, 
                                   fixKval = input$fixKvalchoice, 
                                   initKval = 0.7)
           rv$thetaSE <- ThetaSEcreateacrosssizes(SEdata = rv$SEdataIn, 
                                   SEvars = rv$SEvars,
                                  sizeclasscol = rv$SEsizeclasscol,
                                  rv$SEmods, input$Niterations,
                                  fixK = input$fixKchoice, 
                                  fixKval = input$fixKvalchoice)

          rv$SEaictable <- AICtabcreateSEmods(rv$SEmods, sortby = "AIC")

          rv$SEsizeclasses <- unique(rv$SEdataIn[,input$SEsizeclasscol])
          rv$NsizeclassesSE <- length(rv$SEsizeclasses)
          rv$SEmodstouse <- rep(NA, rv$NsizeclassesSE)
          rv$SEnmods <- length(rv$SEmods[[1]])
          rv$SEmodnames <- names(rv$SEmods[[1]])

          if(length(input$SEsizeclasscol) == 0){
            rv$SEsizeclasses <- "all" 
          }
          updateSelectizeInput(session, "SEaicsizeclass",
                                 choices = rv$SEsizeclasses)
          updateSelectizeInput(session, "SEfigsizeclass", 
                                 choices = rv$SEsizeclasses)
          updateSelectizeInput(session, "SEfigmodel",
                                 choices = rv$SEmodnames)

          output$SEaictable <- renderDataTable({
              NULL
          })
          output$SEfig <- renderPlot({
              NULL
          })
        })


        observeEvent(input$SEaictablerun, {
          scofi <- which(rv$SEsizeclasses == input$SEaicsizeclass)
          scofi[length(scofi) == 0] <- 1
          output$SEaictable <- renderDataTable({
              rv$SEaictable[[scofi]]
          })
        })

        observeEvent(input$SEfigrun, {
          scofi <- which(rv$SEsizeclasses == input$SEfigsizeclass)
          scofi[length(scofi) == 0] <- 1
          mofi <- which(rv$SEmodnames == input$SEfigmodel)
          mofi[length(mofi) == 0 ] <- 1
          CWM <- rv$SEnmods
          CWM[length(CWM) == 0] <- 1
          output$SEfig <- renderPlot({
                            SEgraphcreate(SEdata = rv$SEdataIn, 
                                rv$SEvars, rv$thetaSE, 
                                obscols = rv$SEobscols,  
                                input$Niterations, 
                                sizeclasscol = rv$SEsizeclasscol, 
                                r = scofi, j = mofi, CellWiseModel = CWM)
          })
        })


        observe({

          if(input$SEmodOpsPop == 0 ){
            return()
          }

          isolate({

            output$SEmodselectinputs <- renderUI({

              w <- ""


              for(i in 1:rv$NsizeclassesSE){
                w <- paste(w, selectizeInput(paste("SEmodstouse", 
                              i, sep = ""),
                              rv$SEsizeclasses[i], choices = rv$SEmodnames))
              }

              HTML(w)
            })


          })
        })



      })




    # Carcass Persistence

      observeEvent(input$CPFile, {

        rv$CP <- T
        rv$CPdataIn <- read.csv(input$CPFile$datapath, header = T)
        rv$cn_CP <- colnames(rv$CPdataIn)
        output$CPin <- renderDataTable(rv$CPdataIn)

        if(rv$CP == F){
          rv$cn_CP <- character(0) 
        }

        updateSelectizeInput(session, "CPvars", choices = rv$cn_CP)
        updateSelectizeInput(session, "CPsizeclasscol", choices = rv$cn_CP)
        updateSelectizeInput(session, "CPltp", choices = rv$cn_CP)
        updateSelectizeInput(session, "CPfta", choices = rv$cn_CP)


        observeEvent(input$CPltp, {
          output$selected_CP <- renderTable(rv$CPdataIn[,which(rv$cn_CP %in%
                                    c(input$CPltp, input$CPfta, 
                                      input$CPvars, input$CPsizeclasscol))])
        })

        observeEvent(input$CPmodrun, {
          rv$CPvars <- input$CPvars
          rv$CPsizeclasscol <- input$CPsizeclasscol
          rv$CPltp <- input$CPltp
          rv$CPfta <- input$CPfta



           rv$CPmods <- CPmodsacrosssizes(CPdata = rv$CPdataIn, 
                                   CPvars = rv$CPvars,
                                   sizeclasscol = rv$CPsizeclasscol, 
                                   CPltp = rv$CPltp, CPfta = rv$CPfta)

           rv$thetaCP <- ThetaCPcreateacrosssizes(CPdata = rv$CPdataIn, 
                                          CPvars = rv$CPvars,
                                          sizeclasscol = rv$CPsizeclasscol,
                                          rv$CPmods, input$Niterations)

          rv$CPaictable <- AICtabcreateCPmods(rv$CPmods, sortby = "AIC")

          rv$CPsizeclasses <- unique(rv$CPdataIn[,input$CPsizeclasscol])


          if(length(input$CPsizeclasscol) == 0){
            rv$CPsizeclasses <- "all" 
          }
          rv$CPdistnames <- c("exponential", "weibull", 
                                "loglogistic", "lognormal")

          rv$CPmodnames1 <- names(rv$CPmods[[1]])
          rv$nCPmodnames1 <- length(rv$CPmodnames1)
          rv$CPmodcomps <- rv$CPmodnames1[seq(1, rv$nCPmodnames1, 4)]

              rv$NsizeclassesCP <- length(rv$CPsizeclasses)
              rv$CPmodstouse <- rep(NA, rv$NsizeclassesCP)
              rv$CPmodnames <- paste(rv$CPmodnames1, 
                                     rv$CPdistnames, sep = " ")

          updateSelectizeInput(session, "CPaicsizeclass", 
                                choices = rv$CPsizeclasses)
          updateSelectizeInput(session, "CPfigsizeclass", 
                                choices = rv$CPsizeclasses)
          updateSelectizeInput(session, "CPfigmodelcomplexity", 
                                choices = rv$CPmodcomps)
          updateSelectizeInput(session, "CPfigdistemph", 
                                choices = rv$CPdistnames)

          output$CPaictable <- renderDataTable({
              NULL
          })
          output$CPfig <- renderPlot({
              NULL
          })

        })


        observeEvent(input$CPaictablerun, {
          scofi <- which(rv$CPsizeclasses == input$CPaicsizeclass)
          scofi[length(scofi) == 0] <- 1
          output$CPaictable <- renderDataTable({
              rv$CPaictable[[scofi]]
          })
        })

        observeEvent(input$CPfigrun, {
          scofi <- which(rv$CPsizeclasses == input$CPfigsizeclass)
          scofi[length(scofi) == 0] <- 1
          mofi <- input$CPfigmodelcomplexity
          mofi[length(mofi) == 0 ] <- "~ 1"
          DC <- input$CPfigdistemph
          DC[length(DC) == 0] <- NULL
          output$CPfig <- renderPlot({
                             CPgraphcreate(CPmods = rv$CPmods, 
                                CPdata = rv$CPdataIn, rv$CPvars, rv$thetaCP,
                                input$Niterations, timeunit = "days", 
                                sizeclasscol = rv$CPsizeclasscol, 
                                CPltp = rv$CPltp, CPfta = rv$CPfta, 
                                r = scofi, modelcomplexity = mofi, 
                                distchoice = DC)
          })
        })

        observe({

          if(input$CPmodOpsPop == 0 ){
            return()
          }

          isolate({

            output$CPmodselectinputs <- renderUI({

              w <- ""

              for(i in 1:rv$NsizeclassesCP){
                w <- paste(w, selectizeInput(paste("CPmodstouse", 
                               i, sep = ""),
                               rv$CPsizeclasses[i], 
                               choices = rv$CPmodnames))
              }

              HTML(w)
            })


          })
        })


      })


    # Search Schedule

      observeEvent(input$SSFile, {

        rv$SS <- T
        rv$SSdataIn <- read.csv(input$SSFile$datapath, header = T)

        output$SSin <- renderDataTable(rv$SSdataIn)

        rv$SSs <- SSveccreate(SSdata = rv$SSdataIn)

        rv$DWPdatatab <- DWPtablecreate(SSdata = rv$SSdataIn)

      })


    # Detection Probability

      observeEvent(input$grun, {

        for(i in 1:rv$NsizeclassesSE){
          rv$SEmodstouse[i] <- which(rv$SEmodnames ==
                                 input[[sprintf("SEmodstouse%d", i)]])
        }   
        for(i in 1:rv$NsizeclassesCP){
          rv$CPmodstouse[i] <- which(rv$CPmodnames == 
                                input[[sprintf("CPmodstouse%d", i)]])
        }      

        rv$garray <- gcreateacrosssizes(CPdata = rv$CPdataIn, 
                                SEdata = rv$SEdataIn, 
                                SSdata = rv$SSs, 
                                Niterations = input$Niterations, 
                                CPvars = rv$CPvars, SEvars = rv$SEvars, 
                                rv$thetaCP, rv$thetaSE, rv$CPmods,
                                rv$SEmodstouse, rv$CPmodstouse)

        rv$gtable <- gtablecreate(rv$garray, CIw = input$gCIw)

        output$gtable <- renderDataTable({
              rv$gtable
        })

      })


    # Fatality Observations

      observeEvent(input$COFile, {

        rv$CO <- T
        rv$COdataIn <- read.csv(input$COFile$datapath, header = T)
        rv$cn_CO <- colnames(rv$COdataIn)
        output$COin <- renderDataTable(rv$COdataIn)

        if(rv$CO == F){
          rv$cn_CO <- character(0) 
        }

        updateSelectizeInput(session, "COsplitcol", choices = rv$cn_CO)
        updateSelectizeInput(session, "COsizeclasscol", choices = rv$cn_CO)
        updateSelectizeInput(session, "COsscol", choices = rv$cn_CO)
        updateSelectizeInput(session, "COunitcol", choices = rv$cn_CO)

      })

      observeEvent(input$Mrun, {

        rv$Mhatarray <- Mhatgenerator(COdata = rv$COdataIn, 
                            DWPdata = rv$DWPdatatab, 
                            sizeclasscol = input$COsizeclasscol, 
                            splitcol = input$COsplitcol, 
                            unitcol = input$COunitcol, 
                            sscol = input$COsscol, 
                            Niterations = input$Niterations,
                            seedset = input$SEED, CPvars = rv$CPvars, 
                            SEvars = rv$SEvars, CPdata = rv$CPdataIn, 
                            SEdata = rv$SEdataIn, garray = rv$garray) 

        rv$Mhatsc <- Mhatcondense(rv$Mhatarray)

        output$Mhattab <-  renderTable({
                       Mhattable(Mhatl = rv$Mhatsc, ffs = input$ffs, 
                                CIw = input$MCIw)
        })

        rv$Nsplitcats <- length(unique(rv$COdataIn[,input$COsplitcol]))


        output$Mhatfig <- renderPlot({
 
                  par(mfrow = c(1, rv$Nsplitcats))
              for(i in 1:rv$Nsplitcats){
                  Mhatgraph(Mhatlspecific = rv$Mhatsc[,i], 
	              splitcatname = colnames(rv$Mhatsc)[i], ffs = input$ffs)
              }
        })  

      })

    # Meta Data

      observeEvent(input$MDFile, {

        rv$MD <- T
        rv$data_MD <- read.csv(input$MDFile$datapath, header = T)

        output$MDin <- renderTable(rv$data_MD)
      })
})

