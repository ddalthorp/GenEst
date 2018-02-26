#############################################################################
#
#  This script contains the server code for the GenEst application
#



# main server function

  function(input, output, session) {

    # welcome disclaimer

      version_number <- packageDescription("GenEst", field = "Version")
      version_date <- packageDescription("GenEst", field = "Date")
      output$version_info <- renderText(paste("This is version ", 
                                              version_number, " (", 
                                              version_date, ")", 
                                              sep = ""))

      showModal(modalDialog(title = paste("GenEst v", version_number, " (",
                                            version_date, ")", sep = ""), 
         "This software is preliminary or provisional and is subject to 
          revision. It is being provided to meet the need for timely best 
          science. The software has not received final approval by the U.S.
          Geological Survey (USGS). No warranty, expressed or implied, is 
          made by the USGS or the U.S. Government as to the functionality 
          of the software and related material nor shall the fact of release 
          constitute any such warranty. The software is provided on the 
          condition that neither the USGS nor the U.S. Government shall be 
          held liable for any damages resulting from the authorized or 
          unauthorized use of the software.", easyClose = F, 
          footer = modalButton("Ok")))

    # initialize the object to hold reactive values

      rv <- reactiveValues(
 
              SEdataIn = "NONE", SEcolnames = character(0), 
              SEaictable = NULL, SEaicorder = NULL, 
              SEsizeclasses = NULL, SEnsizeclasses = NULL,  
              SEmods = NULL, SEmodnames = NULL, SEnmods = NULL, 
              SEmodstouse = NULL, SEtheta = NULL, SEdt = NULL,
              SEvars = NULL, SEobscols = NULL, SEsizeclasscols = NULL,
              fixKchoice = NULL, fixKvalchoice = NULL,

              CPdataIn = "NONE", CPcolnames = character(0), 
              CPaictable = NULL, CPaicorder = NULL, CPaicmatrix = NULL,
              CPsizeclasses = NULL, CPnsizeclasses = NULL,
              CPmods = NULL, CPmodnames = NULL, CPmodnames1 = NULL, 
              CPnmodnames1 = NULL, CPmodcomps = NULL, CPdistnames = NULL,
              CPmodstouse = NULL, CPtheta = NULL,   
              CPvars = NULL, CPltp = NULL, CPfta = NULL, CPsizeclasscols = NULL,
            
              SSdataIn = "NONE", SSs = NULL, 
              gtable = NULL, garray = NULL,  

              COdataIn = "NONE", COcolnames = character(0), 
              DWPdatatab = NULL, ffs = NULL, 
              COsizeclasscol = NULL, COsplitcol = NULL, COunitcol = NULL, 
              COdfcol = NULL, 

              Mhatarray = NULL, Mhatsc = NULL, splitcats = NULL, 
              Nsplitcats = NULL, 

              Niterations = NULL, CL = NULL, 

              MDdataIn = "NONE")


    # Data Input

      # when the SE file is uploaded, pull data in and update column options

        observeEvent(input$SEFile, {

          rv$SEdataIn <- read.csv(input$SEFile$datapath, header = T)
          rv$SEcolnames <- colnames(rv$SEdataIn)
          output$SEdata <- renderDataTable(rv$SEdataIn)

          updateSelectizeInput(session, "SEvars", choices = rv$SEcolnames)
          updateSelectizeInput(session, "SEobscols", choices = rv$SEcolnames)
          updateSelectizeInput(session, "SEsizeclasscol", 
                                choices = rv$SEcolnames)

          updateTabsetPanel(session, "LoadedDataViz", "Search Efficiency")

        })

      # when the CP file is uploaded, pull data in and update column options

        observeEvent(input$CPFile, {

          rv$CPdataIn <- read.csv(input$CPFile$datapath, header = T)
          rv$CPcolnames <- colnames(rv$CPdataIn)
          output$CPin <- renderDataTable(rv$CPdataIn)

          updateSelectizeInput(session, "CPvars", choices = rv$CPcolnames)
          updateSelectizeInput(session, "CPsizeclasscol", 
                                 choices = rv$CPcolnames)
          updateSelectizeInput(session, "CPltp", choices = rv$CPcolnames)
          updateSelectizeInput(session, "CPfta", choices = rv$CPcolnames)

          updateTabsetPanel(session, "LoadedDataViz", "Carcass Persistence")

        })

      # when the SS file is uploaded, pull data in and update tables

        observeEvent(input$SSFile, {

          rv$SSdataIn <- read.csv(input$SSFile$datapath, header = T)
          output$SSin <- renderDataTable(rv$SSdataIn)
          rv$SSs <- create_ss_vec(data = rv$SSdataIn)
          rv$DWPdatatab <- create_DWP_table(data = rv$SSdataIn)
          output$SStable <- renderTable(create_ss_table(rv$SSdataIn))

          updateTabsetPanel(session, "LoadedDataViz", "Search Schedule")

        })

      # when the CO file is uploaded, pull data in and update column options

        observeEvent(input$COFile, {

          rv$COdataIn <- read.csv(input$COFile$datapath, header = T)
          rv$COcolnames <- colnames(rv$COdataIn)
          output$COin <- renderDataTable(rv$COdataIn)

          updateSelectizeInput(session, "COsplitcol", choices = rv$COcolnames)
          updateSelectizeInput(session, "COsizeclasscol", 
                                 choices = rv$COcolnames)
          updateSelectizeInput(session, "COdfcol", choices = rv$COcolnames)
          updateSelectizeInput(session, "COunitcol", choices = rv$COcolnames)

          updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")

        })

 

    # Search Efficiency

      # when the observation columns are selected, output the table
 
        observeEvent(input$SEobscols, {

          output$selected_SE <- renderDataTable(rv$SEdataIn[ ,
                                     which(rv$SEcolnames
                                     %in% c(input$SEobscols, 
                                        input$SEsizeclasscol, input$SEvars))])
        })

      # when the SE model run button is pushed, run the SE models, calculate
      #   theta, produce the AIC tables, and set up options for the model
      #   selections and outputs

        observeEvent(input$SEmodrun, {
          withProgress(message = "Running SE Model", {

            rv$SEvars <- input$SEvars
            rv$SEobscols <- input$SEobscols
            rv$SEsizeclasscol <- input$SEsizeclasscol
            rv$fixKchoice <- input$fixKchoice
            rv$fixKvalchoice <- input$fixKvalchoice
            rv$Niterations <- input$Niterations


            if(rv$fixKvalchoice < 0){
              rv$fixKvalchoice <- 0
              updateNumericInput(session, "fixKvalchoice", 
                               "Value for fixed k:", value = 0, 
                                min = 0, max = 1, step = 0.001)
            }
            if(rv$fixKvalchoice > 1){
              rv$fixKvalchoice <- 1
              updateNumericInput(session, "fixKvalchoice", 
                               "Value for fixed k:", value = 1, 
                                min = 0, max = 1, step = 0.001)
            }


            rv$SEmods <- se_model_set_across_sizes_fit(data = rv$SEdataIn, 
                                   observation_columns = rv$SEobscols,
                                   predictors = rv$SEvars,
                                   size_class_column = rv$SEsizeclasscol,
                                   fix_k = rv$fixKchoice, 
                                   fix_k_value = rv$fixKvalchoice, 
                                   init_k_value = 0.7)
            rv$SEtheta <- se_theta_create(data = rv$SEdataIn, 
                                  predictors = rv$SEvars,
                                  size_class_column = rv$SEsizeclasscol,
                                  model_fits = rv$SEmods, 
                                  replicates = rv$Niterations,
                                  fix_k = rv$fixKchoice, 
                                  fix_k_value = rv$fixKvalchoice)

            rv$SEaictable <- se_aicc_table_create(models = rv$SEmods)
            rv$SEaicorder <- order_se_models(rv$SEmods)
            rv$SEsizeclasses <- unique(rv$SEdataIn[, rv$SEsizeclasscol])

            if(length(rv$SEsizeclasscol) == 0){
              rv$SEsizeclasses <- "all" 
            }
            rv$SEnsizeclasses <- length(rv$SEsizeclasses)
            rv$SEmodstouse <- rep(NA, rv$SEnsizeclasses)
            rv$SEnmods <- length(rv$SEmods[[1]])
            rv$SEmodnames <- model_namer(names(rv$SEmods[[1]]))


            updateSelectizeInput(session, "SEaicsizeclass",
                                 choices = rv$SEsizeclasses)
            updateSelectizeInput(session, "SEfigsizeclass", 
                                 choices = rv$SEsizeclasses)

            updateSelectizeInput(session, "SEfigmodel",
                                 choices = rv$SEmodnames[rv$SEaicorder[[1]]])


            output$SEaictable <- renderDataTable({
                NULL
            })
            output$SEfig <- renderPlot({
                NULL
            })

            # hop over to the model table tab

              updateTabsetPanel(session, "SE_Analysis", "Model Table")

            # auto populate the model table tab

              scofi <- which(rv$SEsizeclasses == input$SEaicsizeclass)
              scofi[length(scofi) == 0] <- 1
              output$SEaictable <- renderDataTable({
                  rv$SEaictable[[scofi]]
              })

            # auto populate the figure

              scofi <- which(rv$SEsizeclasses == input$SEfigsizeclass)
              scofi[length(scofi) == 0] <- 1
              mofi <- which(rv$SEmodnames == input$SEfigmodel)
              mofi[length(mofi) == 0 ] <- 1
              CWM <- rv$SEnmods
              CWM[length(CWM) == 0] <- 1
              output$SEfig <- renderPlot({
                                create_se_figure(data = rv$SEdataIn, 
                                    predictors = rv$SEvars, 
                                    theta = rv$SEtheta,
                                    observation_columns = rv$SEobscols,  
                                    replicates = rv$Niterations, 
                                    size_class_column = rv$SEsizeclasscol, 
                                    r = scofi, j = mofi, cellwise = CWM)
              })

            isolate({

              output$SEmodselectinputs <- renderUI({

                w <- ""
  
                for(i in 1:rv$SEnsizeclasses){
                  w <- paste(w, selectizeInput(paste("SEmodstouse", 
                                i, sep = ""),
                                rv$SEsizeclasses[i], 
                                choices = rv$SEmodnames[rv$SEaicorder[[i]]]))
                }
  
                HTML(w)
              })
            })
          })
        })

      # when the size class is changed, update the table

        observeEvent(input$SEaicsizeclass, {

          scofi <- which(rv$SEsizeclasses == input$SEaicsizeclass)
          scofi[length(scofi) == 0] <- 1
          output$SEaictable <- renderDataTable({
              rv$SEaictable[[scofi]]
          })
        })

      # when the size class or model are changed, update the figure

        observeEvent(input$SEfigsizeclass, {

          scofi <- which(rv$SEsizeclasses == input$SEfigsizeclass)
          scofi[length(scofi) == 0] <- 1

          updateSelectizeInput(session, "SEfigmodel",
                              choices = rv$SEmodnames[rv$SEaicorder[[scofi]]])

          observeEvent(input$SEfigmodel, {

            mofi <- which(rv$SEmodnames == input$SEfigmodel)
            mofi[length(mofi) == 0 ] <- 1
            CWM <- rv$SEnmods
            CWM[length(CWM) == 0] <- 1
            output$SEfig <- renderPlot({
                              create_se_figure(data = rv$SEdataIn, 
                                predictors = rv$SEvars, theta = rv$SEtheta,
                                observation_columns = rv$SEobscols,  
                                replicates = rv$Niterations, 
                                size_class_column = rv$SEsizeclasscol, 
                                r = scofi, j = mofi, cellwise = CWM)
            })
          })
        })


    # Carcass Persistence

      # when the observation columns are selected, output the table

        observeEvent(input$CPltp, {
          observeEvent(input$CPfta, {

            output$selected_CP <- renderDataTable(rv$CPdataIn[ ,
                                       which(rv$CPcolnames
                                       %in% c(input$CPltp, input$CPfta, 
                                        input$CPsizeclasscol, input$CPvars))])
          })
        })

      # when the CP model run button is pushed, run the CP models, calculate
      #   theta, produce the AIC tables, and set up options for the model
      #   selections and outputs

        observeEvent(input$CPmodrun, {
          withProgress(message = "Running CP Model", {

            rv$CPvars <- input$CPvars
            rv$CPltp <- input$CPltp
            rv$CPfta <- input$CPfta
            rv$CPsizeclasscol <- input$CPsizeclasscol
            rv$Niterations <- input$Niterations

            rv$CPdistnames <- c("exponential", "weibull", 
                                "loglogistic", "lognormal")

            rv$CPmods <- cp_model_set_across_sizes_fit(data = rv$CPdataIn, 
                                   predictors = rv$CPvars,
                                   size_class_column = rv$CPsizeclasscol, 
                                   last_time_present_column = rv$CPltp, 
                                   first_time_absent_column = rv$CPfta)

            rv$CPtheta <- cp_theta_create(data = rv$CPdataIn, 
                                     predictors = rv$CPvars,
                                     size_class_column = rv$CPsizeclasscol,
                                     model_fits = rv$CPmods, 
                                     replicates = rv$Niterations)

            rv$CPaictable <- cp_aicc_table_create(models = rv$CPmods)
            rv$CPaicorder <- order_cp_models(models = rv$CPmods)
            rv$CPsizeclasses <- unique(rv$CPdataIn[, rv$CPsizeclasscol])

            if(length(input$CPsizeclasscol) == 0){
              rv$CPsizeclasses <- "all" 
            }


            rv$CPmodnames1 <- model_namer(names(rv$CPmods[[1]]))
            rv$CPnmodnames1 <- length(rv$CPmodnames1)
            rv$CPmodcomps <- rv$CPmodnames1[seq(1, rv$CPnmodnames1, 4)]

            rv$CPnsizeclasses <- length(rv$CPsizeclasses)
            rv$CPmodstouse <- rep(NA, rv$CPnsizeclasses)
            rv$CPmodnames <- paste(rv$CPmodnames1, 
                                     rv$CPdistnames, sep = " ")

            updateSelectizeInput(session, "CPaicsizeclass", 
                                  choices = rv$CPsizeclasses)
            updateSelectizeInput(session, "CPfigsizeclass", 
                                  choices = rv$CPsizeclasses)

            updateSelectizeInput(session, "CPfigmodelcomplexity",
                              choices = unique(rv$CPaictable[[1]]$model))
            bm <- unique(rv$CPaictable[[1]]$model)[1]
            wbm <- which(rv$CPaictable[[1]]$model == bm)
            updateSelectizeInput(session, "CPfigdistemph", 
                      choices = (rv$CPaictable[[1]]$distribution)[wbm])

            output$CPaictable <- renderDataTable({
                NULL
            })
            output$CPfig <- renderPlot({
                NULL
            })

            # hop over to the model table tab

              updateTabsetPanel(session, "CP_Analysis", "Model Table")

            # auto populate the model table tab

              scofi <- which(rv$CPsizeclasses == input$CPaicsizeclass)
              scofi[length(scofi) == 0] <- 1
              output$CPaictable <- renderDataTable({
                  rv$CPaictable[[scofi]]
              })

            # auto populate the figure

              scofi <- which(rv$CPsizeclasses == input$CPfigsizeclass)
              scofi[length(scofi) == 0] <- 1
              mofi <- model_name_reverser(input$CPfigmodelcomplexity)
              mofi[which(mofi == "~ Model not yet run") ] <- "~ 1"
              mofi[length(mofi) == 0 ] <- "~ 1"
              DC <- input$CPfigdistemph
              DC[length(DC) == 0] <- "exponential"
              output$CPfig <- renderPlot({

                                create_cp_figure(models = rv$CPmods, 
                                   data = rv$CPdataIn, 
                                   predictors = rv$CPvars, 
                                   theta = rv$CPtheta, 
                                   time_unit = "days", 
                                   size_class_column = rv$CPsizeclasscol, 
                                   last_time_present_column = rv$CPltp, 
                                   first_time_absent_column = rv$CPfta, 
                                   r = scofi, model_complexity = mofi, 
                                   distribution_choice = DC)
              })

          isolate({

            output$CPmodselectinputs <- renderUI({

              w <- ""

              for(i in 1:rv$CPnsizeclasses){
                w <- paste(w, selectizeInput(paste("CPmodstouse", 
                               i, sep = ""),
                               rv$CPsizeclasses[i], 
                               choices = rv$CPmodnames[rv$CPaicorder[[i]]]))
              }

              HTML(w)
            })
          })

          })
        })

      # update the table when the size class is changed

        observeEvent(input$CPaicsizeclass, {

          scofi <- which(rv$CPsizeclasses == input$CPaicsizeclass)
          scofi[length(scofi) == 0] <- 1
          output$CPaictable <- renderDataTable({
              rv$CPaictable[[scofi]]
          })
        })

      # update the figure when the size class or model or dist has changed

        observeEvent(input$CPfigsizeclass, {

          scofi <- which(rv$CPsizeclasses == input$CPfigsizeclass)
          scofi[length(scofi) == 0] <- 1
          updateSelectizeInput(session, "CPfigmodelcomplexity",
                              choices = unique(rv$CPaictable[[scofi]]$model))

          observeEvent(input$CPfigmodelcomplexity, {

            bm <- unique(rv$CPaictable[[scofi]]$model)[1]
            wbm <- which(rv$CPaictable[[scofi]]$model == bm)
            updateSelectizeInput(session, "CPfigdistemph", 
                      choices = (rv$CPaictable[[scofi]]$distribution)[wbm])

            observeEvent(input$CPfigdistemph, {
              mofi <- model_name_reverser(input$CPfigmodelcomplexity)
              mofi[length(mofi) == 0 ] <- "~ 1"
              DC <- input$CPfigdistemph
              DC[length(DC) == 0] <- NULL
              output$CPfig <- renderPlot({
                                create_cp_figure(models = rv$CPmods, 
                                   data = rv$CPdataIn, predictors = rv$CPvars, 
                                   theta = rv$CPtheta, 
                                   time_unit = "days", 
                                   size_class_column = rv$CPsizeclasscol, 
                                   last_time_present_column = rv$CPltp, 
                                   first_time_absent_column = rv$CPfta, 
                                   r = scofi, model_complexity = mofi, 
                                   distribution_choice = DC)
              })
            })
          })
        })



    # Detection Probability

      # when the Estimate Detection Probability button is pushed, estimate
      #  detection probability for each size class

        observeEvent(input$grun, {
          withProgress(message = "Estimating Detection Probability", {

            rv$Niterations <- input$Niterations
            rv$CL <- input$CL

            for(i in 1:rv$SEnsizeclasses){
              rv$SEmodstouse[i] <- which(rv$SEmodnames ==
                                     input[[sprintf("SEmodstouse%d", i)]])
            }   
            for(i in 1:rv$CPnsizeclasses){
              rv$CPmodstouse[i] <- which(rv$CPmodnames == 
                                    input[[sprintf("CPmodstouse%d", i)]])
            }      

            rv$garray <- estimate_g_across_sizes(cp_data = rv$CPdataIn, 
                                se_data = rv$SEdataIn, 
                                ss_data = rv$SSdataIn, 
                                replicates = rv$Niterations, 
                                cp_predictors = rv$CPvars, 
                                se_predictors = rv$SEvars, 
                                cp_theta = rv$CPtheta, se_theta = rv$SEtheta, 
                                cp_models = rv$CPmods,
                                cp_models_to_use = rv$CPmodstouse,
                                se_models_to_use = rv$SEmodstouse)

            rv$gtable <- create_g_table(rv$garray, 
                                        confidence_level = rv$CL)

            output$gtable <- renderDataTable({
                                              rv$gtable
            })
          })
        })


    # Fatality Estimation

      # when the unit and date found columns are selected, output the table
 
        observeEvent(input$COunitcol, {
          observeEvent(input$COdfcol, {
            output$selected_CO <- renderDataTable(rv$COdataIn[ ,
                                     which(rv$COcolnames
                                     %in% c(input$COunitcol, input$COdfcol,
                                       input$COsizeclasscol, input$COsplitcol
                                       ))])
          })
        })

      # when the Estimate Total Carcasses button is pushed, estimate
      #  total carcasses for each split category, output figure and table

        observeEvent(input$Mrun, {
          withProgress(message = "Estimating Fatalities", {

            rv$Niterations <- input$Niterations
            rv$CL <- input$CL

            rv$ffs <- input$ffs
            rv$COsizeclasscol <- input$COsizeclasscol
            rv$COsplitcol <- input$COsplitcol
            rv$COunitcol <- input$COunitcol
            rv$COdfcol <- input$COdfcol


            rv$Mhatarray <- estimate_mhat(co_data = rv$COdataIn, 
                            ss_data = rv$SSdataIn, 
                            size_class_column = input$COsizeclasscol, 
                            split_column = input$COsplitcol, 
                            unit_column = input$COunitcol, 
                            df_column = input$COdfcol,
                            replicates = rv$Niterations,  
                            cp_predictors = rv$CPvars, 
                            se_predictors = rv$SEvars, cp_data = rv$CPdataIn, 
                            se_data = rv$SEdataIn, garray = rv$garray) 

            rv$Mhatsc <- condense_mhat(rv$Mhatarray)

            output$Mhattab <-  renderTable({
                                  create_mhat_table(
                                            condensed_mhat = rv$Mhatsc, 
                                            fraction_area_sampled = rv$ffs, 
                                            confidence_level = rv$CL)
            })

            rv$Nsplitcats <- length(unique(rv$COdataIn[,input$COsplitcol]))
            rv$Nsplitcats[rv$Nsplitcats == 0] <- 1
            output$Mhatfig <- renderPlot({
 
                par(mfrow = c(1, rv$Nsplitcats))
                for(i in 1:rv$Nsplitcats){
                    create_mhat_figure(condensed_mhat_split = rv$Mhatsc[,i], 
	                split_category_name = colnames(rv$Mhatsc)[i],
                      fraction_area_sampled = input$ffs)
                }
            }) 

            updateTabsetPanel(session, "M_Analysis", "Table")

          }) 
        })

}

