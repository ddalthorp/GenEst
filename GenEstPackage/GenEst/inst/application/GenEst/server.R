#############################################################################
#
#  This script contains the server code for the GenEst package
#
#  version 0.0.1.2 November 2017
#
#  Held under GNU GPL v >= 3	
#
#############################################################################


# main server function

  function(input, output, session) {

    # welcome disclaimer

      showModal(modalDialog(title = "GenEst, v 0.0.0.5, November 2017", 
         "This software is preliminary or provisional and is subject to 
          revision. It is being provided to meet the need for timely best 
          science. The software has not received final approval by the U.S. s
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

              CPdataIn = "NONE", CPcolnames = character(0), 
              CPaictable = NULL, CPaicorder = NULL, 
              CPsizeclasses = NULL, CPnsizeclasses = NULL,
              CPmods = NULL, CPmodnames = NULL, CPmodnames1 = NULL, 
              CPnmodnames1 = NULL, CPmodcomps = NULL, CPdistnames = NULL,
              CPmodstouse = NULL, CPtheta = NULL,   
            
              SSdataIn = "NONE", SSs = NULL, 
              gtable = NULL, garray = NULL,  

              COdataIn = "NONE", COcolnames = character(0), 
              DWPdatatab = NULL, 

              Mhatarray = NULL, Mhatsc = NULL, splitcats = NULL, 
              Nsplitcats = NULL, 

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

        })

      # when the SS file is uploaded, pull data in and update tables

        observeEvent(input$SSFile, {

          rv$SSdataIn <- read.csv(input$SSFile$datapath, header = T)
          output$SSin <- renderDataTable(rv$SSdataIn)
          rv$SSs <- create_ss_vec(data = rv$SSdataIn)
          rv$DWPdatatab <- create_DWP_table(data = rv$SSdataIn)
          output$SStable <- renderTable(create_ss_table(rv$SSdataIn))

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

        })

      # when the MD file is uploaded, pull data in 

        observeEvent(input$MDFile, {

          rv$MDdataIn <- read.csv(input$MDFile$datapath, header = T)
          output$MDin <- renderTable(rv$MDdataIn)
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
            rv$SEmods <- se_model_set_across_sizes_fit(data = rv$SEdataIn, 
                                   observation_columns = input$SEobscols,
                                   predictors = input$SEvars,
                                   size_class_column = input$SEsizeclasscol,
                                   fix_k = input$fixKchoice, 
                                   fix_k_value = input$fixKvalchoice, 
                                   init_k_value = 0.7)
            rv$SEtheta <- se_theta_create(data = rv$SEdataIn, 
                                  predictors = input$SEvars,
                                  size_class_column = input$SEsizeclasscol,
                                  model_fits = rv$SEmods, replicates = input$Niterations,
                                  fix_k = input$fixKchoice, 
                                  fix_k_value = input$fixKvalchoice)

            rv$SEaictable <- se_aicc_table_create(models = rv$SEmods)
            rv$SEaicorder <- order_se_models(rv$SEmods)
            rv$SEsizeclasses <- unique(rv$SEdataIn[, input$SEsizeclasscol])

            if(length(input$SEsizeclasscol) == 0){
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
                                 choices = rv$SEmodnames)

            output$SEaictable <- renderDataTable({
                NULL
            })
            output$SEfig <- renderPlot({
                NULL
            })
          })
        })

      # when the generate AIC table button for the SE models is pushed, 
      #   output the table for the respective size class 

        observeEvent(input$SEaictablerun, {

          scofi <- which(rv$SEsizeclasses == input$SEaicsizeclass)
          scofi[length(scofi) == 0] <- 1
          output$SEaictable <- renderDataTable({
              rv$SEaictable[[scofi]]
          })
        })

      # when the generate SE figure button is pushed, output the figure for
      #   the specific size class and model selected

        observeEvent(input$SEfigrun, {
          scofi <- which(rv$SEsizeclasses == input$SEfigsizeclass)
          scofi[length(scofi) == 0] <- 1
          mofi <- which(rv$SEmodnames == input$SEfigmodel)
          mofi[length(mofi) == 0 ] <- 1
          CWM <- rv$SEnmods
          CWM[length(CWM) == 0] <- 1
          output$SEfig <- renderPlot({
                            create_se_figure(data = rv$SEdataIn, 
                                predictors = input$SEvars, theta = rv$SEtheta, 
                                observation_columns = input$SEobscols,  
                                replicates = input$Niterations, 
                                size_class_column = input$SEsizeclasscol, 
                                r = scofi, j = mofi, cellwise = CWM)
          })
        })


      # when the Population Options button is pushed, so long as the model
      #   has been run, output drop down selections for each size class

        observe({

          if(input$SEmodOpsPop == 0 ){
            return()
          }

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
            rv$CPmods <- cp_model_set_across_sizes_fit(data = rv$CPdataIn, 
                                   predictors = input$CPvars,
                                   size_class_column = input$CPsizeclasscol, 
                                   last_time_present_column = input$CPltp, 
                                   first_time_absent_column = input$CPfta)

            rv$CPtheta <- cp_theta_create(data = rv$CPdataIn, 
                                          predictors = input$CPvars,
                                          size_class_column = input$CPsizeclasscol,
                                          model_fits = rv$CPmods, 
                                          replicates = input$Niterations)

            rv$CPaictable <- cp_aicc_table_create(models = rv$CPmods)
            rv$CPaicorder <- order_cp_models(rv$CPmods)
            rv$CPsizeclasses <- unique(rv$CPdataIn[, input$CPsizeclasscol])

            if(length(input$CPsizeclasscol) == 0){
              rv$CPsizeclasses <- "all" 
            }
            rv$CPdistnames <- c("exponential", "weibull", 
                                "loglogistic", "lognormal")

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
        })

      # when the generate AIC table button for the CP models is pushed, 
      #   output the table for the respective size class 

        observeEvent(input$CPaictablerun, {

          scofi <- which(rv$CPsizeclasses == input$CPaicsizeclass)
          scofi[length(scofi) == 0] <- 1
          output$CPaictable <- renderDataTable({
              rv$CPaictable[[scofi]]
          })
        })

      # when the generate CP figure button is pushed, output the figure for
      #   the specific size class and model selected

        observeEvent(input$CPfigrun, {

          scofi <- which(rv$CPsizeclasses == input$CPfigsizeclass)
          scofi[length(scofi) == 0] <- 1
          mofi <- model_name_reverser(input$CPfigmodelcomplexity)
          mofi[length(mofi) == 0 ] <- "~ 1"
          DC <- input$CPfigdistemph
          DC[length(DC) == 0] <- NULL
          output$CPfig <- renderPlot({
                            create_cp_figure(models = rv$CPmods, 
                               data = rv$CPdataIn, predictors = input$CPvars, 
                               theta = rv$CPtheta, 
                               time_unit = "days", 
                               size_class_column = input$CPsizeclasscol, 
                               last_time_present_column = input$CPltp, 
                               first_time_absent_column = input$CPfta, 
                               r = scofi, model_complexity = mofi, 
                               distribution_choice = DC)
          })
        })

      # when the Population Options button is pushed, so long as the model
      #   has been run, output drop down selections for each size class

        observe({

          if(input$CPmodOpsPop == 0 ){
            return()
          }

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


    # Detection Probability

      # when the Estimate Detection Probability button is pushed, estimate
      #  detection probability for each size class

        observeEvent(input$grun, {
          withProgress(message = "Estimating Detection Probability", {
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
                                ss_data = rv$SSdataIn, replicates = input$Niterations, 
                                cp_predictors = input$CPvars, se_predictors = input$SEvars, 
                                cp_theta = rv$CPtheta, se_theta = rv$SEtheta, 
                                cp_models = rv$CPmods,
                                se_models_to_use = rv$SEmodstouse, 
                                cp_models_to_use = rv$CPmodstouse)

            rv$gtable <- create_g_table(rv$garray, confidence_level = input$CL)

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
            rv$Mhatarray <- estimate_mhat(co_data = rv$COdataIn, 
                            ss_data = rv$SSdataIn, 
                            size_class_column = input$COsizeclasscol, 
                            split_column = input$COsplitcol, 
                            unit_column = input$COunitcol, 
                            df_column = input$COdfcol,
                            replicates = input$Niterations,  
                            cp_predictors = rv$CPvars, 
                            se_predictors = rv$SEvars, cp_data = rv$CPdataIn, 
                            se_data = rv$SEdataIn, garray = rv$garray) 

            rv$Mhatsc <- condense_mhat(rv$Mhatarray)

            output$Mhattab <-  renderTable({
                                  create_mhat_table(condensed_mhat = rv$Mhatsc, 
                                            fraction_area_sampled = input$ffs, 
                                            confidence_level = input$CL)
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
          }) 
        })

}

