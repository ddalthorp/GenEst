function(input, output, session){

  v_number <- packageDescription("GenEst", field = "Version")
  v_date <- packageDescription("GenEst", field = "Date")
  v_text <- paste("This is version ", v_number, " (", v_date, ")", sep = "")
  output$version_info <- renderText(v_text)

  disclaimer_title <- paste("GenEst v", v_number, " (", v_date, ")", sep = "")
  showModal(modalDialog(title = disclaimer_title, 
         "This software is preliminary or provisional and is subject to 
          revision. It is being provided to meet the need for timely best 
          science. The software has not received final approval by the U.S.
          Geological Survey (USGS). No warranty, expressed or implied, is 
          made by the USGS or the U.S. Government as to the functionality 
          of the software and related material nor shall the fact of release 
          constitute any such warranty. The software is provided on the 
          condition that neither the USGS nor the U.S. Government shall be 
          held liable for any damages resulting from the authorized or 
          unauthorized use of the software.",  
          easyClose = F, footer = modalButton("OK")))

  SE_mod_run_msg <- NULL
  SE_mod_fit_msg <- NULL
  rv <- reactiveValues(
          SE_data = NULL, SE_colnames = NULL, 
          SE_obs_cols = NULL, SE_vars = NULL, 
          fix_k_choice = 0, fixed_k = NULL,
          p_predictors = NULL, k_predictors = NULL,
          pformula = NULL, kformula = NULL,
          SE_mods = NULL, SE_mods_check = NULL, 
          SE_mod_names = NULL, SE_mod_tab = NULL, 
          sizeclass_chosen = NULL,
          SE_AICc_table = NULL, SE_mod_order = NULL,
          SE_models_to_use = NULL,
          CP_data = NULL, CP_colnames = NULL, 
          SS_data = NULL,  
          CO_data = NULL, CO_colnames = NULL,
          sc_options = NULL, sizeclass_col = NULL, 
          sizeclasses = NULL, n_sizeclasses = NULL, 
          CL = 0.9, n_iterations = 1000)



  observeEvent(input$SE_file, {
    rv$SE_data <- read.csv(input$SE_file$datapath, header = T)
    rv$SE_colnames <- colnames(rv$SE_data)
    if(length(rv$sc_options) == 0){
      rv$sc_options <- rv$SE_colnames 
    }else{
      rv$sc_options <- rv$SE_colnames[rv$SE_colnames %in% rv$sc_options]
    }
    output$SE_data <- DT::renderDataTable({
                            DT::datatable(data.frame(rv$SE_data))
                      })
    updateSelectizeInput(session, "SE_vars", choices = rv$SE_colnames)
    updateSelectizeInput(session, "SE_obs_cols", choices = rv$SE_colnames)
    updateSelectizeInput(session, "sizeclass_col", choices = rv$sc_options)
    updateTabsetPanel(session, "LoadedDataViz", "Search Efficiency")
  })
  observeEvent(input$CP_file, {
    rv$CP_data <- read.csv(input$CP_file$datapath, header = T)
    rv$CP_colnames <- colnames(rv$CP_data)
    if(length(rv$sc_options) == 0){
      rv$sc_options <- rv$CP_colnames 
    }else{
      rv$sc_options <- rv$CP_colnames[rv$CP_colnames %in% rv$sc_options]
    }
    output$CP_data <- DT::renderDataTable(rv$CP_data)
    updateSelectizeInput(session, "CP_vars", choices = rv$CP_colnames)
    updateSelectizeInput(session, "sizeclass_col", choices = rv$sc_options)
    updateSelectizeInput(session, "CP_ltp", choices = rv$CP_colnames)
    updateSelectizeInput(session, "CP_fta", choices = rv$CP_colnames)
    updateTabsetPanel(session, "LoadedDataViz", "Carcass Persistence")
  })
  observeEvent(input$SS_file, {
    rv$SS_data <- read.csv(input$SS_file$datapath, header = T)
    output$SS_data <- DT::renderDataTable(rv$SS_data)
    updateTabsetPanel(session, "LoadedDataViz", "Search Schedule")
  })
  observeEvent(input$CO_file, {
    rv$CO_data <- read.csv(input$CO_file$datapath, header = T)
    rv$CO_colnames <- colnames(rv$CO_data)
    if(length(rv$sc_options) == 0){
      rv$sc_options <- rv$CO_colnames 
    }else{
      rv$sc_options <- rv$CO_colnames[rv$CO_colnames %in% rv$sc_options]
    }
    output$CO_data <- DT::renderDataTable(rv$CO_data)
    updateSelectizeInput(session, "CO_split_col", choices = rv$CO_colnames)
    updateSelectizeInput(session, "sizeclass_col", choices = rv$sc_options)
    updateSelectizeInput(session, "CO_df_col", choices = rv$CO_colnames)
    updateSelectizeInput(session, "CO_unit_col", choices = rv$CO_colnames)
    updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")
  })

  observeEvent(input$SE_obs_cols, {
    selected_cols <- c(input$SE_obs_cols, input$sizeclass_col, input$SE_vars)
    selected_table <- rv$SE_data[ , which(rv$SE_colnames %in% selected_cols)]
    selected_dataframe <- data.frame(selected_table)
    if(length(selected_cols) == 1){
      colnames(selected_dataframe) <- selected_cols
    }
    output$selected_SE <- DT::renderDataTable(selected_dataframe)
  })
  observeEvent(input$SE_vars, {
    selected_cols <- c(input$SE_obs_cols, input$sizeclass_col, input$SE_vars)
    selected_table <- rv$SE_data[ , which(rv$SE_colnames %in% selected_cols)]
    selected_dataframe <- data.frame(selected_table)
    if(length(selected_cols) == 1){
      colnames(selected_dataframe) <- selected_cols
    }
    output$selected_SE <- DT::renderDataTable(selected_dataframe)
  })

  observeEvent(input$SE_mod_run, {
    SE_mod_run_msg <- showNotification("Running Searcher Efficiency Model")
    rv$SE_obs_cols <- input$SE_obs_cols
    rv$SE_vars <- input$SE_vars
    if(input$fix_k_choice == 1 & is.numeric(input$fixed_k)){
      rv$fixed_k <- input$fixed_k
    }
    rv$n_iterations <- input$n_iterations
    rv$CL <- input$CL
    rv$sizeclass_col <- input$sizeclass_col
    rv$fix_k_choice <- input$fix_k_choice
 
    rv$sizeclasses <- as.character(unique(rv$SE_data[ , rv$sizeclass_col]))
    if(length(rv$sizeclass_col) == 0){
      rv$sizeclasses <- "all"
    }

    rv$pk_predictors <- paste(rv$SE_vars, collapse = "*")
    if(length(rv$SE_vars) == 0){
      rv$pk_predictors <- 1
    }

    rv$pformula <- formula(paste("p~", rv$pk_predictors, sep = ""))
    rv$kformula <- formula(paste("k~", rv$pk_predictors, sep = "")) 
    rv$SE_mods <- pkm_set_size(pformula = rv$pformula,
                               kformula = rv$kformula,
                               data = rv$SE_data, 
                               obs_cols = rv$obs_cols, 
                               sizeclass_col = rv$sizeclass_col,
                               fixed_k = rv$fixed_k, k_init = 0.7)
    rv$SE_mods_check <- pkm_check(rv$SE_mods)

    removeNotification(SE_mod_run_msg)

    if(length(rv$sizeclasses) == 1){
      rv$SE_AICc_tab <- pkm_set_aicc_tab(rv$SE_mods) 
      rv$SE_mod_order <- as.numeric(row.names(rv$SE_AICc_tab))
      rv$SE_mod_names <- names(rv$SE_mods)[rv$SE_mod_order]
      rv$SE_mod_tab <- rv$SE_mods[[rv$SE_mod_order[1]]]$cellwise_pk
    }else{
      rv$sizeclass_chosen <- which(rv$sizeclasses == input$SE_AICc_sc)
      if(length(rv$sizeclass_chosen) == 0){
        rv$sizeclass_chosen <- 1
      }
      rv$SE_AICc_tab <- pkm_set_aicc_tab(rv$SE_mods[[rv$sizeclass_chosen]])
      rv$SE_mod_order <- as.numeric(row.names(rv$SE_AICc_tab))
      rv$SE_mod_names <- names(rv$SE_mods[[1]])[rv$SE_mod_order]
      rv$SE_mod_tab <- rv$SE_mods[[1]][[rv$SE_mod_order[1]]]$cellwise_pk
    }

    output$SE_AICc_table <- DT::renderDataTable({rv$SE_AICc_tab})    
    output$SE_mod_tab <- DT::renderDataTable({rv$SE_mod_tab})
    updateSelectizeInput(session, "SE_MT_sc", choices = rv$sizeclasses)
    updateSelectizeInput(session, "SE_MT_mod", choices = rv$SE_mod_names)
    updateSelectizeInput(session, "SE_AICc_sc", choices = rv$sizeclasses)
    updateTabsetPanel(session, "SE_analyses", "Model Comparison Tables")

    if(rv$SE_mods_check == FALSE){
      SE_mod_fit_msg <- showNotification("Not all models were fit properly.",
                                         type = "warning", duration = NULL)
    }
    isolate({

      output$SE_model_menu <- renderUI({
        
        SE_mod_menu <- ""
        n_sizeclasses <- length(rv$sizeclasses)
        if(n_sizeclasses > 0){
          if(n_sizeclasses == 1){
            AICc_tab <- pkm_set_aicc_tab(rv$SE_mods) 
            mod_order <- as.numeric(row.names(AICc_tab))
            mod_names <- names(rv$SE_mods)[mod_order]
            mtu_text <- "SE_models_to_use"
            sc_text <- "Model choice"
            mod_select <- selectizeInput(mtu_text, sc_text, mod_names)
            SE_mod_menu <- paste(SE_mod_menu, mod_select)  
          }else{
            for(i in 1:n_sizeclasses){
              AICc_tab <- pkm_set_aicc_tab(rv$SE_mods[[i]])
              mod_order <- as.numeric(row.names(AICc_tab))
              mod_names <- names(rv$SE_mods[[i]])[mod_order]
              mtu_text <- paste("SE_models_to_use", i, sep = "") 
              sc_text <- paste("Model choice, ", rv$sizeclasses[i], sep = "")
              mod_select <- selectizeInput(mtu_text, sc_text, mod_names)
              SE_mod_menu <- paste(SE_mod_menu, mod_select)  
            }
          }
        }  
        HTML(SE_mod_menu)
      })
    })


  })

  observeEvent(input$SE_AICc_sc, {
    if(length(rv$SE_mods) > 0){
      if(length(rv$sizeclasses) == 1){
        rv$SE_AICc_tab <- pkm_set_aicc_tab(rv$SE_mods) 
      }else{
        rv$sizeclass_chosen <- which(rv$sizeclasses == input$SE_AICc_sc)
        if(length(rv$sizeclass_chosen) == 0){
          rv$sizeclass_chosen <- 1
        }
        rv$SE_AICc_tab <- pkm_set_aicc_tab(rv$SE_mods[[rv$sizeclass_chosen]])
      }
      output$SE_AICc_table <- DT::renderDataTable({rv$SE_AICc_tab})
    }
  })

  observeEvent(input$SE_MT_sc, {
    if(length(rv$SE_mods) > 0){

      if(length(rv$sizeclasses) == 1){
        rv$SE_AICc_tab <- pkm_set_aicc_tab(rv$SE_mods) 
        rv$SE_mod_order <- as.numeric(row.names(rv$SE_AICc_tab))
        rv$SE_mod_names <- names(rv$SE_mods)[rv$SE_mod_order]
        rv$SE_mod_tab <- rv$SE_mods[[rv$SE_mod_order[1]]]$cellwise_pk
      }else{
        rv$sizeclass_chosen <- which(rv$sizeclasses == input$SE_MT_sc)
        if(length(rv$sizeclass_chosen) == 0){
          rv$sizeclass_chosen <- 1
        }
        rv$SE_AICc_tab <- pkm_set_aicc_tab(rv$SE_mods[[rv$sizeclass_chosen]])
        rv$SE_mod_order <- as.numeric(row.names(rv$SE_AICc_tab))
        rv$SE_mod_names <- names(rv$SE_mods[[1]])[rv$SE_mod_order]
        rv$SE_mod_tab <- 
          rv$SE_mods[[rv$sizeclass_chosen]][[rv$SE_mod_order[1]]]$cellwise_pk
      }

      output$SE_mod_tab <- DT::renderDataTable(rv$SE_mod_tab)
      updateSelectizeInput(session, "SE_MT_mod", choices = rv$SE_mod_names)

      observeEvent(input$SE_MT_mod, {
        if(length(rv$sizeclasses) == 1){
          rv$SE_mod_tab <- rv$SE_mods[[input$SE_MT_mod]]$cellwise_pk
        }else{
          rv$SE_mod_tab <- 
            rv$SE_mods[[rv$sizeclass_chosen]][[input$SE_MT_mod]]$cellwise_pk
        }
        output$SE_mod_tab <- DT::renderDataTable(rv$SE_mod_tab)
      })
    }
  })
}

