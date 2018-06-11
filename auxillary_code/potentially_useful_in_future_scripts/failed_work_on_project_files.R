## server

  dir.create("projects", showWarnings = FALSE)
  present_projs <- list.files("projects")
  proj_prefix <- "projects/GenEst_proj"
  proj_suffix <- ".Rdata"  
  proj_date <- Sys.Date()
  todays_prefix <- paste("GenEst_proj_", proj_date, sep = "")
  projs_on_date <- grep(todays_prefix, present_projs)
  n_projs_on_date <- length(projs_on_date)
  proj_number <- n_projs_on_date + 1
  proj_name <- paste("_", proj_date, "_n_", proj_number, sep = "") 
  file_name <- paste(proj_prefix, proj_name, proj_suffix, sep = "")


  observeEvent(input$GenEst_proj, {
    load_file_name <- paste("projects/", input$GenEst_proj$name, sep = "")
    load(load_file_name)

    output$SE_data <- DT::renderDataTable({DT::datatable(rv$SE_data)})
    updateSelectizeInput(session, "SE_vars", choices = rv$SE_colnames,
                         selected = rv$SE_vars)
    updateSelectizeInput(session, "SE_obs_cols", choices = rv$SE_colnames,
                         selected = rv$SE_obs_cols)
    updateSelectizeInput(session, "sizeclass_col", choices = rv$sc_options,
                         selected = rv$sizeclass_col)
    updateRadioButtons(session, "fix_k_choice", 
                       choices = list("No" = 0, "Yes" = 1), 
                       selected = rv$fix_k_choice)    
    updateTabsetPanel(session, "LoadedDataViz", "Search Efficiency")
  })



#### ui


,
      fileInput("GenEst_proj", "GenEst Project",
                accept = c(".Rdata"))