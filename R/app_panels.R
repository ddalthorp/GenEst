
#' @title Create a Data Tab Panel for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a data input 
#'   visualization panel used in the GenEst GUI, based on the data type 
#'   (\code{dataType}).
#'
#' @param dataType Toggle control for the model type of the panel. One of 
#'   "SE", "CP", "SS", "DWP", or "CO".  
#'
#' @return HTML for the panel
#'
#' @export
#'
dataTabPanel <- function(dataType){

  if (!dataType %in% c("SE", "CP", "SS", "DWP", "CO")){
    stop(paste0("input dataType (", dataType, ") not supported"))
  }

  Label <- switch(dataType, "SE" = "Searcher Efficiency",
                            "CP" = "Carcass Persistence",
                            "SS" = "Search Schedule",
                            "DWP" = "Density Weighted Proportion",
                            "CO" = "Carcass Observation")
  TableName <- switch(dataType, "SE" = "data_SE",
                                "CP" = "data_CP",
                                "SS" = "data_SS",
                                "DWP" = "data_DWP",
                                "CO" = "data_CO")
  tabPanel(Label, br(), dataTableOutput(TableName))
}

#' @title Create a Selected Data Tab Panel for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a data input 
#'   visualization panel used in the GenEst GUI, based on the data type 
#'   (\code{dataType}).
#'
#' @param modType Toggle control for the model type of the panel. One of 
#'   "SE", "CP", or "g".  
#'
#' @return HTML for the panel
#'
#' @export
#'
selectedDataPanel <- function(modType){

  if (!modType %in% c("SE", "CP", "g")){
    stop(paste0("input modType (", modType, ") not supported"))
  }

  if (modType == "SE"){
    tabPanel("Selected Data", br(),
      conditionalPanel(condition = "input.obsCols_SE == null",
        em("Select observation columns to view data")
      ), 
      conditionalPanel(
        condition = "output.filename_SE != null & input.obsCols_SE != null",
        em(textOutput("filename_SE"))
      ), 
      br(), 
      dataTableOutput("selected_SE")
    )
  } else if (modType == "CP"){
    tabPanel("Selected Data", br(),
      conditionalPanel(condition = "input.ltp == null | input.fta == null",
        em("Select observation columns to view data")
      ), 
      conditionalPanel(
        condition = "output.filename_CP != null & input.ltp != null & 
          input.fta != null",
        em(textOutput("filename_CP"))
      ), 
      br(), 
      conditionalPanel(condition = "input.ltp != null & input.fta != null",
        dataTableOutput("selected_CP")
      )
    )
  } else if (modType == "g"){
    tabPanel("Schedule",         
      br(), 
      b(u(big("Search Schedule:"))),
      br(), br(), 
      textOutput("SStext")
    )
  }
}

#' @title Create a Model Output Tab Panel for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a model output 
#'   panel used in the GenEst GUI, based on the output type 
#'   (\code{outType}).
#'
#' @param dataType Toggle control for the model type of the panel. One of 
#'   "SEFigures", "SEEstimates", "SEModComparison", "SEModSelection",
#'   "CPFigures", "CPEstimates", "CPModComparison", "CPModSelection",
#'   "gFigures", or "gSummary".
#'
#' @return HTML for the panel
#'
#' @export
#'
modelOutputPanel <- function(outType){

  if (!outType %in% c("SEFigures", "SEEstimates", "SEModComparison", 
                      "SEModSelection", "CPFigures", "CPEstimates", 
                      "CPModComparison", "CPModSelection", "MFigures", 
                      "MSummary", "gFigures", "gSummary")){
    stop(paste0("input outType (", outType, ") not supported"))
  }

  if (outType == "SEFigures"){
    tabPanel("Figures", br(), 
      conditionalPanel(condition = "output.fig_SE == null",
        em("Run model to view figures")
      ),
      conditionalPanel(condition = "output.SEModDone == 'OK'",
        textOutput("sizeclass_SE1"), br(), 
        plotOutput("fig_SE", inline = TRUE), br(), br(),
        downloadButton("dlSEfig", "Download")
      )
    )
  } else if(outType == "SEEstimates"){
    tabPanel("Estimates", br(),  
      conditionalPanel(condition = "output.modTab_SE == null",
        em("Run model to view model estimates")
      ),
      conditionalPanel(condition = "output.SEModDone == 'OK'",
        textOutput("sizeclass_SE2"), br(), 
        textOutput("text_SE_est"), br(),
        dataTableOutput("modTab_SE"), br(),
        downloadButton("dlSEest", "Download")
      )
    )
  } else if(outType == "SEModComparison"){
    tabPanel("Model Comparison", br(), 
      conditionalPanel(condition = "output.AICcTab_SE == null",
        em("Run models to view model comparison")
      ),
      conditionalPanel(condition = "output.SEModDone == 'OK'",
        textOutput("sizeclass_SE3"), br(), 
        dataTableOutput("AICcTab_SE"), br(),
        downloadButton("dlSEAICc", "Download")
      )
    )
  } else if(outType == "SEModSelection"){
    tabPanel("Model Selection", br(), 
      conditionalPanel(condition = "output.modelMenu_SE == null",
        em("Run models to select models")
      ),
      htmlOutput("modelMenu_SE")
    )
  } else if (outType == "CPFigures"){
    tabPanel("Figures", br(), 
      conditionalPanel(condition = "output.fig_CP == null",
        em("Run model to view figures")
      ),
      conditionalPanel(condition = "output.CPModDone == 'OK'",
        textOutput("sizeclass_CP1"), br(), 
        plotOutput("fig_CP", inline = TRUE), br(), br(),
        downloadButton("dlCPfig", "Download")
      )
    )
  } else if(outType == "CPEstimates"){
    tabPanel("Estimates", br(),  
      conditionalPanel(condition = "output.modTab_CP == null",
        em("Run model to view model estimates")
      ),
      conditionalPanel(condition = "output.CPModDone == 'OK'",
        textOutput("sizeclass_CP2"), br(), 
        textOutput("text_CP_est"), br(),
        dataTableOutput("modTab_CP"), br(),
        downloadButton("dlCPest", "Download")
      )
    )
  } else if(outType == "CPModComparison"){
    tabPanel("Model Comparison", br(), 
      conditionalPanel(condition = "output.AICcTab_CP == null",
        em("Run models to view model comparison")
      ),
      conditionalPanel(condition = "output.CPModDone == 'OK'",
        textOutput("sizeclass_CP3"), br(), 
        dataTableOutput("AICcTab_CP"), br(),
        downloadButton("dlCPAICc", "Download")
      )
    )
  } else if(outType == "CPModSelection"){
    tabPanel("Model Selection", br(), 
      conditionalPanel(condition = "output.modelMenu_CP == null",
        em("Run models to select models")
      ),
      htmlOutput("modelMenu_CP")
    )
  } else if (outType == "MFigures"){
    tabPanel("Figure", br(),
      conditionalPanel(
        condition = 
          "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
           output.sizeclasses_SE != output.sizeclasses_CP",
       em("Select SE and CP models fit to matching size classes to run model")
      ), 
      conditionalPanel(
        condition = "output.fig_M == null & input.modelChoices_SE1 != null & 
           input.modelChoices_CP1 != null &
           output.sizeclasses_SE == output.sizeclasses_CP",
        em("Run estimate to view figure")
      ), 
      conditionalPanel(condition = "output.MModDone == 'OK'",
        plotOutput("fig_M", inline = TRUE), br(), br(),
        downloadButton("dlMfig", "Download")
      )
    )
  } else if (outType == "MSummary"){
    tabPanel("Summary", br(), 
      conditionalPanel(
        condition = 
          "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
           output.sizeclasses_SE != output.sizeclasses_CP",
       em("Select SE and CP models fit to matching size classes to run model")
      ), 
      conditionalPanel(
        condition = "output.fig_M == null & input.modelChoices_SE1 != null & 
           input.modelChoices_CP1 != null &
           output.sizeclasses_SE == output.sizeclasses_CP",
        em("Run estimate to view summary")
      ), 
      conditionalPanel(condition = "output.MModDone == 'OK'",
        br(), dataTableOutput("table_M"), br(),
        downloadButton("dlMtab", "Download")
      )
    )
  } else if (outType == "gFigures"){
    tabPanel("Figure", br(), 
      conditionalPanel(condition = "output.fig_g == null",
        em("Run estimate to view figure")
      ), 
      conditionalPanel(condition = "output.gModDone == 'OK'",
        textOutput("sizeclass_g1"), br(), 
        plotOutput("fig_g", inline = TRUE), br(), br(),
        downloadButton("dlgfig", "Download")
      )
    )
  } else if (outType == "gSummary"){
    tabPanel("Summary", br(), 
      conditionalPanel(condition = "output.table_g == null",
        em("Run estimate to view summary")
      ), 
      conditionalPanel(condition = "output.gModDone == 'OK'",
        textOutput("sizeclass_g2"), br(), 
        br(), dataTableOutput("table_g"), br(),
        downloadButton("dlgtab", "Download")
      )
    )
  }


}
