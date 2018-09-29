
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

  tName <- switch(modType,
                  "SE" = "Selected Data",
                  "CP" = "Selected Data",
                  "g" = "Schedule")
  Condition1 <- switch(modType,
                  "SE" = "input.obsCols_SE == null",
                  "CP" = "input.ltp == null | input.fta == null",
                  "g" = NULL)
  Condition2 <- switch(modType,
                  "SE" = "output.filename_SE != null & 
                          input.obsCols_SE != null",
                  "CP" = "output.filename_CP != null & input.ltp != null & 
                          input.fta != null",
                  "g" = NULL)
  Text1 <- switch(modType,
            "SE" = em("Select observation columns to view data"),
            "CP" = em("Select observation columns to view data"),
            "g" = b(u(big("Search Schedule:"))))
  Text2 <- switch(modType,
            "SE" = em(textOutput("filename_SE")),
            "CP" = em(textOutput("filename_CP")),
            "g" = list(br(), br(), textOutput("SStext")))
  Data <- switch(modType,
            "SE" = dataTableOutput("selected_SE"),
            "CP" = dataTableOutput("selected_CP"),
            "g" = NULL)

  tabPanel(tName, br(),
    conditionalPanel(condition = Condition1, Text1), 
    conditionalPanel(condition = Condition2, list(Text2, br(), Data))
  ) 

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

  tName <- switch(outType,
             "SEFigures" = "Figures",
             "SEEstimates" = "Estimates", 
             "SEModComparison" = "Model Comparison", 
             "SEModSelection" = "Model Selection", 
             "CPFigures" = "Figures", 
             "CPEstimates" = "Estimates", 
             "CPModComparison" = "Model Comparison", 
             "CPModSelection" = "Model Selection",
             "MFigures" = "Figures", 
             "MSummary" = "Summary", 
             "gFigures" = "Figures", 
             "gSummary" = "Summary")

  Condition1 <- switch(outType,
             "SEFigures" = "output.fig_SE == null",
             "SEEstimates" = "output.modTab_SE == null", 
             "SEModComparison" = "output.AICcTab_SE == null", 
             "SEModSelection" = "output.modelMenu_SE == null", 
             "CPFigures" = "output.fig_CP == null", 
             "CPEstimates" = "output.modTab_CP == null", 
             "CPModComparison" = "output.AICcTab_CP == null", 
             "CPModSelection" = "output.modelMenu_CP == null",
             "MFigures" = "input.modelChoices_SE1 == null | 
                           input.modelChoices_CP1 == null | 
                           output.sizeclasses_SE != output.sizeclasses_CP", 
             "MSummary" = "input.modelChoices_SE1 == null | 
                           input.modelChoices_CP1 == null | 
                           output.sizeclasses_SE != output.sizeclasses_CP", 
             "gFigures" = "output.fig_g == null", 
             "gSummary" = "output.table_g == null")

  Content1 <- switch(outType,
             "SEFigures" = em("Run model to view figures"),
             "SEEstimates" =  em("Run model to view model estimates"), 
             "SEModComparison" = em("Run models to view model comparison"), 
             "SEModSelection" = em("Run models to select models"), 
             "CPFigures" = em("Run model to view figures"), 
             "CPEstimates" = em("Run model to view model estimates"), 
             "CPModComparison" = em("Run models to view model comparison"), 
             "CPModSelection" = em("Run models to select models"),
             "MFigures" = em("Select SE and CP models fit to matching size 
                             classes to run model"), 
             "MSummary" = em("Select SE and CP models fit to matching size 
                             classes to run model"), 
             "gFigures" = em("Run estimate to view figure"), 
             "gSummary" = em("Run estimate to view summary"))

  Condition2 <- switch(outType,
             "SEFigures" = "output.SEModDone == 'OK'",
             "SEEstimates" = "output.SEModDone == 'OK'", 
             "SEModComparison" = "output.SEModDone == 'OK'", 
             "SEModSelection" = "output.SEModDone == 'OK'", 
             "CPFigures" = "output.CPModDone == 'OK'", 
             "CPEstimates" = "output.CPModDone == 'OK'", 
             "CPModComparison" = "output.CPModDone == 'OK'", 
             "CPModSelection" = "output.CPModDone == 'OK'",
             "MFigures" = "output.fig_M == null & 
                           input.modelChoices_SE1 != null & 
                           input.modelChoices_CP1 != null &
                           output.sizeclasses_SE == output.sizeclasses_CP", 
             "MSummary" = "output.fig_M == null & 
                           input.modelChoices_SE1 != null & 
                           input.modelChoices_CP1 != null &
                           output.sizeclasses_SE == output.sizeclasses_CP", 
             "gFigures" = "output.gModDone == 'OK'", 
             "gSummary" = "output.gModDone == 'OK'")

  Content2 <- switch(outType,
             "SEFigures" = list(textOutput("sizeclass_SE1"), br(), 
                             plotOutput("fig_SE", inline = TRUE), br(), br(),
                             downloadButton("dlSEfig", "Download")
                           ),
             "SEEstimates" = list(textOutput("sizeclass_SE2"), br(), 
                               textOutput("text_SE_est"), br(),
                               dataTableOutput("modTab_SE"), br(),
                               downloadButton("dlSEest", "Download")
                             ), 
             "SEModComparison" = list(textOutput("sizeclass_SE3"), br(), 
                                   dataTableOutput("AICcTab_SE"), br(),
                                   downloadButton("dlSEAICc", "Download")
                                 ), 
             "SEModSelection" = list(htmlOutput("modelMenu_SE")), 
             "CPFigures" = list(textOutput("sizeclass_CP1"), br(), 
                             plotOutput("fig_CP", inline = TRUE), br(), br(),
                             downloadButton("dlCPfig", "Download")
                           ), 
             "CPEstimates" = list(textOutput("sizeclass_CP2"), br(), 
                               textOutput("text_CP_est"), br(),
                               dataTableOutput("modTab_CP"), br(),
                               downloadButton("dlCPest", "Download")
                             ), 
             "CPModComparison" = list(textOutput("sizeclass_CP3"), br(), 
                                   dataTableOutput("AICcTab_CP"), br(),
                                   downloadButton("dlCPAICc", "Download")
                                 ), 
             "CPModSelection" = list(htmlOutput("modelMenu_CP")),
             "MFigures" = list(em("Run estimate to view figure")), 
             "MSummary" = list(em("Run estimate to view summary")), 
             "gFigures" = list(textOutput("sizeclass_g1"), br(), 
                            plotOutput("fig_g", inline = TRUE), br(), br(),
                            downloadButton("dlgfig", "Download")), 
             "gSummary" = list(textOutput("sizeclass_g2"), br(), 
                            br(), dataTableOutput("table_g"), br(),
                            downloadButton("dlgtab", "Download")))

  Condition3 <- switch(outType,
             "SEFigures" = NULL,
             "SEEstimates" = NULL, 
             "SEModComparison" = NULL, 
             "SEModSelection" = NULL, 
             "CPFigures" = NULL, 
             "CPEstimates" = NULL, 
             "CPModComparison" = NULL, 
             "CPModSelection" = NULL,
             "MFigures" = "output.MModDone == 'OK'", 
             "MSummary" = "output.MModDone == 'OK'", 
             "gFigures" = NULL, 
             "gSummary" = NULL)

  Content3 <- switch(outType,
             "SEFigures" = NULL,
             "SEEstimates" = NULL, 
             "SEModComparison" = NULL, 
             "SEModSelection" = NULL, 
             "CPFigures" = NULL, 
             "CPEstimates" = NULL, 
             "CPModComparison" = NULL, 
             "CPModSelection" = NULL,
             "MFigures" = list(plotOutput("fig_M", inline = TRUE), br(), br(),
                            downloadButton("dlMfig", "Download")), 
             "MSummary" = list(br(), dataTableOutput("table_M"), br(),
                            downloadButton("dlMtab", "Download")), 
             "gFigures" = NULL, 
             "gSummary" = NULL)
  tabPanel(tName, br(), 
    conditionalPanel(condition = Condition1, Content1),
    conditionalPanel(condition = Condition2, Content2),
    conditionalPanel(condition = Condition3, Content3)
  )

}
