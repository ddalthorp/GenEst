#' @title Create a Data Input Widget for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a data input 
#'   widget used in the GenEst GUI, based on the data type (\code{dataType}).
#'   Included within the widget is a conditional panel that allows removal of
#'   the specific data file (and clearing of all downstream models) once
#'   it has been loaded.
#'
#' @param dataType Toggle control for the model type of the widget. One of 
#'   "SE", "CP", "SS", "DWP", or "CO".  
#'
#' @return HTML for the data input widget. 
#'
#' @export
#'
dataInputWidget <- function(dataType){

  if (!dataType %in% c("SE", "CP", "SS", "DWP", "CO")){
    stop(paste0("input dataType (", dataType, ") not supported"))
  }

  okft <- c("text/csv", "text/comma-separated-values", ".csv")
  llab <- "Load file"
  clab <- "Clear file"
  cstyle <- cButtonStyle()

  Label <- switch(dataType, "SE" = "Searcher Efficiency Data",
                            "CP" = "Carcass Persistence Data",
                            "SS" = "Search Schedule Data",
                            "DWP" = "Density Weighted Proportion Data",
                            "CO" = "Carcass Observation Data")
  ButtonName <- switch(dataType, "SE" = "file_SE",
                                 "CP" = "file_CP",
                                 "SS" = "file_SS",
                                 "DWP" = "file_DWP",
                                 "CO" = "file_CO")
  ClearButtonName <- switch(dataType, "SE" = "file_SE_clear",
                                      "CP" = "file_CP_clear",
                                      "SS" = "file_SS_clear",
                                      "DWP" = "file_DWP_clear",
                                      "CO" = "file_CO_clear")
  ConditionPrefix <- switch(dataType, "SE" = "output.filename_",
                                      "CP" = "output.filename_",
                                      "SS" = "output.data_",
                                      "DWP" = "output.data_",
                                      "CO" = "output.data_")
  PanelCondition <- paste0(ConditionPrefix, dataType, " != null")

  list(
    h5(b(Label)),
    fileInput(ButtonName, label = NULL, accept = okft, buttonLabel = llab), 
    conditionalPanel(condition = PanelCondition,
      actionButton(ClearButtonName, clab, style = cstyle) 
    )
  )
}

#' @title Create a Data Download Widget for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a data download 
#'   widget (fluid row with name and button) used in the GenEst GUI, based on 
#'   the data set (\code{set}).
#'
#' @param set Name of data set. One of "RP", "RPbat", "cleared", "powerTower",
#'   "PV", "trough", "mock", or "mock2"
#'
#' @return HTML for the data download widget
#'
#' @export
#'
dataDownloadWidget <- function(set){

  if (!set %in% c("RP", "RPbat", "cleared", "powerTower", "PV", "trough", 
                  "mock", "mock2")){
    stop(paste0("input set (", set, ") not supported"))
  }

  setNames <- c("RP" = "Wind---Road and pad searches, bats + birds",
                "RPbat" = "Wind---Road and pad searches, bats",
                "cleared" = "Wind---Cleared plots, bats + birds",
                "powerTower" = "Solar---Power tower",
                "PV" = "Solar---Photovoltaic (PV)",
                "trough" = "Solar---Trough",
                "mock" = "Mock data",
                "mock2" = "Mock data with European-style csvs")

  setName <- setNames[set]
  setButtonName <- paste0("download_", set)

  fluidRow(
    column(6, h4(setName)), 
    column(1, downloadButton(setButtonName, "Download"))
  )
}


#' @title Create a Model Input Widget for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a model input 
#'   widget used in the GenEst GUI, based on the input type (\code{inType}).
#'
#' @param inType Toggle control for the input type of the widget. One of 
#'   "nsim", "CL", "sizeclassCol", "obsCols_SE", "preds_SE", "kFixed",
#'   "ltp", "fta", "preds_CP", "dists", "kFill", "frac", "DWPCol", 
#'   "dateFoundCol", "kFill_g", "gSearchInterval", "gSearchMax", 
#'   "useSSinputs", or "useSSdata".  
#'
#' @return HTML for the model input widget. 
#'
#' @export
#'
modelInputWidget <- function(inType){

  if (!inType %in% c("nsim", "CL", "sizeclassCol", "obsCols_SE", "preds_SE", 
                     "kFixed", "ltp", "fta", "preds_CP", "dists", "kFill",
                     "frac", "DWPCol", "dateFoundCol", "kFill_g", 
                     "gSearchInterval", "gSearchMax", "useSSinputs",
                     "useSSdata")){
    stop(paste0("input inType (", inType, ") not supported"))
  }

  if (inType == "nsim"){
    numericInput("nsim", "Number of Iterations:", value = 1000,
      min = 1, max = 10000, step = 1
    )
  } else if(inType == "CL"){
    numericInput("CL", "Confidence Level:", value = 0.90, min = 0,
      max = 1, step = 0.001
    )
  } else if(inType == "sizeclassCol"){
    selectizeInput("sizeclassCol", "Size Class Column (optional):", 
      c("No data input yet"), multiple = TRUE, 
      options = list(maxItems = 1)
    )
  } else if(inType == "obsCols_SE"){
    selectizeInput("obsCols_SE", "Observations:", c("No data input yet"), 
      multiple = TRUE
    )
  } else if(inType == "preds_SE"){
    selectizeInput("preds_SE", "Predictor Variables:", 
      c("No data input yet"), multiple = TRUE
    )
  } else if(inType == "kFixed"){
    htmlOutput("kFixedInput")
  } else if(inType == "ltp"){
    selectizeInput("ltp", "Last Time Present:", c("No data input yet"), 
      multiple = TRUE, options = list(maxItems = 1)
    )
  } else if(inType == "fta"){
    selectizeInput("fta", "First Time Absent:", c("No data input yet"),
      multiple = TRUE, options = list(maxItems = 1)
    )
  } else if(inType == "preds_CP"){
    selectizeInput("preds_CP", "Predictor Variables:", 
      c("No data input yet"), multiple = TRUE
    )
  } else if(inType == "dists"){
    checkboxGroupInput("dists", label = "Distributions to Include",
      choices = CPdistOptions(), 
      selected = c("exponential", "weibull", "lognormal", "loglogistic"),
      inline = TRUE
    )
  } else if(inType == "kFill"){
    conditionalPanel(
      condition = "output.kFillNeed == 'yes'",
      htmlOutput("kFillInput")
    )
  } else if(inType == "frac"){
    numericInput("frac", "Fraction of Facility Surveyed:", value = 1.0, 
      min = 0.01, max = 1.0, step = 0.01
    )
  } else if(inType == "DWPCol"){
    conditionalPanel(
      condition = "output.DWPNeed == 'yes'",
      selectizeInput("DWPCol", "Density Weighted Proportion:", 
        c("No data input yet"), multiple = TRUE, options = list(maxItems = 1)
      )
    )
  } else if(inType == "dateFoundCol"){
    selectizeInput("dateFoundCol", "Date Found:", c("No data input yet"), 
      multiple = TRUE, options = list(maxItems = 1)
    )
  } else if(inType == "kFill_g"){
    conditionalPanel(
      condition = "output.kFillNeed == 'yes'",
      htmlOutput("kFillInput_g")
    )
  } else if(inType == "gSearchInterval"){
    numericInput("gSearchInterval", "Generic Search Interval (days):", 
      value = 7, min = 1, max = 400, step = 1)
  } else if(inType == "gSearchMax"){
    numericInput("gSearchMax", "Generic Final Search (day):",
      value = 364, min = 1, max = 1000, step = 1)
  } else if(inType == "useSSinputs"){
    actionButton("useSSinputs", "Create Custom Generic Schedule")
  } else if(inType == "useSSdata"){
    conditionalPanel(    
      condition = "output.data_SS != null",
      actionButton("useSSdata", "Create Average Schedule from SS Data")
    )
  }
}

#' @title Create a Model Run Widget for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a model run 
#'   widget used in the GenEst GUI, based on the model type (\code{modType}).
#'   The widget includes the model run button and the model clear button 
#'   (once the model has finished running).
#'
#' @param modType Toggle control for the model type of the widget. One of 
#'   "SE", "CP", "M", or "g". 
#'
#' @return HTML for the model run widget. 
#'
#' @export
#'
modelRunWidget <- function(modType){

  if (!modType %in% c("SE", "CP", "M", "g")){
    stop(paste0("input modType (", modType, ") not supported"))
  }

  if (modType == "SE"){
    list(
      conditionalPanel(condition = "input.obsCols_SE == null",
        br(), 
        center(em("Select observation columns to run model"))
      ),
      conditionalPanel(condition = "input.obsCols_SE != null",
        br(), 
        actionButton("runMod_SE", "Run Model")          
      ),
      conditionalPanel(condition = "output.SEModDone == 'OK'", 
        actionButton("runMod_SE_clear", "Clear Model", 
          style = cButtonStyle()
        ),
        br(), br()
      )
    )
  } else if (modType == "CP"){
    list(
      conditionalPanel(
         condition = "input.ltp == null | input.fta == null",
        br(), 
        center(em("Select observation columns to run model"))
      ),
      conditionalPanel(
        condition = "input.ltp != null & input.fta != null",
        br(),
        actionButton("runMod_CP", "Run Model")
      ), 
      conditionalPanel(condition = "output.CPModDone == 'OK'", 
        actionButton("runMod_CP_clear", "Clear Model", 
          style = cButtonStyle()
        ),
        br(), br()
      )
    )
  } else if (modType == "M"){
    list(
      conditionalPanel(
        condition = 
          "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
           output.sizeclasses_SE != output.sizeclasses_CP",
        br(), 
        center(em("Select SE and CP models fit to matching size classes to 
          run model"))
      ),
      conditionalPanel(
        condition = "output.data_SS == null",
        br(), 
        center(em("Input Search Schedule data to run model"))
      ),
      conditionalPanel(
        condition = 
          "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
           output.sizeclasses_SE == output.sizeclasses_CP & 
           (input.DWPCol == null | input.dateFoundCol == null)",
        br(), 
        center(em("Select input columns to run model"))
      ),
      conditionalPanel(
        condition = "output.kFillNeed == 'yes' & 
                     input.modelChoices_SE1 != null",
        br(), 
        center(em("A value for k is required to estimate mortality. 
                  Return to Search Efficiency tab and fix k."))
      ),
      conditionalPanel(
        condition = 
          "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
           output.sizeclasses_SE == output.sizeclasses_CP & 
           output.data_SS != null & 
           input.DWPCol != null & input.dateFoundCol != null &
         output.kFillNeed != 'yes'",
        br(),
        actionButton("runMod_M", "Estimate")
      )
    )
  } else if (modType == "g"){
    list(
      conditionalPanel(
        condition = 
          "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
           output.sizeclasses_SE != output.sizeclasses_CP",
        br(), 
        center(em("Select SE and CP models fit to matching size
          classes to run model"))
      ),
      conditionalPanel(
        condition = "output.kFillNeed == 'yes' & 
                     input.modelChoices_SE1 != null",
        br(), 
        center(em("A value for k is required to estimate detection
                  probability. Return to Search Efficiency tab and fix k."))
      ),
      conditionalPanel(
        condition = 
          "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
           output.sizeclasses_SE == output.sizeclasses_CP &
         output.kFillNeed != 'yes'",
        br(), br(),
        actionButton("runMod_g", "Estimate")
      ),
      conditionalPanel(condition = "output.gModDone == 'OK'",
        actionButton("runMod_g_clear", "Clear Estimate", 
          style = cButtonStyle()
        )
      )
    )
  }
}

#' @title Create a Model Output Widget for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a widget used in 
#'   the GenEst GUI to control the outputs based on the model type
#'   (\code{modType}).
#'
#' @param modType Toggle control for the model type of the widget. One of 
#'   "SE", "CP", "M", or "g". 
#'
#' @return HTML for the model run widget. 
#'
#' @export
#'
modelOutputWidget <- function(modType){

  if (!modType %in% c("SE", "CP", "M", "g")){
    stop(paste0("input modType (", modType, ") not supported"))
  }

  if (modType == "SE"){
    conditionalPanel(condition = "output.SEModDone == 'OK'", 
      b(u(big("Table & Figure Selection:"))),
      br(), br(), 
      conditionalPanel(condition = "output.sizeclass_SEyn == 'YES'",
        selectizeInput("outsizeclassSE", "Size Class:", " ", multiple = FALSE)
      ),
      selectizeInput("outSEp", "p Model:", " ", multiple = FALSE), 
      selectizeInput("outSEk", "k Model:", " ", multiple = FALSE)
    )
  } else if (modType == "CP"){
    conditionalPanel(condition = "output.CPModDone == 'OK'", 
      b(u(big("Table & Figure Selection:"))),
      br(), br(),
      conditionalPanel(condition = "output.sizeclass_CPyn == 'YES'",
        selectizeInput("outsizeclassCP", "Size Class:", " ", multiple = FALSE)
      ),
      selectizeInput("outCPdist", "Distribution:", " ", multiple = FALSE),
      selectizeInput("outCPl", "Location Model:", " ", multiple = FALSE),
      selectizeInput("outCPs", "Scale Model:", " ", multiple = FALSE)        
    )
  } else if (modType == "M"){
    conditionalPanel(
      condition = "output.MModDone == 'OK'",
      actionButton("runMod_M_clear", "Clear Estimate", 
        style = cButtonStyle()
      ), br(), br(), 
      b(u(big("Splitting Mortality:"))),
      br(), br(), 
      em("Max. two total splits, max. one schedule-based split"),
      br(), br(),
      selectizeInput("split_SS", "Search Schedule (SS) Variable:", 
        " ", multiple = TRUE, options = list(maxItems = 1)
      ),
      selectizeInput("split_CO", "Carcass Observation (CO) Variable:", 
        " ", multiple = TRUE, options = list(maxItems = 2)
      ),
      fluidRow(
        column(width = 6,  
          actionButton("splitM", "Split Estimate")
        ),
        column(width = 6,
          conditionalPanel(
            condition = "output.MSplitDone == 'OK' & output.nMSplits > 1",
            actionButton("transposeSplit", "Transpose")
          )
        )
      ),
      conditionalPanel(condition = "output.MSplitDone == 'OK'", 
        actionButton("splitM_clear", "Clear Split", style = cButtonStyle())
      )
    )
  } else if (modType == "g"){
    conditionalPanel(
      condition = "output.gModDone == 'OK' & output.sizeclass_gyn == 'YES'", 
      br(), br(), 
      b(u(big("Table & Figure Selection:"))),
      br(), br(),
      selectizeInput("outsizeclassg", "Size Class:", 
        " ", multiple = FALSE
      )
    )
  }
}


#' @title Make a Model Selection Widget
#'
#' @description Produce a Size-Class-based model selection widget based 
#'   on the model inputs. 
#'
#' @param mods Model Set Size object (from the reactive values list).
#'
#' @param modType Model type, either "SE" or "CP".
#'
#' @return Rendered HTML model selection menu widget.
#'
#' @export
#'
modelSelectionWidget <- function(mods, modType){

  if (!any(attr(mods, "class") %in% c("cpmSetSize", "pkmSetSize"))){
    stop("mods must be a cpmSetSize or pkmSetSize object")
  }
  if (!modType %in% c("SE", "CP")){
    stop(paste0("input modType (", modType, ") not supported"))
  }
  nsizeclasses <- length(mods)
  menuHeader <- modelSelectionWidgetHeader(mods)
  modelMenu <- menuHeader
  if (nsizeclasses > 0){
    for(sci in 1:nsizeclasses){
      modelMenuRow <- modelSelectionWidgetRow(mods, modType, sci)
      modelMenu <- paste(modelMenu, modelMenuRow)  
    }
  }
  return(renderUI({HTML(modelMenu)}))
}

#' @rdname modelSelectionWidget
#'
#' @details \code{modelSelectionWidgetHeader} creates header text depending on
#'   if there is one size class or more than one.
#'
#' @export
#'
modelSelectionWidgetHeader <- function(mods){

  if (!any(attr(mods, "class") %in% c("cpmSetSize", "pkmSetSize"))){
    stop("mods must be a cpmSetSize or pkmSetSize object")
  }
  nsizeclasses <- length(mods)
  if (nsizeclasses == 1){
    ntext <- "model"
  } else if (nsizeclasses > 1){
    ntext <- "models"
  } else{
    stop("nsizeclasses input is improper")
  }
  prefix <- "Select "
  suffix <- " for mortality and detection probability estimation"
  menuHeader <- em(paste0(prefix, ntext, suffix))

  menuBreak <- NULL
  if (nsizeclasses > 1){
    menuBreak <- br("")
  } 
  paste(menuHeader, menuBreak)
}

#' @rdname modelSelectionWidget
#'
#' @details \code{modelSelectionWidgetRow} creates a row of the widget (input
#'   for one size class).
#'
#' @param sci Numeric size class element index.
#'
#' @export
#'
modelSelectionWidgetRow <- function(mods, modType, sci){

  if (!any(attr(mods, "class") %in% c("cpmSetSize", "pkmSetSize"))){
    stop("mods must be a cpmSetSize or pkmSetSize object")
  }
  if (!modType %in% c("SE", "CP")){
    stop(paste0("input modType (", modType, ") not supported"))
  }
  sizeclasses <- names(mods)
  nsizeclasses <- length(mods)
  if (!is.numeric(sci)){
    stop("sci needs to be numeric")
  }
  if (sci > nsizeclasses){
    stop("sci out of range")
  }
  if (modType == "SE"){
    AICcTab <- pkmSetAICcTab(mods[[sci]], quiet = TRUE)
  }
  if (modType == "CP"){
    AICcTab <- cpmSetAICcTab(mods[[sci]], quiet = TRUE)
  }
  modOrder <- as.numeric(row.names(AICcTab))
  modNames <- names(mods[[sci]])[modOrder]
  modNames <- gsub("; NULL", "", modNames)
  modNames <- gsub("dist: ", "", modNames)
  modNames <- gsub("~ 1", "~ constant", modNames)

  modNames_nchar <- nchar(modNames)
  modNames_maxchar <- max(modNames_nchar)
  modNames_nspaces <- modNames_maxchar - modNames_nchar + 10
  modSpaces <- sapply(modNames_nspaces, 
                     function(x){paste(rep(" ", x), collapse = "")}
                   )
  modDeltaAICcs <- AICcTab[ , "Delta AICc"]
  modLabels <- paste0(modNames, " (delta AICc: ", modDeltaAICcs, ")")
  names(modNames) <- modLabels
  labels_nchar <- nchar(modLabels)
  labels_maxchar <- max(labels_nchar)
  widthval <- max(c(400, labels_maxchar * 7 + 20))
  widthtxt <- paste0(widthval, "px")
  mtuText <- paste0("modelChoices_", modType, sci) 
  scText <- paste0(sizeclasses[sci], ":")
  if (nsizeclasses == 1){
    scText <- ""
  }

  selectizeInput(mtuText, scText, modNames, multiple = TRUE, width = widthtxt, 
    options = list(maxItems = 1)
   )
}

#' @title Make a kFixed Widget
#'
#' @description Produce a kFixed input widget based on the size classes. 
#'
#' @param sizeclasses Vector of size class names (from the reactive values 
#'   list).
#'
#' @return Rendered HTML kFixed input widget.
#'
#' @export
#'
kFixedWidget <- function(sizeclasses){
  nsizeclasses <- length(sizeclasses)
  widgetHeader <- kFixedWidgetHeader(sizeclasses)

  kFixedMenu <- widgetHeader
  for(sci in 1:nsizeclasses){
    kFixedRow <- kFixedWidgetRow(sizeclasses, sci)
    kFixedMenu <- paste(kFixedMenu, kFixedRow)  
  }
  renderUI({HTML(kFixedMenu)})
}

#' @rdname kFixedWidget
#'
#' @details \code{kFixedWidgetHeader} creates the widget header based on
#'   the number of size classes.
#'
#' @export
#'
kFixedWidgetHeader <- function(sizeclasses){
  nsizeclasses <- length(sizeclasses)
  if (nsizeclasses == 1){
    fluidRow(
      column(width = 4, align = "center", b("Fix k?")), 
      column(width = 4, align = "center", b("Value"))
    )
  } else if (nsizeclasses > 1){
    fluidRow(
      column(width = 2, div("")),
      column(width = 4, align = "center", b("Fix k?")), 
      column(width = 4, align = "center", b("Value"))
    )
  }
}


#' @rdname kFixedWidget
#'
#' @details \code{kFixedWidgetRow} creates a row of the widget (input
#'   for one size class).
#'
#' @param sci Numeric size class element index.
#'
#' @export
#'
kFixedWidgetRow <- function(sizeclasses, sci){

  nsizeclasses <- length(sizeclasses)
  if (!is.numeric(sci)){
    stop("sci needs to be numeric")
  }
  if (sci > nsizeclasses){
    stop("sci out of range")
  }

  mvText <- paste0("kFixed_val_", sci) 
  mynText <- paste0("kFixed_yn_", sci) 

  scText <- paste0(sizeclasses[sci], ":")
  rowName <- paste0("string_", sci)

  rowNameStyle <- style(type = "text/css", 
                    paste0("#", rowName, " { margin-top: 10px;}"))
  checkStyle <- style(type = "text/css", 
                     paste0("#", mynText, " { margin-top: 5px;}"))
  numStyle <- style(type = "text/css", 
                   paste0("#", mvText, " { margin-top: -15px;}"))

  spacerCol <- column(width = 1, div(""))
  rowNameCol <- column(width = 1,  
                  div(id = rowName, b(scText)), align = "right", 
                  rowNameStyle
                )
  if (nsizeclasses == 1){
    spacerCol <- NULL
    rowNameCol <- NULL
  }
  checkCol <- column(width = 4, align = "center", 
                checkboxInput(mynText, ""), 
                checkStyle
              )
  numCol <- column(width = 4, align = "center", 
              numericInput(mvText, "", value = "", min = 0, max = 1, 
                step = 0.001),
              numStyle
            )

  fluidRow(spacerCol, rowNameCol, checkCol, numCol)
}
