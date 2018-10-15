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
#'   "PV", "trough", "mock"
#'
#' @return HTML for the data download widget
#'
#' @export
#'
dataDownloadWidget <- function(set){

  if (!set %in% c("RP", "RPbat", "cleared", "powerTower", "PV", "trough", 
                  "mock")){
    stop(paste0("input set (", set, ") not supported"))
  }

  setNames <- c("RP" = "Wind---Road and pad searches, bats + birds",
                "RPbat" = "Wind---Road and pad searches, bats",
                "cleared" = "Wind---Cleared plots, bats + birds",
                "powerTower" = "Solar---Power tower",
                "PV" = "Solar---Photovoltaic (PV)",
                "trough" = "Solar---Trough",
                "mock" = "Mock data")

  setName <- setNames[set]
  setButtonName <- paste0("download_", set)
  fluidRow(
    column(6, h4(setName)), 
    column(2, downloadButton(setButtonName, "Download"))
  )
}


#' @title Create a Model Input Widget for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a model input 
#'   widget used in the GenEst GUI, based on the input type (\code{inType}).
#'
#' @param inType Toggle control for the input type of the widget. One of 
#'   "nsim", "CL", "col_class", "col_obsSE", "col_predsSE", "kFixed",
#'   "ltp", "fta", "preds_CP", "dist", "frac", "DWPCol",
#'   "COdate", "gSearchInterval", "gSearchMax",
#'   "useSSinputs", or "useSSdata".  
#'
#' @return HTML for the model input widget. 
#'
#' @export
#'
modelInputWidget <- function(inType){

  if (!inType %in% c("nsim", "CL", "class", "obsSE", "predsSE",
                     "kFixedInput", "ltp", "fta", "predsCP", "dist",
                     "frac", "DWPCol", "COdate",
                     "gSearchInterval", "gSearchMax", "useSSinputs",
                     "useSSdata")){
    stop(paste0("input inType (", inType, ") not supported"))
  }

  Name <- inType

  Label <- switch(inType, 
             "nsim" = "Number of Iterations:", 
             "CL" = "Confidence Level:", 
             "class" = "Size Class Column (optional):",
             "obsSE" = "Observations:", 
             "predsSE" = "Predictor Variables:", 
             "kFixedInput" = NULL, 
             "ltp" = "Last Time Present:", 
             "fta" = "First Time Absent:", 
             "predsCP" = "Predictor Variables:", 
             "dist" = "Distributions to Include",
             "frac" = "Fraction of Facility Surveyed:", 
             "DWPCol" = "Density Weighted Proportion:", 
             "COdate" = "Date Found:",
             "gSearchInterval" = "Generic Search Interval (days):",
             "gSearchMax" = "Generic Final Search (day):",
             "useSSinputs" = "Create Custom Generic Schedule",
             "useSSdata" = "Create Average Schedule from SS Data")

  widgetFun <- switch(inType, 
                 "nsim" = "numericInput", 
                 "CL" = "numericInput", 
                 "class" = "selectizeInput",
                 "obsSE" = "selectizeInput", 
                 "predsSE" = "selectizeInput", 
                 "kFixedInput" = "htmlOutput", 
                 "ltp" = "selectizeInput", 
                 "fta" = "selectizeInput", 
                 "predsCP" = "selectizeInput", 
                 "dist" = "checkboxGroupInput",
                 "frac" = "numericInput", 
                 "DWPCol" = "selectizeInput", 
                 "COdate" = "selectizeInput",
                 "gSearchInterval" = "numericInput",
                 "gSearchMax" = "numericInput",
                 "useSSinputs" = "actionButton",
                 "useSSdata" = "actionButton")

  Args <- switch(inType, 
            "nsim" = list(value = 1000, min = 1, max = 10000, step = 1), 
            "CL" = list(value = 0.90, min = 0, max = 1, step = 0.001), 
            "class" = list(c("No data input yet"), multiple = TRUE,
                               options = list(maxItems = 1)), 
            "obsSE" = list(c("No data input yet"), multiple = TRUE), 
            "predsSE" = list(c("No data input yet"), multiple = TRUE),
            "kFixedInput" = list(NULL),
            "ltp" = list(c("No data input yet"), multiple = TRUE,
                      options = list(maxItems = 1)),
            "fta" = list(c("No data input yet"), multiple = TRUE,
                      options = list(maxItems = 1)),
            "predsCP" = list(c("No data input yet"), multiple = TRUE),
            "dist" = list(choices = CPdistOptions(),
                        selected = unlist(CPdistOptions()), inline = TRUE),
            "frac" = list(value = 1.0, min = 0.01, max = 1.0, step = 0.01),
            "DWPCol" = list(c("No data input yet"), multiple = TRUE, 
                            options = list(maxItems = 1)),
            "COdate" = list(c("No data input yet"), multiple = TRUE,
                                  options = list(maxItems = 1)),
            "gSearchInterval" = list(value = 7, min = 1, max = 400, step = 1),
            "gSearchMax" = list(value = 364, min = 1, max = 1000, step = 1),
            "useSSinputs" = list(NULL),
            "useSSdata" = list(NULL))

  Condition <- switch(inType, 
                 "nsim" = NULL, 
                 "CL" = NULL, 
                 "class" = NULL,
                 "obsSE" = NULL, 
                 "predsSE" = NULL, 
                 "kFixedInput" = NULL, 
                 "ltp" = NULL, 
                 "fta" = NULL, 
                 "predsCP" = NULL, 
                 "dist" = NULL,
                 "frac" = NULL, 
                 "DWPCol" = "output.DWPNeed == 'yes'", 
                 "COdate" = NULL,
                 "gSearchInterval" = NULL,
                 "gSearchMax" = NULL,
                 "useSSinputs" = NULL,
                 "useSSdata" = "output.data_SS != null")

  widgetMaker(Condition, Name, widgetFun, Label, Args)
}


#' @title Input Widget Maker
#'
#' @description Basic generalized function for creating an input widget based
#'   on the condition of the widget being presented, the name of the widget, 
#'   the function used to create it, it's label on the UI, and any additional 
#'   arguments. 
#'
#' @param Condition Condition under which the widget is present to the user. 
#'
#' @param Name Name (id) of the widget created.
#'
#' @param Fun Function name (as character) used to create the widget.
#'
#' @param Label Label presented to the user in the UI for the widget.
#'
#' @param Args List of any additional arguments to be passed to the widget 
#'   creating function.
#'
#' @return HTML for the widget. 
#'
#' @export
#'
widgetMaker <- function(Condition, Name, Fun, Label, Args){
  allArgs <- Args
  if (Fun == "htmlOutput"){
    allArgs[["id"]] <- Name
  } else{
    allArgs[["inputId"]] <- Name
  }
  allArgs[["label"]] <- Label
  if(is.null(Condition) || Condition == ""){
    do.call(what = Fun, args = allArgs)
  } else {
    conditionalPanel(condition = Condition, 
      do.call(what = Fun, args = allArgs)
    )
  }
}


#' @title Create a Model Run Widget for the GenEst User Interface HTML
#'
#' @description This is a generalized function for creating a model run 
#'   widget used in the GenEst GUI, based on the model type (\code{modType}).
#'   The widget includes the model run button, the model clear button 
#'   (once the model has finished running), and any text displayed prior to
#'   model running being allowed.
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

  rName <- switch(modType,
             "SE" = "run_SE",
             "CP" = "run_CP",
             "M" = "runMod_M",
             "g" = "runMod_g")
  rLabel <- switch(modType, 
              "SE" = "Run Model",
              "CP" = "Run Model",
              "M" = "Estimate",
              "g" = "Estimate")
  rCondition <- switch(modType, 
                  "SE" = "input.obsSE != null",
                  "CP" = "input.ltp != null & input.fta != null",
                  "M" = "input.modelChoices_SE1 != null & 
                         input.modelChoices_CP1 != null & 
                         output.sizeclasses_SE == output.sizeclasses_CP & 
                         output.data_SS != null & 
                         input.DWPCol != null & input.COdate != null &
                         output.kFillNeed != 'yes'",
                  "g" = "input.modelChoices_SE1 != null & 
                         input.modelChoices_CP1 != null & 
                         output.sizeclasses_SE == output.sizeclasses_CP &
                         output.kFillNeed != 'yes'")

  cName <- switch(modType, 
             "SE" = "run_SE_clear",
             "CP" = "run_CP_clear",
             "M" = "runMod_M_clear",
             "g" = "runMod_g_clear")
  cLabel <- switch(modType, 
              "SE" = "Clear Model",
              "CP" = "Clear Model",
              "M" = "Clear Estimate",
              "g" = "Clear Estimate")
  cCondition <- switch(modType, 
                  "SE" = "output.SEModDone == 'OK'",
                  "CP" = "output.CPModDone == 'OK'",
                  "M" = "output.MModDone == 'OK'",
                  "g" = "output.gModDone == 'OK'")

  rButton <- conditionalPanel(condition = rCondition,
               br(), 
               actionButton(rName, rLabel)          
             )
  cButton <- conditionalPanel(condition = cCondition,
               actionButton(cName, cLabel, style = cButtonStyle()),
               br(), br()          
             )

  preText <- preTextMaker(modType)
  lpre <- length(preText)
  preText[[lpre + 1]] <- rButton
  preText[[lpre + 2]] <- cButton
  preText
}


#' @rdname modelRunWidget
#'
#' @details \code{preTextMaker} creates pre-model-run text depending on
#'   the model type.
#'
#' @return HTML for the model run widget pre-run text. 
#'
#' @export
#'
preTextMaker <- function(modType){

  if (!modType %in% c("SE", "CP", "M", "g")){
    stop(paste0("input modType (", modType, ") not supported"))
  }

  Condition <- switch(modType, 
                 "SE" = "input.obsSE == null",
                 "CP" = "input.ltp == null | input.fta == null",
                 "M" = c("input.modelChoices_SE1 == null |
                         input.modelChoices_CP1 == null | 
                         output.sizeclasses_SE != output.sizeclasses_CP",
                         "output.data_SS == null",
                         "input.modelChoices_SE1 != null & 
                         input.modelChoices_CP1 != null & 
                         output.sizeclasses_SE == output.sizeclasses_CP & 
                         (input.DWPCol == null | input.COdate == null)",
                         "output.kFillNeed == 'yes' & 
                         input.modelChoices_SE1 != null"),
                 "g" = c("input.modelChoices_SE1 == null | 
                         input.modelChoices_CP1 == null | 
                         output.sizeclasses_SE != output.sizeclasses_CP",
                         "output.kFillNeed == 'yes' & 
                         input.modelChoices_SE1 != null")
                )

  Text <- switch(modType, 
             "SE" = "Select observation columns to run model",
             "CP" = "Select observation columns to run model",
             "M" = c("Select SE and CP models fit to matching size classes to 
                      run model",
                     "Input Search Schedule data to run model",
                     "Select input columns to run model",
                     "A value for k is required to estimate mortality. 
                      Return to Search Efficiency tab and fix k."),
             "g" = c("Select SE and CP models fit to matching size
                     classes to run model",
                     "A value for k is required to estimate detection
                     probability. Return to Search Efficiency tab and fix k.")
           )

  out <- vector("list", length(Condition))
  for (i in 1:length(Condition)){
    out[[i]] <- conditionalPanel(condition = Condition[i], 
                                 br(), center(em(Text[i]))) 
  }
  out
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

  Condition <- switch(modType,
                 "SE" = "output.SEModDone == 'OK'",
                 "CP" = "output.CPModDone == 'OK'",
                 "M" = "output.MModDone == 'OK'",
                 "g" = "output.gModDone == 'OK' & 
                       output.sizeclass_gyn == 'YES'")

  Header <- switch(modType,
                 "SE" = "Table & Figure Selection:",
                 "CP" = "Table & Figure Selection:",
                 "M" = "Splitting Mortality:",
                 "g" = "Table & Figure Selection:")

  sCondition <- switch(modType,
                  "SE" = c("output.sizeclass_SEyn == 'YES'", "", ""),
                  "CP" = c("output.sizeclass_CPyn == 'YES'", rep("", 3)),
                  "M" = c("", ""),
                  "g" = "")

  sName <- switch(modType,
             "SE" = c("outSEclass", "outSEp", "outSEk"),
             "CP" = c("outCPclass", "outCPdist", "outCPl", "outCPs"),
             "M" = c("split_SS", "split_CO"),
             "g" = "outsizeclassg")

  sLabel <- switch(modType,
              "SE" = c("Size Class:", "p Model:", "k Model:"),
              "CP" = c("Size Class:", "Distribution:", "Location:", 
                       "Scale:"),
              "M" = c("Search Schedule (SS) Variable:",
                      "Carcass Observation (CO) Variable:"),
              "g" = "Size Class:")

  sArgs <- switch(modType,
              "SE" = list(list(choices = " ", multiple = FALSE),
                          list(choices = " ", multiple = FALSE),
                          list(choices = " ", multiple = FALSE)),
              "CP" = list(list(choices = " ", multiple = FALSE),
                          list(choices = " ", multiple = FALSE),
                          list(choices = " ", multiple = FALSE),
                          list(choices = " ", multiple = FALSE)),
              "M" = list(list(choices = " ", multiple = TRUE, 
                           options = list(maxItems = 1)),
                         list(choices = " ", multiple = TRUE, 
                           options = list(maxItems = 2))),
              "g" = list(list(choices = " ", multiple = FALSE)))

  
  subs <- vector("list", length = length(sCondition))
  for(i in 1:length(sCondition)){
    subs[[i]] <- widgetMaker(sCondition[i], sName[i], "selectizeInput", 
                   sLabel[i], sArgs[[i]]
                 )
  }

  aText <- NULL
  splitButtons <- NULL
  if (modType == "M"){
    aText <- list(em("Max. two total splits, max. one schedule-based split"),
               br(), br()
             )
    splitButtons <- splitButtonWidget()
  }

  conditionalPanel(condition = Condition,
    b(u(big(Header))), br(), br(), aText, subs, splitButtons
  )
}

#' @rdname modelOutputWidget 
#'
#' @details \code{splitButtonWidget} creates the set of buttons to handle the
#'   splitting of mortality estimates (splitting, transposing the split, and
#'   clearing the split).
#'
#' @export
#'
splitButtonWidget <- function(){
  list(
    fluidRow(
      column(width = 6, actionButton("splitM", "Split Estimate")),
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
    AICcTab <- aicc(mods[[sci]], quiet = TRUE)
  }
  if (modType == "CP"){
    AICcTab <- aicc(mods[[sci]], quiet = TRUE)
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
  modDeltaAICcs <- AICcTab[ , "\u0394AICc"]
  modLabels <- paste0(modNames, " (\u0394AICc: ", modDeltaAICcs, ")")
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
