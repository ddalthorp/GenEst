#' @title Analysis Detection Probability Main Panel UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability 
#'   panel
#'
#' @return HTML for the Analysis Detection Probability panel
#'
#' @export
#'
gPanel <- function(){
  tabPanel("Detection Probability", br(), br(),
    gSidebar(),
    gMainPanel()
  )
}

#' @title Analysis Detection Probability sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability 
#'   sidebar
#'
#' @return HTML for the Analysis Detection Probability sidebar
#'
#' @export
#'
gSidebar <- function(){
  sidebarPanel(width = 3, 
    HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
    br(), br(),
    conditionalPanel(
      condition = "output.kFillNeed == 'yes'",
      numericInput("kFill_g", "Assumed k:", value = 0.5, 
        min = 0, max = 1, step = 0.001
      )
    ),
    conditionalPanel(    
      condition = "output.data_SS != null",
      actionButton("useSSdata", "Create Average Schedule from SS Data")
    ),
    numericInput("gSearchInterval", "Generic Search Interval (days):", 
      value = 7, min = 1, max = 400, step = 1),
    numericInput("gSearchMax", "Generic Final Search (day):",
      value = 364, min = 1, max = 1000, step = 1),
    actionButton("useSSinputs", "Create Custom Generic Schedule"),
    conditionalPanel(
      condition = 
        "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
         output.sizeclasses_SE != output.sizeclasses_CP",
      br(), 
      HTML("<center><em>Select SE and CP models fit to matching size
        classes to run model</center></em>"
      )
    ),
    conditionalPanel(
      condition = 
        "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
         output.sizeclasses_SE == output.sizeclasses_CP",
      br(), br(),
      actionButton("runMod_g", "Estimate")
    ),
    conditionalPanel(condition = "output.gModDone == 'OK'",
      actionButton("runMod_g_clear", "Clear Estimate", style = cButtonStyle())
    ),
    conditionalPanel(
      condition = "output.gModDone == 'OK' & output.sizeclass_gyn == 'YES'", 
      br(), br(), 
      HTML("<big><strong><u> Table & Figure Selection: 
        </u></strong></big>"
      ), br(), br(),
      selectizeInput("outsizeclassg", "Size Class:", 
        " ", multiple = FALSE
      )
    )
  )
}

#' @title Analysis Detection Probability main panel UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability 
#'   main panel
#'
#' @return HTML for the Analysis Detection Probability main panel
#'
#' @export
#'
gMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_g",
      gSchedulePanel(),
      gFigurePanel(),
      gSummaryPanel()
    )
  )
}

#' @title Analysis Detection Probability main panel schedule UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability
#'   main panel schedule element
#'
#' @return HTML for the Analysis Detection Probability schedule panel
#'
#' @export
#'
gSchedulePanel <- function(){
  tabPanel("Schedule",         
    br(), 
    HTML("<big><strong><u> Search Schedule: </u></strong></big>"),
    br(), br(), 
    textOutput("SStext")
  )
}

#' @title Analysis Detection Probability main panel figure UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability
#'   main panel figure element
#'
#' @return HTML for the Analysis Detection Probability figure panel
#'
#' @export
#'
gFigurePanel <- function(){
  tabPanel("Figure", br(), 
    conditionalPanel(condition = "output.fig_g == null",
      HTML("<em>Run estimate to view figure</em>")
    ), 
    conditionalPanel(condition = "output.gModDone == 'OK'",
      textOutput("sizeclass_g1"), br(), 
      plotOutput("fig_g", inline = TRUE), br(), br(),
      downloadButton("dlgfig", "Download")
    )
  )
}

#' @title Analysis Detection Probability main panel summary UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability
#'   main panel summary element
#'
#' @return HTML for the Analysis Detection Probability summary panel
#'
#' @export
#'
gSummaryPanel <- function(){
  tabPanel("Summary", br(), 
    conditionalPanel(condition = "output.table_g == null",
      HTML("<em>Run estimate to view summary</em>")
    ), 
    conditionalPanel(condition = "output.gModDone == 'OK'",
      textOutput("sizeclass_g2"), br(), 
      br(), dataTableOutput("table_g"), br(),
      downloadButton("dlgtab", "Download")
    )
  )
}
