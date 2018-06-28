#' @title Analysis Carcass Persistence Main Panel UI element 
#'
#' @description create the HTML code for the Analysis CP panel
#'
#' @return HTML for the Analysis CP panel
#'
#' @export
#'
CPPanel <- function(){
  tabPanel("Carcass Persistence", br(), br(), 
    CPSidebar(), 
    CPMainPanel()
  )
}

#' @title Analysis Carcass Persistence sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   sidebar
#'
#' @return HTML for the Analysis Carcass Persistence sidebar
#'
#' @export
#'
CPSidebar <- function(){
  sidebarPanel(width = 3,
    HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
    br(), br(),
    selectizeInput("ltp", "Last Time Present:", c("No data input yet"), 
      multiple = TRUE, options = list(maxItems = 1)
    ),
    selectizeInput("fta", "First Time Absent:", c("No data input yet"),
      multiple = TRUE, options = list(maxItems = 1)
    ),
    selectizeInput("preds_CP", "Predictor Variables:", 
      c("No data input yet"), multiple = TRUE
    ),
    checkboxGroupInput("dists", label = "Distributions to Include",
      choices = CPdistOptions(), 
      selected = c("exponential", "weibull", "lognormal", "loglogistic"),
      inline = TRUE
    ),
    conditionalPanel(
       condition = "input.ltp == null | input.fta == null",
      br(), 
      HTML("<center><em>Select observation columns to run model</center></em>"
      )          
    ),
    conditionalPanel(
      condition = "input.ltp != null & input.fta != null",
      br(),
      actionButton("runMod_CP", "Run Model")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'", 
      br(), br(), 
      HTML("<big><strong><u> Table & Figure Selection: </u></strong></big>"),
      br(), br(),
      selectizeInput("outsizeclassCP", "Size Class:",  " ", multiple = FALSE),
      selectizeInput("outCPdist", "Distribution:", " ", multiple = FALSE),
      selectizeInput("outCPl", "Location Model:", " ", multiple = FALSE),
      selectizeInput("outCPs", "Scale Model:", " ", multiple = FALSE)        
    )
  )
}

#' @title Analysis Carcass Persistence main panel UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main
#'
#' @return HTML for the Analysis Carcass Persistence main panel
#'
#' @export
#'
CPMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_CP",
      CPSelectedDataPanel(),
      CPFiguresPanel(),
      CPEstimatesPanel(),
      CPModComparisonPanel(),
      CPModSelectionPanel()
    )
  )
}

#' @title Analysis Carcass Persistence main panel selected data UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel selected data element
#'
#' @return HTML for the Analysis Carcass Persistence selected data panel
#'
#' @export
#'
CPSelectedDataPanel <- function(){
  tabPanel("Selected Data", br(), 
    conditionalPanel(condition = "input.ltp == null & input.fta == null",
      HTML("<em>Select observation columns to view data</em>")
    ), br(), 
    dataTableOutput("selected_CP")
  )
}

#' @title Analysis Carcass Persistence main panel figures UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel figures element
#'
#' @return HTML for the Analysis Carcass Persistence figures panel
#'
#' @export
#'
CPFiguresPanel <- function(){
  tabPanel("Figures", br(), 
    conditionalPanel(condition = "output.fig_CP == null",
      HTML("<em>Run model to view figures</em>")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'",
      textOutput("sizeclass_CP1"), br(), 
      plotOutput("fig_CP", inline = TRUE), br(), br(),
      downloadButton("dlCPfig", "Download")
    )
  )
}

#' @title Analysis Carcass Persistence main panel estimates UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel model estimates element
#'
#' @return HTML for the Analysis Carcass Persistence model estimates panel
#'
#' @export
#'
CPEstimatesPanel <- function(){
  tabPanel("Estimates", br(),  
    conditionalPanel(condition = "output.modTab_CP == null",
      HTML("<em>Run model to view model estimates</em>")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'",
      textOutput("sizeclass_CP2"), br(), 
      dataTableOutput("modTab_CP"), br(),
      downloadButton("dlCPest", "Download")
    )
  )
}

#' @title Analysis Carcass Persistence main panel model comparison UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel model comparison element
#'
#' @return HTML for the Analysis Carcass Persistence model comparison panel
#'
#' @export
#'
CPModComparisonPanel <- function(){
  tabPanel("Model Comparison", br(), 
    conditionalPanel(condition = "output.AICcTab_CP == null",
      HTML("<em>Run models to view model comparison</em>")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'",
      textOutput("sizeclass_CP3"), br(), 
      dataTableOutput("AICcTab_CP"), br(),
      downloadButton("dlCPAICc", "Download")
    )
  )
}

#' @title Analysis Carcass Persistence main panel model selection UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel model selection element
#'
#' @return HTML for the Analysis Carcass Persistence model selection panel
#'
#' @export
#'
CPModSelectionPanel <- function(){
  tabPanel("Model Selection", br(), 
    conditionalPanel(condition = "output.modelMenu_CP == null",
      HTML("<em>Run models to select models</em>")
    ),
    htmlOutput("modelMenu_CP")
  )
}
