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
    b(u(big("Model Inputs:"))),
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
      center(em("Select observation columns to run model"))
    ),
    conditionalPanel(
      condition = "input.ltp != null & input.fta != null",
      br(),
      actionButton("runMod_CP", "Run Model")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'", 
      actionButton("runMod_CP_clear", "Clear Model", style = cButtonStyle()),
      br(), br(),
      b(u(big("Table & Figure Selection:"))),
      br(), br(),
      conditionalPanel(condition = "output.sizeclass_CPyn == 'YES'",
        selectizeInput("outsizeclassCP", "Size Class:", " ", multiple = FALSE)
      ),
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
      CPModComparisonPanel(),
      CPFiguresPanel(),
      CPEstimatesPanel(),
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
      em("Run model to view figures")
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
      em("Run model to view model estimates")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'",
      textOutput("sizeclass_CP2"), br(), 
      textOutput("text_CP_est"), br(), 
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
      em("Run models to view model comparison")
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
      em("Run models to select models")
    ),
    htmlOutput("modelMenu_CP")
  )
}
