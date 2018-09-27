#' @title Analysis Searcher Efficiency Main Panel UI element 
#'
#' @description create the HTML code for the Analysis SE panel
#'
#' @return HTML for the Analysis SE panel
#'
#' @export
#'
SEPanel <- function(){
  tabPanel("Searcher Efficiency", br(), br(), 
    SESidebar(), 
    SEMainPanel()
  )
}

#' @title Analysis Searcher Efficiency sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   sidebar
#'
#' @return HTML for the Analysis Searcher Efficiency sidebar
#'
#' @export
#'
SESidebar <- function(){
  sidebarPanel(width = 3,
    b(u(big("Model Inputs:"))),
    br(), br(),
    selectizeInput("obsCols_SE", "Observations:", c("No data input yet"), 
      multiple = TRUE
    ),
    selectizeInput("preds_SE", "Predictor Variables:", 
      c("No data input yet"), multiple = TRUE
    ),
    htmlOutput("kFixedInput"),
    conditionalPanel(condition = "input.obsCols_SE == null",
      br(), 
      center(em("Select observation columns to run model"))
    ),
    conditionalPanel(condition = "input.obsCols_SE != null",
      br(), 
      actionButton("runMod_SE", "Run Model")          
    ),
    conditionalPanel(condition = "output.SEModDone == 'OK'", 
      actionButton("runMod_SE_clear", "Clear Model", style = cButtonStyle()),
      br(), br(),
      b(u(big("Table & Figure Selection:"))),
      br(), br(), 
      conditionalPanel(condition = "output.sizeclass_SEyn == 'YES'",
        selectizeInput("outsizeclassSE", "Size Class:", " ", multiple = FALSE)
      ),
      selectizeInput("outSEp", "p Model:", " ", multiple = FALSE), 
      selectizeInput("outSEk", "k Model:", " ", multiple = FALSE)
    )
  )
}

#' @title Analysis Searcher Efficiency main panel UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main
#'
#' @return HTML for the Analysis Searcher Efficiency main panel
#'
#' @export
#'
SEMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_SE",
      SESelectedDataPanel(),
      SEModComparisonPanel(),
      SEFiguresPanel(),
      SEEstimatesPanel(),
      SEModSelectionPanel()
    )
  )
}

#' @title Analysis Searcher Efficiency main panel selected data UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel selected data element
#'
#' @return HTML for the Analysis Searcher Efficiency selected data panel
#'
#' @export
#'
SESelectedDataPanel <- function(){
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
}

#' @title Analysis Searcher Efficiency main panel figures UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel figures element
#'
#' @return HTML for the Analysis Searcher Efficiency figures panel
#'
#' @export
#'
SEFiguresPanel <- function(){
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
}

#' @title Analysis Searcher Efficiency main panel estimates UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel model estimates element
#'
#' @return HTML for the Analysis Searcher Efficiency model estimates panel
#'
#' @export
#'
SEEstimatesPanel <- function(){
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
}

#' @title Analysis Searcher Efficiency main panel model comparison UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel model comparison element
#'
#' @return HTML for the Analysis Searcher Efficiency model comparison panel
#'
#' @export
#'
SEModComparisonPanel <- function(){
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
}

#' @title Analysis Searcher Efficiency main panel model selection UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel model selection element
#'
#' @return HTML for the Analysis Searcher Efficiency model selection panel
#'
#' @export
#'
SEModSelectionPanel <- function(){
  tabPanel("Model Selection", br(), 
    conditionalPanel(condition = "output.modelMenu_SE == null",
      em("Run models to select models")
    ),
    htmlOutput("modelMenu_SE")
  )
}


