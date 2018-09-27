#' @title Analysis Mortality Main Panel UI element 
#'
#' @description create the HTML code for the Analysis Mortality panel
#'
#' @return HTML for the Analysis Mortality Estimation panel
#'
#' @export
#'
MPanel <- function(){
  tabPanel("Mortality Estimation", br(), br(), 
    MSidebar(),
    MMainPanel()
  )
}

#' @title Analysis Mortality sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis Mortality sidebar
#'
#' @return HTML for the Analysis Mortality sidebar
#'
#' @export
#'
MSidebar <- function(){
  sidebarPanel(width = 3, 
    b(u(big("Model Inputs:"))),
    br(), br(),
    conditionalPanel(
      condition = "output.kFillNeed == 'yes'",
      htmlOutput("kFillInput")
    ),
    numericInput("frac", "Fraction of Facility Surveyed:", value = 1.0, 
      min = 0.01, max = 1.0, step = 0.01
    ),
    conditionalPanel(
      condition = "output.DWPNeed == 'yes'",
      selectizeInput("DWPCol", "Density Weighted Proportion:", 
        c("No data input yet"), multiple = TRUE, options = list(maxItems = 1)
      )
    ),
    selectizeInput("dateFoundCol", "Date Found:", c("No data input yet"), 
      multiple = TRUE, options = list(maxItems = 1)
    ),
    conditionalPanel(
      condition = 
        "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
         output.sizeclasses_SE != output.sizeclasses_CP",
      br(), 
      center(
       em("Select SE and CP models fit to matching size classes to run model")
      )
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
      condition = 
        "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
         output.sizeclasses_SE == output.sizeclasses_CP & 
         output.data_SS != null & 
         input.DWPCol != null & input.dateFoundCol != null",
      br(),
      actionButton("runMod_M", "Estimate")
    ),
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
        column(width = 4,  
          actionButton("splitM", "Split Estimate")
        ),
        column(width = 4,
          conditionalPanel(
            condition = "output.MSplitDone == 'OK' & output.nMSplits > 1",
            actionButton("transposeSplit", "Transpose Split Plot")
          )
        )
      ),
      conditionalPanel(condition = "output.MSplitDone == 'OK'", 
        actionButton("splitM_clear", "Clear Split", style = cButtonStyle())
      )
    )
  )
}

#' @title Analysis Mortality main panel UI element 
#'
#' @description create the HTML code for the Analysis Mortality main panel
#'
#' @return HTML for the Analysis Mortality main panel
#'
#' @export
#'
MMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_M",
      MFigurePanel(),
      MSummaryPanel()
    )
  )
}


#' @title Analysis Mortality main panel figure UI element 
#'
#' @description create the HTML code for the Analysis Mortality
#'   main panel figure element
#'
#' @return HTML for the Analysis Mortality figure panel
#'
#' @export
#'
MFigurePanel <- function(){
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
}


#' @title Analysis Mortality main panel summary UI element 
#'
#' @description create the HTML code for the Analysis Mortality
#'   main panel summary element
#'
#' @return HTML for the Analysis Mortality summary panel
#'
#' @export
#'
MSummaryPanel <- function(){
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
}

