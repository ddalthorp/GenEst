#' @title Help Getting Started Main Panel UI element 
#'
#' @description create the HTML code for the Help Getting Started panel
#'
#' @return HTML for the Help Getting Started panel
#'
#' @export
#'
gettingStartedPanel <- function(){
  tabPanel("Getting Started", gettingStartedMainPanel())
}


#' @title Help Getting Started main panel UI element
#'
#' @description create the HTML code for the Help Getting Started main panel
#'
#' @return HTML for the Help Getting Started main panel
#'
#' @export
#'
gettingStartedMainPanel <- function(){
  mainPanel(
    column(10, offset = 0,
      br(), br(),
      p("GenEst is an R software package for estimating bird and bat 
        fatalities at wind and solar power facilities. Mortality estimation 
        requires five data files:"
      ),
      ol(li("searcher efficiency field trial results (SE),"),
         li("carcass persistence field trial results (CP),"),
         li("schedule for periodic carcass surveys (SS),"),
         li("fraction of total carcasses falling in the searched area at each
            unit searched or", em("density-weighted proportion"), 
            "(DWP), and"
         ),
         li("summary data from the carcass surveys, including numbers of 
            carcasses observed on each search occasion (CO) and other, 
            optional covariates")
      ),
      br(), 
      p("Analysis involves several steps:"),
      ul(li("uploading data---click the ", code("Data Input"), "tab,"),
         li("entering ", code("General Input"), " parameters---click the ",  
            code("Analyses"), " tab,"),
         li("fitting searcher efficiency and carcass persistence models---
            click the ", code("Searcher Efficiency"), " and ", 
            code("Carcass Persistence"), " tabs, and "),
         li("estimating total mortality and split the mortality estimate by
            various subcategories (such as species or sector or season) as 
            desired---click the ", code("Mortality Estimation"), " tab")
      ),
      br(),
      p("Further details can be found in the ",
        a("User Guide", href = ftpLink("UserGuide"), target = "_blank"),
        " and in a technical manual that describes the",
        a("statistical models", href = ftpLink("Models"), target = "_blank") 
      ), 
      br(),
      p("Example data sets are available under the ", code("Downloads"), 
        " tab.")
    )
  )
}
