###############################################################################
###############################################################################
##
##  This script contains the UI code for the the GenEst app
##
##  Coded by Joseph (Josie, they) L. Simonis, DAPPER Stats
##
##  version 0.0.0.1 2017
##
##  Held under GNU GPL v >= 3	
##
###############################################################################
###############################################################################

##############################
#
#  Table of Contents
#
#  1. load packages
#  2. ui code
#

#1. load code and packages

  source("genestfunctions.R")
  packageLoad()


#2. ui code

# navbar page sets up the general format with the navigation bar and tabs, each
# of which is defined by a tabPanel function
#
#  Home: general page with image
#  Data: page for data entry  
#  Analyses: pages for analyses
#      SE: subpage for searcher efficiency analysis
#      SE: subpage for carcass persistence analysis
#      SE: subpage for carcass estimation  
#  Reporting: page for automating report output
#  About: general info

shinyUI(
  navbarPage("GenEst",
            
    tabPanel("Home",
      br(),
      HTML('<center><img src="GenEstLogoExample2.jpg" height = "500"></center>'),
      br()
    ),

    tabPanel("Data Input",

          sidebarLayout(
            sidebarPanel(width = 3,
              HTML('<b><big><center>Upload data:</center></big></b>'),
              br(),
              fileInput("SEFile", "Choose Search Efficiency Data File",
                accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
              ),
              br(),
              fileInput("CPFile", "Carcass Persistence Data File",
                accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
              ),
              br(),
              fileInput("SSFile", "Search Schedule Data File",
                accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
              ),
              br(),
              fileInput("FOFile", "Fatality Observation Data File",
                accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
              ),
              br(),
              fileInput("MDFile", "Meta Data File",
                accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
              )
            ),
            mainPanel(
              HTML('<b><big><center>View data:</center></big></b>'),
              br(),
              tabsetPanel(
                tabPanel("Search Efficiency",  tableOutput("SEin")),
                tabPanel("Carcass Persistence",  tableOutput("CPin")),
                tabPanel("Search Schedule",  tableOutput("SSin")),
                tabPanel("Fatality Observations",  tableOutput("FOin")),
                tabPanel("Meta Data",  tableOutput("MDin"))
              )
            )
          )



    ),

    tabPanel("Analyses",
      tabsetPanel(
        tabPanel("Search Efficiency",
          sidebarPanel(width = 3, 
            selectizeInput("SEfactorselect", "Choose classification factors:", 
              c("No data input yet"), multiple = T),
            selectizeInput("SEobsselect", "Choose observations:",
              c("No data input yet"), multiple = T)
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Data", tableOutput("selected_SE")),
              tabPanel("Analysis Results",  br()),
              tabPanel("Figures",  br())
            )
          )
        ),
        tabPanel("Carcass Persistence",
          sidebarPanel(width = 3,
            br()
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Data", br()),
              tabPanel("Analysis Results",  br()),
              tabPanel("Figures",  br())
            )
          )
        ),
        tabPanel("Fatality Estimation",
          sidebarPanel(width = 3,
            br()
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Data", br()),
              tabPanel("Analysis Results",  br()),
              tabPanel("Figures",  br())
            )
          )
        )
      )
    ),

    tabPanel("Reporting",
      br()
    ),


    tabPanel("About",
      fluidRow(
        column(6, offset = 3,
          HTML('<img src = "GenEstLogoExample2.jpg" height = "300">'),
          br(), 
          br(),
          HTML('<b>Authors:</b>  
            Daniel Dalthorp
              <a href = "http://www.USGS.gov">(USGS)</a>,
            Joseph L. Simonis
              <a href = "http://www.dapperstats.com">(DAPPER Stats)</a>,
            Lisa Madsen
              <a href = "http://www.OSU.edu">(OSU)</a>,
            Paul Rabie 
              <a href = "http://www.west-inc.com">(WEST)</a>, and
            Manuela Huso
              <a href = "http://www.USGS.gov">(USGS)</a>'),
          br(), 
          br(),
          HTML('GenEst is a tool for estimating fatalities at renewable
            power facilities.'),
          br(),
          br(),
          HTML('It is currently in development
            and should be considered provisional.'),
          br(),
          br(),
          HTML('The development of GenEst is being funded by Bat
            Conservation International.'),
          br(),
          br(),
          HTML('GenEst is provided under GNU GPL version 3 (and any 
            later versions).')
        )
      )
    )
  )
)

