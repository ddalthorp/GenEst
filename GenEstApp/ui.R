##############################################################################
#
#  This script contains the UI code for the the GenEst app
#
#  version 0.0.0.3 October 2017
#
#  Held under GNU GPL v >= 3	
#
##############################################################################

shinyUI(
  navbarPage("GenEst",
            
    tabPanel("Home",
      br(),
      HTML('<center><img src="GenEstLogoExample1.jpg" height = 
                              "500"></center>'),
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
              fileInput("COFile", "Carcass Observation Data File",
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
                tabPanel("Search Efficiency",  dataTableOutput("SEdata")),
                tabPanel("Carcass Persistence",  dataTableOutput("CPin")),
                tabPanel("Search Schedule",  dataTableOutput("SSin")),
                tabPanel("Carcass Observations",  dataTableOutput("COin")),
                tabPanel("Meta Data",  tableOutput("MDin"))
              )
            )
          )
    ),

    tabPanel("Analyses",
      tabsetPanel(
        tabPanel("Main Inputs",
          br(), br(), 
          sidebarPanel(width = 3,
            numericInput("Niterations", "Number of iterations:", value = 1000, 
                         min = 1, max = 10000, step = 1)
          ),
          mainPanel(
            br()
          )
        ),
        tabPanel("Search Efficiency",
          br(), br(), 
          sidebarPanel(width = 3, 
            selectizeInput("SEvars", "Choose predictor variables (max: 2):", 
              c("No data input yet"), multiple = T),
            selectizeInput("SEsizeclasscol", 
              "Choose size class column (max: 1):", 
              c("No data input yet"), multiple = T),
            selectizeInput("SEobscols", 
              "Choose observation columns (in order):",
              c("No data input yet"), multiple = T),
            selectizeInput("fixKchoice", "Use fixed k?",
              c("NO", "YES"), multiple = F),
            numericInput("fixKvalchoice", "Value for fixed k:", value = NULL, 
                         min = 0, max = 1, step = 0.001),
            shiny::actionButton("SEmodrun", "Run SE Model")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Data", br(), br(), tableOutput("selected_SE")),
              tabPanel("Model Table", br(), 
                       selectizeInput("SEaicsizeclass", 
                         "Choose size class for AIC table:", 
                         "Model not yet run", multiple = F ),
                        br(), 
                        shiny::actionButton("SEaictablerun", 
                          "Generate AIC table"), 
                        br(), br(), 
                        dataTableOutput("SEaictable") 
                       ),
              tabPanel("Figures", br(),
                       selectizeInput("SEfigsizeclass", 
                         "Choose size class for SE figure:", 
                         "Model not yet run", multiple = F ),
                       selectizeInput("SEfigmodel", 
                         "Choose model for SE figure:", 
                         "Model not yet run", multiple = F ),
                       br(), 
                       shiny::actionButton("SEfigrun", "Generate SE Figure"), 
                       br(), br(), 
                       plotOutput("SEfig", width = "800px", height = "800px")
                       ),
              tabPanel("Model Selection", 
                       br(), 
                       shiny::actionButton("SEmodOpsPop","Populate Options"),
                       br(), br(),
                       htmlOutput("SEmodselectinputs")) 
            )
          )
        ),
        tabPanel("Carcass Persistence",
          br(), br(), 
          sidebarPanel(width = 3,
            selectizeInput("CPvars", 
                         "Choose predictor variables (max: 2):", 
              c("No data input yet"), multiple = T),
            selectizeInput("CPsizeclasscol", 
              "Choose size class column (max: 1):",
              c("No data input yet"), multiple = T),
            selectizeInput("CPltp", 
              "Choose last time present observation column (max: 1):",
              c("No data input yet"), multiple = T),
            selectizeInput("CPfta", 
              "Choose first time absent observation column (max: 1):",
              c("No data input yet"), multiple = T),
            shiny::actionButton("CPmodrun", "Run CP Model")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Data", tableOutput("selected_CP")),
              tabPanel("Model Table",  br(),
                       selectizeInput("CPaicsizeclass", 
                         "Choose size class for AIC table:", 
                         "Model not yet run", multiple = F ),
                        br(), 
                        shiny::actionButton("CPaictablerun", 
                         "Generate AIC table"),
                        br(), br(), dataTableOutput("CPaictable") 
                       ),
              tabPanel("Figures",  br(),
                       selectizeInput("CPfigsizeclass",
                          "Choose size class for CP figure:",
                          "Model not yet run", multiple = F ),
                       selectizeInput("CPfigmodelcomplexity",
                          "Choose model complexity for CP figure:", 
                          "Model not yet run", multiple = F ),
                       selectizeInput("CPfigdistemph",
                          "Choose distribution to emphasize:", 
                          "Model not yet run", multiple = F ),
                       br(), shiny::actionButton("CPfigrun",
                          "Generate CP Figure"), br(), br(), 
                       plotOutput("CPfig", width = "800px", 
                                   height = "1000px")
                       ),
              tabPanel("Model Selection", br(), 
                        shiny::actionButton("CPmodOpsPop","Populate Options"),
                         br(), br(),  
                         htmlOutput("CPmodselectinputs"))
            )
          )
        ),
        tabPanel("Detection Probability",
          br(), br(), 
          sidebarPanel(width = 3,
                       numericInput("gCIw", "Confidence level:", 
                         value = 0.9, 
                         min = 0, max = 1, step = 0.001),
                       shiny::actionButton("grun", 
                         "Estimate Detection Probability"), br()
          ),
          mainPanel(
            br(), dataTableOutput("gtable"),
            br()
          )
        ),
        tabPanel("Fatality Estimation",
          br(), br(), 
          sidebarPanel(width = 3, 
            selectizeInput("COsplitcol", "Choose split column (max: 1):", 
              c("No data input yet"), multiple = T),
            selectizeInput("COsizeclasscol", 
              "Choose size class column (max: 1):", 
              c("No data input yet"), multiple = T),
            selectizeInput("COsscol", 
              "Choose search schedule column (max: 1):",
              c("No data input yet"), multiple = T),
            selectizeInput("COunitcol", "Choose unit column (max: 1):",
              c("No data input yet"), multiple = T),
            numericInput("MCIw", "Confidence level:", value = 0.9, 
                         min = 0, max = 1, step = 0.001),
            numericInput("ffs", "Fraction of facility surveyed", value = 1.0, 
                         min = 0, max = 1, step = 0.001),
            shiny::actionButton("Mrun", "Estimate total carcasses")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Results Table",  br(), tableOutput("Mhattab")),
              tabPanel("Figures",  br(), 
                plotOutput("Mhatfig", width = "800px", height = "800px"))
            )
          )
        )
      )
    ),

    #tabPanel("Reporting",
    #  br()
    #),


    tabPanel("About",
      fluidRow(
        column(6, offset = 3,
          HTML('<img src = "GenEstLogoExample1.jpg" height = "400">'),
          br(), 
          br(),
          HTML('<b>Authors:</b>  
            Daniel Dalthorp
              <a href = "http://www.USGS.gov">(USGS)</a>,
            Juniper Simonis
              <a href = "http://www.dapperstats.com">(DAPPER Stats)</a>,
            Lisa Madsen
              <a href = "http://www.OSU.edu">(OSU)</a>,
            Paul Rabie 
              <a href = "http://www.west-inc.com">(WEST)</a>, and
            Manuela Huso
              <a href = "http://www.USGS.gov">(USGS)</a>'),
          br(), 
          br(),
          HTML('GenEst is a tool for estimating bird and bat fatalities 
            at renewable power facilities.'),
          br(),
          br(),
          HTML('GenEst is currently in development
            and should be considered provisional.'),
          br(),
          br(),
          HTML('This is version 0.0.0.3, October 2017.'),
          br(),
          br(),
          HTML('The development of GenEst is being supported by Bat
            Conservation International, The US Bureau of Land Management,
            The US Geological Survey, WEST, and Oregon State University.'),
          br(),
          br(),
          HTML('GenEst is provided under GNU GPL version 3 (and any 
            later versions).')
        )
      )
    )
  )
)

