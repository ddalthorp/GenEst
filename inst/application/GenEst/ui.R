##############################################################################
#
#  This script contains the UI code for the the GenEst app
#
#  


# main page

  navbarPage("GenEst",

    # Home tab: splash page            

      tabPanel("Home",
        br(),
        HTML('<center><img src="GenEstLogoExample1.jpg" height = 
                              "500"></center>'),
        br()
      ),

    # Data Input tab: input and view each of the data tables

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
              )
            ),
            mainPanel(
              HTML('<b><big><center>View data:</center></big></b>'),
              br(),
              tabsetPanel(id = "LoadedDataViz",
                tabPanel("Search Efficiency", br(), br(), 
                          dataTableOutput("SEdata")),
                tabPanel("Carcass Persistence", br(), br(), 
                          dataTableOutput("CPin")),
                tabPanel("Search Schedule", br(), br(), 
                          dataTableOutput("SSin")),
                tabPanel("Carcass Observations", br(), br(), 
                          dataTableOutput("COin"))
              )
            )
          )
      ),


    # Analyses tab: set up and run the analyses 

      tabPanel("Analyses",
        tabsetPanel(

          # General Inputs tab

            tabPanel("General Inputs",
              br(), br(),
 
              # side bar of general inputs 
 
                sidebarPanel(width = 3, 
                    numericInput("Niterations", "Number of iterations:", 
                           value = 1000, min = 1, max = 10000, step = 1),
                    numericInput("CL", "Confidence Level:", 
                           value = 0.9, min = 0, max = 1, step = 0.001)
              ),

              # main panel is empty

                mainPanel(
                  br()
                )
            ),

          # Search Efficiency tab

            tabPanel("Search Efficiency",
              br(), br(), 

              # side bar of model inputs

                sidebarPanel(width = 3, 
                           selectizeInput("SEobscols", 
                               "Choose observation columns (in order):",
                               c("No data input yet"), multiple = T),
                           selectizeInput("SEsizeclasscol", 
                               "Choose size class column (optional):", 
                               c("No data input yet"), multiple = T, 
                               options = list(maxItems = 1)),
                           selectizeInput("SEvars", 
                             "Choose predictor variables (optional, max: 2):", 
                             c("No data input yet"), multiple = T, 
                             options = list(maxItems = 2)),
                           selectizeInput("fixKchoice", "Use fixed k?",
                               c(FALSE, TRUE), multiple = F),
                           numericInput("fixKvalchoice", 
                               "Value for fixed k:", value = NULL, 
                                min = 0, max = 1, step = 0.001),
                           actionButton("SEmodrun", "Run SE Model")
                ),

              # main panel is a tabset panel

                mainPanel(
                  tabsetPanel(id = "SE_Analysis",

                    # Data tab shows data being modeled

                      tabPanel("Data", br(), br(), 
                               dataTableOutput("selected_SE")
                      ),

                    # Model Table tab shows AIC table for a given size class

                      tabPanel("Model Table", br(), 
                           selectizeInput("SEaicsizeclass", 
                                      "Choose size class for AIC table:", 
                                      "Model not yet run", multiple = F),
                           br(), br(), 
                           dataTableOutput("SEaictable") 
                      ),

                    # Figure tab produces a figure for a given size class and 
                    #  selected model

                      tabPanel("Figure", br(),
                           selectizeInput("SEfigsizeclass", 
                                      "Choose size class for SE figure:", 
                                      "Model not yet run", multiple = F ),
                           selectizeInput("SEfigmodel", 
                                      "Choose model for SE figure:", 
                                      "Model not yet run", multiple = F ),
                           br(), br(), 
                           plotOutput("SEfig", width = "800px", 
                                       height = "800px")
                      ),

                    # Model Selection tab is used to select a model for each
                    #  size class.

                      tabPanel("Model Selection", 
                           br(), 
                           htmlOutput("SEmodselectinputs")
                      ) 
                  )
                )
            ),


          # Carcass Persistence tab

            tabPanel("Carcass Persistence",
              br(), br(), 

              # side bar of model inputs

                sidebarPanel(width = 3,
                  selectizeInput("CPltp", 
                     "Choose last time present observation column:",
                     c("No data input yet"), multiple = T, 
                     options = list(maxItems = 1)),
                  selectizeInput("CPfta", 
                     "Choose first time absent observation column:",
                     c("No data input yet"), multiple = T, 
                     options = list(maxItems = 1)),
                  selectizeInput("CPsizeclasscol", 
                     "Choose size class column (optional):",
                     c("No data input yet"), multiple = T, 
                     options = list(maxItems = 1)),
                  selectizeInput("CPvars", 
                     "Choose predictor variables (optional, max: 2):", 
                     c("No data input yet"), multiple = T, 
                     options = list(maxItems = 2)),
                  actionButton("CPmodrun", "Run CP Model")
                ),

              # main panel is a tabset panel

                mainPanel(
                  tabsetPanel(id = "CP_Analysis",

                    # Data tab shows data being modeled
 
                      tabPanel("Data", br(), br(), 
                               dataTableOutput("selected_CP")
                      ),

                    # Model Table tab shows AIC table for a given size class

                      tabPanel("Model Table",  br(),
                           selectizeInput("CPaicsizeclass", 
                                          "Choose size class for AIC table:", 
                                          "Model not yet run", multiple = F ),
                           br(), br(), 
                           dataTableOutput("CPaictable") 
                      ),

                    # Figure tab produces a figure for a given size class and 
                    #  selected model

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
                           br(), br(), 
                           plotOutput("CPfig", width = "800px", 
                                   height = "1000px")
                      ),

                    # Model Selection tab is used to select a model for each
                    #  size class. 

                      tabPanel("Model Selection", br(), 
                           htmlOutput("CPmodselectinputs")
                      )
                  )
                )
            ),

          # Detection Probability tab

            tabPanel("Detection Probability",
              br(), br(), 

              # side bar of model run button and search schedule translation

                sidebarPanel(width = 3,
                       actionButton("grun", "Estimate Detection Probability"), 
                       br(), br(), tableOutput("SStable")
                ),

              # main panel displays the detection probability table

                mainPanel(
                  br(), 
                  dataTableOutput("gtable"),
                  br()
                )
            ),


          # Fatality Estimation tab

            tabPanel("Fatality Estimation",
              br(), br(), 

              # side bar of model inputs

                sidebarPanel(width = 3, 
                  selectizeInput("COunitcol", "Choose unit column:",
                         c("No data input yet"), multiple = T, 
                         options = list(maxItems = 1)),
                  selectizeInput("COdfcol", 
                         "Choose date found column:",
                         c("No data input yet"), multiple = T, 
                         options = list(maxItems = 1)),
                  selectizeInput("COsizeclasscol", 
                         "Choose size class column (optional):", 
                         c("No data input yet"), multiple = T, 
                         options = list(maxItems = 1)),
                  selectizeInput("COsplitcol", 
                         "Choose split column (optional):", 
                         c("No data input yet"), multiple = T, 
                         options = list(maxItems = 1)),
                  numericInput("ffs", "Fraction of units or area surveyed",
                         value = 1.0, min = 0, max = 1, step = 0.001),
                  actionButton("Mrun", "Estimate Total Carcasses")
                ),

              # main panel shows output table and figure

                mainPanel(
                  tabsetPanel(id = "M_Analysis",

                    # Data tab shows data being modeled
 
                      tabPanel("Data",   
                           br(), br(), dataTableOutput("selected_CO")

                      ),

                    # Table tab shows the final fatality estimation table

                      tabPanel("Table",  
                           br(), br(),
                           tableOutput("Mhattab")
                      ),

                    # Figure tab shows the final fatality estimation figure

                      tabPanel("Figure",  br(),
                           br(), br(),
                           plotOutput("Mhatfig", width = "800px", 
                                      height = "800px")
                      )
                  )
                )

            )
        )
      ),

          # About tab: details about GenEst

            tabPanel("About",
              fluidRow(
                column(6, offset = 3,
                       HTML('<img src = "GenEstLogoExample1.jpg" 
                              height = "400">'),
                       br(), 
                       br(),
                       HTML('<b>Authors:</b>  
                         Daniel Dalthorp
                           <a href = "http://www.USGS.gov">(USGS)</a>,
                         Juniper Simonis
                           <a href = 
                           "http://www.dapperstats.com">(DAPPER Stats)</a>,
                         Lisa Madsen
                           <a href = "http://www.OSU.edu">(OSU)</a>,
                         Paul Rabie 
                           <a href = "http://www.west-inc.com">(WEST)</a>, 
                         and
                         Manuela Huso
                           <a href = "http://www.USGS.gov">(USGS)</a>'),
                       br(),
                       br(),
                       HTML('GenEst is a tool for estimating bird and bat 
                             fatalities at renewable power facilities.'),
                       br(),
                       br(),
                       HTML('GenEst is currently in development
                             and should be considered provisional.'),
                       br(),
                       br(),
                       textOutput("version_info"),
                       br(),
                       br(),
                       HTML('The development of GenEst is being supported by 
                         BatConservation International, The US Bureau of Land
                         Management, The US Geological Survey, WEST, and 
                         Oregon State University.'),
                       br(),
                       br(),
                       HTML('GenEst is provided under GNU GPL version 3 (and 
                              any later versions).')
                )
              )
            )
  )


