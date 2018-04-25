navbarPage(title = div(img(src = "GenEst.png", style = "margin-top: -8px;", 
                         height = 40
                      )    
                   ),

tabPanel("Home", br(),
  HTML('<center><img src = "Logo.jpg" height = "500"></center>'), br()
),
tabPanel("Data Input",
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput("fileSE", "Search Efficiency Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ), 
      fileInput("fileCP", "Carcass Persistence Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ), 
      fileInput("fileSS", "Search Schedule Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ), 
      fileInput("fileDWP", "Density Weighted Proportion Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ), 
      fileInput("fileCO", "Carcass Observation Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      )
    ), 
    mainPanel(
      tabsetPanel(id = "LoadedDataViz",
        tabPanel("Search Efficiency", br(), DT::dataTableOutput("dataSE")),
        tabPanel("Carcass Persistence", br(), DT::dataTableOutput("dataCP")),
        tabPanel("Search Schedule", br(), DT::dataTableOutput("dataSS")),
        tabPanel("Density Weighted Proportion", br(), 
          DT::dataTableOutput("dataDWP")
        ),
        tabPanel("Carcass Observations", br(), DT::dataTableOutput("dataCO"))
      )
    )
  )
),
tabPanel("Analyses",
  tabsetPanel(
    tabPanel("General Inputs", br(), br(),
      sidebarPanel(width = 3,
        numericInput("niterations", "Number of Iterations:", value = 1000, 
          min = 1, max = 10000, step = 1
        ),
        numericInput("CL", "Confidence Level:", value = 0.9, min = 0, max = 1,
          step = 0.001
        ),
        selectizeInput("sizeclassCol", "Size Class Column (optional):", 
          c("No data input yet"), multiple = TRUE, 
          options = list(maxItems = 1)
        )
      )
    ),
    tabPanel("Searcher Efficiency", br(), br(),
      sidebarPanel(width = 3,
        HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
        br(), br(),
        selectizeInput("obsColsSE", "Observations:", c("No data input yet"), 
          multiple = TRUE
        ),
        selectizeInput("predsSE", "Predictor Variables:", 
          c("No data input yet"), multiple = TRUE
        ),
        radioButtons("kFixedChoice", "Fix k?",
          choices = list("No" = 0, "Yes" = 1), selected = 0
        ),
        conditionalPanel(condition = "input.kFixedChoice == 1",
          numericInput("kFixed", "Value for fixed k:", value = 0.5, 
            min = 0, max = 1, step = 0.001
          )
        ),
        conditionalPanel(condition = "input.obsColsSE != null",
          br(), 
          actionButton("runModSE", "Run Searcher Efficiency Model")          
        ),
        conditionalPanel(condition = "input.runModSE > 0", 
          br(), br(),
          HTML("<big><strong><u> Table & Figure Selection:
          </u></strong></big>"), 
          br(), br(), 
          selectizeInput("tabfigSizeClassSE",  
            "Size Class:", " ", multiple = FALSE
          ),  
          selectizeInput("tabfigSEp", "p Model:",
            " ", multiple = FALSE
          ), 
          selectizeInput("tabfigSEk", "k Model:",
            " ", multiple = FALSE
          )
        )
      ),
      mainPanel(
        tabsetPanel(id = "analysesSE",
          tabPanel("Selected Data", br(), br(), 
            DT::dataTableOutput("selectedSE")
          ),
          tabPanel("Figures", plotOutput("figSE")
          ),
          tabPanel("Model Tables", br(),
            br(),  DT::dataTableOutput("modTabSE")
          ),
          tabPanel("Model Comparison Tables", br(),
            br(), DT::dataTableOutput("AICcTabSE")
          ),
          tabPanel("Model Selection", br(), htmlOutput("modelMenuSE"))
        )
      )
    ),
    tabPanel("Carcass Persistence", br(), br(),
      sidebarPanel(width = 3,
        HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
        br(), br(),
        selectizeInput("ltp", "Last Time Present:", c("No data input yet"), 
          multiple = TRUE, options = list(maxItems = 1)
        ),
        selectizeInput("fta", "First Time Absent:", c("No data input yet"),
          multiple = TRUE, options = list(maxItems = 1)
        ),
        selectizeInput("predsCP", "Predictor Variables:", 
          c("No data input yet"), multiple = TRUE
        ),
        checkboxGroupInput("dists", label = "Distributions to Include",
          choices = list("exponential" = "exponential", "weibull" = "weibull",
                      "lognormal" = "lognormal", "loglogistic" = "loglogistic"
                    ), 
          selected = c("exponential", "weibull", "lognormal", "loglogistic"),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.ltp != null & input.fta != null",
          br(),
          actionButton("runModCP", "Run Carcass Persistence Model")
        ),
        conditionalPanel(condition = "input.runModCP > 0", 
          br(), br(), 
          HTML("<big><strong><u> Table & Figure Selection: 
            </u></strong></big>"), 
          br(), br(),
          selectizeInput("tabfigSizeClassCP", "Size Class:", 
            " ", multiple = FALSE
          ),
          selectizeInput("tabfigCPdist", "Distribution:",
            " ", multiple = FALSE
          ), 
          selectizeInput("tabfigCPl", "Location Model:",
            " ", multiple = FALSE
          ), 
          selectizeInput("tabfigCPs", "Scale Model:", " ", multiple = FALSE)        
        )
      ),
      mainPanel(
        tabsetPanel(id = "analysesCP",
          tabPanel("Selected Data", br(), br(),
            DT::dataTableOutput("selectedCP")),
          tabPanel("Figures", plotOutput("figCP")
          ),
          tabPanel("Model Tables", br(),
            br(), DT::dataTableOutput("modTabCP")
          ),
          tabPanel("Model Comparison Tables", br(),
            br(), DT::dataTableOutput("AICcTabCP")
          ),
          tabPanel("Model Selection", br(), htmlOutput("modelMenuCP"))
        )
      )
    ),
    tabPanel("Detection Probability", br(), br(),
      sidebarPanel(width = 3, 
        HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
        br(), br(),
        numericInput("gSearchInterval", "Standard Search Interval (days):", 
          value = 7, min = 1, max = 400, step = 1),
        conditionalPanel(
          condition = "output.kFillNeed == 'yes'",
          numericInput("kFill", "Assumed k:", value = 0.5, 
            min = 0, max = 1, step = 0.001
          )
        ),
        conditionalPanel(
          condition = 
            "input.modelChoicesSE1 != null & input.modelChoicesCP1 != null & 
             output.sizeclassesSE == output.sizeclassesCP",
          br(), 
          actionButton("runModg", "Estimate Detection Probability")
        )
      ),
      mainPanel(br())
    ),
    tabPanel("Fatality Estimation", br(), br(),
      sidebarPanel(width = 3, 
        HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
        br(), br(),
        selectizeInput("unitCol", "Units:", c("No data input yet"), 
          multiple = FALSE
        ),
        selectizeInput("dateFoundCol", "Date Found:", c("No data input yet"), 
          multiple = FALSE
        ),
        selectizeInput("dateSearchedCol", "Date Searched:",  
          c("No data input yet"), multiple = FALSE
        ),
        conditionalPanel(
          condition = "output.kFillNeed == 'yes'",
          numericInput("kFill", "Assumed k:", value = 0.5, 
            min = 0, max = 1, step = 0.001
          )
        ),
        conditionalPanel(
          condition = 
            "input.modelChoicesSE1 != null & input.modelChoicesCP1 != null & 
             output.sizeclassesSE == output.sizeclassesCP",
          br(), 
          actionButton("runModM", "Estimate Fatalities")
        )
      ),
      mainPanel(br())
    )
  )
),
tabPanel("About",
  fluidRow(
    column(5, offset = 2,
      br(), br(), 
      HTML("<b>Authors:</b>  
        Juniper Simonis 
          <a href = 'http://www.dapperstats.com'>(DAPPER Stats)</a>,
        Daniel Dalthorp
          <a href = 'http://www.USGS.gov'>(USGS)</a>,
        Lisa Madsen
          <a href = 'http://www.oregonstate.edu'>(OSU)</a>,
        Paul Rabie 
          <a href = 'http://www.west-inc.com'>(WEST)</a>, 
        Jared Studyvin
          <a href = 'http://www.west-inc.com'>(WEST)</a>, 
        Robert Wolpert
          <a href = 'http://http://www2.stat.duke.edu/~rlw/'>(Duke)</a>, 
        Franzi Korner-Nievergelt
          <a href = 'http://http://www.oikostat.ch/'>(oikostat)</a>, 
        and Manuela Huso
          <a href = 'http://www.USGS.gov'>(USGS)</a>"),
      br(), br(),
      HTML("GenEst is a tool for estimating fatalities from efficiency, 
        persistence, and carcass data."
      ),
      br(), br(),
      HTML("GenEst is currently in development and should be considered
        provisional."
      ),
      br(), br(),
      textOutput("versionInfo"),
      br(), 
      HTML("The development of GenEst is being supported by 
        <a href = 'https://www.blm.gov/'>(The US Bureau of Land 
          Management)</a>,
        <a href = 'https://www.usgs.gov/'>(The US Geological Survey)</a>,
        <a href = 'https://www.nrel.gov/'>(National Renewable Energy 
          Laboratory)</a>, 
        <a href = 'http://www.westconsultants.com/'>(WEST)</a>, 
        <a href = 'http://www.batcon.org/'>(Bat Conservation
          International)</a>,
        <a href = 'https://awwi.org/'>(American Wind Wildlife Institute)</a>, 
        <a href = 'http://www.avangridrenewables.us/'>(Avangrid 
           Renewables)</a>, and 
        <a href = 'https://oregonstate.edu/'>(Oregon State University)</a>."),
      br(), br(),
      HTML("GenEst is provided under GNU GPL v3 (and later versions)."),
      br(), br(), br(), br(),
      HTML("<img src = 'blm.jpg' height = '60'>"),
      HTML("<img src = 'usgs.png' height = '60'>"),
      HTML("<img src = 'nrel.jpg' height = '60'>"),
      HTML("<img src = 'west.png' height = '60'>"),
      HTML("<img src = 'bci.jpg' height = '60'>"),
      HTML("<img src = 'awwi.png' height = '60'>"),
      HTML("<img src = 'avangrid.png' height = '60'>"),
      HTML("<img src = 'dapper.png' height = '60'>"),
      HTML("<img src = 'oikostat.jpg' height = '60'>"),
      HTML("<img src = 'osu.jpg' height = '60'>"),
      HTML("<img src = 'duke.png' height = '60'>")
    )
  )
),
collapsible = TRUE,
windowTitle = "GenEst"
)


