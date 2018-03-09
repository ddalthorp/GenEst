navbarPage("GenEst",

tabPanel("Home", br(),
  HTML('<center><img src = "Logo.jpg" height = "500"></center>'), br()
),
tabPanel("Data Input",
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput("fileSE", "Search Efficiency Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")), 
      fileInput("fileCP", "Carcass Persistence Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")), 
      fileInput("fileSS", "Search Schedule Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")), 
      fileInput("fileCO", "Carcass Observation Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv"))
    ), 
    mainPanel(
      tabsetPanel(id = "LoadedDataViz",
        tabPanel("Search Efficiency", br(), DT::dataTableOutput("dataSE")),
        tabPanel("Carcass Persistence", br(), DT::dataTableOutput("dataCP")),
        tabPanel("Search Schedule", br(), DT::dataTableOutput("dataSS")),
        tabPanel("Carcass Observations", br(), DT::dataTableOutput("dataCO"))
      )
    )
  )
),
tabPanel("Analyses",
  tabsetPanel(
    tabPanel("General Inputs", br(), br(),
      sidebarPanel(width = 3,
        numericInput("n_iterations", "Number of Iterations:", value = 1000, 
          min = 1, max = 10000, step = 1
        ),
        numericInput("CL", "Confidence Level:", value = 0.9, min = 0, max = 1,
          step = 0.001
        ),
        selectizeInput("sizeclassCol", "Size Class Column (optional):", 
          c("No data input yet"), multiple = T, options = list(maxItems = 1)
        )
      )
    ),
    tabPanel("Searcher Efficiency", br(), br(),
      sidebarPanel(width = 3,
        selectizeInput("obsColsSE", "Observations:", c("No data input yet"), 
          multiple = T
        ),
        selectizeInput("predsSE", "Predictor Variables:", 
          c("No data input yet"), multiple = T
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
          actionButton("runModSE", "Run Searcher Efficiency Model"))
        ),
      mainPanel(
        tabsetPanel(id = "analysesSE",
          tabPanel("Selected Data", br(), br(), 
            DT::dataTableOutput("selectedSE")
          ),
          tabPanel("Figures", br(),
            selectizeInput("figSizeClassSE", width = "400px", 
              "Size Class:", "Model not yet run", multiple = F
            ),  
            selectizeInput("figModSE", width = "400px", "Model:",
              "Model not yet run", multiple = F
            ), 
            br(), plotOutput("figSE")
          ),
          tabPanel("Model Tables", br(),
            selectizeInput("modTabSizeClassSE", width = "400px", 
              "Size Class:", "Model not yet run", multiple = F
            ),  
            selectizeInput("modTabModSE", width = "400px", "Model:",
              "Model not yet run", multiple = F
            ), 
            br(),  DT::dataTableOutput("modTabSE")
          ),
          tabPanel("Model Comparison Tables", br(),
            selectizeInput("aicTabSizeClassSE", width = "400px", 
              "Size Class:", "Model not yet run", multiple = F), br(),  
              DT::dataTableOutput("AICcTabSE")
            ),
          tabPanel("Model Selection", br(), htmlOutput("modelMenuSE"))
        )
      )
    ),
    tabPanel("Carcass Persistence", br(), br(),
      sidebarPanel(width = 3,
        selectizeInput("ltp", "Last Time Present:", c("No data input yet"), 
          multiple = T, options = list(maxItems = 1)
        ),
        selectizeInput("fta", "First Time Absent:", c("No data input yet"),
          multiple = T, options = list(maxItems = 1)
        ),
        selectizeInput("predsCP", "Predictor Variables:", 
          c("No data input yet"), multiple = T
        ),
        conditionalPanel(
          condition = "input.ltp != null & input.fta != null",
          actionButton("runModCP", "Run Carcass Persistence Model")
        )
      ),
      mainPanel(
        tabsetPanel(id = "analysesCP",
          tabPanel("Selected Data", br(), br(),
            DT::dataTableOutput("selectedCP")),
          tabPanel("Figures", br()),
          tabPanel("Model Tables", br()),
          tabPanel("Model Comparison Tables", br()),
          tabPanel("Model Selection", br())
        )
      )
    ),
    tabPanel("Detection Probability", br(), br(),
      sidebarPanel(width = 3, 
        conditionalPanel(
          condition = "input.runModSE > 0 & input.runModCP > 0",
          actionButton("runModg", "Estimate Detection Probability")
        )
      ),
      mainPanel(br())
    ),
    tabPanel("Fatality Estimation", br(), br(),
      sidebarPanel(width = 3,
        selectizeInput("unitColCO", "Search Unit:", c("No data input yet"),
          multiple = T, options = list(maxItems = 1)
        ),
        selectizeInput("splitColCO", "Data Splits:", c("No data input yet"), 
          multiple = T, options = list(maxItems = 2)
        ),
        numericInput("fracSurveyedCO", "Fraction of Area or Units Surveyed:", 
          value = 1.0, min = 0, max = 1, step = 0.001
        ),
        conditionalPanel(
          condition = "input.runModg > 0 & input.unitColCO != null",
          actionButton("runMmod", "Estimate Total Carcasses"))
        ),
      mainPanel(
        tabsetPanel(id = "analysesM",
          tabPanel("Selected Data", br(), br(),
            DT::dataTableOutput("selectedCO")),
          tabPanel("Figures", br()),
          tabPanel("Model Tables", br())
        )
      )
    )
  )
),
tabPanel("About",
  fluidRow(
    column(6, offset = 3,
      HTML('<img src = "Logo.jpg" height = "400">'),
      br(), br(),
      HTML('<b>Authors:</b>  
        Juniper Simonis 
          <a href = "http://www.dapperstats.com">(DAPPER Stats)</a>,
        Daniel Dalthorp
          <a href = "http://www.USGS.gov">(USGS)</a>,
        Lisa Madsen
          <a href = "http://www.OSU.edu">(OSU)</a>,
        Paul Rabie 
          <a href = "http://www.west-inc.com">(WEST)</a>, 
        Jared Studyvin
          <a href = "http://www.west-inc.com">(WEST)</a>, 
        Robert Wolpert
          <a href = "http://http://www2.stat.duke.edu/~rlw/">(Duke)</a>, 
        Franzi Korner-Nievergelt
          <a href = "http://http://www.oikostat.ch/">(oikostat)</a>, 
        and Manuela Huso
          <a href = "http://www.USGS.gov">(USGS)</a>'),
      br(), br(),
      HTML('GenEst is a tool for estimating bird and bat fatalities at 
        renewable power facilities.'
      ),
      br(), br(),
      HTML('GenEst is currently in development and should be considered
        provisional.'
      ),
      br(), br(),
      textOutput("versionInfo"),
      br(), 
      HTML('The development of GenEst is being supported by Bat Conservation 
        International, The US Bureau of Land Management, The US Geological 
        Survey, WEST, and Oregon State University.'),
      br(), br(),
      HTML('GenEst is provided under GNU GPL v3 (and later versions).')
    )
  )
)

)


