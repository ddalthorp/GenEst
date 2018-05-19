#' Create the UI
#' @param request request as required for bookmarking
#' @return UI
#' @export
#'
ui <- function(request) {

navbarPage(

div(a(href = "https://github.com/ddalthorp/GenEst",
      img(src = "GenEst.png", style = "margin-top: -8px;", height = 40)
    )
),
            
tabPanel("Home",
  br(),
  HTML('<center><img src = "Logo.jpg" height = "500"></center>'), br()
),
  
tabPanel("Data Input",
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput("file_SE", "Search Efficiency Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ), 
      fileInput("file_CP", "Carcass Persistence Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ), 
      fileInput("file_SS", "Search Schedule Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ), 
      fileInput("file_DWP", "Density Weighted Proportion Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      ), 
      fileInput("file_CO", "Carcass Observation Data File",
        accept = c("text/csv", "text/comma-separated-values", ".csv")
      )
    ), 
    mainPanel(
      tabsetPanel(id = "LoadedDataViz",
        tabPanel("Search Efficiency", br(), dataTableOutput("data_SE")),
        tabPanel("Carcass Persistence", br(), dataTableOutput("data_CP")),
        tabPanel("Search Schedule", br(), dataTableOutput("data_SS")),
        tabPanel("Density Weighted Proportion", br(), 
          dataTableOutput("data_DWP")
        ),
        tabPanel("Carcass Observations", br(), dataTableOutput("data_CO"))
      )
    )
  )
),
tabPanel("Analyses",
  tabsetPanel(
    tabPanel("General Inputs", br(), br(),
      sidebarPanel(width = 3,
        numericInput("n", "Number of Iterations:", value = 1000, 
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
        selectizeInput("obsCols_SE", "Observations:", c("No data input yet"), 
          multiple = TRUE
        ),
        selectizeInput("preds_SE", "Predictor Variables:", 
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
        conditionalPanel(condition = "input.obsCols_SE != null",
          br(), 
          actionButton("runMod_SE", "Run Model")          
        ),
        conditionalPanel(condition = "input.runMod_SE > 0", 
          br(), br(),
          HTML("<big><strong><u> Table & Figure Selection:
          </u></strong></big>"), 
          br(), br(), 
          selectizeInput("tabfig_sizeclassSE",  
            "Size Class:", " ", multiple = FALSE
          ),  
          selectizeInput("tabfig_SEp", "p Model:", " ", multiple = FALSE), 
          selectizeInput("tabfig_SEk", "k Model:", " ", multiple = FALSE)
        )
      ),
      mainPanel(
        tabsetPanel(id = "analyses_SE",
          tabPanel("Selected Data", br(), br(), dataTableOutput("selected_SE")
          ),
          tabPanel("Figures", br(), plotOutput("fig_SE")),
          tabPanel("Model Tables", br(), br(), dataTableOutput("modTab_SE")
          ),
          tabPanel("Model Comparison", br(), br(), 
            dataTableOutput("AICcTab_SE")
          ),
          tabPanel("Model Selection", br(), htmlOutput("modelMenu_SE"))
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
        selectizeInput("preds_CP", "Predictor Variables:", 
          c("No data input yet"), multiple = TRUE
        ),
        checkboxGroupInput("dists", label = "Distributions to Include",
          choices = CPdistOptions(), 
          selected = c("exponential", "weibull", "lognormal", "loglogistic"),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.ltp != null & input.fta != null",
          br(),
          actionButton("runMod_CP", "Run Model")
        ),
        conditionalPanel(condition = "input.runMod_CP > 0", 
          br(), br(), 
          HTML("<big><strong><u> Table & Figure Selection: 
            </u></strong></big>"
          ), br(), br(),
          selectizeInput("tabfig_sizeclassCP", "Size Class:", 
            " ", multiple = FALSE
          ),
          selectizeInput("tabfig_CPd", "Distribution:",
            " ", multiple = FALSE
          ), 
          selectizeInput("tabfig_CPl", "Location Model:",
            " ", multiple = FALSE
          ), 
          selectizeInput("tabfig_CPs", "Scale Model:", " ", multiple = FALSE)        
        )
      ),
      mainPanel(
        tabsetPanel(id = "analyses_CP",
          tabPanel("Selected Data", br(), br(),
            dataTableOutput("selected_CP")),
          tabPanel("Figures", br(), plotOutput("fig_CP")),
          tabPanel("Model Tables", br(), br(), dataTableOutput("modTab_CP")
          ),
          tabPanel("Model Comparison", br(), br(), 
            dataTableOutput("AICcTab_CP")
          ),
          tabPanel("Model Selection", br(), htmlOutput("modelMenu_CP"))
        )
      )
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
            "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
             output.sizeclassesSE == output.sizeclassesCP",
          br(), 
          actionButton("runModM", "Estimate")
        )
      ),
      mainPanel(br())
    ),
    tabPanel("Detection Probability", br(), br(),
      sidebarPanel(width = 3, 
        HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
        br(), br(),
        HTML("<strong><u> Search Schedule Data: </u></strong>"),
        br(), br(),
        selectizeInput("dateSearchedCol_g", 
          "Date Searched Column in Search Schedule Data (if applicable):",  
          c("No data input yet"), multiple = FALSE
        ),
        actionButton("useSSdata", "Create Schedule"),
        br(), br(),
        HTML("<strong><u> Generic Search Schedule Inputs: </u></strong>"),
        br(), br(),
        numericInput("gSearchInterval", "Search Interval (days):", 
          value = 7, min = 1, max = 400, step = 1),
        numericInput("gSearchMax", "Final Seach (day):", 
          value = 364, min = 1, max = 1000, step = 1),
        actionButton("useSSinputs", "Create Schedule"),
        conditionalPanel(
          condition = "output.kFillNeed == 'yes'",
          br(), br(),
          numericInput("kFill_g", "Assumed k:", value = 0.5, 
            min = 0, max = 1, step = 0.001
          )
        ),
        conditionalPanel(
          condition = 
            "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
             output.sizeclassesSE == output.sizeclassesCP",
          br(), br(),
          actionButton("runMod_g", "Estimate")
        ),
        conditionalPanel(condition = "input.runMod_g > 0", 
          br(), br(), 
          HTML("<big><strong><u> Table & Figure Selection: 
            </u></strong></big>"
          ), br(), br(),
          selectizeInput("tabfig_sizeclassg", "Size Class:", 
            " ", multiple = FALSE
          )
        )
      ),
      mainPanel(
        tabsetPanel(id = "analyses_g",
          tabPanel("Schedule",         
            br(), br(),
            HTML("<big><strong><u> Search Schedule: </u></strong></big>"),
            br(), br(), 
            box(textOutput("SStext"))
          ),
          tabPanel("Table", br(), br(), dataTableOutput("tab_g")),
          tabPanel("Figure", br(), plotOutput("fig_g"))
        )
      )
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
        Manuela Huso
          <a href = 'http://www.USGS.gov'>(USGS)</a>,
        Paul Rabie 
          <a href = 'http://www.west-inc.com'>(WEST)</a>, 
        Jared Studyvin
          <a href = 'http://www.west-inc.com'>(WEST)</a>, 
        Franzi Korner-Nievergelt
          <a href = 'http://www.oikostat.ch/'>(oikostat)</a>, 
        and Robert Wolpert
          <a href = 'http://www2.stat.duke.edu/~rlw/'>(Duke)</a>"),
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
      HTML("<a href='https://www.blm.gov/'>
         <img src = 'blm.jpg' height = '60'></a>
         <a href='https://www.usgs.gov/'>
         <img src = 'usgs.png' height = '60'></a>
         <a href='https://www.nrel.gov/'>
         <img src = 'nrel.jpg' height = '60'> </a>
         <a href='http://www.westconsultants.com/'>
         <img src = 'west.png' height = '60'></a>
         <a href='http://www.batcon.org/'>
         <img src = 'bci.jpg' height = '60'></a>
         <a href='https://awwi.org/'>
         <img src = 'awwi.png' height = '60'></a>
         <a href='http://www.avangridrenewables.us/'>
         <img src = 'avangrid.png' height = '60'></a>
         <a href='http://www.dapperstats.com'>
         <img src = 'dapper.png' height = '60'></a>
         <a href='http://www.oikostat.ch/'>
         <img src = 'oikostat.jpg' height = '60'> </a>
         <a href='https://www.oregonstate.edu/'>
         <img src = 'osu.jpg' height = '60'> </a>
         <a href='https://www.duke.edu/'>
         <img src = 'duke.png' height = '60'></a>"
       )
    )
  )
),
collapsible = TRUE,
windowTitle = "GenEst"

)

}
