#' @title Create the UI
#'
#' @description Render the active UI for the GenEst application
#'
#' @param request request as required for bookmarking
#'
#' @return UI
#'
#' @export
#'
ui <- function(request){

navbarPage(

div(a(href = "https://github.com/ddalthorp/GenEst",
      img(src = "GenEst.png", style = "margin-top: -8px;", height = 40)
    )
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
        numericInput("CL", "Confidence Level:", value = 0.95, min = 0, 
          max = 1, step = 0.001
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
        conditionalPanel(condition = "input.obsCols_SE == null",
          br(), 
          HTML("<center><em>Select observation columns to run 
            model</center></em>"
          )          
        ),
        conditionalPanel(condition = "input.obsCols_SE != null",
          br(), 
          actionButton("runMod_SE", "Run Model")          
        ),
        conditionalPanel(condition = "output.SEModDone == 'OK'", 
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
          tabPanel("Selected Data", br(), 
            conditionalPanel(condition = "input.obsCols_SE == null",
              HTML("<em>Select observation columns to view data</em>")
            ), br(), 
            dataTableOutput("selected_SE")
          ),
          tabPanel("Figures", br(), 
            conditionalPanel(condition = "output.fig_SE == null",
              HTML("<em>Run model to view figures</em>")
            ),
            conditionalPanel(condition = "output.SEModDone == 'OK'",
              plotOutput("fig_SE", inline = TRUE), br(), br(),
              downloadButton("downloadSEfig", "Download")
            )
          ),
          tabPanel("Estimates", br(),  
            conditionalPanel(condition = "output.modTab_SE == null",
              HTML("<em>Run model to view model estimates</em>")
            ),
            conditionalPanel(condition = "output.SEModDone == 'OK'",
                textOutput("sizeclass_SE1"), br(), 
              dataTableOutput("modTab_SE"), br(),
              downloadButton("downloadSEest", "Download")
            )
          ),
          tabPanel("Model Comparison", br(), 
            conditionalPanel(condition = "output.AICcTab_SE == null",
              HTML("<em>Run models to view model comparison</em>")
            ),
            conditionalPanel(condition = "output.SEModDone == 'OK'",
              textOutput("sizeclass_SE2"), br(), 
              dataTableOutput("AICcTab_SE"), br(),
              downloadButton("downloadSEAICc", "Download")
            )
          ),
          tabPanel("Model Selection", br(), 
            conditionalPanel(condition = "output.modelMenu_SE == null",
              HTML("<em>Run models to select models</em>")
            ),
            htmlOutput("modelMenu_SE")
            )
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
          condition = "input.ltp == null | input.fta == null",
          br(), 
          HTML("<center><em>Select observation columns to run 
            model</center></em>"
          )          
        ),
        conditionalPanel(
          condition = "input.ltp != null & input.fta != null",
          br(),
          actionButton("runMod_CP", "Run Model")
        ),
        conditionalPanel(condition = "output.CPModDone == 'OK'", 
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
          tabPanel("Selected Data", br(),
            conditionalPanel(
              condition = "input.ltp == null | input.fta == null",
              HTML("<em>Select observation columns to view data</em>")
            ), br(), 
            dataTableOutput("selected_CP")),
          tabPanel("Figures", br(),
            conditionalPanel(condition = "output.fig_CP == null",
              HTML("<em>Run model to view figures</em>")
            ),
            conditionalPanel(condition = "output.CPModDone == 'OK'",
              plotOutput("fig_CP", inline = TRUE), br(), br(),
              downloadButton("downloadCPfig", "Download")
            )
          ),
          tabPanel("Estimates", br(), 
            conditionalPanel(condition = "output.modTab_CP == null",
              HTML("<em>Run model to view model estimates</em>")
            ), 
            conditionalPanel(condition = "output.CPModDone == 'OK'",
              textOutput("sizeclass_CP1"), br(), 
              dataTableOutput("modTab_CP"), br(),
              downloadButton("downloadCPest", "Download")
            )
          ),
          tabPanel("Model Comparison", br(), 
            conditionalPanel(condition = "output.AICcTab_CP == null",
              HTML("<em>Run models to view model comparison</em>")
            ), 
            conditionalPanel(condition = "output.CPModDone == 'OK'",
              textOutput("sizeclass_CP2"), br(), 
              dataTableOutput("AICcTab_CP"), br(),
              downloadButton("downloadCPAICc", "Download")
            )
          ),
          tabPanel("Model Selection", br(), 
            conditionalPanel(condition = "output.modelMenu_CP == null",
              HTML("<em>Run models to select models</em>")
            ),
            htmlOutput("modelMenu_CP")
          )
        )
      )
    ),
    tabPanel("Mortality Estimation", br(), br(),
      sidebarPanel(width = 3, 
        HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
        br(), br(),
        numericInput("frac", "Fraction of Facility Surveyed:", value = 1.0, 
          min = 0.01, max = 1.0, step = 0.01
        ),
        selectizeInput("dateFoundCol", "Date Found:", c("No data input yet"), 
          multiple = FALSE
        ),
        conditionalPanel(
          condition = "output.kFillNeed == 'yes'",
          numericInput("kFill", "Assumed k:", value = 0.5, 
            min = 0, max = 1, step = 0.001
          )
        ),
        conditionalPanel(
          condition = "output.DWPNeed == 'yes'",
          selectizeInput("DWPCol", "DWP Column", c("No data input yet"), 
            multiple = FALSE
          )
        ),
        conditionalPanel(
          condition = 
            "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
             output.sizeclassesSE != output.sizeclassesCP",
          br(), 
          HTML("<center><em>Select SE and CP models fit to matching size
            classes to run model</center></em>"
          )
        ),
        conditionalPanel(
          condition = 
            "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
             output.sizeclassesSE == output.sizeclassesCP",
          br(), 
          actionButton("runMod_M", "Estimate")
        ),
        conditionalPanel(
          condition = "output.MModDone == 'OK'",

          br(), br(), 
          HTML("<big><strong><u> Splitting Mortality: 
            </u></strong></big>"
          ), br(), br(), 
          HTML("<em>Max. two splits, of which only one can be 
            search-based</em>"
          ), br(), br(),
          selectizeInput("split_SS", "Search Split:", 
            " ", multiple = TRUE, options = list(maxItems = 1)
          ),
          selectizeInput("split_CO", "Carcass Splits:", 
            " ", multiple = TRUE, options = list(maxItems = 2)
          ),
          br(),
          actionButton("splitM", "Split Estimate")
        )
      ),
      mainPanel(
        tabsetPanel(id = "analyses_M",
          tabPanel("Figure", br(), 
            conditionalPanel(condition = "output.fig_M == null",
              HTML("<em>Run estimate to view figure</em>")
            ), 
            conditionalPanel(condition = "output.MModDone == 'OK'",
              plotOutput("fig_M", inline = TRUE), br(), br(),
              downloadButton("downloadMfig", "Download")
            )
          ),
          tabPanel("Summary", br(), 
            conditionalPanel(condition = "output.table_M == null",
              HTML("<em>Run estimate to view summary</em>")
            ), 
            conditionalPanel(condition = "output.MModDone == 'OK'",
              br(), dataTableOutput("table_M"), br(),
              downloadButton("downloadMtab", "Download")
            )
          )
        )
      )
    ),
    tabPanel("Detection Probability", br(), br(),
      sidebarPanel(width = 3, 
        HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
        br(), br(),
        HTML("<strong><u> Search Schedule Data: </u></strong>"),
        conditionalPanel(    
          condition = "output.data_SS == null",
          br(), 
          HTML("<center><em>Input search schedule data file</center></em>")
        ),
        conditionalPanel(    
          condition = "output.data_SS != null",
          br(), 
          actionButton("useSSdata", "Create Schedule")
        ),
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
            "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
             output.sizeclassesSE != output.sizeclassesCP",
          br(), 
          HTML("<center><em>Select SE and CP models fit to matching size
            classes to run model</center></em>"
          )
        ),
        conditionalPanel(
          condition = 
            "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
             output.sizeclassesSE == output.sizeclassesCP",
          br(), br(),
          actionButton("runMod_g", "Estimate")
        ),
        conditionalPanel(condition = "output.gModDone == 'OK'", 
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
          tabPanel("Figure", br(), 
            conditionalPanel(condition = "output.fig_g == null",
              HTML("<em>Run estimate to view figure</em>")
            ), 
            conditionalPanel(condition = "output.gModDone == 'OK'",
              plotOutput("fig_g"), br(), br(),
              downloadButton("downloadgfig", "Download")
            )
          ),
          tabPanel("Summary", br(), 
            conditionalPanel(condition = "output.table_g == null",
              HTML("<em>Run estimate to view summary</em>")
            ), 
            conditionalPanel(condition = "output.gModDone == 'OK'",
              br(), dataTableOutput("table_g"), br(),
              downloadButton("downloadgtab", "Download")
            )
          )
        )
      )
    )
  )
),
tabPanel("About",
  fluidRow(
    column(5, offset = 2,
      br(),
      HTML('<center><img src = "Logo.jpg" width = "600"></center>'), 

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
      HTML("GenEst is a tool for estimating mortalities from efficiency, 
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
