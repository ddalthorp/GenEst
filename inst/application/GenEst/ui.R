navbarPage("GenEst",

tabPanel("Home", br(),
  HTML('<center><img src = "GenEstLogoExample1.jpg" 
                 height = "500"></center>'), br()),

tabPanel("Data Input",
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput("SE_file", "Search Efficiency Data File",
                accept = c("text/csv", "text/comma-separated-values", 
                           "text/plain", ".csv")), 
      fileInput("CP_file", "Carcass Persistence Data File",
                accept = c("text/csv", "text/comma-separated-values", 
                           "text/plain", ".csv")), 
      fileInput("SS_file", "Search Schedule Data File",
                accept = c("text/csv", "text/comma-separated-values", 
                           "text/plain", ".csv")), 
      fileInput("CO_file", "Carcass Observation Data File",
                accept = c("text/csv", "text/comma-separated-values", 
                           "text/plain", ".csv"))),
    mainPanel(
      tabsetPanel(id = "LoadedDataViz",
        tabPanel("Search Efficiency", DT::dataTableOutput("SE_data")),
        tabPanel("Carcass Persistence", DT::dataTableOutput("CP_data")),
        tabPanel("Search Schedule", DT::dataTableOutput("SS_data")),
        tabPanel("Carcass Observations", DT::dataTableOutput("CO_data")))))),

tabPanel("Analyses",
  tabsetPanel(

    tabPanel("General Inputs", br(), br(),
      sidebarPanel(width = 3,
        numericInput("n_iterations", "Number of Iterations:", 
                     value = 1000, min = 1, max = 10000, step = 1),
        numericInput("CL", "Confidence Level:", 
                     value = 0.9, min = 0, max = 1, step = 0.001),
        selectizeInput("sizeclass_col",
                       "Size Class Column (optional):", 
                       c("No data input yet"), multiple = T, 
                       options = list(maxItems = 1)))),

    tabPanel("Searcher Efficiency", br(), br(),
      sidebarPanel(width = 3,
        selectizeInput("SE_obs_cols", "Observations:",
                       c("No data input yet"), multiple = T),
        selectizeInput("SE_vars", "Predictor Variables:", 
                       c("No data input yet"), multiple = T),
        radioButtons("fix_k_choice", "Fix k?",
                     choices = list("No" = 0, "Yes" = 1), 
                     selected = 0),
        conditionalPanel(condition = "input.fix_k_choice == 1",
          numericInput("fixed_k", "Value for fixed k:", value = 0.5, 
                       min = 0, max = 1, step = 0.001)),
        conditionalPanel(condition = "input.SE_obs_cols != null",
          actionButton("SE_mod_run", "Run Searcher Efficiency Model"))),
      mainPanel(
        tabsetPanel(id = "SE_analyses",
          tabPanel("Selected Data", br(), br(), 
            DT::dataTableOutput("selected_SE")),
          tabPanel("Figures", br()),
          tabPanel("Model Tables", br(),
            selectizeInput("SE_MT_sc", width = "400px", "Size Class:",
                           "Model not yet run", multiple = F),  
            selectizeInput("SE_MT_mod", width = "400px", "Model:",
                           "Model not yet run", multiple = F), br(), br(),
            DT::dataTableOutput("SE_mod_tab")),
          tabPanel("Model Comparison Tables", br(),
            selectizeInput("SE_AICc_sc", width = "400px", "Size Class:",
                           "Model not yet run", multiple = F), br(), br(), 
            DT::dataTableOutput("SE_AICc_table")),
          tabPanel("Model Selection", br(),
            htmlOutput("SE_model_menu"))))),

    tabPanel("Carcass Persistence", br(), br(),
      sidebarPanel(width = 3,
        selectizeInput("CP_ltp", "Last Time Present:",
                       c("No data input yet"), multiple = T,
                       options = list(maxItems = 1)),
        selectizeInput("CP_fta", "First Time Absent:", 
                       c("No data input yet"), multiple = T,
                       options = list(maxItems = 1)),
        selectizeInput("CP_vars", "Predictor Variables:", 
                       c("No data input yet"), multiple = T),
        conditionalPanel(
          condition = "input.CP_ltp != null & input.CP_fta != null",
          actionButton("CP_mod_run", "Run Carcass Persistence Model"))),
      mainPanel(
        tabsetPanel(id = "CP_analyses",
          tabPanel("Selected Data", br(), br(),
            DT::dataTableOutput("selected_CP")),
          tabPanel("Figures", br()),
          tabPanel("Model Tables", br()),
          tabPanel("Model Comparison Tables", br()),
          tabPanel("Model Selection", br())))),

    tabPanel("Detection Probability", br(), br(),
      sidebarPanel(width = 3, 
        conditionalPanel(
          condition = "input.SE_mod_run > 0 & input.CP_mod_run > 0",
          actionButton("g_run", "Estimate Detection Probability"))),
      mainPanel(br())),

    tabPanel("Fatality Estimation", br(), br(),
      sidebarPanel(width = 3,
        selectizeInput("CO_unit_col", "Search Unit:",
                       c("No data input yet"), multiple = T,
                       options = list(maxItems = 1)),
        selectizeInput("CO_splits", "Data Splits:", 
                       c("No data input yet"), multiple = T,
                       options = list(maxItems = 2)),
        numericInput("CO_f_surveyed", "Fraction of Area or Units Surveyed:", 
                     value = 1.0, min = 0, max = 1, step = 0.001),
        conditionalPanel(
          condition = "input.g_run > 0 & input.CO_unit_col != null",
          actionButton("M_mod_run", "Estimate Total Carcasses"))),
      mainPanel(
        tabsetPanel(id = "M_analyses",
          tabPanel("Selected Data", br(), br(),
            DT::dataTableOutput("selected_CO")),
          tabPanel("Figures", br()),
          tabPanel("Model Tables", br())))))),

tabPanel("About",
  fluidRow(column(6, offset = 3,
    HTML('<img src = "GenEstLogoExample1.jpg" height = "400">'),
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
      and
      Manuela Huso
         <a href = "http://www.USGS.gov">(USGS)</a>'),
    br(), br(),
    HTML('GenEst is a tool for estimating bird and bat fatalities at renewable
         power facilities.'),
    br(), br(),
    HTML('GenEst is currently in development and should be considered
          provisional.'),
    br(), br(),
    textOutput("version_info"),
    br(), 
    HTML('The development of GenEst is being supported by Bat Conservation 
          International, The US Bureau of Land Management, The US Geological 
          Survey, WEST, and Oregon State University.'),
    br(), br(),
    HTML('GenEst is provided under GNU GPL v3 (and later versions).'))))

)


