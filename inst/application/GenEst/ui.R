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
      fileInput("CP_file", "Persistence Data File",
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
        tabPanel("Search Efficiency", br(), DT::dataTableOutput("SE_data")),
        tabPanel("Carcass Persistence", br(), DT::dataTableOutput("CP_data")),
        tabPanel("Search Schedule", br(), DT::dataTableOutput("SS_data")),
        tabPanel("Carcass Observations", br(), DT::dataTableOutput("CO_data"))
      )))),

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
                       options = list(maxItems = 1))
      )
    ),
    tabPanel("Searcher Efficiency", br(), br(),
      sidebarPanel(width = 3,
        selectizeInput("SE_obs_cols", "Observation Columns:",
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
            DT::dataTableOutput("SE_mod_tab")
          ),
          tabPanel("Model Comparison Tables", br(),
            selectizeInput("SE_AICc_sc", width = "400px", "Size Class:",
                           "Model not yet run", multiple = F), br(), br(), 
            DT::dataTableOutput("SE_AICc_table")
          ),
          tabPanel("Model Selection", br())
        ))
    )
  )
)
)


