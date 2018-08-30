navbarPage(navbar(), collapsible = TRUE, windowTitle = "GenEst", 
  tabPanel("Data Input", dataInputPanel()),
  tabPanel("Analyses", analysisPanel()),
  tabPanel("Help", helpPanel()),
  tabPanel(paste0("About (", createvtext("Short"), ")"), aboutPanel())
)

