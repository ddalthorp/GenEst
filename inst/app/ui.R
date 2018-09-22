navbarPage(navbar(), collapsible = TRUE, windowTitle = "GenEst",
  tabPanel("Data Input", dataInputPanel()),
  tabPanel("Analyses", analysisPanel()),
  tabPanel("Help/Resources", resourcesPanel()),
  selected = "Help/Resources"
)



