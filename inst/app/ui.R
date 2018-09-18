navbarPage(navbar(), collapsible = TRUE,
  windowTitle = "GenEst",
  tabPanel("Data Input", dataInputPanel()),
  tabPanel("Analyses", analysisPanel()),
  tabPanel(createvtext(type = "Short"), resourcesPanel())
)



