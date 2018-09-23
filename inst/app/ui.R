navbarPage(navbar(), collapsible = TRUE, windowTitle = createvtext("Name"),
  tabPanel("Data Input", dataInputPanel()),
  tabPanel("Analyses", analysisPanel()),
  tabPanel("Help", helpPanel()),
  selected = "Help"
)



