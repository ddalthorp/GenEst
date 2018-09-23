## ui.R

library(shiny)
library(GenEst)

shinyUI(
  navbarPage(navbar(), collapsible = TRUE, windowTitle = createvtext("Name"),
    tabPanel("Data Input", dataInputPanel()),
    tabPanel("Analyses", analysisPanel()),
    tabPanel("Help", helpPanel("deploy")),
    selected = "Help"
  )
)