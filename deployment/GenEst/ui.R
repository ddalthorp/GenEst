## ui.R

library(shiny)
library(GenEst)

shinyUI(
  navbarPage(navbar(), collapsible = TRUE, windowTitle = "GenEst", 
    tabPanel("Data Input", dataInputPanel()),
    tabPanel("Analyses", analysisPanel()),
    navbarMenu(paste0("Help (", createvtext("Short"), ")"),
      tabPanel("About", aboutPanel()),
      tabPanel("Downloads", downloadsPanel())
    )
  )
)