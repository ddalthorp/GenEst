##' @title Resources Main Panel UI element
##'
##' @description create the HTML code for the Resources panel
##'
##' @return HTML for the Analysis panel
##'
##' @export
##'
resourcesPanel <- function(){
  tabsetPanel(id = "ResourcesViz",
    tabPanel("Getting Started", HTML(panel_GS)),
    tabPanel("Downloads", panel_downloads()),
    tabPanel("Help", panel_help()),
    tabPanel("Disclaimers", panel_disclaim())
  )
}

USGSpath <- "ftp://ftpext.usgs.gov/pub/wr/or/corvallis/Dalthorp/"

panel_GS <- paste0("<br/><br/>",
  "GenEst is an R software package for estimating bird and bat fatalities ",
  "at wind and solar power facilities. Mortality estimation requires five ",
  "data files:<br/><ol>",
  "<li>searcher efficiency field trial results (SE),</li>",
  "<li>carcass persistence field trial results (CP),</li>",
  "<li>schedule for periodic carcass surveys (SS),</li>",
  "<li>fraction of total carcasses falling in the searched area at each ",
        "unit searched or <i>density-weighted proportion</i> (DWP), and</li>",
  "<li>summary data from the carcass surveys, including numbers of ",
        "carcasses observed on each search occasion (CO) and other, optional ",
        "covariates.</li></ol><br/>",
  "Analysis involves several steps:<br/><ul>",
  "<li>upload data---click the <code>Data Input</code> tab,</li>",
  "<li>entering <code>General Input</code> parameters---",
      "click the <code>Analyses</code> tab, </li>",
  "<li>fitting searcher efficiency and carcass persistence models---click the ",
  "   <code>Searcher Efficiency</code> and <code>Carcass Persistence</code> ",
      "tabs, and </li>",
  "<li>estimate total mortality and split the mortality estimate by various ",
  "subcategories (such as species or sector or season) as desired---click ",
  "the <code>Mortality Estimation</code> tab.</li></ul><br/>",
  "Further details can be found in the <a href=\"", USGSpath,
  "GenEst_User_Guide%200.2.0.pdf\", target = \"_blank\">",
   "User Guide</a>, and in a technical manual that describes the ",
   "<a href=\"", USGSpath, "GenEst_Statistical_Models.pdf\", ",
   "target = \"_blank\"> statistical models </a>.<br/><br/>",
   "Example data sets are available under the <code>Downloads</code> tab."
  )
panel_downloads <- function(){
  mainPanel(
    h3("Example data sets"),
    fluidRow(
      column(width = 6, h4("Wind---Road and pad searches, bats")),
      column(width = 1, downloadButton('download_RPbat', 'Download'))
    ),
    fluidRow(
      column(width = 6, h4("Wind---Road and pad searches, bats + birds")),
      column(width = 1, downloadButton("download_RP", "Download"))
    ),
    fluidRow(
      column(width = 6, h4("Wind---Cleared plots, bats + birds")),
      column(width = 1, downloadButton('download_cleared', 'Download'))
    ),
    fluidRow(
      column(width = 6, h4("Solar---Power tower")),
      column(width = 1, downloadButton('download_powerTower', 'Download'))
    ),
    fluidRow(
      column(width = 6, h4("Solar---Photovoltaic (PV)")),
      column(width = 1, downloadButton('download_PV', 'Download'))
    ),
    fluidRow(
      column(width = 6, h4("Solar---Trough")),
      column(width = 1, downloadButton('download_trough', 'Download'))
    ),
    fluidRow(
      column(width = 6, h4("Mock data")),
      column(width = 1, downloadButton('download_mock', 'Download'))
    )
  )
}

panel_help <- function(){
  mainPanel(
    h3(paste0("GenEst, v", packageDescription("GenEst", fields = "Version"),
      " (",packageDescription("GenEst", fields = "Date"), ")")),
    h4(HTML("<a href=\"", USGSpath, "GenEst_User_Guide%200.2.0.pdf\",
      target = \"_blank\">User Guide</a>")),
    h4(HTML("<a href=\"", USGSpath, "GenEst_Statistical_Models.pdf\",
      target = \"_blank\">GenEst Statistical Models</a>")),
    br(),
    p(HTML("<b>Authors:</b>
      Daniel Dalthorp
        <a href = 'http://www.USGS.gov'>(USGS)</a>,
      Juniper Simonis
        <a href = 'http://www.dapperstats.com'>(DAPPER Stats)</a>,
      Manuela Huso
        <a href = 'http://www.USGS.gov'>(USGS)</a>,
      Lisa Madsen
        <a href = 'http://www.oregonstate.edu'>(OSU)</a>,
      Paul Rabie
        <a href = 'http://www.west-inc.com'>(WEST)</a>,
      Jeffrey Mintz
        <a href = 'http://www.USGS.gov'>(USGS)</a>,
      Robert Wolpert
        <a href = 'http://www2.stat.duke.edu/~rlw/'>(Duke)</a>,
      Jared Studyvin
        <a href = 'http://www.west-inc.com'>(WEST)</a>,
      and Franzi Korner-Nievergelt
        <a href = 'http://www.oikostat.ch/'>(oikostat)</a>")),
    br(),
    p(HTML("<b>Web Design and Graphics User Interface Programming:</b>
      Juniper Simonis
        <a href = 'http://www.dapperstats.com'>(DAPPER Stats)</a>")),
    br(),
    p(HTML("GenEst is provided under universal public domain, license
      <a href = 'https://creativecommons.org/publicdomain/zero/1.0/legalcode'>
      CC0 1.0.</a>")),
    br(),
    p(HTML("The development of GenEst was supported by
      <a href = 'https://www.blm.gov/'>The US Bureau of Land Management</a>,
      <a href = 'https://www.usgs.gov/'>The US Geological Survey</a>,
      <a href = 'https://www.nrel.gov/'>National Renewable Energy
        Laboratory</a>,
      <a href = 'http://www.westconsultants.com/'>WEST, Inc.</a>,
      <a href = 'http://www.batcon.org/'>Bat Conservation International</a>,
      <a href = 'http://www.avangridrenewables.us/'>Avangrid Renewables</a>,
      <a href = 'https://awwi.org/'>American Wind Wildlife Institute</a>,
      and
      <a href = 'https://oregonstate.edu/'>Oregon State University</a>.")),
    br(), br(), br(),
    p(HTML("<a href='https://www.blm.gov/'>
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
       <img src = 'duke.png' height = '60'></a>")
    )
  )
}
panel_disclaim <- function(){
  mainPanel(
    br(),
    h4("US Geological Survey (USGS)"),
    p(modalTextUSGS()),
    br(),
    h4("Western EcoSystems Technology, Inc. (WEST)"),
    p(modalTextWEST())
  )
}
