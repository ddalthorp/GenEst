#' @title About Main Panel UI element
#'
#' @description create the HTML code for the About main panel
#'
#' @return HTML for the about panel
#'
#' @export
#'
aboutPanel <- function(){
  fluidRow(
    HTML("GenEst is a tool for estimating mortality from efficiency,
      persistence, search schedule, and carcass data."
    ),
    HTML("<b>Authors:</b>
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
        <a href = 'http://www.oikostat.ch/'>(oikostat)</a>"),
    br(), br(),
    HTML("<b>Web Design and Graphics User Interface Programming:</b>  
      Juniper Simonis 
        <a href = 'http://www.dapperstats.com'>(DAPPER Stats)</a>"
    ),
    br(), br(),
    HTML("The development of GenEst was supported by
      <a href = 'https://www.blm.gov/'>The US Bureau of Land Management</a>,
      <a href = 'https://www.usgs.gov/'>The US Geological Survey</a>,
      <a href = 'https://www.nrel.gov/'>National Renewable Energy 
        Laboratory</a>, 
      <a href = 'http://www.westconsultants.com/'>WEST, Inc.</a>, 
      <a href = 'http://www.batcon.org/'>Bat Conservation International</a>,
      <a href = 'http://www.avangridrenewables.us/'>Avangrid Renewables</a>,
      <a href = 'https://awwi.org/'>American Wind Wildlife Institute</a>, 
      and
      <a href = 'https://oregonstate.edu/'>Oregon State University</a>."),
    br(), br(),
    HTML("GenEst is provided under universal public domain, license
      <a href = 'https://creativecommons.org/publicdomain/zero/1.0/legalcode'>
      CC0 1.0.</a>"),
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
}

