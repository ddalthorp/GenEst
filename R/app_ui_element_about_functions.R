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
        <a href = 'http://www.USGS.gov', title = 'USGS'>(USGS)</a>,
      Juniper Simonis 
        <a href = 'http://www.dapperstats.com', title 'DAPPER Stats'> (DAPPER Stats)</a>,
      Manuela Huso
        <a href = 'http://www.USGS.gov', title = 'USGS'>(USGS)</a>,
      Lisa Madsen
        <a href = 'http://www.oregonstate.edu', title = 'OSU'>(OSU)</a>,
      Paul Rabie 
        <a href = 'http://www.west-inc.com', title = 'WEST'>(WEST)</a>,
      Jeffrey Mintz
        <a href = 'http://www.USGS.gov', title = 'USGS'>(USGS)</a>,
      Robert Wolpert
        <a href = 'http://www2.stat.duke.edu/~rlw/', title = 'Duke'>(Duke)</a>,
      Jared Studyvin
        <a href = 'http://www.west-inc.com', title = 'WEST'>(WEST)</a>,
      and Franzi Korner-Nievergelt
        <a href = 'http://www.oikostat.ch/', title = 'oikostat'>(oikostat)</a>"),
    br(), br(),
    HTML("<b>Web Design and Graphics User Interface Programming:</b>  
      Juniper Simonis 
        <a href = 'http://www.dapperstats.com', title = 'DAPPER Stats'>
          (DAPPER Stats)</a>"
    ),
    br(), br(),
    HTML("The development of GenEst was supported by
      <a href = 'https://www.blm.gov/', title = 'BLM'>
        The US Bureau of Land Management</a>,
      <a href = 'https://www.usgs.gov/', title = 'USGS'>
        The US Geological Survey</a>,
      <a href = 'https://www.nrel.gov/', title = 'NREL'>
        National Renewable Energy Laboratory</a>,
      <a href = 'http://www.westconsultants.com/', title = 'WEST' >WEST, Inc.</a>,
      <a href = 'http://www.batcon.org/', title = 'BCI'>
        Bat Conservation International</a>,
      <a href = 'http://www.avangridrenewables.us/', title = 'Avangrid'>
        Avangrid Renewables</a>,
      <a href = 'https://awwi.org/', title = 'AWWI'>
        American Wind Wildlife Institute</a>,
      and
      <a href = 'https://oregonstate.edu/', title = 'OSU'>
        Oregon State University</a>."),
    br(), br(),
    HTML("GenEst is provided under universal public domain, license
      <a href = 'https://creativecommons.org/publicdomain/zero/1.0/legalcode'>
      CC0 1.0.</a>"),
    br(), br(), br(), br(),
    HTML("<a href='https://www.blm.gov/', title = 'BLM'>
       <img src = 'blm.jpg' height = '60', alt = 'BLM logo'></a>
       <a href='https://www.usgs.gov/', title = 'USGS'>
       <img src = 'usgs.png' height = '60', alt = 'USGS logo'></a>
       <a href='https://www.nrel.gov/', title = 'NREL'>
       <img src = 'nrel.jpg' height = '60', alt = 'NREL logo'> </a>
       <a href='http://www.westconsultants.com/', title = 'WEST'>
       <img src = 'west.png' height = '60', alt = 'WEST logo'></a>
       <a href='http://www.batcon.org/', title = 'BCI'>
       <img src = 'bci.jpg' height = '60', alt = 'BCI logo'></a>
       <a href='https://awwi.org/', title = 'AWWI'>
       <img src = 'awwi.png' height = '60', alt = 'AWWI logo'></a>
       <a href='http://www.avangridrenewables.us/', title = 'Avangrid'>
       <img src = 'avangrid.png' height = '60', alt = 'Avangrid logo'></a>
       <a href='http://www.dapperstats.com', , title = 'DAPPER Stats'>
       <img src = 'dapper.png' height = '60', alt = 'DapperStats logo'></a>
       <a href='http://www.oikostat.ch/', title = 'Oikostat'>
       <img src = 'oikostat.jpg' height = '60', alt = 'Oikostat logo'> </a>
       <a href='https://www.oregonstate.edu/', title = 'OSU'>
       <img src = 'osu.jpg' height = '60', alt = 'OSU logo'> </a>
       <a href='https://www.duke.edu/', title = 'Duke University'>
       <img src = 'duke.png' height = '60', alt = 'Duke U. logo'></a>"
     )
  )
}

