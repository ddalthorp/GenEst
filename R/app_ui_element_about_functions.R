#' @title Help About Main Panel UI element 
#'
#' @description create the HTML code for the Help About panel
#'
#' @return HTML for the Help About panel
#'
#' @export
#'
aboutPanel <- function(){
  tabPanel("About", aboutMainPanel())
}

#' @title Help About main panel UI element
#'
#' @description create the HTML code for the Help About main panel
#'
#' @return HTML for the Help About main panel
#'
#' @export
#'
aboutMainPanel <- function(){
  mainPanel(
    column(10, offset = 0,
      br(), 
      h3(createvtext("NameDate")),
      h4(a("User Guide", href = ftpLink("UserGuide"), target = "_blank")),
      h4(a("GenEst Statistical Models", href = ftpLink("Models"), 
           target = "_blank"
         )
      ),
      GenEstAuthors(),
      GenEstGUIauthors(),
      GenEstLicense(),
      GenEstAcknowledgements(),
      GenEstLogos()
    )
  )
}


#' @title Help About authors
#'
#' @description create the HTML code for the Help About authors
#'
#' @return HTML for the authors on the About panel
#'
#' @export
#'
GenEstAuthors <- function(){
  HTML(
    paste0(br(), 
      b("Authors:"),
      "Daniel Dalthorp ", 
      a("(USGS)", href = "https://www.USGS.gov", target = "_blank"),
      ", Juniper Simonis ",
      a("(DAPPER Stats)", href = "https://www.dapperstats.com", 
        target = "_blank"
      ),
      ", Manuela Huso ",
      a("(USGS)", href = "https://www.USGS.gov", target = "_blank"),
      ", Lisa Madsen ",
      a("(OSU)", href = "https://oregonstate.edu", target = "_blank"),
      ", Paul Rabie ",
      a("(WEST)", href = "https://www.west-inc.com", target = "_blank"),
      ", Jeffrey Mintz ",
      a("(USGS)", href = "https://www.USGS.gov", target = "_blank"),
      ", Robert Wolpert ",
      a("(Duke)", href = "http://www2.stat.duke.edu/~rlw/", 
        target = "_blank"
      ),
      ", Jared Studyvin ",
      a("(WEST)", href = "https://www.west-inc.com", target = "_blank"),
      ", and Franzi Korner-Nievergelt ",
      a("(oikostat)", href = "http://www.oikostat.ch", target = "_blank"), 
      "."
    )
  )
}

#' @title Help About GUI authors
#'
#' @description create the HTML code for the Help About GUI authors
#'
#' @return HTML for the GUI authors on the About panel
#'
#' @export
#'
GenEstGUIauthors <- function(){
  HTML(
    paste0(br(), br(),
      b("Web Design and Graphics User Interface Programming:"), 
      " Juniper Simonis ",
      a("(DAPPER Stats)", href = "http://www.dapperstats.com", 
        target = "_blank"
      ), 
      "."
    )
  )
}

#' @title Help About acknowledgements
#'
#' @description create the HTML code for the Help About acknowledgements
#'
#' @return HTML for the acknowledgements on the About panel
#'
#' @export
#'
GenEstAcknowledgements <- function(){
  HTML(
    paste0(br(), br(),
      "The development of GenEst was supported by ",
      a("The US Bureau of Land Management",
        href = "https://www.blm.gov", target = "_blank"
      ), 
      ", ",    
      a("The US Geological Survey",
        href = "https://www.usgs.gov", target = "_blank"
      ), 
      ", ",
      a("The National Renewable Energy",
        href = "https://www.nrel.gov", target = "_blank"
      ), 
      ", ",
      a("WEST Inc.",
        href = "https://www.west-inc.com", target = "_blank"
      ), 
      ", ",
      a("Bat Conservation International",
        href = "https://www.batcon.org", target = "_blank"
      ), 
      ", ",
      a("Avangrid Renewables",
        href = "http://www.avangridrenewables.us", target = "_blank"
      ), 
      ", ",
      a("American Wind Wildlife Institute",
        href = "https://www.awwi.org", target = "_blank"
      ), 
      ", and ",
      a("Oregon State University",
        href = "https://oregonstate.edu", target = "_blank"
      ), 
      "."
    )
  )
}

#' @title Help About license
#'
#' @description create the HTML code for the Help About license
#'
#' @return HTML for the license on the About panel
#'
#' @export
#'
GenEstLicense <- function(){
  HTML(
    paste0(br(), br(),
      "GenEst is provided under universal public domain, license ",
      a("CC0 1.0",
        href = "https://creativecommons.org/publicdomain/zero/1.0/legalcode",
        target = "_blank"
      ), "."
    )
  )
}

#' @title Help About logos
#'
#' @description create the HTML code for the Help About logos
#'
#' @return HTML for the logos on the About panel
#'
#' @export
#'
GenEstLogos <- function(){
  HTML(
    paste0(br(), br(),
      a(img(src = "blm.jpg", height = "60", alt = "BLM logo"),
        href = "https://www.blm.gov", target = "_blank"
      ),
      a(img(src = "usgs.png", height = "60", alt = "USGS logo"),
        href = "https://www.usgs.gov", target = "_blank"
      ),
      a(img(src = "nrel.jpg", height = "60", alt = "NREL logo"),
        href = "https://www.nrel.gov", target = "_blank"
      ),
      a(img(src = "west.png", height = "60", alt = "WEST logo"),
        href = "https://www.west-inc.com", target = "_blank"
      ),
      a(img(src = "bci.jpg", height = "60", alt = "BCI logo"),
        href = "https://www.batcon.org", target = "_blank"
      ),
      a(img(src = "awwi.png", height = "60", alt = "AWWI logo"),
        href = "https://www.awwi.org", target = "_blank"
      ),
      a(img(src = "avangrid.png", height = "60", alt = "Avangrid logo"),
        href = "http://www.avangridrenewables.us", target = "_blank"
      ),
      a(img(src = "dapper.png", height = "60", alt = "DAPPER stats logo"),
        href = "https://www.dapperstats.com", target = "_blank"
      ),
      a(img(src = "oikostat.jpg", height = "60", alt = "oikostat logo"),
        href = "http://www.oikostat.ch", target = "_blank"
      ),
      a(img(src = "osu.jpg", height = "60", alt = "Oregon State logo"),
        href = "https://oregonstate.edu/", target = "_blank"
      ),
      a(img(src = "duke.png", height = "60", alt = "Duke logo"),
        href = "https://www.duke.edu", target = "_blank"
      )
    )
  )
}
