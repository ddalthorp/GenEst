#' @title Create the version text for GenEst 
#'
#' @description Create a text string of the version number and date
#'
#' @param type "Full" or "Short" or "Name" or "NameDate"
#'
#' @return version text
#'
#' @export
#'
createvtext <- function(type = "Full"){
  vnumber <- packageDescription("GenEst", fields = "Version")
  vdate <- packageDescription("GenEst", fields = "Date")
  if (type == "Full"){
    vtext <- paste0("This is version ", vnumber, " (", vdate, ")")
  }
  if (type == "Short"){
    vtext <- paste0("v", vnumber)
  }
  if (type == "Name"){
    vtext <- paste0("GenEst ", "v", vnumber)
  }
  if (type == "NameDate"){
    vtext <- paste0("GenEst ", "v", vnumber, " (", vdate, ")")
  }
  return(vtext)
}

#' @title Create the Content for the Getting Started Pabel
#'
#' @description Create the HTML code for the Help Getting Started main panel,
#'   which describes basic usage of the app and directs users to additional
#'   documentation. 
#'
#' @return HTML for the Help Getting Started main panel.
#'
#' @export
#'
gettingStartedContent <- function(){
  mainPanel(
    column(10, offset = 0,
      br(), br(),
      p("GenEst is an R software package for estimating bird and bat 
        fatalities at wind and solar power facilities. Mortality estimation 
        requires five data files:"
      ),
      ol(li("searcher efficiency field trial results (SE),"),
         li("carcass persistence field trial results (CP),"),
         li("schedule for periodic carcass surveys (SS),"),
         li("fraction of total carcasses falling in the searched area at each
            unit searched or", em("density-weighted proportion"), 
            "(DWP), and"
         ),
         li("summary data from the carcass surveys, including numbers of 
            carcasses observed on each search occasion (CO) and other, 
            optional covariates")
      ),
      br(), 
      p("Analysis involves several steps:"),
      ul(li("uploading data---click the ", code("Data Input"), "tab,"),
         li("entering ", code("General Input"), " parameters---click the ",  
            code("Analyses"), " tab,"),
         li("fitting searcher efficiency and carcass persistence models---
            click the ", code("Searcher Efficiency"), " and ", 
            code("Carcass Persistence"), " tabs, and "),
         li("estimating total mortality and splitting mortality estimate by
            various subcategories (such as species or sector or season) as 
            desired---click the ", code("Mortality Estimation"), " tab")
      ),
      br(),
      p("Example data sets are available in comma-separated (.csv) files that
        may be downloaded under the ", code("Downloads"), " tab. ")
    )
  )
}


#' @title Create the Content for the About Pabel
#'
#' @description This set of functions create the HTML code for the Help About 
#'   main panel, which gives information about the product and project. \cr
#'   \cr \code{aboutContent} collates the authors (\code{GenEstAuthors} and
#'   \code{GenEstGUIAuthors}), license (\code{GenEstLicense}), 
#'   acknowledgements (\code{GenEstAcknowledgements}), and logo 
#'   (\code{GenEstLogos}) contents. 
#'
#' @return HTML for the Help About main panel.
#'
#' @export
#'
aboutContent <- function(){
  mainPanel(
    column(10, offset = 0,
      br(), 
      h3(createvtext("NameDate")),
      GenEstAuthors(),
      GenEstGUIauthors(),
      GenEstLicense(),
      GenEstAcknowledgements(),
      GenEstLogos()
    )
  )
}

#' @rdname aboutContent
#'
#' @description \code{GenEstAuthors} creates the HTML code for the Authors
#'   section.
#'
#' @return \code{GenEstAuthors}: HTML for the Help About panel Author text.
#'
#' @export
#'
GenEstAuthors <- function(){
  HTML(
    paste0(br(), 
      b("Authors: "),
      "Daniel Dalthorp ", 
      a("(USGS)", href = "https://www.USGS.gov", target = "_blank"),
      ", Juniper Simonis ",
      a("(DAPPER Stats)", href = "https://www.dapperstats.com", 
        target = "_blank"),
      ", Lisa Madsen ",
      a("(OSU)", href = "https://oregonstate.edu", target = "_blank"),
      ", Manuela Huso ",
      a("(USGS)", href = "https://www.USGS.gov", target = "_blank"),
      ", Paul Rabie ",
      a("(WEST)", href = "https://www.west-inc.com", target = "_blank"),
      ", Jeffrey Mintz ",
      a("(USGS)", href = "https://www.USGS.gov", target = "_blank"),
      ", Robert Wolpert ",
      a("(Duke)", href = "http://www2.stat.duke.edu/~rlw", target = "_blank"),
      ", Jared Studyvin ",
      a("(WEST)", href = "https://www.west-inc.com", target = "_blank"),
      ", and Franzi Korner-Nievergelt ",
      a("(oikostat)", href = "http://www.oikostat.ch", target = "_blank"), 
      "."
    )
  )
}

#' @rdname aboutContent
#'
#' @description \code{GenEstGUIauthors} creates the HTML code for the GUI
#'   Authors section.
#'
#' @return \code{GenEstGUIauthors}: HTML for the Help About panel GUI Author
#'   text.
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

#' @rdname aboutContent
#'
#' @description \code{GenEstLicense} creates the HTML code for the License
#'   section.
#'
#' @return \code{GenEstLicense}: HTML for the Help About panel License text.
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

#' @rdname aboutContent
#'
#' @description \code{GenEstAcknowledgements} creates the HTML code for the
#'   Acknowledgements section.
#'
#' @return \code{GenEstAcknowledgements}: HTML for the Help About panel 
#'   Acknowledgements text.
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

#' @rdname aboutContent
#'
#' @description \code{GenEstLogos} creates the HTML code for the Logos
#'   section.
#'
#' @return \code{GenEstLogos}: HTML for the Help About panel logos.
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

#' @title Create the Content for the Disclaimers Pabel
#'
#' @description Create the HTML code for the Help Disclaimers main panel,
#'   which gives the required legal disclaimers for using the application,
#'   based on \code{appType}. \cr \cr \code{disclaimersContent} collates the
#'   USGS (\code{disclaimerUSGS} and WEST, Inc. \code{disclaimerWEST})
#'   disclaimers.
#'
#' @param appType "base" (for local version) or "deploy" (for hosted version)
#'
#' @return HTML for the Help Disclaimers main panel.
#'
#' @export
#'
disclaimersContent <- function(appType = "base"){
  if (!appType %in% c("base", "deploy")){
    stop(paste0("input appType (", appType, ") not supported"))
  }
  mainPanel(
    column(10, 
      br(),
      h3("US Geological Survey (USGS)"),
      disclaimerUSGS(), 
      br(), 
      h3("Western EcoSystems Technology, Inc. (WEST)"),
      disclaimerWEST(appType)
    )
  )
}


#' @rdname disclaimersContent
#'
#' @description \code{disclaimerUSGS} creates the text for the USGS 
#'   disclaimer.
#'
#' @return \code{disclaimerUSGS}: text for the USGS disclaimer.
#'
#' @export
#'
disclaimerUSGS <- function(){
#  "This software is preliminary or provisional and is subject to revision.
#  It is being provided to meet the need for timely best science. The
#  software has not received final approval by the U.S. Geological Survey
#  (USGS). No warranty, expressed or implied, is made by the USGS or the U.S.
#  Government as to the functionality of the software and related material
#  nor shall the fact of release constitute any such warranty. The software
#  is provided on the condition that neither the USGS nor the U.S. Government
#  shall be held liable for any damages resulting from the authorized or
#  unauthorized use of the software."
  "This software has been approved for release by the U.S. Geological
  Survey (USGS). Although the software has been subjected to rigorous
  review, the USGS reserves the right to update the software as needed
  pursuant to further analysis and review. No warranty, expressed or
  implied, is made by the USGS or the U.S. Government as to the
  functionality of the software and related material nor shall the fact of
  release constitute any such warranty. Furthermore, the software is
  released on condition that neither the USGS nor the U.S. Government shall
  be held liable for any damages resulting from its authorized or
  unauthorized use."
}

#' @rdname disclaimersContent
#'
#' @description \code{disclaimerWEST} creates the text for the WEST, Inc.  
#'   disclaimer.
#'
#' @return \code{disclaimerWEST}: text for the WEST, Inc. disclaimer.
#'
#' @export
#'
disclaimerWEST <- function(appType){
  if (!appType %in% c("base", "deploy")){
    stop(paste0("input appType (", appType, ") not supported"))
  }
  extraBR <- switch(appType, "base" = NULL, "deploy" = c(br(), br()))
  out <- NULL

  if (appType == "deploy"){
    out <- c(out, "Western EcoSystems Technology, Inc. does not host nor 
                   maintain the Shinyapp.io website. It is advised that users 
                   not upload sensitive data containing personally 
                   identifiable information (SSN, birthdates, medical 
                   information, etc.). Western EcoSystems Technology, Inc. is 
                   not liable for any damages, including but not limited to 
                   general, compensatory, special or punitive damages, 
                   sustained by user arising out of another party or entity
                   using said sensitive data or for the use of any data by 
                   another party or entity which is obtained from viruses, 
                   Trojans or other malware. Shinyapp.io is actively 
                   maintained by the RStudio Company on Amazon Web Services.")
  }
  out <- c(out, "This program is an 'AS IS' without warranty of any kind, 
                 either expressed or implied, including but not limited to, 
                 the implied warranties of merchantability and fitness for a
                 particular purpose. The entire risk as to the quality and 
                 performance of the program is with you. Should the program 
                 prove defective, you assume all cost of all necessary 
                 servicing, repair or correction. If this program is modified 
                 and/or redistributed, Western EcoSystems Technology, Inc. is 
                 not liable for any damages, including any general, special, 
                 incidental or consequential damages arising out of the use or 
                 inability to use this program (including but not limited to 
                 loss of data or data being rendered inaccurate or losses 
                 sustained by you or third parties or a failure of the program
                 to operate with any other programs), even if such holder or 
                 other party has been advised of the possibility of such 
                 damages.")
  out
}

#' @title Create a Link to the FTP-Housed Document of Interest
#'
#' @description The GenEst User Guide and Models Document live on the USGS
#'   FTP and can be linked to, rather than stored in the app. This function
#'   provides the link of interest. 
#'
#' @param doc "UserGuide" or "Models".
#'
#' @return Character element of the link to the document.
#'
#' @export
#'
ftpLink <- function(doc = "UserGuide"){
  if (!doc %in% c("UserGuide", "Models")){
    stop(paste0("doc ", doc, " not supported."))
  }
  mainLink <- "ftp://ftpext.usgs.gov/pub/wr/or/corvallis/Dalthorp/"
  if (doc == "UserGuide"){
    paste0(mainLink, "GenEst_User_Guide%200.2.0.pdf")
  } else if (doc == "Models"){
    paste0(mainLink, "GenEst_Statistical_Models.pdf")
  }
}
