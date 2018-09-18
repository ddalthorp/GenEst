#' @title Create the modal welcome for GenEst 
#'
#' @description Create a modal welcome and basic info screen for the GenEst
#'   application
#'
#' @param type "base" (just USGS text) or "deploy" (USGS and WEST text)
#'
#' @return Nothing
#'
#' @export
#'
modalWelcome <- function(type = "base"){  
  if (type == "base"){
    vnumber <- packageDescription("GenEst", fields = "Version")
    vdate <- packageDescription("GenEst", fields = "Date")
    disclaimer <- paste("GenEst v", vnumber, " (", vdate, ")", sep = "")
    showModal(modalDialog(title = disclaimer, modalTextUSGS(),
      easyClose = FALSE, footer = modalButton("OK"))
    )
  }
  if (type == "deploy"){
    vnumber <- packageDescription("GenEst", fields = "Version")
    vdate <- packageDescription("GenEst", fields = "Date")
    disclaimer <- paste("GenEst v", vnumber, " (", vdate, ")", sep = "")
    showModal(modalDialog(title = disclaimer, modalTextUSGS(), br(), br(),
      modalTextWEST(), easyClose = FALSE, footer = modalButton("OK"))
    )
  }
}

#' @title Create USGS text for the modal welcome
#'
#' @description Create USGS text for the modal welcome
#'
#' @return HTML element of the USGS text
#'
#' @export
#'
modalTextUSGS <- function(){
  HTML(
    "This software has been approved for release by the U.S. Geological Survey
    (USGS). Although the software has been subjected to rigorous review, the
    USGS reserves the right to update the software as needed pursuant to further
    analysis and review. No warranty, expressed or implied, is made by the USGS
    or the U.S. Government as to the functionality of the software and related
    material nor shall the fact of release constitute any such warranty.
    Furthermore, the software is released on condition that neither the USGS nor
    the U.S. Government shall be held liable for any damages resulting from its
    authorized or unauthorized use."
  )
}

#' @title Create WEST text for the modal welcome
#'
#' @description Create WEST text for the modal welcome
#'
#' @return HTML element of the WEST text
#'
#' @export
#'
modalTextWEST <- function(){
  HTML("Western EcoSystems Technology, Inc. does not host nor maintain the 
    Shinyapp.io website. It is advised that users not upload sensitive data 
    containing personally identifiable information (SSN, birthdates, medical 
    information, etc.). Western EcoSystems Technology, Inc. is not liable for 
    any damages, including but not limited to general, compensatory, special 
    or punitive damages, sustained by user arising out of another party or 
    entity using said sensitive data or for the use of any data by another 
    party or entity which is obtained from viruses, Trojans or other malware. 
    Shinyapp.io is actively maintained by the RStudio Company on Amazon Web 
    Services.", "<br>", "<br>", 
    "This program is an 'AS IS' without warranty of any kind, either expressed
    or implied, including but not limited to, the implied warranties of 
    merchantability and fitness for a particular purpose. The entire risk as
    to the quality and performance of the program is with you. Should the 
    program prove defective, you assume all cost of all necessary servicing,
    repair or correction. If this program is modified and/or redistributed, 
    Western EcoSystems Technology, Inc. is not liable for any damages,
    including any general, special, incidental or consequential damages 
    arising out of the use or inability to use this program (including but not
    limited to loss of data or data being rendered inaccurate or losses 
    sustained by you or third parties or a failure of the program to operate 
    with any other programs), even if such holder or other party has been
    advised of the possibility of such damages."
  )
}

#' @title Create the version text for GenEst 
#'
#' @description Create a text string of the version number and date
#'
#' @param type "Full" or "Short"
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
  return(vtext)
}