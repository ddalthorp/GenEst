#' @title Create the modal welcome for GenEst 
#'
#' @description Create a modal welcome and basic info screen for the GenEst
#'   application
#'
#' @return Nothing
#'
#' @export
#'
modalWelcome <- function(){  
  vnumber <- packageDescription("GenEst", fields = "Version")
  vdate <- packageDescription("GenEst", fields = "Date")
  vtext <- paste("This is version ", vnumber, " (", vdate, ")", sep = "")

  disclaimer <- paste("GenEst v", vnumber, " (", vdate, ")", sep = "")
  showModal(modalDialog(title = disclaimer, 
    "This software is preliminary or provisional and is subject to revision. 
    It is being provided to meet the need for timely best science. The 
    software has not received final approval by the U.S. Geological Survey 
    (USGS). No warranty, expressed or implied, is made by the USGS or the U.S.
    Government as to the functionality of the software and related material 
    nor shall the fact of release constitute any such warranty. The software 
    is provided on the condition that neither the USGS nor the U.S. 
    Government shall be held liable for any damages resulting from the 
    authorized or unauthorized use of the software.",  
    easyClose = FALSE, footer = modalButton("OK"))
  )
}