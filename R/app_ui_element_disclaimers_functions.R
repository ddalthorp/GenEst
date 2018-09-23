#' @title Help Disclaimers Main Panel UI element 
#'
#' @description create the HTML code for the Help Disclaimers panel
#'
#' @param type "base" (for local versions) or "deploy" (for hosted version)
#'
#' @return HTML for the Help Disclaimers panel
#'
#' @export
#'
disclaimersPanel <- function(type){
  tabPanel("Disclaimers", disclaimersMainPanel(type))
}

#' @title Help Disclaimers main panel UI element
#'
#' @description create the HTML code for the Help Disclaimers main panel
#'
#' @param type "base" (for local versions) or "deploy" (for hosted version)
#'
#' @return HTML for the Help Disclaimers main panel
#'
#' @export
#'
disclaimersMainPanel <- function(type){
  mainPanel(
    column(10, offset = 0,
      br(), 
      disclaimers(type)
    )
  )
}

#' @title Help Disclaimers UI element
#'
#' @description create the HTML code for the Help Disclaimers
#'
#' @param type "base" (for local versions) or "deploy" (for hosted version)
#'
#' @return HTML for the Help Disclaimers text
#'
#' @export
#'
disclaimers <- function(type){
  extraBR <- switch(type, "base" = NULL, "deploy" = br())

  list(h3("US Geological Survey (USGS)"),
       disclaimerUSGS(), 
       br(), 
       h3("Western EcoSystems Technology, Inc. (WEST)"),
       disclaimerDeploy(type), extraBR, extraBR, 
       disclaimerWEST()
  )
}


#' @title Create USGS disclaimer text 
#'
#' @description Create USGS disclaimer text
#'
#' @return HTML element of the USGS disclaimer text
#'
#' @export
#'
disclaimerUSGS <- function(){
  "This software is preliminary or provisional and is subject to revision. 
  It is being provided to meet the need for timely best science. The 
  software has not received final approval by the U.S. Geological Survey 
  (USGS). No warranty, expressed or implied, is made by the USGS or the U.S.
  Government as to the functionality of the software and related material 
  nor shall the fact of release constitute any such warranty. The software
  is provided on the condition that neither the USGS nor the U.S. Government
  shall be held liable for any damages resulting from the authorized or 
  unauthorized use of the software."
  #"This software has been approved for release by the U.S. Geological 
  #Survey (USGS). Although the software has been subjected to rigorous 
  #review, the USGS reserves the right to update the software as needed 
  #pursuant to further analysis and review. No warranty, expressed or 
  #implied, is made by the USGS or the U.S. Government as to the 
  #functionality of the software and related material nor shall the fact of 
  #release constitute any such warranty. Furthermore, the software is 
  #released on condition that neither the USGS nor the U.S. Government shall
  #be held liable for any damages resulting from its authorized or
  #unauthorized use."
}

#' @title Create WEST disclaimer text
#'
#' @description Create WEST disclaimer text 
#'
#' @return HTML element of the WEST text
#'
#' @export
#'
disclaimerWEST <- function(){
  "This program is an 'AS IS' without warranty of any kind, either 
   expressed or implied, including but not limited to, the implied 
   warranties of merchantability and fitness for a particular purpose. 
   The entire risk asto the quality and performance of the program is 
   with you. Should the program prove defective, you assume all cost 
   of all necessary servicing,repair or correction. If this program is
   modified and/or redistributed, Western EcoSystems Technology, Inc. 
   is not liable for any damages,including any general, special, 
   incidental or consequential damages arising out of the use or 
   inability to use this program (including but notlimited to loss of 
   data or data being rendered inaccurate or losses sustained by you 
   or third parties or a failure of the program to operate with any 
   other programs), even if such holder or other party has been 
   advised of the possibility of such damages."
}

#' @title Create the WEST disclaimer text for deployment
#'
#' @description Create WEST deployment disclaimer text 
#'
#' @param type "base" (for local versions) or "deploy" (for hosted version)
#'
#' @return HTML element of the WEST deployment disclaimer text
#'
#' @export
#'
disclaimerDeploy <- function(type){
  if (type == "base"){
    return(NULL)
  }
  "Western EcoSystems Technology, Inc. does not host nor maintain the 
  Shinyapp.io website. It is advised that users not upload sensitive data 
  containing personally identifiable information (SSN, birthdates, medical 
  information, etc.). Western EcoSystems Technology, Inc. is not liable for 
  any damages, including but not limited to general, compensatory, special 
  or punitive damages, sustained by user arising out of another party or 
  entity using said sensitive data or for the use of any data by another 
  party or entity which is obtained from viruses, Trojans or other malware. 
  Shinyapp.io is actively maintained by the RStudio Company on Amazon Web 
  Services."
}


