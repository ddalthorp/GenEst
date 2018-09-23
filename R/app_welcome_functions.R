#' @title Create the modal welcome for GenEst 
#'
#' @description Create a modal welcome and basic info screen for the GenEst
#'   application
#'
#' @param type "base" (for local versions) or "deploy" (for hosted version)
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
    showModal(modalDialog(title = disclaimer, disclaimerUSGS(), br(), br(),
      disclaimerWEST("base"), easyClose = FALSE, footer = modalButton("OK"))
    )
  }
  if (type == "deploy"){
    vnumber <- packageDescription("GenEst", fields = "Version")
    vdate <- packageDescription("GenEst", fields = "Date")
    disclaimer <- paste("GenEst v", vnumber, " (", vdate, ")", sep = "")
    showModal(modalDialog(title = disclaimer, disclaimerUSGS(), br(), br(),
      disclaimerWEST("deploy"), easyClose = FALSE, footer = modalButton("OK"))
    )
  }
}

