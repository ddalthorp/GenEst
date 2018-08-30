#' @title Downloads Main Panel UI element
#'
#' @description create the HTML code for the Downloads main panel
#'
#' @return HTML for the help panel
#'
#' @export
#'
downloadsPanel <- function(){
  fluidRow(
    column(5, offset = 2,
      br(), br(),
      a(href = "GenEst_User_Guide.pdf", target = "blank", 
        "User Guide (pdf)",
        download  = "GenEst_User_Guide.pdf"),
      br(), br(), 
      a(href = "GenEst_Statistical_Models.pdf", target = "blank", 
        "Technical Manual (pdf)",
        download  = "GenEst_Statistical_Models.pdf"),
      br(), br(), 
      a(href = "extdata.zip", target = "blank", 
        "Example Datasets (zip)",
        download  = "extdata.zip")
    )
  )
}