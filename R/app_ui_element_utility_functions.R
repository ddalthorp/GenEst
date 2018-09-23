#' @title navbar UI element 
#'
#' @description create the HTML code for the navbar element
#'
#' @return HTML for the navbar element
#'
#' @export
#'
navbar <- function(){
  div(
    div(
      img(src = "GenEst.png", style = "margin-top: -8px;", alt = "GenEst",
        height = 40
       ), 
      small(createvtext("Short"))
    )
  )
}

#' @title Make a model menu
#'
#' @description Produce a size-based model-selection menu object based on
#'   model inputs
#'
#' @param mods size-indexed list of model sets
#'
#' @param sizeclasses size class names
#'
#' @param type model type, either "SE" or "CP"
#'
#' @return rendered HTML model selection menu object
#'
#' @export
#'
makeMenu <- function(mods, sizeclasses, type){

  modelMenu <- ""
  nsizeclasses <- length(sizeclasses)
  if (nsizeclasses > 0){
    for(sci in 1:nsizeclasses){
      if (type == "SE"){
        AICcTab <- pkmSetAICcTab(mods[[sci]], quiet = TRUE)
      }
      if (type == "CP"){
        AICcTab <- cpmSetAICcTab(mods[[sci]], quiet = TRUE)
      }
      modOrder <- as.numeric(row.names(AICcTab))
      modNames <- names(mods[[sci]])[modOrder]
      modNames <- gsub("; NULL", "", modNames)
      modNames <- gsub("dist: ", "", modNames)
      modNames <- gsub("~ 1", "~ constant", modNames)

      modNames_nchar <- nchar(modNames)
      modNames_maxchar <- max(modNames_nchar)
      modNames_nspaces <- modNames_maxchar - modNames_nchar + 10
      modSpaces <- sapply(modNames_nspaces, 
                     function(x){paste(rep(" ", x), collapse = "")}
                   )
      modDeltaAICcs <- AICcTab[ , "Delta AICc"]
      modLabels <- paste0(modNames, " (delta AICc: ", modDeltaAICcs, ")")
      names(modNames) <- modLabels
      labels_nchar <- nchar(modLabels)
      labels_maxchar <- max(labels_nchar)
      widthval <- max(c(400, labels_maxchar * 7 + 20))
      widthtxt <- paste0(widthval, "px")
      mtuText <- paste("modelChoices_", type, sci, sep = "") 
      scText <- paste("Model for ", sizeclasses[sci], sep = "")
      modSelect <- selectizeInput(mtuText, scText, modNames, multiple = TRUE, 
                     width = widthtxt, options = list(maxItems = 1))
      modelMenu <- paste(modelMenu, modSelect)  
    }
  }        
  return(renderUI({HTML(modelMenu)}))
}

#' @title HTML ol function
#'
#' @description Generate an ordered list HTML object. This simply pulls the
#'   function definition from the tag environment in htmltools
#'
#' @param ... attributes and children of the element
#'
#' @return HTML ordered list
#'
#' @export
#'
ol <- function(...){
  tags$ol(...)
}

#' @title HTML ul function
#'
#' @description Generate an unordered list HTML object. This simply pulls the
#'   function definition from the tag environment in htmltools
#'
#' @param ... attributes and children of the element
#'
#' @return HTML unordered list
#'
#' @export
#'
ul <- function(...){
  tags$ul(...)
}

#' @title HTML li function
#'
#' @description Generate an HTML list element. This simply pulls the
#'   function definition from the tag environment in htmltools
#'
#' @param ... attributes and children of the element
#'
#' @return HTML ordered list
#'
#' @export
#'
li <- function(...){
  tags$li(...)
}

#' @title HTML b function
#'
#' @description Generate an HTML bolded text element. This simply pulls 
#'   the function definition from the tag environment in htmltools
#'
#' @param ... attributes and children of the element
#'
#' @return HTML ordered list
#'
#' @export
#'
b <- function(...){
  tags$b(...)
}

#' @title HTML small function
#'
#' @description Generate a small text HTML object. This simply pulls the
#'   function definition from the tag environment in htmltools
#'
#' @param ... attributes and children of the element
#'
#' @return HTML small text
#'
#' @export
#'
small <- function(...){
  tags$small(...)
}

#' @title Creates a link to the FTP document of interest
#'
#' @description create the FTP link for the User Guide or Models document
#'
#' @param doc "UserGuide" or "Models"
#'
#' @return link to the document
#'
#' @export
#'
ftpLink <- function(doc = "UserGuide"){
  mainLink <- "ftp://ftpext.usgs.gov/pub/wr/or/corvallis/Dalthorp/"
  if (doc == "UserGuide"){
    docLink <- paste0(mainLink, "GenEst_User_Guide%200.2.0.pdf")
  }
  if (doc == "Models"){
    docLink <- paste0(mainLink, "GenEst_Statistical_Models.pdf")
  }
  docLink
}
