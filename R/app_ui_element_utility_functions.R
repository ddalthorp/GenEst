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
    a(href = "https://github.com/ddalthorp/GenEst", 
      img(src = "GenEst.png", style =  "margin-top: -8px;", height = 40)
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
      modSelect <- selectizeInput(mtuText, scText, modNames, width = widthtxt)
      modelMenu <- paste(modelMenu, modSelect)  
    }
  }
          
  return(renderUI({HTML(modelMenu)}))
}