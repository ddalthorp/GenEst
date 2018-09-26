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

#' @title Make a model menu header
#'
#' @description Produce a size-based model-selection menu header object based 
#'   on model inputs
#'
#' @param n number of size classed
#'
#' @return rendered HTML model selection menu object
#'
#' @export
#'
modelMenuHeader <- function(n){
 
  if (n == 1){
    ntext <- "model"
  } else if (n > 1){
    ntext <- "models"
  } else{
    stop("n input is improper")
  }
  prefix <- "Select "
  suffix <- " for mortality and detection probability estimation"
  em(paste0(prefix, ntext, suffix))
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
  nsizeclasses <- length(sizeclasses)

  menuHeader <- modelMenuHeader(nsizeclasses)
  modelMenu <- ""
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
      mtuText <- paste0("modelChoices_", type, sci) 
      scText <- paste0(sizeclasses[sci], ":")
      if (nsizeclasses == 1){
        scText <- ""
      }
      modSelect <- selectizeInput(mtuText, scText, modNames, multiple = TRUE, 
                     width = widthtxt, options = list(maxItems = 1))
      modelMenu <- paste(modelMenu, modSelect)  
    }
  }
  menuBreak <- NULL
  if (nsizeclasses > 1){
    menuBreak <- br("")
  }      
  fullMenu <- paste(menuHeader, menuBreak, modelMenu)
  return(renderUI({HTML(fullMenu)}))
}

#' @title Make a kFixed selection menu
#'
#' @description Produce a size-based kFixed selection menu object based on
#'   column selection
#'
#' @param rv reactive values list
#'
#' @return rendered HTML k selection menu object
#'
#' @export
#'
makekFixedInput <- function(rv){
  sizeclasses <- rv$sizeclasses_k
  nsizeclasses <- length(sizeclasses)

  if (nsizeclasses == 1){
    fullk <- fluidRow(
               column(width = 4, align = "center", b("Fix k?")), 
               column(width = 4, align = "center", b("Value"))
             )
    mvText <- "kFixed_val_1" 
    mynText <- "kFixed_yn_1" 
    speck <- fluidRow(
               column(width = 4, align = "center", 
                 checkboxInput("kFixed_yn_1", ""),
                 style(type='text/css', "#kFixed_yn_1 { margin-top: 5px;}")
               ), 
               column(width = 4, align = "center", 
                 numericInput("kFixed_val_1", "", value = "", 
                   min = 0, max = 1, step = 0.001),
                 style(type='text/css', "#kFixed_val_1 { margin-top: -15px;}")
               )
             )
 
      fullk <- paste(fullk, speck)
  }

  if (nsizeclasses > 1){
    fullk <- fluidRow(
             column(width = 2, div("")),
             column(width = 4, align = "center", b("Fix k?")), 
             column(width = 4, align = "center", b("Value"))
           )
    for(sci in 1:nsizeclasses){

      mvText <- paste0("kFixed_val_", sci) 
      mynText <- paste0("kFixed_yn_", sci) 
      scText <- paste0(sizeclasses[sci], ":")
      rowName <- paste0("string_", sci)
      speck <- fluidRow(
                 column(width = 1, div("")),
                 column(width = 1, 
                   div(id = rowName, b(scText)), align = "right",
                   style(type='text/css', 
                     paste0("#", rowName, " { margin-top: 10px;}"))
                 ),
                 column(width = 4, align = "center", 
                   checkboxInput(mynText, ""),
                   style(type='text/css', 
                     paste0("#", mynText, " { margin-top: 5px;}"))
                 ), 
                 column(width = 4, align = "center", 
                   numericInput(mvText, "", value = "", min = 0, max = 1,
                     step = 0.001),
                   style(type='text/css', 
                   paste0("#", mvText, " { margin-top: -15px;}")))
               )
 
      fullk <- paste(fullk, speck)
    }
  }
  renderUI({HTML(fullk)})
}


#' @title Make a kFill selection menu
#'
#' @description Produce a size-based kFill selection menu object based on
#'   column selection
#'
#' @param rv reactive values list
#'
#' @param type "g" or "M"
#'
#' @return rendered HTML k selection menu object
#'
#' @export
#'
makekFillInput <- function(rv, type = "g"){
  sizeclasses <- rv$sizeclasses_k
  nsizeclasses <- length(sizeclasses)
  addin <- switch(type, "g" = "g_", "M" = NULL)

  if (nsizeclasses == 1){
    fullk <- ""
    mvText <- paste0("kFill_", addin, 1) 
    speck <- paste(
             fluidRow(
               column(width = 10,  
                 div(id = "kFill_yn_1", b("Assumed k:")),
                 style(type='text/css', "#kFill_yn_1 { margin-top: 0px;}")
               )), fluidRow(
               column(width = 12, align = "center", 
                 numericInput(mvText, "", value = "", 
                   min = 0, max = 1, step = 0.001),
                 style(type='text/css', 
                     paste0("#", mvText, 
                       " {margin-top: -15px;margin-bottom: 15px;}")
               ))
             )
)
 
      fullk <- paste(fullk, speck)
  }

  if (nsizeclasses > 1){
    fullk <- fluidRow( 
               column(width = 6, div(id = "kfillHead", b("Assumed k(s):")),
                 style(type='text/css', "#kfillHead { margin-bottom: 4px;}")
               )
             )
    for(sci in 1:nsizeclasses){
      if (is.na(rv$kFixed[sci])){
        mvText <- paste0("kFill_", addin, sci) 
        scText <- paste0(sizeclasses[sci], ":")
        rowName <- paste0("string_", sci)
        speck <- fluidRow(
                   column(width = 1, div("")),
                   column(width = 1, 
                     div(id = rowName, b(scText)), align = "right",
                     style(type='text/css', 
                       paste0("#", rowName, " { margin-bottom: 10px;}"))
                   ),
                   column(width = 4, align = "center", 
                     numericInput(mvText, "", value = "", min = 0, max = 1,
                       step = 0.001),
                     style(type='text/css', 
                     paste0("#", mvText, 
                       " {margin-top: -15px;margin-bottom: 5px;}")))
                 )
 
        fullk <- paste(fullk, speck)
      }
    }
    fullk <- paste(fullk, br())
  }
  renderUI({HTML(paste(fullk))})
}



#' @title HTML style function
#'
#' @description Generate HTML inline style for an element
#'
#' @param ... attributes and children of the element
#'
#' @return HTML style for use with a specific element
#'
#' @export
#'
style <- function(...){
  tags$style(...)
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
#' @return HTML bolded
#'
#' @export
#'
b <- function(...){
  tags$b(...)
}


#' @title HTML u function
#'
#' @description Generate an HTML underlined text element. This simply pulls 
#'   the function definition from the tag environment in htmltools
#'
#' @param ... attributes and children of the element
#'
#' @return HTML underlined
#'
#' @export
#'
u <- function(...){
  tags$u(...)
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

#' @title Shiny Java Script (via shinyjs) enabler
#'
#' @description Enable the use of the \code{shinyjs} package within GenEst.
#'   Simply a wrapper to bring function into namespace.
#'
#' @param ... to be passed down
#'
#' @export
#'
GenEstShinyJS <- function(...){
  useShinyjs(...)
}

#' @title Inline CSS definition
#'
#' @description Define the inline CSS code for GenEst.
#'
#' @param ... to be passed down
#'
#' @export
#'
GenEstInlineCSS <- function(...){
  inlineCSS(
    list(".shiny-input-container" = "margin-bottom: 0px", 
         "#file_SE_progress" = "margin-bottom: 2px", 
         "#file_CP_progress" = "margin-bottom: 2px",
         "#file_SS_progress" = "margin-bottom: 2px",
         "#file_DWP_progress" = "margin-bottom: 2px",
         "#file_CO_progress" = "margin-bottom: 2px",
         "#file_SE_clear" = "margin-bottom: 20px", 
         "#file_CP_clear" = "margin-bottom: 20px",
         "#file_SS_clear" = "margin-bottom: 20px",
         "#file_DWP_clear" = "margin-bottom: 20px",
         "#file_CO_clear" = "margin-bottom: 20px",
         "#nsim" = "margin-bottom: 15px",
         "#CL" = "margin-bottom: 15px",
         "#frac" = "margin-bottom: 15px",
         "#gSearchInterval" = "margin-bottom: 15px",
         "#kFill" = "margin-bottom: 15px",
         "#gSearchMax" = "margin-bottom: 20px",
         "#kFill_g" = "margin-bottom: 15px",
         "#useSSdata" = "margin-bottom: 15px",
         "#runMod_SE" = "margin-bottom: 10px",
         "#runMod_CP" = "margin-bottom: 10px",
         "#runMod_M" = "margin-bottom: 10px",
         "#splitM" = "margin-bottom: 10px",
         "#runMod_g" = "margin-bottom: 10px",
         "#runMod_SE_clear" = "margin-bottom: 20px",
         "#runMod_CP_clear" = "margin-bottom: 20px",
         "#runMod_M_clear" = "margin-bottom: 20px",
         "#runMod_g_clear" = "margin-bottom: 20px",
         "#split_CO" = "margin-bottom: 15px"
    ), ...
  )
}

#' @title Define the style for the clear buttons
#'
#' @description Define the style tag for clear buttons used throughout GenEst.
#'
#' @return character element defining the style
#'
#' @export
#'
cButtonStyle <- function(){
  "padding:4px; font-size:80%; background-color: #FFCD72"
}