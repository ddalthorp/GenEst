#' @title Create the HTML for the navigation bar UI element 
#'
#' @description create the HTML code for the navigation bar in the GenEst app.
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

#' @title HTML tag functions
#'
#' @description This suite of functions are used for producing specific
#'   HTML tags, in complement to those imported from the htmltools package.
#'   \cr \cr \code{style}: in-line style.
#'
#' @param ... attributes and children of the element
#'
#' @return HTML tagged elements
#'
#' @export
#'
style <- function(...){
  tags$style(...)
}

#' @rdname style
#'
#' @description \code{ol}: ordered list.
#'
#' @export
#'
ol <- function(...){
  tags$ol(...)
}

#' @rdname style
#'
#' @description \code{ul}: unordered list.
#'
#' @export
#'
ul <- function(...){
  tags$ul(...)
}

#' @rdname style
#'
#' @description \code{li}: list element.
#'
#' @export
#'
li <- function(...){
  tags$li(...)
}

#' @rdname style
#'
#' @description \code{b}: bolded text.
#'
#' @export
#'
b <- function(...){
  tags$b(...)
}

#' @rdname style
#'
#' @description \code{u}: underlined text.
#'
#' @export
#'
u <- function(...){
  tags$u(...)
}

#' @rdname style
#'
#' @description \code{small}: small text.
#'
#' @export
#'
small <- function(...){
  tags$small(...)
}

#' @rdname style
#'
#' @description \code{big}: big text.
#'
#' @param text text to wrap in the tag
#'
#' @export
#'
big <- function(text = NULL){
  HTML(paste0("<big>", text, "</big>"))
}

#' @rdname style
#'
#' @description \code{center}: center-justified text.
#'
#' @export
#'
center <- function(text = NULL){
  HTML(paste0("<center>", text, "</center>"))
}

#' @title Generate the Inline CSS Definition
#'
#' @description Defines the inline CSS code for the GenEst app.
#'
#' @param ... Arguments to be passed down to \code{\link[shinyjs]{inlineCSS}}.
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
         "#sizeCol" = "margin-bottom: 15px",
         "#obsSE" = "margin-bottom: 15px",
         "#predsSE" = "margin-bottom: 15px",
         "#ltp" = "margin-bottom: 15px",
         "#fta" = "margin-bottom: 15px",
         "#predsCP" = "margin-bottom: 15px",
         "#frac" = "margin-bottom: 15px",
         "#useSSinputs" = "margin-bottom: 20px",
         "#gSearchInterval" = "margin-bottom: 15px",
         "#kFill" = "margin-bottom: 15px",
         "#gSearchMax" = "margin-bottom: 20px",
         "#kFill_g" = "margin-bottom: 15px",
         "#useSSdata" = "margin-bottom: 15px",
         "#run_SE" = "margin-bottom: 10px",
         "#run_CP" = "margin-bottom: 10px",
         "#run_M" = "margin-bottom: 10px",
         "#split_M" = "margin-bottom: 10px",
         "#run_g" = "margin-bottom: 10px",
         "#run_SE_clear" = "margin-bottom: 20px",
         "#run_CP_clear" = "margin-bottom: 20px",
         "#run_M_clear" = "margin-bottom: 20px",
         "#run_g_clear" = "margin-bottom: 20px",
         "#split_CO" = "margin-bottom: 15px"
    ), ...
  )
}

#' @title Shiny Java Script (via shinyjs) Enabler
#'
#' @description Enable the use of the \code{shinyjs} package within GenEst.
#'   This function is a simple wrapper to bring 
#'   \code{\link[shinyjs]{useShinyjs}} into the namespace.
#'
#' @param ... Arguments to be passed down to 
#'   \code{\link[shinyjs]{useShinyjs}}.
#'
#' @export
#'
GenEstShinyJS <- function(...){
  useShinyjs(...)
}

#' @title Define Clear Button Style
#'
#' @description Define the style tag for clear buttons used throughout GenEst.
#'
#' @param buttonType "single" (for clearing a single component) or "all" (for
#'   clearing everything).
#'
#' @return Character element defining the style.
#'
#' @export
#'
cButtonStyle <- function(buttonType = "single"){

  if (!buttonType %in% c("single", "all")){
    stop(paste0("button Type ", buttonType, " not supported."))
  }
  if (buttonType == "single"){
    "padding:4px; font-size:80%; background-color: #FFCD72"
  } else if (buttonType == "all"){
    "padding:6px; font-size:90%; background-color: #FF8484;
     align: center; margin-top: 20px"
  }
}



