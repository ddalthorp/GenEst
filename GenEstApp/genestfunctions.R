###############################################################################
#
# This script contains the functions of the GenEst package
#
# Coded by Joseph (Josie, they) L. Simonis, DAPPER Stats
#
# version 0.0.0.1 2017
#
#
###############################################################################

# need to format everything in the headers better!

#' TITLEHERE
#'
#' DESCRIPTIONHERE
#' @param 
#' @return
#' @keywords 
#' @export
#' @examples
#' 
#


###############################################################################
# packageLoad	
#  function for checking package needs and downloading, then loading
#
# inputs
#  none
#
# outputs
#  none
###############################################################################
	
packageLoad <- function(...){

  # presently installed local packages

    lps <- installed.packages()[,"Package"]

  # packages needed from CRAN

    crpks <- c("devtools", "shiny", "rhandsontable", "httr")
    for(i in 1:length(crpks)){
      if(!(crpks[i] %in% lps)){
	  install.packages(crpks[i])
      }
    }
    library(devtools)
    library(shiny)
    library(rhandsontable)

  # pakages needed from github

    # workaround the firewall

      set_config(config(ssl_verifypeer = 0L))

    if(!("shinysky" %in% lps)){
      install_github("AnalytixWare/ShinySky")
    }
    library(shinysky)

  # return
	
    return()
}
