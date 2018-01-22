##############################################################################
#
# package development code for GenEst
#


  # load devtools

    library(devtools)


  # set the package location

    pkgloc <- getwd()


  # Set up files

  #
  #  NOTE: This code only needs to be run once!
  #         Currently commented out
  #

  #  # create the barebones package structure 
  
  #    devtools::create(pkgloc)

  #  # create the bare bones vignette

  #    devtools::use_vignette("command-line-example", pkgloc)


  # add dependencies to the description and load them here 

    pkgdpns <- c("shiny", "rhandsontable", "httr", "survival", 
                 "mvtnorm", "matrixStats", "gsl")

    for(i in 1:length(pkgdpns)){
      devtools::use_package(pkgdpns[i], "Imports", pkgloc)
    }


  devtools::load_all(devtools::as.package(pkgloc))
  devtools::document(devtools::as.package(pkgloc))


  devtools::build_vignettes(pkgloc)
  devtools::build(pkgloc, binary = T)

  devtools::load_all(devtools::as.package(pkgloc))
  runGenEst()
