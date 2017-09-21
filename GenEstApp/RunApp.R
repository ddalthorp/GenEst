################################################################################
#
#    This script deploys a local version of the GenEst application
#
#    version 0.0.0.2 September 2017
#
#    Held under GNU GPL v >= 3	
#
################################################################################
  # show disclaimer
    msg0 <- "This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software."
    tcltk::tkmessageBox(message = msg0, title = "GenEst, v0.0.0.2, September 2017")

  # load dependencies

    source("genEstfunctions.R")
    packageLoad()


  # run application 

    runApp()

