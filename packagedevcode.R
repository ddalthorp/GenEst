
  library(devtools)

  pkgloc <- getwd()

  pkgdpns <- c("shiny", "shinythemes", "rhandsontable", "httr", "survival", 
                "mvtnorm", "matrixStats", "gsl", "DT")

  for(i in 1:length(pkgdpns)){
    devtools::use_package(pkgdpns[i], "Imports", pkgloc)
  }

  devtools::load_all(devtools::as.package(pkgloc))
  devtools::document(devtools::as.package(pkgloc))

  devtools::build_vignettes(pkgloc)
  devtools::build(pkgloc, binary = T)

  devtools::load_all(devtools::as.package(pkgloc))
  runGenEst()
