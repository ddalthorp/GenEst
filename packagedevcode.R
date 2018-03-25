
pkgloc <- getwd()
pkgdpns <- c("shiny", "shinythemes", "httr", "survival", "EnvStats",
               "mvtnorm", "matrixStats", "gsl", "DT", "numDeriv")

for(i in 1:length(pkgdpns)){
  devtools::use_package(pkgdpns[i], "Imports", pkgloc)
}

devtools::load_all()
devtools::document()

devtools::build_vignettes()
devtools::build(binary = T)

devtools::load_all()
runGenEst()
