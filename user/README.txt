# GenEst: Generalized Fatality Estimator    
GenEst is a tool for estimating mortalities from efficiency, persistence,
and carcass data.

############## DISCLAIMER ##################
This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

Installation
With this preliminary version of GenEst, setup and installation require several steps. Do not skip any steps.


### REQUIRED: Updated version of R (>= 3.5.0, released on 23 April 2018)
R is free and open source software for statistical computing. If R is not installed on your computer or if your version of R is older than 3.5.0, download and install the latest version from https://cran.r-project.org/, following the instructions provided at the site. If you already have an older copy of R installed on your computer, the new version will be installed alongside the old. Unless you know a reason why you want to keep both versions, it is usually a good idea to uninstall the old version to avoid confusion and clutter. 

### REQUIRED: Third-party packages
Several third-party pacakges are required; all are free and open source and available from CRAN (https://cran.r-project.org/). The easiest way to install them is to run the following commands (copy-and-paste) in R (with guidance concerning potential dialog boxes given below the commands):

   package_new <- c("cbinom", "digest", "DT", "gsl", "gtools", "htmltools", "lubridate", 
      "matrixStats", "mvtnorm", "Rcpp", "shiny", "shinydashboard", "sticky", "survival")
   package_new <- package_new[!(package_new %in% installed.packages()[,"Package"])] 
   if(length(new_packages) > 0) install.packages(new_packages)

-- If asked about a "CRAN mirror", choose the nearest location.
-- If asked whether you want to use a "personal library instead", choose "Yes"
-- If you are on Windows and are asked whether you want to install packages and their 
   dependencies "ffrom source", choose "No" (unless you are ready to go to lunch, in which case, 
    you can select "Yes" and the installation may well be done by the time you get back).

### REQUIRED: GenEst package

-- For Windows, download the compressed folder GenEst_0.1.0.zip (do not unzip) and install from the local .zip folder by running the following command in R:

   install.packages(file.choose()) # <-- select GenEst_0.1.0.zip to install

-- For Mac OS or Unix-like OS, download the compressed file GenEst_0.1.0.tar.gz and install from the local .tar.gz file by running the following command in R:

   install.packages(file.choose()) # <-- select GenEst_0.1.0.tar.gz to install


######## Getting Started

# Graphical user interface (GUI): easy-to-use buttons and menus
To start the GUI, open R and enter the command:

   library(GenEst)
   runGenEst()

Download the User Guide to learn about opening GenEst, data requirements, examples, etc.

# R command line: more functionality and flexibility for the experienced R user
To get started, run the following in R:

   library(GenEst)
   browseVignettes("GenEst")

Also, help files for GenEst functions are accessible in the standard R way, for example:

   ?pkm
   help(pkm)

An easy-to-use index of functions can be found via:
   ?GenEst
