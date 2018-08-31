# GenEst Deploy
This is a repository for deploying the GenEst app on shinyapps.io.

The following instructions assume that you have already created a shinyapps.io
account and have configured **rsconnect** to work with your account.

1. Download of the suite of dependency packages:

```
packages <- c("cbinom", "devtools", "digest", "DT", "gsl", "gtools", 
                 "htmltools", "lubridate", "matrixStats", "mvtnorm", "Rcpp",
                 "rsconnect", "shiny", "sticky", "survival")
packages <- package_new[!(packages %in% installed.packages()[,"Package"])] 
if(length(packages) > 0){
  install.packages(packages)
}
```

2. Download the newest version of **GenEst** from GitHub:

```
devtools::install_github("ddalthorp/genest")
```

3. Clone this repository to your local machine, and set the working directory
to the `/GenEst` folder within the repository's location on your local machine.


4. Test that the app can build locally:

```
shiny::runApp()
```

5. Deploy to shinyapps.io:

```
rsconnect::deployApp()
```
