################################################################################
#
#	this script is currently focused on pulling all of the components of the 
#       analysis together. as of now, this includes analyses for SE and CP and 
#       creation of g
#	it is flexible to 0, 1, or 2 factors for each , including overlap and not
#	k can be fixed at any value 
#		if fixed at 0 tho, can only give one col of observations!
#
################################################################################



  # general inputs

    # source code

      library(survival)
      library(mvtnorm)
      library(matrixStats)
      library(gsl)
      source("genEstFunctions.R")

    # read in data

      SEdataIn <- read.csv("ExampleSearcherEfficiency.csv")
      CPdataIn <- read.csv("ExampleCarcassPersistence.csv")
      SSdataIn <- read.csv("ExampleSearchSchedule.csv", header = F)
      PWASdataIn <- read.csv("ExampleProportionWeightedAreaSearched.csv")
      COdataIn <- read.csv("ExampleCarcassObservations.csv")       


    # input number of iterations

      Niterations <- 1000


  # for timing

    t1 <- Sys.time()

  # Searcher Efficiency

    # select predictors, observation columns, and size class column

      SEpvars <- c("Season", "HabitatType")
      SEobscols <- 8:11
      SEsizeclasscol <- "Size"
  
    # fix k?

      fixKchoice <- "NO"
      fixKvalchoice <- NULL

    # run the estimator for each of the possible models for each size class

      SEmods <- SEmodsacrosssizes(SEdata = SEdataIn, obscols = SEobscols,
                                   pvars = SEpvars,
                                   sizeclasscol = SEsizeclasscol,
                                   fixK = fixKchoice , fixKval = fixKvalchoice, 
                                   initKval = 0.7
                                  )

    # select models

      SEmodstouse <- c(1, 1, 1, 1)
		
    # create a theta for each cell within each size class

      thetaSE <- ThetaSEcreateacrosssizes(SEdata = SEdataIn, pvars = SEpvars,
                                  sizeclasscol = SEsizeclasscol,
                                  SEmods, SEmodstouse, Niterations,
                                  fixK = fixKchoice, fixKval = fixKvalchoice)


  # Carcass Persistence

    # select predictors and size class column

      CPpvars <- c("Visibility", "GroundCover")
      CPsizeclasscol <- "Size"
	
    # run the estimator for each of the possible models for each size class

      CPmods <- CPmodsacrosssizes(CPdata = CPdataIn, 
                                   pvars = CPpvars,
                                   sizeclasscol = CPsizeclasscol)

    # select models

      CPmodstouse <- c(1, 1, 1, 1)

    # create a theta for each cell within each size class

      thetaCP <- ThetaCPcreateacrosssizes(CPdata = CPdataIn, pvars = CPpvars,
                                          sizeclasscol = CPsizeclasscol,
                                          CPmods, CPmodstouse, Niterations)


  # combine with search schedules to get g

    garray <- gcreateacrosssizes(CPdata = CPdataIn, SEdata = SEdataIn, 
                                SSdata = SSdataIn, 
                                CPvars = CPpvars, SEvars = SEpvars, 
                                thetaCP, thetaSE, CPmods, CPmodstouse)

  # estimate Mhat
  #   dimension: [Niterations, Nss, Nturbines, Nsplitcats, Nsizeclasses]

     Mhatarray <- Mhatgenerator(COdata = COdataIn, PWASdata = PWASdataIn, 
                            sizeclasscol = "Size", splitcol = "Split", 
                            turbinecol = "Turbine", sscol = "SearchSchedule",
                            seedset = 1234, CPvars = CPpvars, 
                            SEvars = SEpvars, CPdata = CPdataIn, 
                            SEdata = SEdataIn, garray = garray) 

  # for timing

    t2 <- Sys.time()

  t2 - t1
