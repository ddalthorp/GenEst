################################################################################
#
#	example of a GenEst analysis
#
################################################################################

  #
  # general inputs
  #

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


  # 
  # Create output directory
  #

    dir.create("Output")

  #
  # Searcher Efficiency
  #

    # select predictors, observation columns, and size class column

      SEvars <- c("Season", "HabitatType")
      SEobscols <- 8:11
      SEsizeclasscol <- "Size"
  
    # fix k?

      fixKchoice <- "NO"
      fixKvalchoice <- NULL

    # run the estimator for each of the possible models for each size class

      SEmods <- SEmodsacrosssizes(SEdata = SEdataIn, obscols = SEobscols,
                                   SEvars = SEvars,
                                   sizeclasscol = SEsizeclasscol,
                                   fixK = fixKchoice, fixKval = fixKvalchoice, 
                                   initKval = 0.7)

    # create a theta for each cell within each each model in each size class
    #   dimension: [Niterations, 2, Ncells(SE), Nmodels(SE), Nsizeclasses]

      thetaSE <- ThetaSEcreateacrosssizes(SEdata = SEdataIn, SEvars = SEvars,
                                  sizeclasscol = SEsizeclasscol,
                                  SEmods, Niterations,
                                  fixK = fixKchoice, fixKval = fixKvalchoice)

    # create output subdirectory

      dir.create("Output/SE")

    # table outputs

      SEmodsAICtab <- AICtabcreateSEmods(SEmods, sortby = "AIC")

    # plot the results

       SEgraphscreate(SEmods, SEdata = SEdataIn, SEvars, thetaSE, SEobscols, 
                              Niterations, sizeclasscol = SEsizeclasscol)


  #
  # Carcass Persistence
  #

    # select predictors and size class column

      CPvars <- c("Visibility", "GroundCover")
      CPsizeclasscol <- "Size"
	
    # run the estimator for each of the possible models for each size class

      CPmods <- CPmodsacrosssizes(CPdata = CPdataIn, 
                                   CPvars = CPvars,
                                   sizeclasscol = CPsizeclasscol, 
                                   unitchoice = "days")

    # create a theta for each cell within each each model in each size class
    #   dimension: [Niterations, 2, Ncells(CP), Nmodels(CP), Nsizeclasses]

      thetaCP <- ThetaCPcreateacrosssizes(CPdata = CPdataIn, CPvars = CPvars,
                                          sizeclasscol = CPsizeclasscol,
                                          CPmods, Niterations)

    # create output subdirectory

      dir.create("Output/CP")

    # table outputs

      CPmodsAICtab <- AICtabcreateCPmods(CPmods, sortby = "AIC")

    # plot the results

       CPgraphscreate(CPmods, CPdata = CPdataIn, CPvars, thetaCP, 
           Niterations, unitchoice = "days", sizeclasscol = CPsizeclasscol)



  #
  # select models to use
  #

    CPmodstouse <- c(1, 1, 1, 1)
    SEmodstouse <- c(1, 1, 1, 1)


  #
  # estimate g
  #

    # garray
    #  dimension: [Niterations, 1, Nsearchschedules, 
    #                Nmodels(SExCP), Nsizeclasses]

      garray <- gcreateacrosssizes(CPdata = CPdataIn, SEdata = SEdataIn, 
                                SSdata = SSdataIn, 
                                CPvars = CPvars, SEvars = SEvars, 
                                thetaCP, thetaSE, CPmods,
                                SEmodstouse, CPmodstouse)

    # create output subdirectory

      dir.create("Output/g")

    # summarize g distributions

      gtable <- gtablecreate(garray, CIw = 0.9)

    # plot g

      ggraphscreate(garray) 


  #
  # estimate Mhat
  #

    # Mhatarray
    #  dimension: [Niterations, Nss, Nunits, Nsplitcats, Nsizeclasses]

       Mhatarray <- Mhatgenerator(COdata = COdataIn, PWASdata = PWASdataIn, 
                            sizeclasscol = "Size", splitcol = "Split", 
                            unitcol = "Unit", sscol = "SearchSchedule",
                            seedset = 124, CPvars = CPvars, 
                            SEvars = SEvars, CPdata = CPdataIn, 
                            SEdata = SEdataIn, garray = garray) 

    # condense Mhat to split categories
    #   dimension: [Niterations, Nsplitcats]
    
      Mhatsc <- Mhatcondense(Mhatarray)

    # create output subdirectory

      dir.create("Output/Mhat")

    # produce Mhat table
    #   allows for expansion to whole facility 
    #    (ffs = fraction facility sampled)

      Mhattab <- Mhattable(Mhatl = Mhatsc, ffs = .85, CIw = 0.9)

    # plot Mhat

      Mhatgraph(Mhatl = Mhatsc, ffs = .85)

