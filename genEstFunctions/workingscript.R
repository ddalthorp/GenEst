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
      SSdataIn <- read.csv("ExampleSearchSchedule.csv")
      COdataIn <- read.csv("ExampleCarcassObservations.csv")       

    # input number of iterations

      Niterations <- 1000

 
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

    # table outputs

      SEmodsAICtab <- AICtabcreateSEmods(SEmods, sortby = "AIC")

    # plot the results
    # indexed by size class (r) and model (j)

      SEgraphcreate(SEdata = SEdataIn, SEvars, thetaSE, obscols = SEobscols,  
                              Niterations, sizeclasscol = SEsizeclasscol, 
                              r = 4, j = 1, CellWiseModel = 25)

  #
  # Carcass Persistence
  #

    # select predictors size class column and observation columns

      CPvars <- c("Visibility", "GroundCover")
      CPsizeclasscol <- "Size"
      CPltp <- "LastPresentDecimalDays"
      CPfta <- "FirstAbsentDecimalDays"
	
    # run the estimator for each of the possible models for each size class

      CPmods <- CPmodsacrosssizes(CPdata = CPdataIn, 
                                   CPvars = CPvars,
                                   sizeclasscol = CPsizeclasscol,
                                   CPltp = CPltp,
                                   CPfta = CPfta)

    # create a theta for each cell within each each model in each size class
    #   dimension: [Niterations, 2, Ncells(CP), Nmodels(CP), Nsizeclasses]

      thetaCP <- ThetaCPcreateacrosssizes(CPdata = CPdataIn, CPvars = CPvars,
                                          sizeclasscol = CPsizeclasscol,
                                          CPmods, Niterations)

    # table outputs

      CPmodsAICtab <- AICtabcreateCPmods(CPmods, sortby = "AIC")

    # plot the results

       CPgraphcreate(CPmods, CPdata = CPdataIn, CPvars, thetaCP, 
           Niterations, timeunit = "days", sizeclasscol = CPsizeclasscol,
           CPltp = CPltp, CPfta = CPfta, r = 1, 
		   modelcomplexity = 1, distchoice = 1)


  #
  # select models to use
  #

    CPmodstouse <- c(1, 1, 1, 1)
    SEmodstouse <- c(1, 1, 1, 1)


  #
  # estimate g
  #

    # prep search schedules

      SSs <- SSveccreate(SSdata = SSdataIn)

    # garray
    #  dimension: [Niterations, 1, Nsearchschedules, 
    #                Nmodels(SExCP), Nsizeclasses]

      garray <- gcreateacrosssizes(CPdata = CPdataIn, SEdata = SEdataIn, 
                                SSdata = SSs, 
                                CPvars = CPvars, SEvars = SEvars, 
                                thetaCP, thetaSE, CPmods,
                                SEmodstouse, CPmodstouse)

    # summarize g distributions

      gtable <- gtablecreate(garray, CIw = 0.9)


  #
  # estimate Mhat
  #
 
    # create PWAS table

      PWASdatatab <- PWAStablecreate(SSdata = SSdataIn)

    # Mhatarray
    #  dimension: [Niterations, Nss, Nunits, Nsplitcats, Nsizeclasses]

       Mhatarray <- Mhatgenerator(COdata = COdataIn, PWASdata = PWASdatatab, 
                            sizeclasscol = "Size", splitcol = "Split", 
                            unitcol = "Unit", sscol = "SearchSchedule",
                            seedset = 124, CPvars = CPvars, 
                            SEvars = SEvars, CPdata = CPdataIn, 
                            SEdata = SEdataIn, garray = garray) 

    # condense Mhat to split categories
    #   dimension: [Niterations, Nsplitcats]
    
      Mhatsc <- Mhatcondense(Mhatarray)

    # produce Mhat table
    #   allows for expansion to whole facility 
    #    (ffs = fraction facility sampled)

      Mhattab <- Mhattable(Mhatl = Mhatsc, ffs = .85, CIw = 0.9)

    # plot Mhat for each split

      l <- 1
      Mhatgraph(Mhatlspecific = Mhatsc[,l], 
	              splitcatname = colnames(Mhatsc)[l], ffs = .85)
      l <- 2
      Mhatgraph(Mhatlspecific = Mhatsc[,l], 
	  	              splitcatname = colnames(Mhatsc)[l], ffs = .85)

