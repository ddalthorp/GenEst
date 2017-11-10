################################################################################
#
#  example of a GenEst analysis at the command line
#
#  version 0.0.0.5 November 2017
#
#  Held under GNU GPL v >= 3	
#
################################################################################

  #
  # general inputs
  #

    # source code

      source("genestfunctions.R")
      packageLoad()

    # read in data

      SEdataIn <- read.csv("www/ExampleSearcherEfficiency.csv")
      CPdataIn <- read.csv("www/ExampleCarcassPersistence.csv")
      SSdataIn <- read.csv("www/ExampleSearchSchedule.csv")
      COdataIn <- read.csv("www/ExampleCarcassObservations.csv")       

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

      SEmods <- SEmodsetsacrosssizes(data = SEdataIn, vars = SEvars, 
                                   obscols = SEobscols, 
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

      SEmodsAICtab <- AICtabcreateSEmods(SEmods)

    # plot the results
    # indexed by size class (r) and model (j)

      SEgraphcreate(SEdata = SEdataIn, SEvars, thetaSE, obscols = SEobscols,  
                              Niterations, sizeclasscol = SEsizeclasscol, 
                              r = 4, j = 1, CellWiseModel = 25)

  #
  # Carcass Persistence
  #

    # select predictors size class column and observation columns

      CPvars <- NULL
      CPsizeclasscol <- "Size"
      CPltp <- "LastPresentDecimalDays"
      CPfta <- "FirstAbsentDecimalDays"
	
    # run the estimator for each of the possible models for each size class

      CPmods <- CPmodsetsacrosssizes(data = CPdataIn, vars = CPvars, 
                                     sizeclasscol = CPsizeclasscol, 
                                     ltpc = CPltp, ftac = CPfta)

    # create a theta for each cell within each each model in each size class
    #   dimension: [Niterations, 2, Ncells(CP), Nmodels(CP), Nsizeclasses]

      thetaCP <- ThetaCPcreateacrosssizes(CPdata = CPdataIn, CPvars = CPvars,
                                          sizeclasscol = CPsizeclasscol,
                                          CPmods, Niterations)

    # table outputs

      CPmodsAICtab <- AICtabcreateCPmods(CPmods)

    # plot the results

       CPgraphcreate(CPmods, CPdata = CPdataIn, CPvars, thetaCP, 
           Niterations, timeunit = "days", sizeclasscol = CPsizeclasscol,
           CPltp = CPltp, CPfta = CPfta, r = 1, 
		   modelcomplexity = "~ 1", distchoice = "exponential")


  #
  # select models to use
  #

    CPmodstouse <- c(1, 1, 1, 1)
    SEmodstouse <- c(1, 1, 1, 1)


  #
  # estimate g
  #

    # garray
    #  dimension: [Niterations, Nsearchschedules, 
    #                Nmodels(SExCP), Nsizeclasses]

      garray <- gcreateacrosssizes(CPdata = CPdataIn, SEdata = SEdataIn, 
                                SSdata = SSdataIn, Niterations, 
                                CPvars = CPvars, SEvars = SEvars, 
                                thetaCP, thetaSE, CPmods,
                                SEmodstouse, CPmodstouse)

    # summarize g distributions

      gtable <- gtablecreate(garray, CL = 0.9)


  #
  # estimate Mhat
  #

    # Mhatarray
    #  dimension: [Niterations, Nunits, Nsplitcats, Nsizeclasses]

       Mhatarray <- Mhatgenerator(COdata = COdataIn, SSdata = SSdataIn, 
                            sizeclasscol = "Size", splitcol = "Split", 
                            unitcol = "Unit", dfcol = "DateFound",
                            Niterations,  
                            CPvars = CPvars, 
                            SEvars = SEvars, CPdata = CPdataIn, 
                            SEdata = SEdataIn, garray = garray) 

    # condense Mhat to split categories
    #   dimension: [Niterations, Nsplitcats]
    
      Mhatsc <- Mhatcondense(Mhatarray)

    # produce Mhat table
    #   allows for expansion to whole facility 
    #    (ffs = fraction facility sampled)

      Mhattab <- Mhattable(Mhatl = Mhatsc, ffs = 0.85, CL = 0.9)

    # plot Mhat for each split

      par(mfrow = c(1, 2))
      l <- 1
      Mhatgraph(Mhatlspecific = Mhatsc[,l], 
	              splitcatname = colnames(Mhatsc)[l], ffs = 0.85)
      l <- 2
      Mhatgraph(Mhatlspecific = Mhatsc[,l], 
	  	              splitcatname = colnames(Mhatsc)[l], ffs = 0.85)

