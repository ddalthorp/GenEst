################################################################################
#
# functions for genEst
#
################################################################################


################################################################################
#
# Mhatgenerator
#
################################################################################

  Mhatgenerator <- function(COdata, PWASdata, sizeclasscol, splitcol, 
                      turbinecol, sscol, seedset = 1234, CPvars, SEvars, 
                      CPdata, SEdata, garray, ...){

    # turbines 

      turbcol <- which(colnames(COdata) == turbinecol)
      turbines <- as.character(unique(COdata[ , turbcol]))
      turbines[length(turbines) == 0] <- 1
      Nturbines <- length(turbines)

      turbs <- as.character(COdata[ , turbcol])
      turbs[rep(length(turbs) == 0, nrow(COdata))] <- "1"

    # size classes

      sccol <- which(colnames(COdata) == sizeclasscol)

      sizeclasses <- as.character(unique(COdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(COdata[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(COdata))] <- "1"

    # split categories

      spcol <- which(colnames(COdata) == splitcol)

      splitcats <- as.character(unique(COdata[ , spcol]))
      splitcats[length(splitcats) == 0] <- 1
      Nsplitcats <- length(splitcats)

      splits <- as.character(COdata[ , spcol])
      splits[rep(length(splits) == 0, nrow(COdata))] <- "1"

    # search schedules 

      sschedcol <- which(colnames(COdata) == sscol)
      ssops <- as.character(unique(COdata[ , sschedcol]))
      ssops[length(ssops) == 0] <- 1
      Nss <- length(ssops)

      sscheds <- as.character(COdata[ , sschedcol])
      sscheds[rep(length(sscheds) == 0, nrow(COdata))] <- "1"

    # expand the factors across the models 

      expanded <- crossmodelcells(CPvars = CPpvars, SEvars = SEpvars, 
                                   CPdata = CPdataIn, SEdata = SEdataIn)

      Ncellcombs <- nrow(expanded)

    # set up the X, Xtilde, Mtilde, and Mhat arrays

      Xarray <- array(NA, dim = c(Ncellcombs, Nss, Nturbines, 
                                     Nsplitcats, Nsizeclasses))
      Xtildearray <- array(NA, dim = c(Niterations, Ncellcombs, Nss, Nturbines, 
                                        Nsplitcats, Nsizeclasses))
      Mtildearray <- array(NA, dim = c(Niterations, Ncellcombs, Nss, Nturbines, 
                                        Nsplitcats, Nsizeclasses))
      Mhatarray <- array(NA, dim = c(Niterations, Nss, Nturbines, Nsplitcats, 
                                      Nsizeclasses))


    # fill it in

      EXPinCO <- which(colnames(COdata) %in% colnames(expanded) )
      COinEXP <- which(colnames(expanded) %in% colnames(COdata))

      COpaste <- apply(COdata[, EXPinCO], 1, paste, collapse = "")
      EXPpaste <- apply(expanded[, COinEXP], 1, paste, collapse = "")

      set.seed(seedset)


      for(q in 1:Nsizeclasses){
        for(l in 1:Nsplitcats){
          for(k in 1:Nturbines){    
            for(j in 1:Nss){
              for(i in 1:Ncellcombs){

                spot <- which(COpaste == EXPpaste[i] &
                               sscheds == ssops[j] &
                               turbs == turbines[k] &
                               splits == splitcats[l] &
                               sizes == sizeclasses[q])

                X_specific <- COdata[spot, "Carcasses"]
          
                g_specifics <- garray[1:Niterations, 1, which(ssops== ssops[j]), 
							which(EXPpaste == EXPpaste[i]),
                                          which(sizeclasses == sizeclasses[q])]

              


                Xtilde_specifics <- rbinom(Niterations, X_specific, g_specifics)
                Mtilde_specifics <- Xtilde_specifics/g_specifics

                Xarray[i, j, k, l, q] <- X_specific
                Xtildearray[ , i, j, k, l, q] <- Xtilde_specifics
                Mtildearray[ , i, j, k, l, q] <- Mtilde_specifics


              }

              sumMtilde <- apply(Mtildearray[ , 1:Ncellcombs, j, k, l, q], 
                                  1, sum)

              PWASsccol <- which(colnames(PWASdata) == sizeclasscol)
              PWAStcol <- which(colnames(PWASdata) == turbinecol)
              PWASsscol <- which(colnames(PWASdata) == sscol)

              PWASspot <- which(PWASdata[ , PWASsccol] == sizeclasses[q] &
                                PWASdata[ , PWAStcol] == turbines[k] &
                                PWASdata[ , PWASsscol] == ssops[j] )

              PWAS_specific <- PWASdata[ PWASspot, "PWAS"]

              Mhatarray[ , j, k, l, q] <- sumMtilde / PWAS_specific
            }
          }
        }
      }

    # return 

      return(Mhatarray)

  }



################################################################################
#
# crossmodelcells
#
#     function for combining the predictors from the two models
#
################################################################################

	
  crossmodelcells <- function(CPvars, SEvars, CPdata, SEdata, ...){

      lCPv <- length(CPvars)
      clCPv <- as.character(length(CPvars))

      pv1 <- NULL
      pv2 <- NULL
      pv3 <- NULL
      pv4 <- NULL

      pv1 <- CPvars[1][length(CPvars) > 0]
      pv2 <- CPvars[2][length(CPvars) > 1]
      pv3 <- SEvars[1][length(SEvars) > 0]
      pv4 <- SEvars[2][length(SEvars) > 1]

      lev1 <- as.character(unique(CPdata[, pv1]))
      lev2 <- as.character(unique(CPdata[, pv2]))
      lev3 <- as.character(unique(SEdata[, pv3]))
      lev4 <- as.character(unique(SEdata[, pv4]))

      lev1[length(lev1) == 0] <- 1
      lev2[length(lev2) == 0] <- 1
      lev3[length(lev3) == 0] <- 1
      lev4[length(lev4) == 0] <- 1

      nlev1 <- length(lev1)
      nlev2 <- length(lev2)
      nlev3 <- length(lev3)
      nlev4 <- length(lev4)

      maxlev <- max(c(nlev1, nlev2, nlev3, nlev4))

      combvars <- rep(NA, 4)
      combvarsCP1 <- NA
      combvarsCP2 <- NA
      combvarsSE1 <- NA
      combvarsSE2 <- NA

      combvarsCP1[length(CPvars) > 0] <- CPvars[1]
      combvarsCP2[length(CPvars) > 1] <- CPvars[2]
      combvarsSE1[length(SEvars) > 0] <- SEvars[1]
      combvarsSE2[length(SEvars) > 1] <- SEvars[2]

      combvarsCP <- c(combvarsCP1, combvarsCP2)
      combvarsSE <- c(combvarsSE1, combvarsSE2)

      repeats <- which(combvarsSE %in% combvarsCP)
      combvarsSE[repeats] <- NA

      combmat <- matrix(NA, ncol = 4, nrow = maxlev)
      combmat[ , 1] <- c(lev1, rep(NA, maxlev - nlev1))
      combmat[ , 2] <- c(lev2, rep(NA, maxlev - nlev2))
      combmat[ , 3] <- c(lev3, rep(NA, maxlev - nlev3))
      combmat[ , 4] <- c(lev4, rep(NA, maxlev - nlev4))

      combmat[ , 2 + repeats] <- c("1", rep(NA, maxlev - 1))

      expanded <- expand.grid(na.omit(combmat[,1]), na.omit(combmat[,2]),
                              na.omit(combmat[,3]), na.omit(combmat[,4]))

      colnames(expanded) <- c(combvarsCP, combvarsSE)

      return(expanded)

  }

################################################################################
#
# gcreateacrosssizes
#
################################################################################

  gcreateacrosssizes <- function(CPdata, SEdata, SSdata, CPvars, SEvars, 
                                 thetaCP, thetaSE, CPmods, CPmodstouse, ...){
 

    # combine the factors across the models

      expanded <- crossmodelcells(CPvars, SEvars, CPdata, SEdata)

      Ncombcell <- nrow(expanded)

    # now align those cells with each of the CP and SE cells

      mapmat <- matrix(NA, nrow = Ncombcell, ncol = 3)
      mapmat[ , 1] <- 1:Ncombcell

      SEfct <- factorcombinations(SEpvars, data = SEdata)
      CPfct <- factorcombinations(CPpvars, data = CPdata)

      EXPinSE <- which(colnames(SEfct) %in% colnames(expanded) )
      SEinEXP <- which(colnames(expanded) %in% colnames(SEfct))

      EXPinCP <- which(colnames(CPfct) %in% colnames(expanded) )
      CPinEXP <- which(colnames(expanded) %in% colnames(CPfct))

      for(i in 1:Ncombcell){

        EXPse <- apply(expanded[i, SEinEXP] ,1, paste,  collapse = " ")
        SEops <-  apply(SEfct[ , EXPinSE], 1, paste, collapse = " ")
        mapmat[i , 2] <- which(SEops == EXPse)

        EXPcp <- apply(expanded[i, CPinEXP] , 1, paste, collapse = " ")
        CPops <-  apply(CPfct[ , EXPinCP], 1, paste, collapse = " ")
        mapmat[i , 3] <- which(CPops == EXPcp)			


      }

      colnames(mapmat) <- c("CombCell", "SEcell", "CPcell")

    # search schedules

      Nss <- nrow(SSdata)				

    # Nsizeclasses

      Nsizeclasses <- length(CPmods)

    # set up output array

      garray <- array(NA, dim = c(Niterations, 1, Nss, Ncombcell, Nsizeclasses))


    # estimate g

      for(i in 1:Nsizeclasses){

        for(j in 1:Ncombcell){

          for(k in 1:Nss){

            specificSS <- as.numeric(na.omit(as.numeric(SSdata[k, ])))

            specificCPcell <- mapmat[j, "CPcell"]
            specificCPtheta <- thetaCP[ , , specificCPcell, i]
            specificCPdist <- CPmods[[i]][[CPmodstouse[i]]]$dist

            specificSEcell <- mapmat[j, "SEcell"]
            specificSEtheta <- thetaSE[ , , specificSEcell, i]

            gvals <- gvec(days = specificSS, CPab = specificCPtheta, 
                           persdist = specificCPdist, seef = specificSEtheta)
				
            garray[ , , k, j, i ] <- gvals

          }
        }
      }


    # return
      
      return(garray)

  }

################################################################################
#
# CPmodsacrosssizes 
#
################################################################################

  CPmodsacrosssizes <- function(CPdata, sizeclasscol,
                                 pvars, ... ){ 

    # set up the response (surv object)

      dp <- as.Date(as.character(CPdata$DatePlaced), format = "%m/%d/%Y")
      ldp <- as.Date(as.character(CPdata$LastDatePresent), format = "%m/%d/%Y")
      fda <- as.Date(as.character(CPdata$FirstDateAbsent), format = "%m/%d/%Y")

      t1 <- as.numeric(difftime(ldp, dp, units = "days"))
      t1[t1 == 0] <- 0.001
      t2 <- as.numeric(difftime(fda, dp, units = "days"))

      event <- rep(3, length(t1))
      event[which(is.na(t2))] <- 0

      CPobvs_survobj <- Surv(time = t1, time2 = t2, event = event, 
                              type = "interval")

    # set up the predictors

      preds<-rep(NA, 5)
      preds[length(pvars)==0][1]<-c("1")
      preds[length(pvars)==1][1:2]<-c("1", pvars[1])
      preds[length(pvars)==2]<-c("1", pvars[1], pvars[2], paste(pvars[1], 
                                    pvars[2], sep = " + "),
                                    paste(pvars[1], pvars[2], sep = " * "))

      preds <- as.character(na.omit(preds))
      eqs <- paste("~", preds, sep = " ")

    # size classes

      sccol <- which(colnames(CPdata) == sizeclasscol)

      sizeclasses <- as.character(unique(CPdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(CPdata[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(CPdata))] <- "1"

    # set up the factor combinations
 
      CPfct <- factorcombinations(pvars, data = CPdata)

    # select the distributions to use

      distselected <- c("exponential", "weibull", "loglogistic", "lognormal")

    # setting up the models to run

      eqstouse <- rep(eqs, each = length(distselected))
      distouse <- rep(distselected, length(eqs))
      Nmods <- length(eqstouse)

    # for each size class, run the set of models

      CPmods <- vector("list", Nsizeclasses)

      for(i in 1:Nsizeclasses){

        output <- vector("list", Nmods)

        CPobvs_survobj_i <- CPobvs_survobj[sizes == sizeclasses[i]]
        CPdata_i <- CPdata[sizes == sizeclasses[i], ]

        for(j in 1:Nmods){
          mform <- formula(paste("CPobvs_survobj_i" , eqstouse[j], sep = " "))
          output[[j]] <- survreg(mform, CPdata_i, dist = distouse[j])
        }

        CPmods[[i]] <- output

      }


    # return 

      return(CPmods)

  }

################################################################################
#
# ThetaCPcreateacrosssizes
#
################################################################################

  ThetaCPcreateacrosssizes <- function(CPdata, pvars, sizeclasscol, 
                                       CPmods, CPmodstouse, Niterations, ... ){ 

    # set up the factor combination table and count the cells

      CPfct <- factorcombinations(pvars = pvars, data = CPdata)
      Ncells <- nrow(CPfct)

    # size classes

      sccol <- which(colnames(CPdata) == sizeclasscol)
      sizeclasses <- as.character(unique(CPdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

    # create a theta for each combination of factors (cell) within 
    #   each size class

      thetaCP <- array(NA, dim = c(Niterations, 2, Ncells, Nsizeclasses))

    # for each size class

      for(i in 1:Nsizeclasses){

        # select the model

          smod <- CPmods[[i]][[CPmodstouse[i]]]

        # note the distribution

          distforuse <- smod$dist
				
        # pull the mean values and vcv matrix from the selected model 

          means <- c(smod$coef, log(smod$scale))
          vcv <- smod$var

        # in the case of exponential distributions, the vcv from the model is 
        #  missing the scale column and row (because they're all 0s), 
        #  so we add them   
			
          vcv <- cbind(vcv, rep(0, length = length(means) - ncol(vcv)))
          vcv <- rbind(vcv, rep(0, length = length(means) - nrow(vcv)))

        # draw the Niterations of model terms

          modelTermDraws <- rmvnorm(Niterations, means, vcv)

        # create a model matrix to translate model terms to cells 

          mm <- model.matrix(as.formula(paste("~",as.character(formula(smod)[3],
                                        collapse = NULL))), data = CPfct)

        # create the output array (Niterations, 2, Ncells)

          thetaCPi <- array(NA, c(Niterations, 2, Ncells))

        # fill in for each cell

          for(j in 1:Ncells){

            a <- switch(distforuse, 
		      exponential = 
                    rep(1, Niterations), 
		      weibull = 
                    1 / (exp(modelTermDraws[, ncol(modelTermDraws)])),
		      loglogistic = 
                    1 / (exp(modelTermDraws[, ncol(modelTermDraws)])),
		      lognormal = 
                    exp(modelTermDraws[, ncol(modelTermDraws)])^2
		     )
		b <- switch(distforuse, 
		      exponential = 
                    exp(as.matrix(modelTermDraws[ , 1:ncol(mm)]) %*% mm[j,]),
		      weibull = 
                    exp(as.matrix(modelTermDraws[ , 1:ncol(mm)]) %*% mm[j,]),
		      loglogistic = 
                    exp(as.matrix(modelTermDraws[ , 1:ncol(mm)]) %*% mm[j,]),
		      lognormal = 
                    (as.matrix(modelTermDraws[ , 1:ncol(mm)]) %*% mm[j,])
		     )

            thetaCPi[ , , j] <- matrix(c(a, b), byrow = F, ncol = 2)

          }			

        thetaCP[ , , , i] <- thetaCPi

      }

    # return

      return(thetaCP)

  }

################################################################################
#
# SEmodsacrosssizes
#
################################################################################

  SEmodsacrosssizes <- function(SEsizeclasscol, SEdata, sizeclasscol,
                                 fixK, fixKval, initKval,
                                 pvars, obscols, ... ){ 

    # obs cols
    #   zeros: # of times the carcass was missed
    #   found: # of search on which the carcass was found

      zeros <- rowCounts(as.matrix(SEdata[, obscols]), value = 0, na.rm = T)
      foundInd <- which(rowCounts(as.matrix(SEdata[, obscols]), value = 1, 
                         na.rm = T) == 1)
      found <- numeric(length(zeros))
      found[foundInd] <- zeros[foundInd] + 1
      obs <- cbind(zeros, found)

    # pred cols

      preds<-rep(NA, 5)
      preds[length(pvars)==0][1]<-c("1")
      preds[length(pvars)==1][1:2]<-c("1", pvars[1])
      preds[length(pvars)==2]<-c("1", pvars[1], pvars[2], paste(pvars[1], 
                                        pvars[2], sep = " + ")
                                    , paste(pvars[1], pvars[2], sep = " * "))
      preds <- as.character(na.omit(preds))
      eqs <- paste("~", preds, sep = " ")

      peqs <- switch(fixK, 
                     NO = rep(eqs, length(eqs)),
                     YES = eqs)
      keqs <- switch(fixK,
                     NO = rep(eqs, each = length(eqs)),
                     YES = rep(paste("~fixed_at_", fixKval, sep = ""), 
                             length(eqs)))
      Nmods <- length(peqs)

    # size classes

      sccol <- which(colnames(SEdata) == sizeclasscol)

      sizeclasses <- as.character(unique(SEdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(SEdata[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(SEdata))] <- "1"

    # set up the factor combinations
 
      SEfct <- factorcombinations(pvars, data = SEdata)

    # run the estimator for each of the possible models for each size class

      SEmods <- vector("list", Nsizeclasses)

    # for each size class

      for(i in 1:Nsizeclasses){

        SEmodsi <- vector("list", Nmods)				
        obsi <- obs[sizes == sizeclasses[i], ]
        SEdatai <- SEdata[sizes == sizeclasses[i] , ]

        for(j in 1:Nmods){

          pformula <- formula(peqs[j])
          kformulap <- formula(keqs[j])
          kformula <- switch(fixK, 
                             NO = formula(keqs[j]),
                             YES = formula("~1"))

          # set up the model 

            Xp <- model.matrix(pformula, SEdatai)
            np <- dim(Xp)[2]

            Xk <- model.matrix(kformula, SEdatai)
            nk <- dim(Xk)[2]

          # create the model matrices

            miniXp <- model.matrix(pformula, SEfct)
            miniXk <- model.matrix(kformula, SEfct)
            facts <- cbind(miniXp, miniXk)
            nfact <- dim(facts)[1]
            tXpk <- t(cbind(Xp, Xk))

          # set up the groups of observations

            groups <- numeric(dim(Xp)[1])
					
            for (k in 1:nfact){
              groups[colSums(tXpk == facts[k,]) == np + nk] <- k
            }

          # set up the starting values for theta
          #  Cells with all 0's set to 0.1
          #  Cells with all 1's set to 0.9

            empp <- numeric(nrow(Xp))

            s1data <- SEdatai[, obscols[1]]

            for(k in 1:dim(miniXp)[1]){
              empp[which(groups==k)] <- mean(s1data[which(groups == k & 
                                             s1data >= 0)], na.rm = TRUE)
            }
            empp[which(empp == 0)] <- 0.1
            empp[which(empp == 1)] <- 0.9 
            theta <- c(solve(t(Xp) %*% Xp) %*% t(Xp) %*% logit(empp), 
                       logit(rep(initKval, nk)))

            theta <- switch(fixK, 
                            YES = theta[-length(theta)],
                            NO = theta)

          # run the pk model

            result <- optim(par = theta, fn = pkfunction, method = "BFGS", 
                            hessian = T, groups = groups, np = np,
                            facts = facts, fixK = fixK, fixKval = fixKval,
                            zeros = obsi[,"zeros"], found = obsi[,"found"],  
                            maxmiss = max(obsi[,"zeros"]))

          # prep the output

            output <- vector("list", 7) 
            names(output) <- c("pmodel", "kmodel", "betaphat", "betakhat", 
                               "vartheta", "aic", "convergence")
            output$pmodel <- pformula
            output$kmodel <- kformulap
            output$betaphat <- result$par[1:ncol(Xp)]
            output$betakhat <- switch(fixK,
                                 NO = result$par[(ncol(Xp)+1):length(theta)],
                                 YES = NULL)
            output$vartheta <- solve(result$hessian)
            output$aic <- 2 * result$value + 2 * length(result$par)
            output$convergence <- result$convergence
            output$miniXp <- miniXp
            output$miniXk <- miniXk
            output$np <- np
            output$nk <- nk
            output$fixedK <- fixKval

          SEmodsi[[j]] <- output

        }

        SEmods[[i]] <- SEmodsi

      }

    # return

      return(SEmods)

  }

################################################################################
#
# factorcombinations
#
################################################################################

  factorcombinations <- function(pvars, data, ... ){

          # create the cell table, count cells

            pv1 <- NULL
            pv2 <- NULL

            pv1 <- pvars[1][length(pvars) > 0]
            pv2 <- pvars[2][length(pvars) > 1]

            lev1 <- as.character(unique(data[, pv1]))
            lev2 <- as.character(unique(data[, pv2]))

            nlev1 <- length(lev1)
            nlev2 <- length(lev2)

            nlev1[length(lev1) == 0] <- 1
            nlev2[length(lev2) == 0] <- 1

            fctlev1 <- rep(lev1, nlev2)
            fctlev2 <- rep(lev2, each = nlev1)
            fctlev1[length(fctlev1) == 0] <- ""
            fctlev2[length(fctlev2) == 0] <- ""

            fctCN <- paste(fctlev1, fctlev2, sep = "")

            fct <- data.frame(fctlev1, fctlev2, fctCN)
            colnames(fct) <- pvars
            colnames(fct)[ncol(fct)] <- "CellNames"

            colnames(fct)[1][length(pvars) == 0] <- "groups"
            fct <- fct[, which(is.na(colnames(fct)) == F)]
            fct[,1] <- as.character(fct[,1])
            fct[,1][which(fct[,1]=="")] <- "all"
            fct[,"CellNames"] <- as.character(fct[,"CellNames"])
            fct[,"CellNames"][which(fct[,"CellNames"]=="")] <- "all"

          # return factor combination table

            return(fct)
  }

################################################################################
#
# ThetaSEcreateacrosssizes
#
################################################################################

  ThetaSEcreateacrosssizes <- function(SEdata, pvars, sizeclasscol, 
                                       SEmods, SEmodstouse, 
                                       Niterations, fixK, fixKval, ... ){ 

    # set up the factor combination table and count the cells

      SEfct <- factorcombinations(pvars = pvars, data = SEdata)
      Ncells <- nrow(SEfct)

    # size classes

      sccol <- which(colnames(SEdata) == sizeclasscol)
      sizeclasses <- as.character(unique(SEdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

    # create a theta for each cell within each size class

      thetaSE <- array(NA, dim = c(Niterations, 2, Ncells, Nsizeclasses))

    # for each size class

      for(i in 1:Nsizeclasses){

        # select the model

          smod <- SEmods[[i]][[SEmodstouse[i]]]

        # draw the parameters and combine to form p and k

          betaSim <- rmvnorm(Niterations, mean = c(smod$betaphat, 
                              smod$betakhat), sigma = smod$vartheta)
          pSim <- alogit(betaSim[,1:smod$np]%*%t(smod$miniXp))
          kSim <- switch(fixK,
                         NO = alogit(betaSim[,(smod$np + 1):(smod$np + 
                                      smod$nk)] %*%t (smod$miniXk)),
                         YES = matrix(fixKval, ncol = Ncells, 
                                      nrow = Niterations))

        # fill into the array   

          for(j in 1:Ncells){				

            thetaSE[ , , j, i] <- c(pSim[,j], kSim[,j])

          }
      }

    # return

    return(thetaSE)

  }


################################################################################
#
# logit
#
################################################################################

  logit <- function(x){
    log(x/(1-x))
  }

################################################################################
#
# alogit
#
################################################################################

  alogit <- function(x){
    1/(1+exp(-x))
  }

################################################################################
#
# pkfunction
#
################################################################################

  pkfunction <- function(zeros, found, theta, np, groups, maxmiss, facts, 
                           fixK, fixKval, ...){
    theta <- switch(fixK,
                    NO = theta,
                    YES = c(theta, logit(fixKval)))

    Beta <- array(numeric(length(theta) * 2), dim = c(length(theta), 2))  
    Beta[1:np,1] <- theta[1:np]
    Beta[(np+1):length(theta), 2] <- theta[(np+1):length(theta)]

    pk <- alogit(facts %*% Beta)

    powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(dim(pk)[1], maxmiss + 1))
    powk[,1] <- 1
    powk <- rowCumprods(powk)

    pmiss <- rowCumprods(matrix(1 - (pk[,1]*powk[,1:(maxmiss+1)]), 
                                  nrow = dim(pk)[1]))

    pfind.si <- cbind(pk[,1], rowDiffs(1 - pmiss))

    -(sum(log(pmiss[cbind(groups[found == 0], zeros[found == 0])])) + 
      sum(log(pfind.si[cbind(groups[found > 0], found[found > 0])]))
    )
  }

################################################################################
# gvec 
#          for calculating detection probability in a single class 
#          given search schedule and simulated columns of carcass persistence 
#          times and pk parameters
#
# ARGUMENTS
#   days = vector of search times, beginning at 0
#   CPab = simulated vector of persistence parameters (Weibull if 2-d or 
#             exponential if 1-d)
#     parameterization: for exponential? means; for Weibull? R's shape and 
#                         scale (like eoa)
#   seef = searcher efficiency parameters
#     if k not derived from a pk model of field trial data, then seef is a 
#       vector of simulated searcher efficiencies and k is a constant defined 
#       by the user
#     if k is derived from field trial data, then seef is an nsim x 2 array of 
#       simulated p and k
#
# VALUE
#   A vector of simulated detection probabilities
#
################################################################################

  gvec <- function(days, CPab, persdist, seef, k = NULL){

    nsim <- dim(CPab)[1]
    samtype <- ifelse(length(unique(diff(days))) == 1, "Formula", "Custom")
    nsearch <- length(days) - 1

    if(persdist %in% c("Exponential", "exponential")){
      pdb <- CPab
      pda <- 1/pdb
      pdb0 <- exp(mean(log(pdb)))
      pda0 <- 1/pdb0
    } else{
      pda <- CPab[, 1]
      pdb <- CPab[, 2]
      pdb0 <- exp(mean(log(pdb)))
      pda0 <- 1/mean(1/pda)
    }

    pk <- array(dim = c(nsim, 2))
    if (is.vector(seef)){
      pk[, 1] <- seef
      pk[, 2] <- k
    } else{
      pk <- seef
    }

    # setting estimation control parameters
    #  search limit: number of searches after arrival to include in estimate 
    #      of seef [when the number of searches is high, including them all in 
    #      the estimation is calculation intensive but does not contribute 
    #      signficantly to the result]

      f0 <- mean(pk[, 1])
      k0 <- mean(pk[, 2])
      ind1 <- rep(1:nsearch, times = nsearch:1)
      ind2 <- ind1+1
      ind3 <- unlist(lapply(1:nsearch, function(x) x:nsearch)) + 1
      schedule.index <- cbind(ind1,ind2,ind3)
      schedule <- cbind(days[ind1],days[ind2],days[ind3])
      nmiss <- schedule.index[,3] - schedule.index[,2]
      maxmiss <- max(nmiss)
      powk <- cumprod(c(1, rep(k0, maxmiss))) # vector of k^i's
      notfind <- cumprod(1 - f0*powk[-length(powk)])
      nvec <- c(1, notfind) * f0
  
    # conditional probability of finding a carcass on the ith search (row) 
    # after arrival for given (simulated) searcher efficiency (column):

      pfind.si <- nvec * powk

    # persistences:

      intxsearch <- unique(cbind(schedule[,2] - schedule[,1], 
                           schedule[,3] - schedule[,2]), MAR = 1)
      ppersu <- ppersist(persdist,
                         t_arrive0 = 0,
                         t_arrive1 = intxsearch[,1],
                         t_search = intxsearch[,1] + intxsearch[,2],
                         pda = pda0, pdb = pdb0
                         )

      arrvec <- (schedule[,2] - schedule[,1])/max(days)
      prob_obs <- numeric(dim(schedule)[1])

      for (i in 1:length(prob_obs)){

        prob_obs[i] <- pfind.si[nmiss[i]+1] * 
                     ppersu[which(abs(intxsearch[,1] - 
                                    (schedule[i,2] - schedule[i,1])) < 0.001 &
                                  abs(intxsearch[,2] - (schedule[i,3] - 
                                       schedule[i,2])) < 0.001
                         ),] * arrvec[i]

      }

      ggnm <- numeric(maxmiss+1)

      for (missi in 0:maxmiss){
        ggnm[missi+1] <- sum(prob_obs[nmiss==missi])
      }


    # if more than 10 searches, consider truncating search schedule because 
    #   very few carcasses will be found after being missed 9 or more times, 
    #   but many searches is costly in terms of calculation efficiency

      if (nsearch > 10){

        iskip <- min(which(cumsum(ggnm)/sum(ggnm) > 0.99)) + 1

        # cutting off the search schedule introduces a slight bias.
        # Correct by multiplying the final g's by gadj = sum(ggnm)/ggnm[iskip]

        gadj <- sum(ggnm)/sum(ggnm[1:iskip])

      } else{
    
        iskip <- maxmiss
        gadj <- 1
      }

    # estimation of g
    # subset the search schedule
    # ignoring probabilities of detection carcasses after they have been 
    #   missed several times):

      schedule <- cbind(days[ind1], days[ind2], 
                        days[ind3])[ind2 >= ind3 - iskip + 1,]

      # columns for arrival interval and search number:

        schedule.index <- cbind(ind1, ind2, ind3)[ind2 >= ind3 - iskip + 1,]
        nmiss <- schedule.index[,3] - schedule.index[,2]
        maxmiss <- max(nmiss)

      # searcher efficiencies

        if (maxmiss == 0) {

          pfind.si <- pk[,1]

        } else if (maxmiss == 1){

          pfind.si<-cbind(pk[,1], (1 - pk[,1]) * pk[,2]*pk[,1])

        } else {

          powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(nsim, maxmiss+1))
          powk[,1] <- 1
          powk <- rowCumprods(powk)
          pfind.si <- pk[,1] * powk * cbind(rep(1, nsim), 
                        rowCumprods(1 - (pk[,1] * powk[, 1:maxmiss])))
        }

      intxsearch <- unique(cbind(schedule[,2] - schedule[,1], 
                     schedule[,3] - schedule[,2]), MAR = 1)
      ppersu <- ppersist(persdist,
                          t_arrive0 = 0,
                          t_arrive1 = intxsearch[,1],
                          t_search = intxsearch[,1] + intxsearch[,2],
                          pda = CPab[,1], pdb = CPab[,2])

      # assume uniform arrivals

        arrvec <- (schedule[,2]-schedule[,1])/max(days) 

      # add the probabilities

        prob_obs <- numeric(nsim)

        if (maxmiss > 0){

          for (i in 1:dim(schedule)[1]){

            prob_obs <- prob_obs +
                          pfind.si[,nmiss[i]+1] *
                          ppersu[which(
                            abs(intxsearch[,1] - (schedule[i,2] - 
                                 schedule[i,1])) < 0.001 &
                            abs(intxsearch[,2] - (schedule[i,3] - 
                                 schedule[i,2])) < 0.001),
                          ] * arrvec[i]
          }
        } else {

          for (i in 1:dim(schedule)[1]){

            prob_obs <- prob_obs +
                            pfind.si[nmiss[i]+1] *
                            ppersu[which(
                              abs(intxsearch[,1] - (schedule[i,2] - 
                                 schedule[i,1])) < 0.001 &
                              abs(intxsearch[,2] - (schedule[i,3] - 
                                 schedule[i,2])) < 0.001),
                            ] * arrvec[i]
          }

        }

    # g for monitored period

      prob_obs
  }


################################################################################        
#
# ppersist
#
# This function calculates the probability that a carcass that arrives in the 
# interval between t_arrive0 and t_arrive1 persists until t_search. It is a 
# generalization of the "r" statistic in Huso et al.'s DS729 fatality estimator 
# and is needed if k ! = 0. It vectorized with respect to both the arrival & 
# search intervals and pda & pdb parameters, and it uses "exact" integrals 
# rather than numerical approximations and is very fast (except for the 
# loglogistic, which is artificially vectored via the super-slow "Vectorize" 
# function.) This is the same as EoA's ppersist except that it has options 
# for different spellings of the distributions.
#
################################################################################        

  ppersist <- function(persistence_distn, t_arrive0, t_arrive1, t_search, 
                       pdb, pda = NULL, ...){

    if(persistence_distn %in% c("Weibull", "weibull")){

      return(t((pgamma(outer(1 / pdb, t_search - t_arrive0)^pda, 1 / pda) -
              pgamma(outer(1 / pdb, t_search - t_arrive1)^pda, 1 / pda)) * 
              gamma(1 + 1 / pda) * outer(pdb, 1 / (t_arrive1 - t_arrive0))))

    }

    if(persistence_distn %in% c("Exponential", "exponential")){

      return((exp(outer(t_arrive1 - t_search, 1 / pdb)) - exp(outer(t_arrive0 - 
              t_search, 1 / pdb))) / (outer(t_arrive1 - t_arrive0, 1 / pdb)))

    }

    if(persistence_distn %in% c("Lognormal", "lognormal")){

      root_pda <- sqrt(pda)
      exp_value <- exp((pda / 2) + pdb)
      tt <- t_search - t_arrive0
      part0 <- t(pnorm(outer(pdb, -log(tt), "+") / root_pda)) * tt + 
               t(pnorm(outer(-pdb, log(tt), "+") / root_pda - root_pda) *
               exp_value)
      tt <- t_search - t_arrive1
      part1 <- t(pnorm(outer(pdb, -log(tt), "+")/root_pda)) * tt + 
               t(pnorm(outer(-pdb, log(tt), "+")/root_pda - root_pda) * 
               exp_value)
      return(-(part1 - part0) / (t_arrive1 - t_arrive0))

    }

    if(persistence_distn %in% c("Log-Logistic", "loglogistic")) {

      return(Vectorize(function(t_arrive0, t_arrive1, t_search, pda, pdb){
        t1 <- t_search-t_arrive1 
        t0 <- t_search-t_arrive0
        part1 <- ifelse(t1 == 0, 0, t1 / (1 + (t1 / pdb)^pda) * hyperg_2F1(1, 
                  1, 1 + 1 / pda, 1 / (1 + (t1 / pdb)^(-pda))))
        part0 <- t0 / (1 + (t0 / pdb)^pda)*hyperg_2F1(1, 1, 1 + 1 / pda, 
                  1 / (1 + (t0 / pdb)^(-pda)))

        -(part1 - part0)/(t_arrive1 - t_arrive0)
        },
        vectorize.args = c('pdb', 'pda'))(t_arrive0, t_arrive1,
           t_search, pda, pdb))
    }
  }