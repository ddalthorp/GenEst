##############################################################################
#
#  This script contains the functions of the GenEst package
#
#  version 0.0.0.3 October 2017
#
#  Held under GNU GPL v >= 3	
#
##############################################################################


##############################################################################
# packageLoad	
#  function for checking package needs and downloading, then loading
#
# inputs
#  none
#
# outputs
#  none
##############################################################################
	
packageLoad <- function(...){

  # presently installed local packages

    lps <- installed.packages()[,"Package"]

  # packages needed from CRAN

    crpks <- c("shiny", "rhandsontable", "httr", "survival", # "devtools",
                "mvtnorm", "matrixStats", "gsl")
    for(i in 1:length(crpks)){
      if(!(crpks[i] %in% lps)){
	  install.packages(crpks[i])
      }
    }
    #library(devtools)
    library(shiny)
    library(rhandsontable)
    library(httr)
    library(survival)
    library(mvtnorm)
    library(matrixStats)
    library(gsl)

  # pakages needed from github (not currently needed)

    # workaround the firewall

    #  set_config(config(ssl_verifypeer = 0L))

    #if(!("shinysky" %in% lps)){
    #  install_github("AnalytixWare/ShinySky")
    #}
    #library(shinysky)

  # return

}


##############################################################################
#
# SEmod
#
##############################################################################

  SEmod <- function(peq, keq, fct, data, obs, obscols, 
                                 initKval, fixK, fixKval, ... ){ 

    # set up the formulas

       pformula <- formula(peq)
       kformulap <- formula(keq)
       kformula <- switch(fixK, 
                             NO = formula(keq),
                             YES = formula("~1"))

    # set up the model 

      Xp <- model.matrix(pformula, data)
      np <- dim(Xp)[2]

      Xk <- model.matrix(kformula, data)
      nk <- dim(Xk)[2]

    # create the model matrices

      miniXp <- model.matrix(pformula, fct)
      miniXk <- model.matrix(kformula, fct)
      facts <- cbind(miniXp, miniXk)
      nfact <- dim(facts)[1]
      tXpk <- t(cbind(Xp, Xk))

    # set up the groups

      groups <- numeric(dim(Xp)[1])
					
      for (k in 1:nfact){
        groups[colSums(tXpk == facts[k,]) == np + nk] <- k
      }

    # set up the starting values for theta
    #  Cells with all 0's set to 0.1
    #  Cells with all 1's set to 0.9

      empp <- numeric(nrow(Xp))

      s1data <- data[, obscols[1]]

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
                        zeros = obs[,"zeros"], 
                        found = obs[,"found"],  
                        maxmiss = max(obs[,"zeros"]))

    # prep the output

      output <- vector("list", 8) 
      names(output) <- c("pmodel", "kmodel", "betaphat", "betakhat", 
                         "vartheta", "AIC", "AICc", "convergence")
      output$pmodel <- pformula
      output$kmodel <- kformulap
      output$betaphat <- result$par[1:ncol(Xp)]
      output$betakhat <- switch(fixK,
                                NO = result$par[(ncol(Xp)+1):length(theta)],
                                YES = NULL)
      output$vartheta <- solve(result$hessian)

      npar <- length(result$par)
      nobs <- length(obs)
      output$AIC <- 2 * result$value + 2 * npar
      output$AICc <- output$AIC + 
                       (2 * npar * (npar + 1)) / (nobs - npar - 1)
      output$convergence <- result$convergence
      output$miniXp <- miniXp
      output$miniXk <- miniXk
      output$np <- np
      output$nk <- nk
      output$fixedK <- fixKval
 
    # return

      return(output)

  }
  


##############################################################################
#
# SEmodset
#
##############################################################################


  SEmodset <- function(vars, data, initKval, fixK, fixKval, obscols, ... ){

    # set up the response data (observations)

    #   zeros: # of times the carcass was missed
    #   found: # of search on which the carcass was found

      zeros <- rowCounts(as.matrix(data[, obscols]), value = 0, na.rm = T)
      foundInd <- which(rowCounts(as.matrix(data[, obscols]), value = 1, 
                         na.rm = T) == 1)
      found <- numeric(length(zeros))
      found[foundInd] <- zeros[foundInd] + 1
      obs <- cbind(zeros, found)

    # set up the factor combinations
 
      fct <- factorcombinations(pvars = vars, data = data)

    # set up the models

      preds <- rep(NA, 5)
      preds[length(vars) == 0][1] <- c("1")
      preds[length(vars) == 1][1:2] <- c("1", vars[1])
      preds[length(vars) == 2] <- c("1", vars[1], vars[2], paste(vars[1], 
                                        vars[2], sep = " + ")
                                   , paste(vars[1], vars[2], sep = " * "))
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

    # set up the output

      output <- vector("list", Nmods)

    # run SEmod for each of the models

      for(i in 1:Nmods){

        output[[i]] <- SEmod(peq = peqs[i], keq = keqs[i],  
                             fct = fct, data = data, obscols = obscols, 
                             obs = obs, initKval = initKval,  
                             fixK = fixK, fixKval = fixKval)

      }

          names(output) <- paste("p: ", peqs, ", k: ", 
                                      keqs, sep = "")

    # return

      return(output)

  } 


##############################################################################
#
# SEmodsetsacrosssizes 
#
##############################################################################


  SEmodsetsacrosssizes <- function(data, vars, obscols, sizeclasscol,
                                   initKval, fixK, fixKval, ...){

    # set up the size classes

      sccol <- which(colnames(data) == sizeclasscol)

      sizeclasses <- as.character(unique(data[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(data[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(data))] <- "1"

    # prep the output

      output <- vector("list", Nsizeclasses)
      names(output) <- sizeclasses

    # iterate across size classes

      for(i in 1:Nsizeclasses){

        sizeclass <- sizeclasses[i]
        datai <- data[sizes == sizeclass , ]

        output[[i]] <- SEmodset(vars = vars, data = datai, 
                                obscols = obscols, initKval = initKval, 
                                fixK = fixK, fixKval = fixKval)
      }

    # return

      return(output)

  }


##############################################################################
#
# ThetaSEcreateacrosssizes
#
##############################################################################

  ThetaSEcreateacrosssizes <- function(SEdata, SEvars, sizeclasscol, 
                                       SEmods, 
                                       Niterations, fixK, fixKval, ... ){ 

    # set up the factor combination table and count the cells

      SEfct <- factorcombinations(pvars = SEvars, data = SEdata)
      Ncells <- nrow(SEfct)

    # numer of models 

      Nmodels <- length(SEmods[[1]])

    # size classes

      sccol <- which(colnames(SEdata) == sizeclasscol)
      sizeclasses <- as.character(unique(SEdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

    # create a theta for each cell within each size class

      thetaSE <- array(NA, dim = c(Niterations, 2, Ncells, 
                        Nmodels, Nsizeclasses))

    # for each size class

      for(i in 1:Nsizeclasses){

        for(j in 1:Nmodels){

          # select the model

            smod <- SEmods[[i]][[j]]

          # draw the parameters and combine to form p and k

            betaSim <- rmvnorm(Niterations, mean = c(smod$betaphat, 
                              smod$betakhat), sigma = smod$vartheta, 
                                method = "svd")
            pSim <- alogit(betaSim[,1:smod$np]%*%t(smod$miniXp))
            kSim <- switch(fixK,
                           NO = alogit(betaSim[,(smod$np + 1):(smod$np + 
                                      smod$nk)] %*%t (smod$miniXk)),
                           YES = matrix(fixKval, ncol = Ncells, 
                                      nrow = Niterations))

          # fill into the array   

            for(k in 1:Ncells){				

              thetaSE[ , , k, j, i] <- cbind(pSim[,k], kSim[,k])

            }

        } 
      }
      

    # return

      return(thetaSE)

  }


##############################################################################
#
# AICtabcreateSEmods
#
##############################################################################

  AICtabcreateSEmods <- function(SEmods, sortby = "AIC", ...){

    # determine number of size classes

      nsc <- length(SEmods)

    # determine number of models w/in size classes

      nmods <- length(SEmods[[1]])

    # set up list of tables for output

      output <- vector("list", nsc)

    # fill in

      for(i in 1:nsc){

        tsctab <- data.frame(matrix(NA, nrow = nmods, ncol = 4))
        colnames(tsctab) <- c("p model", "k model", "AIC", "AICc")

        for(j in 1:nmods){

          tsctab[j, ] <- c(
                         paste(as.character(unlist(SEmods[[i]][[j]]$pmodel)),
                           collapse = " "),
                         paste(as.character(unlist(SEmods[[i]][[j]]$kmodel)),
                           collapse = " "), 
                         round(unlist(SEmods[[i]][[j]]$AIC), 3),
                         round(unlist(SEmods[[i]][[j]]$AICc), 3))

        }

        AICorders <- order(as.numeric(tsctab[ , "AIC"]))
        AICcorders <- order(as.numeric(tsctab[ , "AICc"]))

        orders <- switch(sortby, 
                          NULL = 1:nmods,
                          "AIC" = AICorders,
                          "AICc" = AICsorders) 
        outtab <- tsctab[orders, ]
        output[[i]] <- outtab

      }

    # add size col names

      names(output) <- names(SEmods)

    # return

      return(output)

  }




##############################################################################
#
# SEgraphcreate
#
##############################################################################

  SEgraphcreate <- function(SEdata, SEvars, thetaSE, obscols, 
                              Niterations, sizeclasscol, r, j, CellWiseModel,
                              ... ){

    # size classes

      sccol <- which(colnames(SEdata) == sizeclasscol)

      sizeclasses <- as.character(unique(SEdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(SEdata[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(SEdata))] <- "1"

    # factor set up

      # set up the cells via the factor combination table

        fct <- factorcombinations(pvars = SEvars, data = SEdata) 
        Ncells <- nrow(fct)

        pv1 <- NULL	
        pv2 <- NULL
        pv1 <- SEvars[1][length(SEvars) > 0]
        pv2 <- SEvars[2][length(SEvars) > 1]
        lev1 <- as.character(unique(SEdata[, pv1]))
        lev2 <- as.character(unique(SEdata[, pv2]))
        nlev1 <- length(lev1)
        nlev2 <- length(lev2)
        nlev1[length(lev1) == 0] <- 1
        nlev2[length(lev2) == 0] <- 1

      # combine factors

        combnames <- rep(NA, nrow(SEdata))

        for(i in 1:nrow(SEdata)){
          colchoice <- which(colnames(SEdata) %in% c(pv1, pv2))
          colchoice2 <- colchoice[c(which(colnames(SEdata)[colchoice] == pv1),
                         which(colnames(SEdata)[colchoice] == pv2))]
          tempname <- paste(as.character(t(SEdata[i, 
                             colchoice2])), 
                             collapse = "")
          tempname[tempname == ""] <- "all"
          combnames[i] <-  tempname
        }
  
      # max searches

        maxs <- length(obscols)

      # create the figure

        # dummy initial figure

              par(fig = c(0, 1, 0.75, 1))
              par(mar = c(1, 1, 1, 1))
              plot(1,1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', 
                    xlab = "", ylab = "")

        # divvy up the bottom 3/4 of the image for the cell matrix

              figxspace <- 1 / nlev2
              figyspace <- 0.75 / nlev1

              x1 <- rep(figxspace * ((1:nlev2) - 1), each = nlev1)
              x2 <- rep(figxspace * ((1:nlev2)), each = nlev1)

              y1 <- rep(figyspace * ((nlev1:1) - 1), nlev2)
              y2 <- rep(figyspace * ((nlev1:1)), nlev2)

              ps <- matrix(NA, nrow = Niterations, ncol = Ncells)
              ks <- matrix(NA, nrow = Niterations, ncol = Ncells)
              CMps <- matrix(NA, nrow = Niterations, ncol = Ncells)
              CMks <- matrix(NA, nrow = Niterations, ncol = Ncells)

              par(mar = c(5,6,2,1))

        # plot each cell's figure

          for(i in 1:Ncells){

            # restrict the data and the thetas

                SEdata_ri <- SEdata[which(sizes == sizeclasses[r] & 
                                         combnames == fct[i, "CellNames"]), ]
                NAconv <- is.na(SEdata_ri[, obscols])

                arraydim <- length(dim(NAconv))

                if(arraydim == 0){
                  carcassavail <- length(NAconv) - sum(NAconv)
                } else{
                  carcassavail <- nrow(NAconv) - apply(NAconv, 2, sum)
                }
                thetaSErji <- thetaSE[, , i, j, r]

                par(fig = c(x1[i], x2[i], y1[i], y2[i]), new = T)

                meanpar <- apply(thetaSErji, 2, mean)
                #lqpar <- apply(thetaSErji, 2, quantile, probs = 0.025)
                #uqpar <- apply(thetaSErji, 2, quantile, probs = 0.975)

                ps[, i] <- thetaSErji[, 1] 
                ks[, i] <- thetaSErji[, 2] 

                predxs <- seq(1, maxs, 1)
                predys <- meanpar[1] * meanpar[2] ^ (predxs - 1)
                #predyl <- lqpar[1] * lqpar[2] ^ (predxs - 1)
                #predyu <- uqpar[1] * uqpar[2] ^ (predxs - 1)

                CMthetaSErji <- thetaSE[, , i, CellWiseModel, r]
                CMmeanpar <- apply(CMthetaSErji, 2, mean)
                #CMlqpar <- apply(CMthetaSErji, 2, quantile, probs = 0.025)
                #CMuqpar <- apply(CMthetaSErji, 2, quantile, probs = 0.975)

                CMps[, i] <- CMthetaSErji[, 1] 
                CMks[, i] <- CMthetaSErji[, 2] 

                CMpredys <- CMmeanpar[1] * CMmeanpar[2] ^ (predxs - 1)
                #CMpredyl <- CMlqpar[1] * CMlqpar[2] ^ (predxs - 1)
                #CMpredyu <- CMuqpar[1] * CMuqpar[2] ^ (predxs - 1)

                xpts <- 1:length(obscols)

                if(arraydim == 0){
                  ypts <- mean(SEdata_ri[, obscols], na.rm = T)
                } else{
                  ypts <- apply(SEdata_ri[, obscols], 2, mean, na.rm = T)
                }
                

                plot(xpts, ypts, ylim = c(0, 1), 
                     xlim = c(0.5, maxs + 0.5), main = fct[i, "CellNames"], 
                     xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "L", 
                     col = rgb(0.2, 0.2, 0.2, 0.5), lwd = 2, pch = 1, 
                     cex = 1.5, cex.main = 0.75)

                axis(1, las = 1, cex.axis = 1.)
                axis(2, las = 1, cex.axis = 1., at = seq(0, 1, .2))
                mtext(side = 1, "Search", line = 3, cex = 1.25)
                mtext(side = 2, "Searcher Efficiency", line = 4, cex = 1.25)

                text(xpts + 0.1, ypts + 0.075, carcassavail, xpd = T, 
                       cex = 0.75, col = rgb(0.5, 0.5, 0.5, 0.9))


                points(predxs, CMpredys, type = 'l', lwd = 3, 
                        col = rgb(0.1, 0.1, 0.8, 0.6))
                #points(predxs, CMpredyl, type = 'l', lwd = 2, lty = 2, 
                #        col = rgb(0.1, 0.1, 0.8, 0.6))
                #points(predxs, CMpredyu, type = 'l', lwd = 2, lty = 2, 
                #        col = rgb(0.1, 0.1, 0.8, 0.6))

                points(predxs, predys, type = 'l', lwd = 3)
                #points(predxs, predyl, type = 'l', lwd = 2, lty = 2)
                #points(predxs, predyu, type = 'l', lwd = 2, lty = 2)
 
              }

        # plot the ps and ks at the top

          # ps

              par(mar = c(3,4,1,1))
              par(fig = c(0, .5, 0.75, 1), new = T)

              plot(1, 1, type = "n", xlim = c(0.5, Ncells + 0.5), 
                    ylim = c(0, 1), 
                    bty = "L", xlab = "", ylab = "", xaxt = "n", yaxt = "n") 

              for(i in 1:Ncells){

                PS <- ps[, i]
                PX <- runif(length(PS), i - 0.1, i + 0.1) - 0.2  
                medianp <- median(PS)
                meanp <- mean(PS)
                iqp <- quantile(PS, c(0.25, 0.75))
                minp <- min(PS)
                maxp <- max(PS)

               # points(PX, PS, pch = 1, cex = 0.5, lwd = 1, 
               #         col = rgb(0.3, 0.3, 0.3, 0.05))
                rect(i - 0.1 - 0.2, iqp[1], i + 0.1 - 0.2, iqp[2], lwd = 3, 
                        col = rgb(1, 1, 1, 0.4))
                points(c(i - 0.1, i + 0.1) - 0.2, rep(medianp, 2), type = "l",
                        lwd = 2)
                points(c(i - 0.05, i + 0.05) - 0.2, rep(minp, 2), type = "l", 
                        lwd = 1)
                points(c(i - 0.05, i + 0.05) - 0.2, rep(maxp, 2), type = "l", 
                        lwd = 1)
                points(c(i, i) - 0.2, c(iqp[1], minp), type = "l", lwd = 1) 
                points(c(i, i) - 0.2, c(iqp[2], maxp), type = "l", lwd = 1) 

                CMPS <- CMps[, i]
                CMPX <- runif(length(CMPS), i - 0.1, i + 0.1) + 0.2  
                CMmedianp <- median(CMPS)
                CMmeanp <- mean(CMPS)
                CMiqp <- quantile(CMPS, c(0.25, 0.75))
                CMminp <- min(CMPS)
                CMmaxp <- max(CMPS)

                #points(CMPX, CMPS, pch = 1, cex = 0.5, lwd = 1, 
                #        col = rgb(0.1, 0.1, 0.8, 0.01))
                rect(i - 0.1 + 0.2, CMiqp[1], i + 0.1 + 0.2, CMiqp[2], 
                        lwd = 3, col = rgb(0.1, 0.1, 0.8, 0.6))
                points(c(i - 0.1, i + 0.1) + 0.2, rep(CMmedianp, 2), 
                        type = "l", lwd = 2, col = rgb(0.1, 0.1, 0.8, 0.6))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMminp, 2), 
                        type = "l", lwd = 1, col = rgb(0.1, 0.1, 0.8, 0.6))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmaxp, 2), 
                        type = "l", lwd = 1, col = rgb(0.1, 0.1, 0.8, 0.6))
                points(c(i, i) + 0.2, c(CMiqp[1], CMminp), type = "l", 
                        lwd = 1, col = rgb(0.1, 0.1, 0.8, 0.6)) 
                points(c(i, i) + 0.2, c(CMiqp[2], CMmaxp), type = "l", 
                        lwd = 1, col = rgb(0.1, 0.1, 0.8, 0.6)) 

              }

              axis(1, at = 1:Ncells, labels = F, cex.axis = 0.5, line = 0)
              mtext(side = 1, line = 0.75, at = 1:Ncells, fct[, "CellNames"], 
                        cex = 1)

              axis(2, las =1, at = seq(0, 1, .2), cex.axis = 1)
              mtext(side = 2, line = 2.75, "p", cex = 1)

          # ks

              par(fig = c(0.5, 1, 0.75, 1), new = T)
              par(mar = c(3, 4, 1, 1))

              plot(1, 1, type = "n", xlim = c(0.5, Ncells + 0.5), 
                        ylim = c(0, 1), 
                    bty = "L", xlab = "", ylab = "", xaxt = "n", yaxt = "n") 

              for(i in 1:Ncells){

                KS <- ks[, i]
                KX <- runif(length(KS), i - 0.1, i + 0.1) - 0.2 
                mediank <- median(KS)
                meank <- mean(KS)
                iqk <- quantile(KS, c(0.25, 0.75))
                mink <- min(KS)
                maxk <- max(KS)

               # points(KX, KS, pch = 1, cex = 0.5, lwd = 1, 
               #         col = rgb(0.3, 0.3, 0.3, 0.05))
                rect(i - 0.1 - 0.2, iqk[1], i + 0.1 - 0.2, iqk[2], 
                        lwd = 3, col = rgb(1, 1, 1, 0.4))
                points(c(i - 0.1, i + 0.1) - 0.2, rep(mediank, 2), 
                        type = "l", lwd = 2)
                points(c(i - 0.05, i + 0.05) - 0.2, rep(mink, 2), 
                        type = "l", lwd = 1)
                points(c(i - 0.05, i + 0.05) - 0.2, rep(maxk, 2), 
                        type = "l", lwd = 1)
                points(c(i, i) - 0.2 , c(iqk[1], mink), type = "l", lwd = 1) 
                points(c(i, i) - 0.2, c(iqk[2], maxk), type = "l", lwd = 1) 


                CMKS <- CMks[, i]
                CMKX <- runif(length(CMKS), i - 0.1, i + 0.1) + 0.2  
                CMmediank <- median(CMKS)
                CMmeank <- mean(CMKS)
                CMiqk <- quantile(CMKS, c(0.25, 0.75))
                CMmink <- min(CMKS)
                CMmaxk <- max(CMKS)

                #points(CMKX, CMKS, pch = 1, cex = 0.5, lwd = 1, 
                #        col = rgb(0.1, 0.1, 0.8, 0.01))
                rect(i - 0.1 + 0.2, CMiqk[1], i + 0.1 + 0.2, CMiqk[2], 
                        lwd = 3, col = rgb(0.1, 0.1, 0.8, 0.6))
                points(c(i - 0.1, i + 0.1) + 0.2, rep(CMmediank, 2), 
                        type = "l", lwd = 2, col = rgb(0.1, 0.1, 0.8, 0.6))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmink, 2), 
                        type = "l", lwd = 1, col = rgb(0.1, 0.1, 0.8, 0.6))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmaxk, 2), 
                        type = "l", lwd = 1, col = rgb(0.1, 0.1, 0.8, 0.6))
                points(c(i, i) + 0.2, c(CMiqk[1], CMmink), type = "l", 
                        lwd = 1, col = rgb(0.1, 0.1, 0.8, 0.6)) 
                points(c(i, i) + 0.2, c(CMiqk[2], CMmaxk), type = "l", 
                        lwd = 1, col = rgb(0.1, 0.1, 0.8, 0.6)) 


              }

              axis(1, at = 1:Ncells, labels = F, cex.axis = 0.5, line = 0)
              mtext(side = 1, line = 0.75, at = 1:Ncells, fct[, "CellNames"],
                      cex = 1)
              axis(2, las =1, at = seq(0, 1, 0.2), cex.axis = 1)
              mtext(side = 2, line = 2.75, "k", cex = 1)

  }




##############################################################################
#
# CPmod
#
##############################################################################

  CPmod <- function(data, obs_survobj, eqtouse, disttouse, ... ){


        mform <- formula(paste("obs_survobj" , eqtouse, sep = " "))
        output <- survreg(mform, data, dist = disttouse)

    # return

      return(output)

  }


##############################################################################
#
# CPmodset
#
##############################################################################

  CPmodset <- function(data, vars, ltpc, ftac, ... ){

    # set up the response (surv object)

      t1 <- data[ , which(colnames(data) == ltpc)]
      t2 <- data[ , which(colnames(data) == ftac)]

      event <- rep(3, length(t1))
      event[which(is.na(t2))] <- 0
      event[which(t1 == t2)] <- 1
      event[which(t1 == 0)] <- 2

      t1[which(t1 == 0)] <- t2[which(t1 == 0)]


      obs_survobj <- Surv(time = t1, time2 = t2, event = event, 
                              type = "interval")

    # select the distributions to use

      distsselected <- c("exponential", "weibull", "loglogistic", "lognormal")

    # set up the predictors

      preds <- rep(NA, 5)
      preds[length(vars) == 0][1] <- c("1")
      preds[length(vars) == 1][1:2] <- c("1", vars[1])
      preds[length(vars) == 2] <- c("1", vars[1], vars[2], paste(vars[1], 
                                    vars[2], sep = " + "),
                                    paste(vars[1], vars[2], sep = " * "))

      preds <- as.character(na.omit(preds))
      eqs <- paste("~", preds, sep = " ")

      eqstouse <- rep(eqs, each = length(distsselected))
      diststouse <- rep(distsselected, length(eqs))
      Nmods <- length(eqstouse)

    # set up the output

      output <- vector("list", Nmods)
      names(output) <- eqstouse

    # iterate across models

      for(i in 1:Nmods){

        output[[i]] <- CPmod(data = data, obs_survobj = obs_survobj, 
                           eqtouse = eqstouse[i], disttouse = diststouse[i])
      }

    # return

      return(output)
  }


##############################################################################
#
# CPmodsetsacrosssizes 
#
##############################################################################

  CPmodsetsacrosssizes <- function(data, vars, sizeclasscol, 
                                   ltpc, ftac, ... ){

    # set up the size classes 

      sccol <- which(colnames(data) == sizeclasscol)

      sizeclasses <- as.character(unique(data[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(data[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(data))] <- "1"

    # set up the output

      output <- vector("list", Nsizeclasses)
      names(output) <- sizeclasses

    # iterate across the size classes

      for(i in 1:Nsizeclasses){

        sizeclass <- sizeclasses[i]
        datai <- data[sizes == sizeclass , ]
        output[[i]] <- CPmodset(data = datai, vars = vars, 
                                ltpc = ltpc, ftac = ftac)

      }

    # return

      return(output)

  }


##############################################################################
#
# ThetaCPcreateacrosssizes
#
##############################################################################

  ThetaCPcreateacrosssizes <- function(CPdata, CPvars, sizeclasscol, 
                                       CPmods, Niterations, ... ){ 

    # set up the factor combination table and count the cells

      CPfct <- factorcombinations(pvars = CPvars, data = CPdata)
      Ncells <- nrow(CPfct)

    # numer of models 

      Nmodels <- length(CPmods[[1]])

    # size classes

      sccol <- which(colnames(CPdata) == sizeclasscol)
      sizeclasses <- as.character(unique(CPdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

    # create a theta for each combination of factors (cell) within 
    #   models within each size class

      thetaCP <- array(NA, dim = c(Niterations, 2, Ncells, Nmodels, 
                   Nsizeclasses))

    # for each size class

      for(i in 1:Nsizeclasses){

        for(j in 1:Nmodels){

          # select the model

            smod <- CPmods[[i]][[j]]

          # note the distribution

            distforuse <- smod$dist
				
          # pull the mean values and vcv matrix from the selected model 

            means <- c(smod$coef, log(smod$scale))
            vcv <- smod$var

          # in the case of exponential distributions, the vcv from the model
          #  is missing the scale column and row (because they're all 0s), 
          #  so we add them   
			
            vcv <- cbind(vcv, rep(0, length = length(means) - ncol(vcv)))
            vcv <- rbind(vcv, rep(0, length = length(means) - nrow(vcv)))

          # draw the Niterations of model terms

            modelTermDraws <- rmvnorm(Niterations, means, vcv, method = "svd")

          # create a model matrix to translate model terms to cells 

            mm <- model.matrix(as.formula(paste("~",
                                           as.character(formula(smod)[3],
                                           collapse = NULL))), data = CPfct)

          # fill in for each cell

            for(k in 1:Ncells){

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
                    exp(as.matrix(modelTermDraws[ , 1:ncol(mm)]) %*% mm[k, ]),
                   weibull = 
                    exp(as.matrix(modelTermDraws[ , 1:ncol(mm)]) %*% mm[k, ]),
                   loglogistic = 
                    exp(as.matrix(modelTermDraws[ , 1:ncol(mm)]) %*% mm[k, ]),
                   lognormal = 
                    (as.matrix(modelTermDraws[ , 1:ncol(mm)]) %*% mm[k, ])
                    )

              thetaCP[ , , k, j, i] <- matrix(c(a, b), byrow = F, ncol = 2)

            }			

          }

      }

    # return

      return(thetaCP)

  }

##############################################################################
#
# AICtabcreateCPmods
#
##############################################################################


  AICtabcreateCPmods <- function(CPmods, sortby = "AIC", ...){

    # determine number of size classes

      nsc <- length(CPmods)

    # determine number of models w/in size classes

      nmods <- length(CPmods[[1]])

    # set up list of tables for output

      output <- vector("list", nsc)

    # fill in

      for(i in 1:nsc){

        tsctab <- data.frame(matrix(NA, nrow = nmods, ncol = 4))
        colnames(tsctab) <- c("model", "ditsribution", "AIC", "AICc")

        for(j in 1:nmods){

          modAIC <- AIC(CPmods[[i]][[j]]) 
          modnpar <- (CPmods[[i]][[j]])$df
          modnobs <- length((CPmods[[i]][[j]])$linear.predictors)
          modAICc <- modAIC + (2 * modnpar * (modnpar + 1) ) / 
                                (modnobs - modnpar - 1 )

          tsctab[j, ] <- c(
                           as.character(names(CPmods[[i]])[j]),
                           as.character(CPmods[[i]][[j]]$dist),

                           round(modAIC, 3),
                           round(modAICc, 3))

        }

        AICorders <- order(as.numeric(tsctab[ , "AIC"]))
        AICcorders <- order(as.numeric(tsctab[ , "AICc"]))

        orders <- switch(sortby, 
                          NULL = 1:nmods,
                          "AIC" = AICorders,
                          "AICc" = AICsorders) 
        outtab <- tsctab[orders, ]
        output[[i]] <- outtab

   
      }

    # add size col names

      names(output) <- names(CPmods)

    # return

      return(output)

  }


##############################################################################
#
# CPgraphcreate 
#
##############################################################################


 CPgraphcreate <- function(CPmods, CPdata, CPvars, thetaCP, Niterations, 
                              timeunit, sizeclasscol, CPltp, CPfta, r, 
                              modelcomplexity, distchoice, ...){


    # set up the response (surv object)

      t1 <- CPdata[ , which(colnames(CPdata) == CPltp)]
      t2 <- CPdata[ , which(colnames(CPdata) == CPfta)]


      event <- rep(3, length(t1))
      event[which(is.na(t2))] <- 0
      event[which(t1 == t2)] <- 1
      event[which(t1 == 0)] <- 2

      t1[which(t1 == 0)] <- t2[which(t1 == 0)]

      CPobvs_survobj <- Surv(time = t1, time2 = t2, event = event, 
                              type = "interval")

    # size classes

      sccol <- which(colnames(CPdata) == sizeclasscol)

      sizeclasses <- as.character(unique(CPdata[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(CPdata[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(CPdata))] <- "1"

    # models

      Nmodels <- length(CPmods[[1]])

      CPmodnames1 <- names(CPmods[[1]])
      nCPmodnames1 <- length(CPmodnames1)
      CPmodcomps <- CPmodnames1[seq(1, nCPmodnames1, 4)]

    # set up the cells via the factor combination table

      fct <- factorcombinations(pvars = CPvars, data = CPdata) 
      Ncells <- nrow(fct)

      pv1 <- NULL
      pv2 <- NULL
      pv1 <- CPvars[1][length(CPvars) > 0]
      pv2 <- CPvars[2][length(CPvars) > 1]
      lev1 <- as.character(unique(CPdata[, pv1]))
      lev2 <- as.character(unique(CPdata[, pv2]))
      nlev1 <- length(lev1)
      nlev2 <- length(lev2)
      nlev1[length(lev1) == 0] <- 1
      nlev2[length(lev2) == 0] <- 1

    # combine factors

      combnames <- rep(NA, nrow(CPdata))

      for(i in 1:nrow(CPdata)){
        colchoice <- which(colnames(CPdata) %in% c(pv1, pv2))
        colchoice2 <- colchoice[c(which(colnames(CPdata)[colchoice] == pv1),
                       which(colnames(CPdata)[colchoice] == pv2))]
        tempname <- paste(as.character(t(CPdata[i, 
                           colchoice2])), 
                           collapse = "")
        tempname[tempname == ""] <- "all"
        combnames[i] <-  tempname
      }


    # for a given size class and model complexity create a matrix of panels 
    #  (one for each cell)

      maxx <- max(na.omit(t2))
      maxx <- ceiling(maxx * 1.1)
      predxs <- seq(0, maxx, length.out = 1000)

    # the four distribution models associated with model complexity

      distchoicenumber <- which(c("exponential", "weibull", 
                            "loglogistic", "lognormal") == distchoice)
      modelcomplexitynumber <- which(CPmodcomps == modelcomplexity)
      MODS <- 4 * (modelcomplexitynumber - 1) + 1:4

        par(mfcol = c(nlev1, nlev2))
            par(mar = c(5, 6, 2, 1))

          for(i in 1:Ncells){

            CPobvs_survobj_ri <- CPobvs_survobj[sizes == sizeclasses[r] & 
                                     combnames == fct[i, "CellNames"]]
            CPdata_ri <- CPdata[sizes == sizeclasses[r] & 
                                 combnames == fct[i, "CellNames"], ]
            mform <- formula("CPobvs_survobj_ri ~ 1")

            plot(survfit(mform, data = CPdata_ri ), ylim = c(0, 1), 
                   xlim = c(0, maxx), main = fct[i, "CellNames"], 
                   xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "L", 
                   col = rgb(0.2, 0.2, 0.2, 0.5), lwd = c(2,1, 1))


            axis(1, las = 1, cex.axis = 1.)
            axis(2, las = 1, cex.axis = 1., at = seq(0, 1, .2))
            mtext(side = 1, timeunit, line = 3., cex = 0.75)
            mtext(side = 2, "Carcass Persistence", line = 4, cex = 0.75)


            thick <- rep(1, 4)
            thick[distchoicenumber] <- 4
            cols <- matrix(c(0.80, 0.38, 0.56, 1,
                             1.00, 0.76, 0.15, 1,
                             0.00, 1.00, 1.00, 1,
                             0.00, 0.41, 0.55, 1), byrow = T, nrow = 4)
            cols[distchoicenumber, 4] <- 0.8           

            for(j in 1:4){

              thetaCPrji <- thetaCP[, , i, MODS[j], r]
              meanpar <- apply(thetaCPrji, 2, mean)
              lqpar <- apply(thetaCPrji, 2, quantile, probs = 0.025)
              uqpar <- apply(thetaCPrji, 2, quantile, probs = 0.975)
              distrj <- CPmods[[r]][[MODS[j]]]$dist

              pts <- predxs[2:length(predxs)]
              pta0 <- rep(0, length(pts)) 
              pta1 <- rep(0.000001, length(pts))
              predys <- ppersist(persistence_distn = distrj, t_arrive0 = pta0, 
                                t_arrive1 = pta1, t_search = pts, 
                                pda = meanpar[1], pdb = meanpar[2]) 

              points(pts, predys, type = 'l', 
                      col = rgb(cols[j,1], cols[j,2], cols[j,3], cols[j,4]), 
                      lwd = thick[j])
 
            }

              j <- distchoicenumber
              j2 <- j
              j2[length(j2) == 0] <- 1
              thetaCPrji <- thetaCP[, , i, MODS[j], r]
              meanpar <- apply(thetaCPrji, 2, mean)
              lqpar <- apply(thetaCPrji, 2, quantile, probs = 0.025)
              uqpar <- apply(thetaCPrji, 2, quantile, probs = 0.975)
              distrj <- CPmods[[r]][[MODS[j2]]]$dist

              pts <- predxs[2:length(predxs)]
              pta0 <- rep(0, length(pts)) 
              pta1 <- rep(0.000001, length(pts))
              predys <- ppersist(persistence_distn = distrj, t_arrive0 = pta0,
                                t_arrive1 = pta1, t_search = pts, 
                                pda = meanpar[1], pdb = meanpar[2]) 
              cols[distchoicenumber, 4] <- 1.0
              points(pts, predys, type = 'l', 
                        col = rgb(cols[j,1], cols[j,2], cols[j,3], cols[j,4]),
                        lwd = thick[j])
 

          }

}


##############################################################################
#
# gcreateacrosssizes
#
##############################################################################

  gcreateacrosssizes <- function(CPdata, SEdata, SSdata, Niterations, 
                                 CPvars, SEvars, 
                                 thetaCP, thetaSE, CPmods, SEmodstouse,
                                 CPmodstouse, ...){
 

    # combine the factors across the models

      expanded <- crossmodelcells(CPvars, SEvars, CPdata, SEdata)

      Ncombcell <- nrow(expanded)
      combname <- rep(NA, Ncombcell)
      
    # now align those cells with each of the CP and SE cells

      mapmat <- matrix(NA, nrow = Ncombcell, ncol = 3)
      mapmat[ , 1] <- 1:Ncombcell

      SEfct <- factorcombinations(SEvars, data = SEdata)
      CPfct <- factorcombinations(CPvars, data = CPdata)

      EXPinSE <- which(colnames(SEfct) %in% colnames(expanded) )
      SEinEXP <- which(colnames(expanded) %in% colnames(SEfct))

      EXPinCP <- which(colnames(CPfct) %in% colnames(expanded) )
      CPinEXP <- which(colnames(expanded) %in% colnames(CPfct))

      for(i in 1:Ncombcell){

        EXPse <- paste(t(t(expanded[i, SEinEXP])), collapse=" ")
        SEops <- rep(NA, nrow(SEfct))
        for(j in 1:nrow(SEfct)){
          SEops[j] <-  paste(t(t(SEfct[ j, EXPinSE])), collapse = " ")
        }
        mapmat[i , 2] <- which(SEops == EXPse)

        EXPcp <- paste(t(t(expanded[i, CPinEXP])), collapse=" ")
        CPops <-  rep(NA, nrow(CPfct))
        for(j in 1:nrow(CPfct)){
          CPops[j] <-  paste(t(t(CPfct[ j, EXPinCP])), collapse = " ")
        }

        mapmat[i , 3] <- which(CPops == EXPcp)			
   
        combname[i] <- paste(as.character(t(expanded[i, ])), collapse = "_")

      }

      colnames(mapmat) <- c("CombCell", "SEcell", "CPcell")

    # search schedules

      Nss <- length(SSdata)				
      ssops <- 1:Nss

    # Nsizeclasses

      Nsizeclasses <- length(CPmods)
      sizeclasses <- names(CPmods)

    # set up output array

      garray <- array(NA, dim = c(Niterations, 1, Nss, 
                                   Ncombcell, Nsizeclasses),
                          dimnames = list(1:Niterations, 1, ssops,
                                      combname, sizeclasses))


    # estimate g

      for(i in 1:Nsizeclasses){

        for(j in 1:Ncombcell){

          for(k in 1:Nss){

            specificSS <- as.numeric(strsplit(SSdata[k], "_")[[1]])

            specificCPcell <- mapmat[j, "CPcell"]
            specificCPtheta <- thetaCP[ , , specificCPcell, CPmodstouse[i], i]
            specificCPdist <- CPmods[[i]][[CPmodstouse[i]]]$dist

            specificSEcell <- mapmat[j, "SEcell"]
            specificSEtheta <- thetaSE[ , , specificSEcell, SEmodstouse[i], i]

            gvals <- gvec(days = specificSS, CPab = specificCPtheta, 
                           persdist = specificCPdist, seef = specificSEtheta)
				
            garray[ , , k, j, i ] <- gvals

          }
        }
      }


    # return
      
      return(garray)

  }


##############################################################################
#
# gtablecreate 
#
##############################################################################

  gtablecreate <- function(garray, CIw, ... ){

      Niterations <- dim(garray)[1]
      Nss <- dim(garray)[3]
      Ncellcombos <- dim(garray)[4]
      Nclasses <- dim(garray)[5]

      ssops <- dimnames(garray)[3][[1]]
      ccops <- dimnames(garray)[4][[1]]
      scops <- dimnames(garray)[5][[1]]

      quants <- c(0 + (1 - CIw) / 2, 1 - (1 - CIw) / 2 )

      outputtable <- data.frame(matrix(NA, ncol = 6, 
                          nrow = Nss * Ncellcombos * Nclasses))

      rowindex <- 1

      for(r in 1:Nclasses){
        for(j in 1:Nss){
          for(i in 1:Ncellcombos){

            # select the data 

              gs <- garray[1:Niterations, 1, j, i, r]   

            # calc mean

              meang <- round(mean(gs), 4)

            # calc quantiles

              gqs <- round(quantile(gs, prob = quants), 4)

            # combine

              outputtable[rowindex, 1] <- scops[r]
              outputtable[rowindex, 2] <- ssops[j]
              outputtable[rowindex, 3] <- ccops[i]
              outputtable[rowindex, 4] <- meang
              outputtable[rowindex, 5] <- gqs[1]
              outputtable[rowindex, 6] <- gqs[2]
        
            rowindex <- rowindex + 1

          }
        }
      }

    # column names

      colnames(outputtable) <- c("Size", "Search Schedule", 
                                  "Cell Combination", 
                                  "Mean g", 
                                  paste(CIw*100, "% CI lower g", sep = ""),
                                  paste(CIw*100, "% CI upper g", sep = ""))


    # return

      return(outputtable)
  }


##############################################################################
#
# ggraphscreate 
#
##############################################################################



  ggraphscreate <- function(garray, ... ){

      Niterations <- dim(garray)[1]
      Nss <- dim(garray)[3]
      Ncellcombos <- dim(garray)[4]
      Nclasses <- dim(garray)[5]

      ssops <- dimnames(garray)[3][[1]]
      ccops <- dimnames(garray)[4][[1]]
      scops <- dimnames(garray)[5][[1]]

      for(r in 1:Nclasses){
        for(j in 1:Nss){
          for(i in 1:Ncellcombos){

            gs <- garray[1:Niterations, 1, j, i, r]   

            xvals <- seq(0, 1, 0.01)
            nxvs <- length(xvals) - 1
            xv1 <- seq(0, 0.99, 0.01)
            xv2 <- seq(0.01, 1.00, 0.01)  
            hts <- rep(NA, nxvs )

            for(d in 1:nxvs ){
              hts[d] <- length(gs[gs >= xv1[d] & gs < xv2[d]])
            }

            hts <- hts / sum(hts)

            maxy <- max(hts) 
            maxy <- ceiling((maxy * 1.1) * 100) / 100

            filename <- paste("Output/g/size_", scops[r], "_searchschedule_",
                             ssops[j], "_classlevels_", ccops[i],  
                            "_gfig.tiff", sep = "")

            tiff(file = filename, width = 12, height = 12, 
                    units = "in", res = 100)

            par(mar = c(5,6,1,1))
            plot(1, 1, type = 'n', xlab = '', ylab = '', 
                  xaxt = 'n', yaxt = 'n', bty = 'n', 
                  ylim = c(0, maxy), xlim = c(0, 1))

            for(d in 1:nxvs){
              rect(xv1[d], 0, xv2[d], hts[d])
            }
            plotxvals1 <- seq(0, 1, .1)
            plotxvals2 <- seq(0, 1, .05)
            plotxvals3 <- seq(0, 1, .01)
            axis(2, las = 1, cex.axis = 1.25)
            mtext(side = 2, "Probability", line = 4, cex = 2)
            axis(1, las = 1, cex.axis = 1.25, at = plotxvals1)
            axis(1, labels = F, tck = -0.005, at = plotxvals2)
            axis(1, labels = F, tck = -0.002, at = plotxvals3)
            mtext(side = 1, "g", line = 3.5, cex = 2)
            dev.off()



          }
        }
      }
  }


##############################################################################
#
# Mhatgenerator
#
##############################################################################

  Mhatgenerator <- function(COdata, DWPdata, sizeclasscol, splitcol, 
                      unitcol, sscol, CPvars, SEvars,  
                      Niterations, CPdata, SEdata, garray, ...){

    # units 

      ucol <- which(colnames(COdata) == unitcol)
      unitoptions <- as.character(unique(COdata[ , ucol]))
      unitoptions[length(unitoptions) == 0] <- 1
      Nunits <- length(unitoptions)

      units <- as.character(COdata[ , ucol])
      units[rep(length(units) == 0, nrow(COdata))] <- "1"

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

      expanded <- crossmodelcells(CPvars = CPvars, SEvars = SEvars, 
                                   CPdata = CPdata, SEdata = SEdata)

      Ncellcombs <- nrow(expanded)

    # set up the X, Xtilde, Mtilde, and Mhat arrays

      Xarray <- array(NA, dim = c(Ncellcombs, Nss, Nunits, 
                                     Nsplitcats, Nsizeclasses))
      Xtildearray <- array(NA, dim = c(Niterations, Ncellcombs, Nss, Nunits, 
                                        Nsplitcats, Nsizeclasses))
      Mtildearray <- array(NA, dim = c(Niterations, Ncellcombs, Nss, Nunits, 
                                        Nsplitcats, Nsizeclasses))
      Mhatarray <- array(NA, dim = c(Niterations, Nss, Nunits, Nsplitcats, 
                                      Nsizeclasses), 
                           dimnames = list(1:Niterations, ssops, unitoptions, 
                                      splitcats, sizeclasses) )


    # fill it in

      EXPinCO <- which(colnames(COdata) %in% colnames(expanded) )
      COinEXP <- which(colnames(expanded) %in% colnames(COdata))



      if(length(EXPinCO) == 1){
        COpaste <- as.character(COdata[, EXPinCO])
        EXPpaste <- as.character(expanded[, COinEXP])
      }else{
        COexpCO <- COdata[, EXPinCO]
        COexpCO <- COexpCO[, order(colnames(COexpCO))]
        EXPcoEXP <- expanded[, COinEXP]
        EXPcoEXP <- EXPcoEXP[, order(colnames(EXPcoEXP))]
        COpaste <- apply(COexpCO, 1, paste, collapse = "")
        EXPpaste <- apply(EXPcoEXP, 1, paste, collapse = "")
      }



      for(r in 1:Nsizeclasses){
        for(l in 1:Nsplitcats){
          for(k in 1:Nunits){    
            for(j in 1:Nss){
              for(i in 1:Ncellcombs){

                spot <- which(COpaste == EXPpaste[i] &
                               sscheds == ssops[j] &
                               units == unitoptions[k] &
                               splits == splitcats[l] &
                               sizes == sizeclasses[r])

                X_specific <- length(spot)
                X_specific[which(is.na(X_specific)) == T] <- 0

                g_specifics <- garray[1:Niterations, 1, 
                                        which(ssops== ssops[j]),
                                        which(EXPpaste == EXPpaste[i]),
                                        which(sizeclasses == sizeclasses[r])]
            


                Xtilde_specifics <- rbinom(Niterations, 
                                            round(X_specific/g_specifics), 
                                            g_specifics)
                Mtilde_specifics <- Xtilde_specifics/g_specifics

                Xarray[i, j, k, l, r] <- X_specific
                Xtildearray[ , i, j, k, l, r] <- Xtilde_specifics
                Mtildearray[ , i, j, k, l, r] <- Mtilde_specifics


              }

              if(Ncellcombs > 1){
                sumMtilde <- apply(Mtildearray[ , 1:Ncellcombs, j, k, l, r], 
                                  1, sum)
              } else {
                sumMtilde <- Mtildearray[ , 1:Ncellcombs, j, k, l, r] 
              }                    


              DWPsccol <- which(colnames(DWPdata) == sizeclasscol)
              DWPucol <- which(colnames(DWPdata) == unitcol)
              DWPsscol <- which(colnames(DWPdata) == sscol)

              DWPspot <- which(DWPdata[ , DWPsccol] == sizeclasses[r] &
                                DWPdata[ , DWPucol] == unitoptions[k] &
                                DWPdata[ , DWPsscol] == ssops[j] )

              DWP_specific <- DWPdata[ DWPspot, "DWP"]

              Mhatspecific <- sumMtilde / DWP_specific
              Mhatspecific[Mhatspecific == "NaN"] <- NA
              Mhatarray[ , j, k, l, r] <- Mhatspecific
            }
          }
        }
      }


    # return 

      return(Mhatarray)

  }



##############################################################################
#
# Mhatcondense 
#
##############################################################################


  Mhatcondense <- function(Mhatarray, ...){

    # condensing to matrix (Mhatl) of dimension Niterations x split categories

      Niterations <- dim(Mhatarray)[1]
      Nsplitcats <- dim(Mhatarray)[4]
      Mhatl <- matrix(NA, nrow = Niterations, ncol = Nsplitcats)
      colnames(Mhatl) <- dimnames(Mhatarray)[[4]]

      for(l in 1:Nsplitcats){

          Mhatl[ , l] <- apply(Mhatarray[  , , , l, ], 1, sum, na.rm = T)

      }
  
    # return

      return(Mhatl)

  }



##############################################################################
#
# Mhattable 
#
##############################################################################

  Mhattable <- function(Mhatl, ffs, CIw, ...){

    # prep out table

      Mhattab <- data.frame(matrix(NA, nrow = ncol(Mhatl), ncol = 2))

      rownames(Mhattab) <- colnames(Mhatl)
      colnames(Mhattab) <- paste(c("Searched Area", "Whole Facility"), 
                                   paste("Mean and ", paste(CIw*100, 
                                         "Percent CI", 
                                           sep = ""), 
                                 " Mortality", sep = ""), sep = " ")

    # expand for whole facility

      Mhatl_wf <- Mhatl / ffs  

    # calc means

      Mhatlmeans <- round(apply(Mhatl, 2, mean), 1)
      Mhatl_wfmeans <- round(apply(Mhatl_wf, 2, mean), 1)

    # calc quantiles

      quants <- c(0 + (1 - CIw) / 2, 1 - (1 - CIw) / 2 )
      Mhatlqs <- round(apply(Mhatl, 2, quantile, prob = quants), 1)
      Mhatl_wfqs <- round(apply(Mhatl_wf, 2, quantile, prob = quants), 1)

    # combine

      Mhattab[ , 1] <- paste(Mhatlmeans, " (", Mhatlqs[1, ], ", ", 
                              Mhatlqs[2, ], ")", sep = "")
      Mhattab[ , 2] <- paste(Mhatl_wfmeans, " (", Mhatl_wfqs[1, ], ", ",
                              Mhatl_wfqs[2, ], ")", sep = "")

      Mhattab <- data.frame("SplitCategory" = rownames(Mhattab), Mhattab)
    # return

      return(Mhattab)

  }

##############################################################################
#
# Mhatgraph
#
##############################################################################


  Mhatgraph <- function(Mhatlspecific, splitcatname, ffs, ... ){

    # iterations

      Niterations <- length(Mhatlspecific)

    # create a facility wide estimate plot

        Mhats <- Mhatlspecific/ffs 
        minM <- min(Mhats)
        minM <- floor(minM - 0.001 * minM)
        maxM <- max(Mhats)
        maxM <- ceiling(maxM + 0.001 * maxM)

        xvals <- seq(minM, maxM, Niterations / 10)
        xvals <- c(xvals, xvals[length(xvals)] +  Niterations / 10)

        nxvs <- length(xvals) - 1
        xv1 <-  xvals[1:(length(xvals)-1)]
        xv2 <-  xvals[2:(length(xvals))]
        hts <-  rep(NA, nxvs)

        for(i in 1:nxvs){
          hts[i] <- length(Mhats[Mhats >= xv1[i] & Mhats < xv2[i]])
        }

        hts <- hts/sum(hts)

        maxy <- max(hts) 
        maxy <- ceiling((maxy * 1.05) * 100) / 100

        par(mar = c(5,6,2,1))
        plot(1, 1, type = 'n', xlab = '', ylab = '', 
              xaxt = 'n', yaxt = 'n', bty = 'n', 
              ylim = c(0, maxy), xlim = c(xvals[1], xvals[length(xvals)]))

        for(i in 1:nxvs){
          rect(xv1[i], 0, xv2[i], hts[i])
        }
        axis(2, las = 1, cex.axis = 1.25)
        mtext(side = 2, "Probability", line = 4, cex = 2)
        axis(1, las = 1, cex.axis = 1.25, at = xvals)
        mtext(side = 1, "Mhat", line = 3.5, cex = 2)
        mtext(side = 3, splitcatname, line = 0 , cex = 1.5)


  }


##############################################################################
#
# factorcombinations
#
##############################################################################

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

##############################################################################
#
# crossmodelcells
#
##############################################################################


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


##############################################################################
#
# pkfunction
#
##############################################################################

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


##############################################################################
#
# gvec 
#
##############################################################################

  gvec <- function(days, CPab, persdist, seef, k = NULL){

    nsim <- dim(CPab)[1]
    samtype <- ifelse(length(unique(diff(days))) == 1, "Formula", "Custom")
    nsearch <- length(days) - 1
    if(persdist %in% c("Exponential", "exponential")){
      pdb <- CPab
      pda <- 1/pdb
      pdb0 <- exp(mean(log(pdb)))
      pda0 <- 1/pdb0
    } else {
      pda <- CPab[, 1]
      pdb <- CPab[, 2]
      if(persdist %in% c("Weibull", "weibull", 
                          "Log-logistic", "loglogistic")){
        pdb0 <- exp(mean(log(pdb)))
        pda0 <- 1/mean(1/pda)
      } else if (persdist %in% c("Lognormal", "lognormal")){
        pdb0 <- mean(pdb)
        pda0 <- mean(sqrt(pda))^2
      }
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
      gadj <- ifelse(max(prob_obs) <= 1/gadj, gadj, 1)
      prob_obs * gadj
  }

##############################################################################        
#
# ppersist
#
##############################################################################        

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


##############################################################################
#
# logit
#
##############################################################################

  logit <- function(x){
    log(x/(1-x))
  }

##############################################################################
#
# alogit
#
##############################################################################

  alogit <- function(x){
    1/(1+exp(-x))
  }

##############################################################################
#
# SSveccreate
#
##############################################################################

  SSveccreate <- function(SSdata, ...){

    unitops <- unique(SSdata$Unit)
    Nunits <- length(unitops)
    maxvisits <- 0
    for(i in 1:Nunits){
      maxvisits <- max(c(maxvisits, 
                         length(SSdata$Unit[SSdata$Unit == unitops[i]])))
    }

    Nunitss <- matrix(NA, nrow = Nunits, ncol = maxvisits)

    for(i in 1:Nunits){
      visitdates <- as.Date(as.character(SSdata$DateSearched[SSdata$Unit
                               == unitops[i]]), format = "%m/%d/%Y")
      visitdays <- visitdates - visitdates[1]
      Nunitss[i, 1:length(visitdays)] <- visitdays
    }

    pastecombo <- apply(Nunitss, 1, paste, collapse = "_")
    uniquepastecombo <- unique(pastecombo)
 
    Nuss <- length(uniquepastecombo)

    for(i in 1:Nuss){
      uniquepastecombo[i] <- gsub("_NA", "", uniquepastecombo[i])
    }

    return(uniquepastecombo)
  }



##############################################################################
#
# DWPtablecreate
#
##############################################################################



  DWPtablecreate <- function(SSdata, ...){

    unitops <- unique(SSdata$Unit)
    Nunits <- length(unitops)
    maxvisits <- 0
    for(i in 1:Nunits){
      maxvisits <- max(c(maxvisits, 
                          length(SSdata$Unit[SSdata$Unit == unitops[i]])))
    }

    Nunitss <- matrix(NA, nrow = Nunits, ncol = maxvisits)

    for(i in 1:Nunits){
      visitdates <- as.Date(as.character(SSdata$DateSearched[SSdata$Unit 
                                == unitops[i]]), format = "%m/%d/%Y")
      visitdays <- visitdates - visitdates[1]
      Nunitss[i, 1:length(visitdays)] <- visitdays
    }

    pastecombo <- apply(Nunitss, 1, paste, collapse = "_")
    uniquepastecombo <- unique(pastecombo)
    Nuss <- length(uniquepastecombo) 

    alignss <- rep(NA, Nunits)
    for(i in 1:Nunits){
      alignss[i] <- which(uniquepastecombo == pastecombo[i])
    }

    DWPbysizecols <- grep("DWP", colnames(SSdata))
    Pbscnames <- colnames(SSdata)[DWPbysizecols]
    sizes <- gsub("DWP_", "", Pbscnames)
    Nsizes <- length(sizes)

    OTsize <- rep(sizes, Nuss * Nunits)
    OTss <- rep(rep(1:Nuss, each = Nsizes), Nunits)
    OTunit <- rep(1:Nunits, each = Nuss * Nsizes)
    OTdwp <- rep(0, length(OTsize))

    for(i in 1:Nunits){

      DWPspecific <- (SSdata[which(SSdata$Unit == unitops[i])[1], 
                                    DWPbysizecols])
      SSspecific <- alignss[i]
      OTdwp[which(OTunit == unitops[i] 
                    & OTss == SSspecific)] <- as.numeric(DWPspecific)
    }

    outtable <- data.frame(DWP = OTdwp, Size = OTsize, 
                            SearchSchedule = OTss, Unit = OTunit)

    return(outtable)

  }
