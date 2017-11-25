#' Fit a single searcher efficiency model for a single size class.
#' 
#' @param pequation Equation for p (character). 
#' @param kequation Equation for k (character).
#' @param factor_combination_table Factor combination table.
#' @param data Searcher efficiency data restricted to a single size class.
#' @param observations Two-column table of searches missed and searches found.
#' @param observation_columns Indicator vector of which columns in the data
#          are the searches.
#' @param init_k_value Initial value of k to use in optim.
#' @param fix_k Logical of whether k should be fixed.
#' @param fix_k_value Value of k if fixed. 
#' @return Model fit list.
#' @examples
#' NA

  se_model_fit <- function(pequation, kequation, factor_combination_table, 
                                 data, observations, observation_columns, 
                                 init_k_value, fix_k, fix_k_value ){ 

    # set up the formulas for p and k
	# if k is fixed, use ~1 instead of kequation verbatim

       pformula <- formula(pequation)
       kformulap <- formula(kequation)
       kformula <- switch(fix_k, 
                             NO = formula(kequation),
                             YES = formula("~1"))

    # set up the model 

      Xp <- model.matrix(pformula, data)
      np <- dim(Xp)[2]

      Xk <- model.matrix(kformula, data)
      nk <- dim(Xk)[2]

    # create the model matrices

      miniXp <- model.matrix(pformula, factor_combination_table)
      miniXk <- model.matrix(kformula, factor_combination_table)
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

      s1data <- data[, observation_columns[1]]

      for(k in 1:dim(miniXp)[1]){
        empp[which(groups==k)] <- mean(s1data[which(groups == k & 
                                       s1data >= 0)], na.rm = TRUE)
      }
      empp[which(empp == 0)] <- 0.1
      empp[which(empp == 1)] <- 0.9 
      theta <- c(solve(t(Xp) %*% Xp) %*% t(Xp) %*% logit(empp), 
                 logit(rep(init_k_value, nk)))

      theta <- switch(fix_k, 
                      YES = theta[-length(theta)],
                      NO = theta)

    # run the pk model

      result <- optim(par = theta, fn = pkfunction, method = "BFGS", 
                        hessian = T, groups = groups, n_theta_p = np,
                        facts = facts, 
                        fix_k = fix_k, fix_k_value = fix_k_value,
                        searches_missed = observations[,"zeros"], 
                        search_found = observations[,"found"],  
                        maxmiss = max(observations[,"zeros"]))

    # prep the output

      output <- vector("list", 15) 
      names(output) <- c("pmodel", "kmodel", "betaphat", "betakhat", 
                         "vartheta", "AIC", "AICc", "convergence",
                         "miniXp", "miniXk", "np", "nk", "fixedK",
                         "pmodelname", "kmodelname")
      output$pmodel <- pformula
      output$kmodel <- kformulap
      output$betaphat <- result$par[1:ncol(Xp)]
      output$betakhat <- switch(fix_k,
                                NO = result$par[(ncol(Xp)+1):length(theta)],
                                YES = NULL)
      output$vartheta <- solve(result$hessian)

      npar <- length(result$par)
      nobs <- nrow(observations)
      output$AIC <- 2 * result$value + 2 * npar
      output$AICc <- output$AIC + 
                       (2 * npar * (npar + 1)) / (nobs - npar - 1)
      output$convergence <- result$convergence
      output$miniXp <- miniXp
      output$miniXk <- miniXk
      output$np <- np
      output$nk <- nk
      output$fixedK <- fix_k_value

      output$pmodelname <- model_namer(pequation)
      output$kmodelname <- model_namer(kequation)
 
    # return

      return(output)

  }
  
#' Fit all possible searcher efficiency models for a single size class
#' 
#' @param predictors Names of predictor variables to include.
#' @param data Searcher efficiency data restricted to a single size class.
#' @param observation_columns Indicator vector of which columns in the data 
#          are the searches.
#' @param init_k_value Initial value of k to use in optim.
#' @param fix_k Logical of whether k should be fixed.
#' @param fix_k_value Value of k if fixed. 
#' @return List of model fit lists.
#' @examples
#' NA

  se_model_set_fit <- function(predictors, data, init_k_value, fix_k, 
                                     fix_k_value, observation_columns ){

    # set up the response data (observations)

    #   zeros: # of times the carcass was missed
    #   found: # of search on which the carcass was found

      zeros <- matrixStats::rowCounts(as.matrix(data[, observation_columns]), 
	                     value = 0, na.rm = T)
      foundInd <- which(matrixStats::rowCounts(
	                                 as.matrix(data[, observation_columns]),
	                                 value = 1, na.rm = T) == 1)
      found <- numeric(length(zeros))
      found[foundInd] <- zeros[foundInd] + 1
      observations <- cbind(zeros, found)

    # set up the factor combinations
 
      fct <- combine_factors(predictors = predictors, data = data)

    # set up the models

      preds <- rep(NA, 5)
      preds[length(predictors) == 0][1] <- c("1")
      preds[length(predictors) == 1][1:2] <- c("1", predictors[1])
      preds[length(predictors) == 2] <- c("1", predictors[1],  predictors[2],
	                                     paste(predictors[1], 
                                                 predictors[2], sep = " + "),
                                           paste(predictors[1], 
                                                 predictors[2], sep = " * "))
      preds <- as.character(na.omit(preds))
      eqs <- paste("~", preds, sep = " ")

      peqs <- switch(fix_k, 
                     NO = rep(eqs, length(eqs)),
                     YES = eqs)
      keqs <- switch(fix_k,
                     NO = rep(eqs, each = length(eqs)),
                     YES = rep(paste("~fixed_at_", fix_k_value, sep = ""), 
                             length(eqs)))
      Nmods <- length(peqs)

    # set up the output

      output <- vector("list", Nmods)

    # run se_model_fit for each of the models

      for(i in 1:Nmods){

        output[[i]] <- se_model_fit(pequation = peqs[i], kequation = keqs[i],  
                             factor_combination_table = fct, data = data, 
                             observation_columns = observation_columns, 
                             observations = observations, 
                             init_k_value = init_k_value,  
                             fix_k = fix_k, fix_k_value = fix_k_value)

      }

          names(output) <- paste("p: ", peqs, ", k: ", 
                                      keqs, sep = "")

    # return

      return(output)

  } 

#' Fit all possible searcher efficiency models across all size classes.
#' 
#' @param predictors Names of predictor variables to include.
#' @param data Searcher efficiency data.
#' @param observation_columns Indicator vector of which columns in the data
#          are the searches.
#' @param size_class_column Column header for the size class column.
#' @param init_k_value Initial value of k to use in optim.
#' @param fix_k Logical of whether k should be fixed.
#' @param fix_k_value Value of k if fixed. 
#' @return List of model fit lists.
#' @examples
#' NA

  se_model_set_across_sizes_fit <- function(data, predictors, 
                                            observation_columns, 
                                            size_class_column,
                                            init_k_value, 
                                            fix_k, fix_k_value){

    # set up the size classes

      sccol <- which(colnames(data) == size_class_column)

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

        output[[i]] <- se_model_set_fit(predictors = predictors, data = datai, 
                                observation_columns = observation_columns, 
                                init_k_value = init_k_value, 
                                fix_k = fix_k, fix_k_value = fix_k_value)
      }

    # return

      return(output)

  }

#' Draw replicate samples of parameters from the searcher efficiency models 
#' across all size classes.
#' 
#' @param predictors Names of predictor variables to include.
#' @param data Searcher efficiency data.
#' @param size_class_column Column header for the size class column.
#' @param model_fits Searcher efficiency model fits.
#' @param replicates Number of replicate samples to draw from the 
#'        distribution.
#' @param fix_k Logical of whether k should be fixed.
#' @param fix_k_value Value of k if fixed. 
#' @return Multidimensional array (replicates, 2, factor combination 
#'     cells, models, size classes) of searcher efficiency parameters (p, k).
#' @examples
#' NA


  se_theta_create <- function(data, predictors, size_class_column, 
                                       model_fits, 
                                       replicates, fix_k, fix_k_value ){ 

    # set up the factor combination table and count the cells

      SEfct <- combine_factors(predictors = predictors, data = data)
      Ncells <- nrow(SEfct)

    # numer of models 

      Nmodels <- length(model_fits[[1]])

    # size classes

      sccol <- which(colnames(data) == size_class_column)
      sizeclasses <- as.character(unique(data[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

    # create a theta for each cell within each size class

      thetaSE <- array(NA, dim = c(replicates, 2, Ncells, 
                        Nmodels, Nsizeclasses))

    # for each size class

      for(i in 1:Nsizeclasses){

        for(j in 1:Nmodels){

          # select the model

            smod <- model_fits[[i]][[j]]

          # draw the parameters and combine to form p and k

            betaSim <- mvtnorm::rmvnorm(replicates, mean = c(smod$betaphat, 
                              smod$betakhat), sigma = smod$vartheta, 
                                method = "svd")
            pSim <- alogit(betaSim[,1:smod$np]%*%t(smod$miniXp))
            kSim <- switch(fix_k,
                           NO = alogit(betaSim[,(smod$np + 1):(smod$np + 
                                      smod$nk)] %*%t (smod$miniXk)),
                           YES = matrix(fix_k_value, ncol = Ncells, 
                                      nrow = replicates))

          # fill into the array   

            for(k in 1:Ncells){				

              thetaSE[ , , k, j, i] <- cbind(pSim[,k], kSim[,k])

            }

        } 
      }
      

    # return

      return(thetaSE)

  }  
  
  
#' Function optimized to fit the searcher efficiency models.
#' 
#' @param searches_missed Number of searches when carcass was present but
#'  not found.
#' @param search_found Search on which carcass was found.
#' @param theta Parameters to be optimized.
#' @param n_theta_p Number of parameters associated with theta
#' @param groups Which cell each observation belongs to.
#' @param maxmiss Maximum possible number of misses for a carcass.
#' @param facts Combined pk model matrix.
#' @param fix_k Logical of whether k should be fixed.
#' @param fix_k_value Value of k if fixed. 
#' @return Negative log likelihood of the observations, given the parameters.
#' @examples
#' NA

  pkfunction <- function(searches_missed, search_found, theta, n_theta_p, 
                         groups, maxmiss, facts, 
                         fix_k, fix_k_value){
    theta <- switch(fix_k,
                    NO = theta,
                    YES = c(theta, logit(fix_k_value)))

    Beta <- array(numeric(length(theta) * 2), dim = c(length(theta), 2))  
    Beta[1:n_theta_p,1] <- theta[1:n_theta_p]
    Beta[(n_theta_p+1):length(theta), 2] <- theta[(n_theta_p+1):length(theta)]

    pk <- alogit(facts %*% Beta)

    powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(dim(pk)[1], maxmiss + 1))
    powk[,1] <- 1
    powk <- rowCumprods(powk)

    pmiss <- rowCumprods(matrix(1 - (pk[,1]*powk[,1:(maxmiss+1)]), 
                                  nrow = dim(pk)[1]))

    pfind.si <- cbind(pk[,1], rowDiffs(1 - pmiss))

    -(sum(log(pmiss[cbind(groups[search_found == 0], 
                          searches_missed[search_found == 0])])) + 
      sum(log(pfind.si[cbind(groups[search_found > 0], 
                             search_found[search_found > 0])]))
    )
  }
  

#' Create the  AICc tables for the searcher efficiency models
#' 
#' @param models Search efficiency models fit for each size class.
#' @return List of AICc tables (one table per size class)
#' @examples
#' NA

  se_aicc_table_create <- function(models){

    # determine number of size classes

      nsc <- length(models)

    # determine number of models w/in size classes

      nmods <- length(models[[1]])

    # set up list of tables for output

      output <- vector("list", nsc)

    # fill in

      for(i in 1:nsc){

        tsctab <- data.frame(matrix(NA, nrow = nmods, ncol = 4))
        colnames(tsctab) <- c("p model", "k model", "AICc", "Delta AICc")

        for(j in 1:nmods){

          tsctab[j, 1:3] <- c(
                     paste(as.character(unlist(models[[i]][[j]]$pmodelname)),
                           collapse = " "),
                     paste(as.character(unlist(models[[i]][[j]]$kmodelname)),
                           collapse = " "), 
                     round(unlist(models[[i]][[j]]$AICc), 3))

        }

        AICcorders <- order(as.numeric(tsctab[ , "AICc"]))

        minAICc <- min(as.numeric(tsctab[ , "AICc"]))
        tsctab[ , 4] <- as.numeric(tsctab[ , 3]) - minAICc

        outtab <- tsctab[AICcorders, ]
        output[[i]] <- outtab

      }

    # add size col names

      names(output) <- names(models)

    # return

      return(output)

  }
  
#' Create the figures for the searcher efficiency models
#' 
#' @param data Full searcher efficiency data set.
#' @param predictors Predictor variable names for the searcher efficiency 
#'        models.
#' @param theta Theta array.
#' @param observation_columns Indicator vector of which columns in the data 
#         are the searches.
#' @param replicates Number of replicate samples to draw from the 
#'        distribution.
#' @param size_class_column Column header for the size class column.
#' @param r Size class index.
#' @param j Model index.
#' @param cellwise Index for the cellwise model.
#' @return NA
#' @examples
#' NA


  create_se_figure <- function(data, predictors, theta, observation_columns, 
                              replicates, size_class_column, r, j, 
                              cellwise){

    # size classes

      sccol <- which(colnames(data) == size_class_column)

      sizeclasses <- as.character(unique(data[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(data[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(data))] <- "1"

    # factor set up

      # set up the cells via the factor combination table

        fct <- combine_factors(predictors = predictors, data = data) 
        Ncells <- nrow(fct)

        pv1 <- NULL	
        pv2 <- NULL
        pv1 <- predictors[1][length(predictors) > 0]
        pv2 <- predictors[2][length(predictors) > 1]
        lev1 <- as.character(unique(data[, pv1]))
        lev2 <- as.character(unique(data[, pv2]))
        nlev1 <- length(lev1)
        nlev2 <- length(lev2)
        nlev1[length(lev1) == 0] <- 1
        nlev2[length(lev2) == 0] <- 1

      # combine factors

        combnames <- rep(NA, nrow(data))

        for(i in 1:nrow(data)){
          colchoice <- which(colnames(data) %in% c(pv1, pv2))
          colchoice2 <- colchoice[c(which(colnames(data)[colchoice] == pv1),
                         which(colnames(data)[colchoice] == pv2))]
          tempname <- paste(as.character(t(data[i, 
                             colchoice2])), 
                             collapse = "")
          tempname[tempname == ""] <- "all"
          combnames[i] <-  tempname
        }
  
      # max searches

        maxs <- length(observation_columns)

      # create the figure

        # dummy initial figure

              par(fig = c(0, 1, 0, 0.75))
              par(mar = c(1, 1, 1, 1))
              plot(1,1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', 
                    xlab = "", ylab = "")
              mtext(side = 1, "Search", line = -0.5, cex = 1.75)
              mtext(side = 2, "Searcher Efficiency", line = -0.5, cex = 1.75)

        # divvy up the bottom 3/4 of the image for the cell matrix

              figxspace <- 0.975 / nlev2
              figyspace <- 0.7 / nlev1

              x1 <- rep(figxspace * ((1:nlev2) - 1), each = nlev1) + 0.025
              x2 <- rep(figxspace * ((1:nlev2)), each = nlev1) + 0.025

              y1 <- rep(figyspace * ((nlev1:1) - 1), nlev2) + 0.025
              y2 <- rep(figyspace * ((nlev1:1)), nlev2) + 0.025

              ps <- matrix(NA, nrow = replicates, ncol = Ncells)
              ks <- matrix(NA, nrow = replicates, ncol = Ncells)
              CMps <- matrix(NA, nrow = replicates, ncol = Ncells)
              CMks <- matrix(NA, nrow = replicates, ncol = Ncells)

              par(mar = c(3,3,2,1))

        # plot each cell's figure

          for(i in 1:Ncells){

            # restrict the data and the thetas

                data_ri <- data[which(sizes == sizeclasses[r] & 
                                         combnames == fct[i, "CellNames"]), ]
                NAconv <- is.na(data_ri[, observation_columns])

                arraydim <- length(dim(NAconv))

                if(arraydim == 0){
                  carcassavail <- length(NAconv) - sum(NAconv)
                  carcassfound <- sum(data_ri[, observation_columns], 
                                      na.rm = T)
                } else{
                  carcassavail <- nrow(NAconv) - apply(NAconv, 2, sum)
                  carcassfound <- apply(data_ri[, observation_columns], 2, 
                                        sum, na.rm = T)
                }
                thetarji <- theta[, , i, j, r]

                par(fig = c(x1[i], x2[i], y1[i], y2[i]), new = T)

                meanpar <- apply(thetarji, 2, mean)

                ps[, i] <- thetarji[, 1] 
                ks[, i] <- thetarji[, 2] 

                predxs <- seq(1, maxs, 1)
                predys <- meanpar[1] * meanpar[2] ^ (predxs - 1)

                CMthetarji <- theta[, , i, cellwise, r]
                CMmeanpar <- apply(CMthetarji, 2, mean)

                CMps[, i] <- CMthetarji[, 1] 
                CMks[, i] <- CMthetarji[, 2] 

                CMpredys <- CMmeanpar[1] * CMmeanpar[2] ^ (predxs - 1)

                xpts <- 1:length(observation_columns)

                if(arraydim == 0){
                  ypts <- mean(data_ri[, observation_columns], na.rm = T)
                } else{
                  ypts <- apply(data_ri[, observation_columns], 2, 
                                mean, na.rm = T)
                }
                

                plot(xpts, ypts, ylim = c(0, 1), 
                     xlim = c(0.5, maxs + 0.5), main = fct[i, "CellNames"], 
                     xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "L",
                     col = rgb(0.02, 0.02, 0.02), lwd = 2, pch = 1, 
                     cex = 1.5, cex.main = 0.75)

                axis(1, las = 1, cex.axis = 1.)
                axis(2, las = 1, cex.axis = 1., at = seq(0, 1, .2))


                text(xpts + 0.1, ypts + 0.075, 
                       paste(carcassfound, carcassavail, sep = "/"), 
                       xpd = T, cex = 0.75, col = rgb(0.05, 0.05, 0.05))


                points(predxs, CMpredys, type = 'l', lwd = 3, 
                        col = rgb(0.1, 0.1, 0.1))
                points(predxs, predys, type = 'l', lwd = 3)
 
              }

        # plot the ps and ks at the top

          # ps

              par(mar = c(2,4,2,1))
              par(fig = c(0, .5, 0.725, 0.975), new = T)

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

                rect(i - 0.1 - 0.2, iqp[1], i + 0.1 - 0.2, iqp[2], lwd = 2, 
                        col = rgb(1, 1, 1, 0.4))
                points(c(i - 0.1, i + 0.1) - 0.2, rep(medianp, 2), type = "l",
                        lwd = 2)
                points(c(i - 0.05, i + 0.05) - 0.2, rep(minp, 2), type = "l",
                        lwd = 2)
                points(c(i - 0.05, i + 0.05) - 0.2, rep(maxp, 2), type = "l",
                        lwd = 2)
                points(c(i, i) - 0.2, c(iqp[1], minp), type = "l", lwd = 2) 
                points(c(i, i) - 0.2, c(iqp[2], maxp), type = "l", lwd = 2) 

                CMPS <- CMps[, i]
                CMPX <- runif(length(CMPS), i - 0.1, i + 0.1) + 0.2  
                CMmedianp <- median(CMPS)
                CMmeanp <- mean(CMPS)
                CMiqp <- quantile(CMPS, c(0.25, 0.75))
                CMminp <- min(CMPS)
                CMmaxp <- max(CMPS)

                rect(i - 0.1 + 0.2, CMiqp[1], i + 0.1 + 0.2, CMiqp[2], 
                        lwd = 2, col = 0, border = rgb(0.1, 0.1, 0.1))
                points(c(i - 0.1, i + 0.1) + 0.2, rep(CMmedianp, 2), 
                        type = "l", lwd = 2, col = rgb(0.1, 0.1, 0.1))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMminp, 2), 
                        type = "l", lwd = 2, col = rgb(0.1, 0.1, 0.1))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmaxp, 2), 
                        type = "l", lwd = 2, col = rgb(0.1, 0.1, 0.1))
                points(c(i, i) + 0.2, c(CMiqp[1], CMminp), type = "l", 
                        lwd = 2, col = rgb(0.1, 0.1, 0.1)) 
                points(c(i, i) + 0.2, c(CMiqp[2], CMmaxp), type = "l", 
                        lwd = 2, col = rgb(0.1, 0.1, 0.1)) 

              }

              axis(1, at = 1:Ncells, labels = F, cex.axis = 0.5, line = 0)
              mtext(side = 1, line = 0.75, at = 1:Ncells, fct[, "CellNames"],
                        cex = 1)

              axis(2, las =1, at = seq(0, 1, .2), cex.axis = 1)
              mtext(side = 2, line = 2.75, "p", cex = 1)

          # ks

              par(fig = c(0.5, 1, 0.725, 0.975), new = T)
              par(mar = c(2, 4, 2, 1))

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

                rect(i - 0.1 - 0.2, iqk[1], i + 0.1 - 0.2, iqk[2], 
                        lwd = 2, col = rgb(1, 1, 1))
                points(c(i - 0.1, i + 0.1) - 0.2, rep(mediank, 2), 
                        type = "l", lwd = 2)
                points(c(i - 0.05, i + 0.05) - 0.2, rep(mink, 2), 
                        type = "l", lwd = 2)
                points(c(i - 0.05, i + 0.05) - 0.2, rep(maxk, 2), 
                        type = "l", lwd = 2)
                points(c(i, i) - 0.2 , c(iqk[1], mink), type = "l", lwd = 1)
                points(c(i, i) - 0.2, c(iqk[2], maxk), type = "l", lwd = 1)


                CMKS <- CMks[, i]
                CMKX <- runif(length(CMKS), i - 0.1, i + 0.1) + 0.2  
                CMmediank <- median(CMKS)
                CMmeank <- mean(CMKS)
                CMiqk <- quantile(CMKS, c(0.25, 0.75))
                CMmink <- min(CMKS)
                CMmaxk <- max(CMKS)

                rect(i - 0.1 + 0.2, CMiqk[1], i + 0.1 + 0.2, CMiqk[2], 
                        lwd = 2, col = 0, border = rgb(0.1, 0.1, 0.1))
                points(c(i - 0.1, i + 0.1) + 0.2, rep(CMmediank, 2), 
                        type = "l", lwd = 2, col = rgb(0.1, 0.1, 0.1))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmink, 2), 
                        type = "l", lwd = 2, col = rgb(0.1, 0.1, 0.1))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmaxk, 2), 
                        type = "l", lwd = 2, col = rgb(0.1, 0.1, 0.1))
                points(c(i, i) + 0.2, c(CMiqk[1], CMmink), type = "l", 
                        lwd = 2, col = rgb(0.1, 0.1, 0.1)) 
                points(c(i, i) + 0.2, c(CMiqk[2], CMmaxk), type = "l", 
                        lwd = 2, col = rgb(0.1, 0.1, 0.1)) 


              }

              axis(1, at = 1:Ncells, labels = F, cex.axis = 0.5, line = 0)
              mtext(side = 1, line = 0.75, at = 1:Ncells, fct[, "CellNames"],
                      cex = 1)
              axis(2, las =1, at = seq(0, 1, 0.2), cex.axis = 1)
              mtext(side = 2, line = 2.75, "k", cex = 1)

        # legend at the top

              par(fig = c(0, 1, 0.95, 1), new = T)
              par(mar = c(0, 0, 0, 0))
              plot(1,1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', 
                    xlab = "", ylab = "", ylim = c(0, 1), xlim = c(0, 1))

              rect(0.15, 0.15, 0.2, 0.45, lwd = 2, col = rgb(0, 0, 0), 
                   border = NA)
              text(x = 0.21, y = 0.3, "= Selected Model", adj = 0)
              rect(0.45, 0.15, 0.5, 0.45, lwd = 2, 
                   col = rgb(0.1, 0.1, 0.1), border = NA)
              text(x = 0.51, y = 0.3, "= Cell Means Model", adj = 0)
  }



