#' Fit a single searcher efficiency model for a single size class.
#' 
#' @param pequation Equation for p (character). 
#' @param kequation Equation for k (character).
#' @param factor_combination_table Factor combination table.
#' @param data Searcher efficiency data restricted to a single size class.
#' @param zeros Vector of the number of searches on which each 
#'         carcass was missed.
#' @param found Vector of the search number for the search on which
#'         each carcass was found.
#' @param observation_columns Indicator vector of which columns in the data
#          are the searches.
#' @param init_k_value Initial value of k to use in optim.
#' @param fix_k Logical of whether k should be fixed.
#' @param fix_k_value Value of k if fixed. 
#' @return Model fit list.
#' @examples
#' NA
#' @export 

  se_model_fit <- function(pequation, kequation, factor_combination_table, 
                                 data, zeros, found, observation_columns, 
                                 init_k_value, fix_k, fix_k_value ){ 

    # set up the formulas for p and k
	# if k is fixed, use ~1 instead of kequation verbatim

       pformula <- formula(pequation)
       kformulap <- formula(kequation)
 
       if(fix_k){
         kformula <- formula("~1")
       } else{
         kformula <- formula(kequation)
       }

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

      if(fix_k){
        theta <- theta[-length(theta)]
      } else{
        theta <- theta
      }

    # run the pk model

      result <- optim(par = theta, fn = pkfunction, method = "BFGS",
                        hessian = T, groups = groups, n_theta_p = np,
                        facts = facts,
                        fix_k = fix_k, fix_k_value = fix_k_value,
                        searches_missed = zeros,
                        search_found = found,
                        maxmiss = max(zeros))

    # prep the output

      if(fix_k){
        kformulap <- paste("k fixed at ", fix_k_value, sep = "")
      } else{
        kformulap <- kformulap
      }

      output <- vector("list", 15) 
      names(output) <- c("pmodel", "kmodel", "betaphat", "betakhat", 
                         "vartheta", "AIC", "AICc", "convergence",
                         "miniXp", "miniXk", "np", "nk", "fixedK",
                         "pmodelname", "kmodelname")
      output$pmodel <- pformula
      output$kmodel <- kformulap
      output$betaphat <- result$par[1:ncol(Xp)]

      if(fix_k){
        output$betakhat <- NULL
      } else{
        output$betakhat <- result$par[(ncol(Xp)+1):length(theta)]
      }

      output$vartheta <- solve(result$hessian)

      npar <- length(result$par)
      nobs <- length(zeros)
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
#' @export 

  se_model_set_fit <- function(predictors, data, init_k_value, fix_k, 
                                     fix_k_value, observation_columns ){

    # set up the response data

    #   zeros: # of times the carcass was missed
    #   found: # of search on which the carcass was found

      zeros <- matrixStats::rowCounts(as.matrix(data[, observation_columns]), 
	                     value = 0, na.rm = T)
      foundInd <- which(matrixStats::rowCounts(
	                                 as.matrix(data[, observation_columns]),
	                                 value = 1, na.rm = T) == 1)
      found <- numeric(length(zeros))
      found[foundInd] <- zeros[foundInd] + 1

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

      if(fix_k){
        peqs <- eqs
      } else{
        peqs <- rep(eqs, length(eqs))
      }

      if(fix_k){
        keqs <- rep(paste("~fixed_at_", fix_k_value, sep = ""), 
                             length(eqs))
      } else{
        keqs <- rep(eqs, each = length(eqs))
      }


      Nmods <- length(peqs)

    # set up the output

      output <- vector("list", Nmods)

    # run se_model_fit for each of the models

      for(i in 1:Nmods){

        output[[i]] <- se_model_fit(pequation = peqs[i], kequation = keqs[i],  
                             factor_combination_table = fct, data = data, 
                             observation_columns = observation_columns, 
                             zeros = zeros, found = found, 
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
#' @export 

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
#' @export 


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

            if(fix_k){
              kSim <- matrix(fix_k_value, ncol = Ncells, 
                                      nrow = replicates)
            } else{
              kSim <- alogit(betaSim[,(smod$np + 1):(smod$np + 
                                      smod$nk)] %*%t (smod$miniXk))
            }

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
#' @export 

  pkfunction <- function(searches_missed, search_found, theta, n_theta_p, 
                         groups, maxmiss, facts, 
                         fix_k, fix_k_value){

    if(fix_k){
      theta <- c(theta, logit(fix_k_value))
    } else{
      theta <- theta 
    }

    Beta <- array(numeric(length(theta) * 2), dim = c(length(theta), 2))  
    Beta[1:n_theta_p,1] <- theta[1:n_theta_p]
    Beta[(n_theta_p+1):length(theta), 2] <- theta[(n_theta_p+1):length(theta)]

    pk <- alogit(facts %*% Beta)

    powk <- array(rep(pk[, 2], maxmiss + 1), dim = c(dim(pk)[1], maxmiss + 1))
    powk[,1] <- 1
    powk <- matrixStats::rowCumprods(powk)

    pmiss <- matrixStats::rowCumprods(matrix(1 - (pk[,1]*powk[,1:(maxmiss+1)]),
                                  nrow = dim(pk)[1]))

    pfind.si <- cbind(pk[,1], matrixStats::rowDiffs(1 - pmiss))

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
#' @export 

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
#' @export 


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
                             collapse = ".")
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


                points(predxs, CMpredys, type = 'l', lwd = 3, 
                        col = rgb(0.5, 0.5, 0.5))
                points(predxs, predys, type = 'l', lwd = 3)


                for(j in 1:length(xpts)){
                  rect((xpts[j] + 0.1) - 0.2, (ypts[j] + 0.1) - 0.05,
                       (xpts[j] + 0.1) + 0.2, (ypts[j] + 0.1) + 0.05,
                       border = NA, col = "white")
                }

                text(xpts + 0.1, ypts + 0.1, 
                       paste(carcassfound, carcassavail, sep = "/"), 
                       xpd = T, cex = 0.75, col = rgb(0.05, 0.05, 0.05))


 
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
                        col = rgb(1, 1, 1))
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
                        lwd = 2, col = 0, border = rgb(0.5, 0.5, 0.5))
                points(c(i - 0.1, i + 0.1) + 0.2, rep(CMmedianp, 2), 
                        type = "l", lwd = 2, col = rgb(0.5, 0.5, 0.5))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMminp, 2), 
                        type = "l", lwd = 2, col = rgb(0.5, 0.5, 0.5))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmaxp, 2), 
                        type = "l", lwd = 2, col = rgb(0.5, 0.5, 0.5))
                points(c(i, i) + 0.2, c(CMiqp[1], CMminp), type = "l", 
                        lwd = 2, col = rgb(0.5, 0.5, 0.5)) 
                points(c(i, i) + 0.2, c(CMiqp[2], CMmaxp), type = "l", 
                        lwd = 2, col = rgb(0.5, 0.5, 0.5)) 

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
                        lwd = 2, col = 0, border = rgb(0.5, 0.5, 0.5))
                points(c(i - 0.1, i + 0.1) + 0.2, rep(CMmediank, 2), 
                        type = "l", lwd = 2, col = rgb(0.5, 0.5, 0.5))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmink, 2), 
                        type = "l", lwd = 2, col = rgb(0.5, 0.5, 0.5))
                points(c(i - 0.05, i + 0.05) + 0.2, rep(CMmaxk, 2), 
                        type = "l", lwd = 2, col = rgb(0.5, 0.5, 0.5))
                points(c(i, i) + 0.2, c(CMiqk[1], CMmink), type = "l", 
                        lwd = 2, col = rgb(0.5, 0.5, 0.5)) 
                points(c(i, i) + 0.2, c(CMiqk[2], CMmaxk), type = "l", 
                        lwd = 2, col = rgb(0.5, 0.5, 0.5)) 


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
                   col = rgb(0.5, 0.5, 0.5), border = NA)
              text(x = 0.51, y = 0.3, "= Cell Means Model", adj = 0)
  }


#' Fit a single searcher efficiency model.
#'
#' Searcher efficiency is modeled as a function of the number of times a carcass
#' has been missed in previous searches and any number of covariates. Format and
#' usage parallel that of common \code{R} functions such as \code{lm},
#' \code{glm}, and \code{gam}. However, the input data (\code{data}) is
#' structured differently to accommodate the multiple-search searcher efficiency
#'  trials (see 'Details'), and model formulas may be entered for both \code{p}
#' (akin to an intercept) and \code{k} (akin to a slope).
#'
#' The probability of finding a carcass that is present at the time of search
#' is \code{p} on the first search after carcass arrival and is assumed to
#' decrease by a factor of \code{k} each time the carcass is missed in searches.
#' Both \code{p} and \code{k} may depend on covariates such as ground cover,
#' season, species, etc., and a separate model format (\code{pformula} and
#' \code{kformula}) may be entered for each. The models are entered as they would
#' be in the familiar \code{lm} or \code{glm} functions in R. For example,
#' \code{p} might vary with \code{visibility}, \code{season}, and
#' \code{site}, while \code{k} varies only with \code{visibility}. A user
#' might then enter \code{p ~ visibility + season + site} for \code{pformula}
#' and \code{k ~ visibility} for \code{kformula}. Other R conventions for defining
#' formulas may also be used, with \code{covar1:covar2} for the interaction
#' between covariates 1 and 2 and \code{covar1 * covar2} as short-hand for
#' \code{covar1 + covar2 + covar1:covar2}.
#'
#' Search trial \code{data} must be entered in a data frame with data in each row
#' giving the fate of a single carcass in the field trials. There must be a
#' column for each search occassion, with 0, 1, or NA depending on whether the
#' carcass was missed, found, or not available (typically because it was found
#' and removed on a previous search, had been earlier removed by scavengers, or
#' was not searched for) on the given search occasion. Additional columns with
#' values for categorical covariates (e.g., visibility = E, M, or D) may also be
#' included.
#'
#' @param pformula Formula for p; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 'Details'.
#
#' @param kformula Formula for k; an object of class "\code{\link{formula}}"
#' (or one that can be coerced to that class): a symbolic description of the
#' model to be fitted. Details of model specification are given under 'Details'.
#
#' @param data Dataframe with results from searcher efficiency trials (required).
#
#' @param observation_columns Vector of names of columns in \code{data}
#' where results for each search occasion are stored (optional). If no \code{
#' observation_columns} are provided, \code{pkm} uses as \code{observation_columns}
#' all columns with names that begin with an \code{'s'} or \code{'S'} and end
#' with a number, e.g., 's1', 's2', 's3', etc. This option is included as a
#' convenience for the user, but care must be taken that other data are not
#' stored in columns with names matching that pattern. Alternatively,
#' \code{observation_columns} may be entered as a vector of names, like
#' \code{c('s1', 's2', 's3')}, \code{paste0('s', 1:3)}, or
#' \code{c('initialSearch', 'anotherSearch', 'lastSearch')}.
#
#' @param k Parameter for user-specified \code{k} value (optional). If a
#' value is provided, \code{kformula} is ignored and the model is fit under the
#' assumption that the \code{k} parameter is fixed and known to be \code{fixk}.
#
#' @return \code{pkm} returns an object of class "\code{pkm}", which is a list
#' whose components characterize the fit of the model. Due to the large number
#' and complexity of components, only a subset of them is printed automatically;
#' the rest can be viewed/accessed directly via the \code{$} operator if desired.
#'
#' The following components are displayed automatically:
#'
#' \describe{
#'  \item{\code{call}}{the function call to fit the model}
#'  \item{\code{predictors}}{list of covariates of \code{p} and/or \code{k}}
#'  \item{\code{stats}}{summary statistics for estimated \code{p} and \code{k},
#'    including the medians and upper & lower bounds on 95% CIs for each parameter,
#'    indexed by cell (or combination of covariate levels).}
#'  \item{\code{AIC}}{the \href{https://en.wikipedia.org/wiki/Akaike_information_criterion}{AIC}
#'    value for the fitted model}
#'  \item{\code{AICc}}{the AIC value as corrected for small sample size}
#'  \item{\code{convergence}}{convergence status of the numerical optimization to
#'    find the maximum likelihood estimates of \code{p} and \code{k}. A value of
#'    \code{0} indicates that the model was fit successfully. For help in
#'    deciphering other values, see \code{\link{optim}}.}
#' }
#'
#' The following components are not printed automatically but can be accessed
#' via the \code{$} operator:
#' \describe{
#'   \item{\code{pformula}}{the model formula for the \code{p} parameter}
#'   \item{\code{kformula}}{the model formula for the \code{k} parameter}
#'  \item{\code{betahat_p}}{parameter estimates for the terms in the regression
#'     model for for \code{p} (logit scale)}
#'   \item{\code{betahat_k}}{parameter estimates for the terms in the regression
#'     model for for \code{k} (logit scale). If \code{k} is fixed and known,
#'     \code{betahat_k} is not calculated.}
#'   \item{\code{varbeta}}{the variance-covariance matrix of the estimators
#'     for \code{c(betahat_p, betahat_k}.}
#'   \item{\code{miniXp}}{a simplified design matrix for covariate structure of
#'     \code{pformula}}
#'   \item{\code{miniXk}}{a simplified design matrix for covariate structure of
#'     \code{kformula}}
#'   \item{\code{plevels}}{all levels of each covariate of \code{p}}
#'   \item{\code{klevels}}{all levels of each covariate of \code{k}}
#'   \item{\code{np, nk}}{number of parameters to fit the \code{p} and \code{k}
#'      models}
#'   \item{\code{cells}}{cell structure of the pk-model, i.e., combinations of
#'     all levels for each covariate of \code{p} and \code{k}. For example, if
#'     \code{covar1} has levels \code{"a"}, \code{"b"}, and \code{"c"}, and
#'     \code{covar2} has levels \code{"X"} and \code{"Y"}, then the cells would
#'     consist of \code{a.X}, \code{a.Y}, \code{b.X}, \code{b.Y}, \code{c.X}, and
#'     \code{c.Y}.}
#'   \item{\code{Ncells}}{total number of cells}
#'}
#'
#' @examples
#' head(pkmdat)
#' pkm(p ~ visibility, k ~ 1, data = pkmdat)
#' pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' pkm(p ~ visibility, k = 0.7, data = pkmdat)
#' @export

pkm <- function(pformula, kformula = NULL, data, observation_columns = NULL, k = NULL){
  ans <- list()
  ans$call <- match.call()
  if (missing(observation_columns))
    observation_columns <- grep("^[sS].*[0-9]$", names(data), value = TRUE)
  zeros <- matrixStats::rowCounts(as.matrix(data[, observation_columns]), value = 0, na.rm = T)
  foundInd <- which(matrixStats::rowCounts(
    as.matrix(data[, observation_columns]),  value = 1, na.rm = T) == 1
  )
  found <- numeric(length(zeros))
  found[foundInd] <- zeros[foundInd] + 1
  if (length(attr(terms(pformula), 'factors')) > 0){
    pformula <- as.formula(reformulate(dimnames(attr(terms(pformula), 'factors'))[[2]]))
  } else {
    pformula <- as.formula(~1)
  }
  if (!missing(kformula) & missing(k)){
    if (length(attr(terms(kformula), 'factors')) > 0){
      kformula <- as.formula(reformulate(dimnames(attr(terms(kformula), 'factors'))[[2]]))
    } else {
      kformula <- as.formula(~1)
    }
  }
  nmp <- dimnames(attr(terms(pformula), 'factors'))[[1]]
  if (!missing(kformula) & missing(k)) {
    nmk <- dimnames(attr(terms(kformula), 'factors'))[[1]]
  } else {
    nmk <- NULL
  }
  predictors <- unique(c(nmp, nmk))
  fct <- combine_factors(predictors = predictors, data = data)
  tmp <- se_model_fit(
    pequation = pformula,
    kequation = kformula,
    factor_combination_table = fct,
    data = data,
#    observations = cbind(zeros = zeros, found  = found),
    zeros = zeros,
    found = found,
    observation_columns = observation_columns,
    init_k_value = 0.7,
    fix_k = ifelse(missing(k), FALSE, TRUE),
    fix_k_value = k
  )
  ans$pformula <- pformula
  if (!missing(k)){
    ans$kformula <- c(fixedk = k)
  } else {
    ans$kformula <- kformula
  }
  ans$betahat_p <- tmp$betaphat
  names(ans$betahat_p) <- dimnames(tmp$miniXp)[[2]]
  if (missing(k)){
    ans$betahat_k <- tmp$betakhat
    names(ans$betahat_k) <- dimnames(tmp$miniXk)[[2]]
  }
  ans$varbeta <- tmp$vartheta
  ans$plevels <- .getXlevels(terms(pformula), data)
  if (missing(k) && is.language(kformula))
    ans$klevels <- .getXlevels(terms(kformula), data)
  ans$miniXp <- tmp$miniXp
  ans$miniXk <- tmp$miniXk
  xp <- ans$miniXp
  bhatp <- ans$betahat_p
  np <- dim(xp)[2]
  xk <- ans$miniXk
  bhatk <- ans$betahat_k
  nk <- dim(xk)[2]
  bpMu <- xp %*% bhatp
  bpVar <- xp %*% ans$varbeta[1:np, 1:np] %*% t(xp)
  pmedian <- alogit(qnorm(0.5, mean = bpMu, sd = sqrt(diag(as.matrix(bpVar)))))
  plwr <- alogit(qnorm(0.025, mean = bpMu, sd = sqrt(diag(as.matrix(bpVar)))))
  pupr <- alogit(qnorm(0.975, mean = bpMu, sd = sqrt(diag(as.matrix(bpVar)))))
  if (missing(k)){
    bkMu <- xk %*% ans$varbeta[(np+1):(np+nk)]
    bkVar <- xk %*% ans$varbeta[(1+np):(np + nk), (1+np):(np + nk)] %*% t(xk)
    kmedian <- alogit(qnorm(0.5, mean = bkMu, sd = sqrt(diag(as.matrix(bkVar)))))
    klwr <- alogit(qnorm(0.025, mean = bkMu, sd = sqrt(diag(as.matrix(bkVar)))))
    kupr <- alogit(qnorm(0.975, mean = bkMu, sd = sqrt(diag(as.matrix(bkVar)))))
  } else {
    kmedian <- rep(ans$kformula, length(pmedian))
    klwr <- kmedian
    kupr <- kmedian
  }
  ans$predictors <- predictors
  ans$stats <- data.frame(
    Cell = fct$CellNames,
    pmedian = pmedian,
    plwr = plwr,
    pupr = pupr,
    kmedian = kmedian,
    klwr = klwr,
    kupr = kupr
  )
  ans$cells <- fct
  ans$Ncells <- dim(ans$stats)[1]
  ans$np <- np
  ans$nk <- nk
  ans$AIC <- tmp$AIC
  ans$AICc <- tmp$AICc
  ans$convergence <- tmp$convergence
  if (!missing(k) & !missing(kformula))
    ans$NOTE <- paste0("Both kformula and fixed k were entered by user. k = ",
      k, " is assumed.")
  class(ans) <- c("pkm", "list")
  attr(ans, "hidden") <- c(
    "pformula", "kformula", "betahat_p", "betahat_k", "varbeta",
    "miniXp", "miniXk", "plevels", "klevels", 'np', 'nk', 'cells', 'Ncells'
  )
  return(ans)
}
#' @export
print.pkm <- function (x) {
  hid <- attr(x, "hidden")
  print(x[!names(x) %in% hid])
}
#' Simulate p and k parameters from a fitted pk model.
#'
#' @param nsim the number of simulation draws
#
#' @param pkmodel A \code{\link{pkm}} object (which is returned from \code{pkm()})
#
#' @return Array of \code{nsim} simulated pairs of \code{p} and \code{k} for
#'  cells defined by the \code{pkmodel} object.
#' @examples
#' pkmod1 <- pkm(p ~ 1, k ~ 1, data = pkmdat)
#' simulated_pk <- rpk(nsim = 10, pkmodel = pkmod1)
#' simulated_pk
#' boxplot(simulated_pk[,, 1])
#'
#' pkmod2 <- pkm(p ~ visibility * season, k ~ site, data = pkmdat)
#' rpk(nsim = 10, pkmodel = pkmod2)
#' @export

rpk <- function(nsim, pkmodel){
  np <- pkmodel$np; nk <- pkmodel$nk
  betaSim <- mvtnorm::rmvnorm(nsim,
    mean = c(pkmodel$betahat_p, pkmodel$betahat_k),
    sigma = pkmodel$varbeta, method = "svd")
  pSim <- alogit(betaSim[,1:np]%*%t(pkmodel$miniXp))
  if (is.language(pkmodel$kformula)){
    kSim <- alogit(betaSim[,(np + 1):(np + pkmodel$nk)] %*%t (pkmodel$miniXk))
  } else {
    kSim <- matrix(pkmodel$kformula, ncol = pkmodel$Ncells, nrow = nsim)
  }
  ans <- array(
    dim = c(nsim, 2, pkmodel$Ncells),
    dimnames = list(NULL, c('p','k'), pkmodel$cells[,'CellNames'])
  )
  for(k in 1:pkmodel$Ncells){
    ans[ , , k] <- cbind(pSim[,k], kSim[,k])
  }
  ans
}
