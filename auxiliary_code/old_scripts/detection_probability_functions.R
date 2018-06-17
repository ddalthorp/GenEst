#' Calculate detection probability (g) for a specific set of CP and SE 
#'  parameters and a specific search schedule.
#' 
#' @param days Specific search schedule.
#' @param CPab Matrix of parameters from CP theta
#'                (rows: replicates, columns: a, b).
#' @param persdist Distribution to use.
#' @param seef Matrix of parameters from SE theta 
#'                (rows: replicates, columns: p, k).
#' @param k k.
#' @return Simulated detection probabilities (length = replicates).
#' @examples
#' NA
#' @export 

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

      intxsearch <- unique(cbind(schedule[,2] - schedule[,1], schedule[,3] - schedule[,2]), MAR = 1)
      ppersu <- ppersist(distr = persdist,
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
          powk <- matrixStats::rowCumprods(powk)
          pfind.si <- pk[,1] * powk * cbind(rep(1, nsim),
                        matrixStats::rowCumprods(1 - (pk[,1] * powk[, 1:maxmiss])))
        }

      intxsearch <- unique(cbind(schedule[,2] - schedule[,1],
                     schedule[,3] - schedule[,2]), MAR = 1)
      ppersu <- ppersist(
	                      distr = persdist,
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


#' Calculate detection probability (g) across all sizes.
#' 
#' @param cp_data Carcass persistence data.
#' @param se_data Seacher efficiency data.
#' @param ss_data Search schedule data.
#' @param replicates Number or iterations.
#' @param cp_predictors Carcass persistence predictor variables.
#' @param se_predictors Seacher efficiency predictor variables.
#' @param cp_theta Carcass persistence theta.
#' @param se_theta Seacher efficiency theta.
#' @param cp_models Carcass persistence model list.
#' @param cp_models_to_use Carcass persistence model to use.
#' @param se_models_to_use Seacher efficiency model to use.
#' @return Array of g values 
#'         (replicates, search schedules, cell combinations, size classes).
#' @examples
#' NA
#' @export 

  estimate_g_across_sizes <- function(cp_data, se_data, ss_data, replicates, 
                                 cp_predictors, se_predictors, 
                                 cp_theta, se_theta, cp_models, 
                                 cp_models_to_use,
                                 se_models_to_use){
 
    # prep search schedules

      SSs <- create_ss_vec(data = ss_data)

    # combine the factors across the models

      expanded <- combine_factors_across_models(cp_predictors, se_predictors, 
	                                          cp_data, se_data)

      Ncombcell <- nrow(expanded)
      combname <- rep(NA, Ncombcell)
      
    # now align those cells with each of the CP and SE cells

      mapmat <- matrix(NA, nrow = Ncombcell, ncol = 3)
      mapmat[ , 1] <- 1:Ncombcell

      SEfct <- combine_factors(predictors = se_predictors, data = se_data)
      CPfct <- combine_factors(predictors = cp_predictors, data = cp_data)

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

      Nss <- length(SSs)				
      ssops <- 1:Nss

    # Nsizeclasses

      Nsizeclasses <- length(cp_models)
      sizeclasses <- names(cp_models)

    # set up output array

      garray <- array(NA, dim = c(replicates, Nss, 
                                   Ncombcell, Nsizeclasses),
                          dimnames = list(1:replicates, ssops,
                                      combname, sizeclasses))

    # estimate g

      for(i in 1:Nsizeclasses){

        for(j in 1:Ncombcell){

          for(k in 1:Nss){

            specificSS <- as.numeric(strsplit(SSs[k], "_")[[1]])

            specificCPcell <- mapmat[j, "CPcell"]
            specificCPtheta <- cp_theta[ , , specificCPcell, 
                                        cp_models_to_use[i], i]
            specificCPdist <- cp_models[[i]][[cp_models_to_use[i]]]$dist

            specificSEcell <- mapmat[j, "SEcell"]
            specificSEtheta <- se_theta[ , , specificSEcell, 
                                        se_models_to_use[i], i]

            gvals <- gvec(days = specificSS, CPab = specificCPtheta,
                                persdist = specificCPdist,
                                seef = specificSEtheta)
				
            garray[ , k, j, i ] <- gvals

          }
        }
      }


    # return
      
      return(garray)

  }

#' Summarize the replicates of detection probability according to each 
#'  size class, search schedule, and cell combination.
#' 
#' @param garray Array of detection probabilities.
#' @param confidence_level Confidence level for the summary.
#' @return Table of detection probabilities with means and confidence level 
#' @examples
#' NA
#' @export 

  create_g_table <- function(garray, confidence_level ){

      Niterations <- dim(garray)[1]
      Nss <- dim(garray)[2]
      Ncellcombos <- dim(garray)[3]
      Nclasses <- dim(garray)[4]

      ssops <- dimnames(garray)[2][[1]]
      ccops <- dimnames(garray)[3][[1]]
      scops <- dimnames(garray)[4][[1]]

      quants <- c(0 + (1 - confidence_level) / 2, 
	              1 - (1 - confidence_level) / 2 )

      outputtable <- data.frame(matrix(NA, ncol = 6, 
                          nrow = Nss * Ncellcombos * Nclasses))

      rowindex <- 1

      for(r in 1:Nclasses){
        for(j in 1:Nss){
          for(i in 1:Ncellcombos){

            # select the data 

              gs <- garray[1:Niterations, j, i, r]   

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
                                  paste(confidence_level*100, 
                                        "% CI lower g", sep = ""),
                                  paste(confidence_level*100, 
                                        "% CI upper g", sep = ""))


    # return

      return(outputtable)
  }
