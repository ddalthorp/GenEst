#' Determine the density weighted proportion (DWP) of area searched for
#'  each size by search schedule by unit combination.
#' 
#' @param data Search schedule data.
#' @return Table of DWP values for each size, search schedule, and unit.
#' @examples
#' NA
#' @export 


  create_DWP_table <- function(data){

    unitops <- unique(data$Unit)
    Nunits <- length(unitops)
    maxvisits <- 0
    for(i in 1:Nunits){
      maxvisits <- max(c(maxvisits, 
                          length(data$Unit[data$Unit == unitops[i]])))
    }

    Nunitss <- matrix(NA, nrow = Nunits, ncol = maxvisits)

    for(i in 1:Nunits){
      visitdates <- as.Date(as.character(data$DateSearched[data$Unit 
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

    DWPbysizecols <- grep("DWP", colnames(data))
    Pbscnames <- colnames(data)[DWPbysizecols]
    sizes <- gsub("DWP_", "", Pbscnames)
    Nsizes <- length(sizes)

    OTsize <- rep(sizes, Nuss * Nunits)
    OTss <- rep(rep(1:Nuss, each = Nsizes), Nunits)
    OTunit <- rep(1:Nunits, each = Nuss * Nsizes)
    OTdwp <- rep(0, length(OTsize))

    for(i in 1:Nunits){

      DWPspecific <- (data[which(data$Unit == unitops[i])[1], 
                                    DWPbysizecols])
      SSspecific <- alignss[i]
      OTdwp[which(OTunit == unitops[i] 
                    & OTss == SSspecific)] <- as.numeric(DWPspecific)
    }

    outtable <- data.frame(DWP = OTdwp, Size = OTsize, 
                            SearchSchedule = OTss, Unit = OTunit)

    return(outtable)

  }

#' Calculate Mhat values across size classes, split categories, units,
#'  and cells.
#' 
#' @param co_data Carcass observation data.
#' @param ss_data Search schedule data.
#' @param cp_data Carcass persistence data.
#' @param se_data Seacher efficiency data.
#' @param size_class_column Column header for the size class column.
#' @param split_column Column header for the split column
#' @param unit_column Column header for the unit column
#' @param df_column Column header for the date found column
#' @param cp_predictors Carcass persistence predictor variables.
#' @param se_predictors Seacher efficiency predictor variables.
#' @param replicates Number or iterations.
#' @param garray garray.
#' @return Array of Mhat values 
#'  (iterations, units, split categories, size classes).
#' @examples
#' NA
#' @export 


  estimate_mhat <- function(co_data, ss_data, cp_data, se_data, 
                      size_class_column,  
                      split_column, unit_column, df_column, cp_predictors, 
                      se_predictors,  
                      replicates, garray){

    # create DWP table

      DWPdata <- create_DWP_table(data = ss_data)

    # units 

      ucol <- which(colnames(co_data) == unit_column)
      unitoptions <- as.character(unique(co_data[ , ucol]))
      unitoptions[length(unitoptions) == 0] <- 1
      Nunits <- length(unitoptions)

      units <- as.character(co_data[ , ucol])
      units[rep(length(units) == 0, nrow(co_data))] <- "1"

    # size classes

      sccol <- which(colnames(co_data) == size_class_column)

      sizeclasses <- as.character(unique(co_data[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- "all"
      Nsizeclasses <- length(sizeclasses)

      sizes <- as.character(co_data[ , sccol])
      sizes[rep(length(sizes) == 0, nrow(co_data))] <- "all"

    # split categories

      spcol <- which(colnames(co_data) == split_column)

      splitcats <- as.character(unique(co_data[ , spcol]))
      splitcats[length(splitcats) == 0] <- 1
      Nsplitcats <- length(splitcats)

      splits <- as.character(co_data[ , spcol])
      splits[rep(length(splits) == 0, nrow(co_data))] <- "1"

    # search schedule for each unit

      unitops <- unique(ss_data$Unit)
      Nunits <- length(unitops)
      maxvisits <- 0
      for(i in 1:Nunits){
        maxvisits <- max(c(maxvisits, 
                            length(ss_data$Unit[ss_data$Unit == unitops[i]])))
      }

      Nunitss <- matrix(NA, nrow = Nunits, ncol = maxvisits)

      for(i in 1:Nunits){
        visitdates <- as.Date(as.character(ss_data$DateSearched[ss_data$Unit 
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

    # expand the factors across the models 

      expanded <- combine_factors_across_models(
                                   cp_predictors = cp_predictors, 
                                   se_predictors = se_predictors, 
                                   cp_data = cp_data, se_data = se_data)

      Ncellcombs <- nrow(expanded)

    # set up the X, Xtilde, Mtilde, and Mhat arrays

      Xarray <- array(NA, dim = c(Ncellcombs, Nunits, 
                                     Nsplitcats, Nsizeclasses))
      Xtildearray <- array(NA, dim = c(replicates, Ncellcombs, Nunits, 
                                        Nsplitcats, Nsizeclasses))
      Mtildearray <- array(NA, dim = c(replicates, Ncellcombs, Nunits, 
                                        Nsplitcats, Nsizeclasses))
      Mhatarray <- array(NA, dim = c(replicates, Nunits, Nsplitcats, 
                                      Nsizeclasses), 
                           dimnames = list(1:replicates, unitoptions, 
                                      splitcats, sizeclasses) )

    # align the expanded factors with the CO data

      EXPinCO <- which(colnames(co_data) %in% colnames(expanded) )
      COinEXP <- which(colnames(expanded) %in% colnames(co_data))

      if(length(EXPinCO) == 1){
        COpaste <- as.character(co_data[, EXPinCO])
        EXPpaste <- as.character(expanded[, COinEXP])
      }else{
        COexpCO <- co_data[, EXPinCO]
        COexpCO <- COexpCO[, order(colnames(COexpCO))]
        EXPcoEXP <- expanded[, COinEXP]
        EXPcoEXP <- EXPcoEXP[, order(colnames(EXPcoEXP))]
        COpaste <- apply(COexpCO, 1, paste, collapse = "")
        EXPpaste <- apply(EXPcoEXP, 1, paste, collapse = "")
      }

    # fill in the arrays

        for(r in 1:Nsizeclasses){
          for(l in 1:Nsplitcats){
            for(k in 1:Nunits){    
              for(i in 1:Ncellcombs){

                spot <- which(COpaste == EXPpaste[i] &
                               units == unitoptions[k] &
                               splits == splitcats[l] &
                               sizes == sizeclasses[r])

                X_specific <- length(spot)
                X_specific[which(is.na(X_specific)) == T] <- 0

                g_specifics <- garray[1:replicates,  
                                        alignss[k], 
                                        which(EXPpaste == EXPpaste[i]),
                                        which(sizeclasses == sizeclasses[r])]
            


                Xtilde_specifics <- rbinom(replicates, 
                                            round(X_specific/g_specifics), 
                                            g_specifics)
                Mtilde_specifics <- Xtilde_specifics/g_specifics

                Xarray[i, k, l, r] <- X_specific
                Xtildearray[ , i, k, l, r] <- Xtilde_specifics
                Mtildearray[ , i, k, l, r] <- Mtilde_specifics


              }

              if(Ncellcombs > 1){
                sumMtilde <- apply(Mtildearray[ , 1:Ncellcombs, k, l, r], 
                                  1, sum)
              } else {
                sumMtilde <- Mtildearray[ , 1:Ncellcombs, k, l, r] 
              }                    


              DWPsccol <- which(colnames(DWPdata) == "Size")
              DWPucol <- which(colnames(DWPdata) == "Unit")
              DWPsscol <- which(colnames(DWPdata) == "SearchSchedule")

              DWPspot <- which(DWPdata[ , DWPsccol] == sizeclasses[r] &
                                DWPdata[ , DWPucol] == unitoptions[k] &
                                DWPdata[ , DWPsscol] == alignss[k])

              DWP_specific <- DWPdata[ DWPspot, "DWP"]

              Mhatspecific <- sumMtilde / DWP_specific
              Mhatspecific[Mhatspecific == "NaN"] <- NA
              Mhatarray[ , k, l, r] <- Mhatspecific
            }
          }
        }


    # return 

      return(Mhatarray)

  }



#' Condense the Mhat array across units and size classes.
#' 
#' @param Mhatarray Mhat array.
#' @return Matrix of Mhat values (rows: iterations, columns: splits).
#' @examples
#' NA
#' @export 


  condense_mhat <- function(Mhatarray){

    # condensing to matrix (Mhatl) of dimension Niterations x split categories

      Niterations <- dim(Mhatarray)[1]
      Nsplitcats <- dim(Mhatarray)[3]
      Mhatl <- matrix(NA, nrow = Niterations, ncol = Nsplitcats)
      colnames(Mhatl) <- dimnames(Mhatarray)[[3]]

      for(l in 1:Nsplitcats){

          Mhatl[ , l] <- apply(Mhatarray[  , , l, ], 1, sum, na.rm = T)

      }
  
    # return

      return(Mhatl)

  }

#' Summarize the Mhat iterations with means and confidence intervals, 
#'  for searched area and expanded area, according to each split. 
#' 
#' @param condensed_mhat Condensed mhat array.
#' @param fraction_area_sampled Fraction of area sampled.
#' @param confidence_level Confidence level for estimates.
#' @return Table of mean (with confidence intervals) of fatalities for 
#'         the searched area and the expanded area.
#' @examples
#' NA
#' @export 

  create_mhat_table <- function(condensed_mhat, fraction_area_sampled, 
                                 confidence_level){

    # prep out table

      Mhattab <- data.frame(matrix(NA, nrow = ncol(condensed_mhat), ncol = 2))

      rownames(Mhattab) <- colnames(condensed_mhat)
      colnames(Mhattab) <- paste(c("Searched Area", "Whole Facility"), 
                                   paste("Mean and ", 
                                    paste(confidence_level * 100, 
                                          "Percent CI", sep = ""), 
                                 " Mortality", sep = ""), sep = " ")

    # expand for whole facility

      condensed_mhat_wf <- condensed_mhat / fraction_area_sampled  

    # calc means

      condensed_mhat_means <- round(apply(condensed_mhat, 2, mean), 1)
      condensed_mhat_wf_means <- round(apply(condensed_mhat_wf, 2, mean), 1)

    # calc quantiles

      quants <- c(0 + (1 - confidence_level) / 2, 
	              1 - (1 - confidence_level) / 2 )
      condensed_mhat_qs <- round(apply(condensed_mhat, 2, quantile, 
                                       prob = quants), 1)
      condensed_mhat_wf_qs <- round(apply(condensed_mhat_wf, 2, quantile, 
                                          prob = quants), 1)

    # combine

      Mhattab[ , 1] <- paste(condensed_mhat_means, " (", 
                             condensed_mhat_qs[1, ], ", ", 
                             condensed_mhat_qs[2, ], ")", sep = "")
      Mhattab[ , 2] <- paste(condensed_mhat_wf_means, " (", 
                             condensed_mhat_wf_qs[1, ], ", ",
                             condensed_mhat_wf_qs[2, ], ")", sep = "")

      Mhattab <- data.frame("SplitCategory" = rownames(Mhattab), Mhattab)
    # return

      return(Mhattab)

  }

#' Create the Mhat figure for a given split category.
#' 
#' @param condensed_mhat_split Condensed Mhat table, for one split only.
#' @param fraction_area_sampled Fraction of the area sampled.
#' @param split_category_name Name of the split category.
#' @return .
#' @examples
#' NA
#' @export 

  create_mhat_figure <- function(condensed_mhat_split, split_category_name, 
                                 fraction_area_sampled ){

    # iterations

      Niterations <- length(condensed_mhat_split)

    # create a facility wide estimate plot

        Mhats <- condensed_mhat_split/fraction_area_sampled 
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
        mtext(side = 3, split_category_name, line = 0 , cex = 1.5)


  }
