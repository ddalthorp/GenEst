  
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
