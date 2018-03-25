#' @name fit2ParamCP
#'
#' @title Fit a single carcass persistence model for a single size class.
#'
#' @description Similar to survreg() but with a non-common scale parameter and only for censored data.
#'
#'
#' @param data Carcass persistence data restricted to a single size class.
#'         Needs to have columns, 'left', and 'right' unless 
#'         'left' and 'right' are specified below
#' @param left Numeric. Last time the carcass was known present. Can be 0
#' @param right Numeric.  First time the carcass was known absent.  
#'         Can be Inf.
#' @param equation_choice Equation to use.
#' @param distribution_choice Choice of distribution 
#'         (exponential, weibull, loglogistic, lognormal).
#' @return Model fit list.
#' @examples
#' @export 
#'

fit2ParamCP = function(data, left = NULL, right = NULL, 
    equation_choice,  distribution_choice = 'weibull' ){
    Call = match.call()

    require(numDeriv)  ##for hessian
    require(survival)   ##for survreg parameterizations
    
	equation_choice = as.formula(equation_choice)
	mm = model.matrix(equation_choice, data = data)  
    ## mm %*% theta will yield  theta for each observation
    mf = model.frame(equation_choice, data = data)   
    mt = attr(mf, 'terms')   
    out <- expand.grid(.getXlevels(mt, mf))
    if(nrow(out) == 0) out = data.frame(est = 'intercept')
    designMatrix = model.matrix(equation_choice, out)
	##designMatrix %*% theta will yield  theta for each factor combination
   
	if(is.null(left))		left = data$left
	if(is.null(right))		right = data$right

    ##names for outputs
    survregNames = apply(
        sapply(1:ncol(out), function(column){
        paste0(colnames(out)[column], out[, column])}), 1,
        paste0, collapse = '')

    modelNames = dimnames(mm)[[2]]

    ##get reasonable starting values
    weibullStart <- function(xbar,varx,maxx){
        ## method of moments for weibull parameters
        wei <- function(b,varx,xbar){
            out <- exp(lgamma(1+2/b)-2*lgamma(1+1/b)) - 1 - varx/xbar^2
            return(out)
        }

        b <- tryCatch({
            uniroot(wei,interval=c(1e-1,maxx),varx=varx,xbar=xbar)$root
        },error=function(cond){
            ## message(cond)
            suppressWarnings(b <- nlminb(start=1e-1,objective=wei,varx=varx,xbar=xbar,lower=1e-5)$par)
            return(b)
        })
        a <- xbar/gamma(1+1/b)
        return(c(log(a), 1/b)) 
    } ## end weibullStart function

	x = (left + pmin(right, 2 * max(right[!is.infinite(right)]))) / 2
    
    thetaStart = switch(distribution_choice, 
        'loglogistic' =    c(mean(log(x)), var(log(x))/3.2),
        'exponential' = 1/mean(x),
        'lognormal' = c( sqrt(var(x)), log(mean(x))),
        'weibull' = weibullStart(mean(x), var(x), max(x))
       )
    
    thetaStart = matrix(c(thetaStart, 
        rep(0, length(thetaStart) * (ncol(designMatrix)-1))),
        nrow = ncol(designMatrix), byrow = T)

    
##########################################

    ###define the likelihood
    ###lots of fiddling in here trying to prevent NaN warnings from
    ###nlminb but without complete success
	if(distribution_choice != 'exponential'){
		
		negLogLik = function(thetaStart, left, right,  distribution_choice = distribution_choice){
            thetaStart = matrix(thetaStart, ncol = 2)
			theta. = matrix(c(mm %*% thetaStart[,1], 
                        mm%*% thetaStart[,2]), ncol = 2)
            ##psurvreg can't figure out pdistr(Inf, theta) for some 
            ##distributions, so take care of it here.
            psurvregRight = numeric(length(right))
            psurvregRight[is.infinite(right)] = 1
            psurvregRight[!is.infinite(right)] =    
                psurvreg(right, theta.[,1], theta.[,2], 
                distribution_choice)[!is.infinite(right)]
            likelihood = psurvregRight - 
                psurvreg(left, theta.[,1], theta.[,2], distribution_choice)
            likelihood[likelihood == 0] = .Machine$double.eps
			-sum(log(likelihood))
    }

	} else {  ##for exponential distribution
		
		negLogLik = function(thetaStart, left, right,  distribution_choice = distribution_choice){
			theta. = matrix(mm %*% thetaStart)
           	-sum(log(psurvreg(right, theta., scale = 1, 
                distribution_choice) - 
                psurvreg(left, theta., scale = 1, distribution_choice)))}
	}

    ##model fit
    fit = suppressWarnings(nlminb(start = thetaStart, objective = negLogLik,
        left = left, right = right,	
        distribution_choice = distribution_choice))

    ##Variance-covariance
    hess =  hessian(func = negLogLik, x = fit$par, 
        left = left, right = right, distribution_choice = distribution_choice)

    var <- solve(hess)

##Format results for output
    par = fit$par
    params = matrix(par, nrow = ncol(designMatrix))
    rownames(params) = modelNames


    if(distribution_choice != 'exponential'){
        thetaHat =	matrix(c(designMatrix %*% params[,1], 
                    designMatrix%*% params[,2]), ncol = 2)
        names(par) = c(paste0( modelNames, 'PVCoef'), 
                        paste0(modelNames, 'ScaleCoef'))

        coefficients = matrix(fit$par, ncol = 2)[, 1]
        scale = matrix(fit$par, ncol = 2)[, 2]
        colnames(thetaHat) = c('predVal', 'scale')
        colnames(params) = c('predValCoef', 'scaleCoef')
    } else {
        thetaHat =	matrix(designMatrix %*% params)
        names(par) = paste0(modelNames, 'PVCoef')
        coefficients = matrix(fit$par, ncol = 1)
        scale = matrix(1, ncol = 1, nrow = nrow(coefficients))
        colnames(thetaHat) = 'predVal'
        
        colnames(params) = c('predValCoef')
    }

    rownames(var) = colnames(var) = names(par)
    rownames(thetaHat) = survregNames
    names(coefficients) = modelNames
    names(scale) = modelNames

    ##AIC calculations
    k = ncol(thetaHat) * ncol(designMatrix)
    AIC = 2 * k + 2 * fit$objective
    AICc =  AIC + (2 * (k^2 + k) / (nrow(dat) - k - 1))

    return(list(
        survregParams = thetaHat, 
        par = par,

        coefficients = coefficients,
        scale = scale,
        var = var,
     
        loglik = fit$objective, 
        iter = fit$iterations,
        k = k,
        AIC = AIC,
        AICc = AICc,
        terms = mt,
        call = Call,
        dist = distribution_choice,
        convergence = fit$convergence,
        optimMessage = fit$message
        # data = data
     ))
}


#' Fit a single carcass persistence model for a single size class
#' 
#' @param data Carcass persistence data restricted to a single size class.
#' @param surv_object Survival object for a single size class.  Not
#'         required if commonScale == F
#' @param equation_choice Equation to use.
#' @param distribution_choice Choice of distribution 
#'         (exponential, weibull, loglogistic, lognormal).
#' @param commonScale Boolean.  If TRUE, cp model is fitted using
#'         survreg() function.  If FALSE, cp model is fitted using
#'         fit2ParamCP
#' @return Model fit list.
#' @examples
#' NA
#' @export 

  cp_model_fit <- function(data, surv_object = NULL, equation_choice, 
                           distribution_choice, commonScale = TRUE){

    if(commonScale){

        mform <- formula(paste("surv_object" , equation_choice, sep = " "))
        output <- survival::survreg(mform, data, dist = distribution_choice)

    # return

      return(output)

    } else {
    
        return(fit2ParamCP(data, left = NULL, right = NULL, 
            equation_choice = equation_choice,  
            distribution_choice = distribution_choice))

  }
  }
  
#' Fit all possible carcass persistence models for a single size class
#' 
#' @param data Carcass persistence data restricted to a single size class.
#' @param predictors Names of predictor variables to include.
#' @param last_time_present_column Column name of the last time present data.
#' @param first_time_absent_column Column name of the first time absent data.
#' @return List of model fit lists.
#' @examples
#' NA
#' @export 

  cp_model_set_fit <- function(data, predictors, last_time_present_column, 
                               first_time_absent_column ){

    # set up the response (surv object)

      t1 <- data[ , which(colnames(data) == last_time_present_column)]
      t2 <- data[ , which(colnames(data) == first_time_absent_column)]

      event <- rep(3, length(t1))
      event[which(is.na(t2))] <- 0
      event[which(t1 == t2)] <- 1

      t1[which(t1 == 0)] <- 0.0001

      surv_object <- survival::Surv(time = t1, time2 = t2, event = event, 
                              type = "interval")

    # select the distributions to use

      distsselected <- c("exponential", "weibull", "loglogistic", "lognormal")

    # set up the predictors

      preds <- rep(NA, 5)
      preds[length(predictors) == 0][1] <- c("1")
      preds[length(predictors) == 1][1:2] <- c("1", predictors[1])
      preds[length(predictors) == 2] <- c("1", predictors[1], predictors[2], 
                                          paste(predictors[1], 
                                                predictors[2], sep = " + "),
                                          paste(predictors[1], predictors[2],
                                                sep = " * "))

      preds <- as.character(na.omit(preds))
      eqs <- paste("~", preds, sep = " ")

      equation_options <- rep(eqs, each = length(distsselected))
      distribution_options <- rep(distsselected, length(eqs))
      Nmods <- length(equation_options)

    # set up the output

      output <- vector("list", Nmods)
      names(output) <- equation_options

    # iterate across models

      for(i in 1:Nmods){

        output[[i]] <- cp_model_fit(data = data, surv_object = surv_object, 
                               equation_choice = equation_options[i], 
                               distribution_choice = distribution_options[i])
      }

    # return

      return(output)
  }


#' Fit all possible carcass persistence models across all size classes
#' 
#' @param data Carcass persistence data across all size classes.
#' @param predictors Names of predictor variables to include.
#' @param last_time_present_column Column name of the last time present data.
#' @param first_time_absent_column Column name of the first time absent data.
#' @param size_class_column Column header for the size class column.
#' @return List of model fit lists.
#' @examples
#' NA
#' @export 


  cp_model_set_across_sizes_fit <- function(data, predictors, 
                                            size_class_column, 
                                            last_time_present_column, 
                                            first_time_absent_column ){

    # set up the size classes 

      sccol <- which(colnames(data) == size_class_column)

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
        output[[i]] <- cp_model_set_fit(data = datai, predictors = predictors,
                         last_time_present_column = last_time_present_column, 
                         first_time_absent_column = first_time_absent_column)

      }

    # return

      return(output)

  }
  

  



#' Draw replicate samples of parameters from the carcass persistence models 
#' across all size classes
#' 
#' @param predictors Names of predictor variables to include.
#' @param data Carcass persistence data.
#' @param size_class_column Column header for the size class column.
#' @param model_fits Carcass persistence model fits.
#' @param replicates Number of replicate samples to draw from the 
#'        distribution.
#' @return Multidimensional array (replicates, 2, factor combination 
#'         cells, models, size classes) of carcass persistence.
#' @examples
#' NA
#' @export 


  cp_theta_create <- function(data, predictors, size_class_column, 
                                       model_fits, replicates ){ 

    # set up the factor combination table and count the cells

      CPfct <- combine_factors(predictors = predictors, data = data)
      Ncells <- nrow(CPfct)

    # numer of models 

      Nmodels <- length(model_fits[[1]])

    # size classes

      sccol <- which(colnames(data) == size_class_column)
      sizeclasses <- as.character(unique(data[ , sccol]))
      sizeclasses[length(sizeclasses) == 0] <- 1
      Nsizeclasses <- length(sizeclasses)

    # create a theta for each combination of factors (cell) within 
    #   models within each size class

      thetaCP <- array(NA, dim = c(replicates, 2, Ncells, Nmodels, 
                   Nsizeclasses))

    # for each size class

      for(i in 1:Nsizeclasses){

        for(j in 1:Nmodels){

          # select the model

            smod <- model_fits[[i]][[j]]

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

            modelTermDraws <- mvtnorm::rmvnorm(replicates, means, vcv, method = "svd")

          # create a model matrix to translate model terms to cells 

            mm <- model.matrix(as.formula(paste("~",
                                           as.character(formula(smod)[3],
                                           collapse = NULL))), data = CPfct)

          # fill in for each cell

            for(k in 1:Ncells){

              a <- switch(distforuse, 
                     exponential = 
                      rep(1, replicates), 
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

#' Create the  AICc tables for the carcass persistence models
#' 
#' @param models Carcass persistence models fit for each size class.
#' @return List of AICc tables (one table per size class)
#' @examples
#' NA
#' @export 

  cp_aicc_table_create <- function(models){

    # determine number of size classes

      nsc <- length(models)

    # determine number of models w/in size classes

      nmods <- length(models[[1]])

    # set up list of tables for output

      output <- vector("list", nsc)

    # fill in

      for(i in 1:nsc){

        tsctab <- data.frame(matrix(NA, nrow = nmods, ncol = 4))
        colnames(tsctab) <- c("model", "distribution", "AICc", "Delta AICc")

        for(j in 1:nmods){

          modAIC <- AIC(models[[i]][[j]]) 
          modnpar <- (models[[i]][[j]])$df
          modnobs <- length((models[[i]][[j]])$linear.predictors)
          modAICc <- modAIC + (2 * modnpar * (modnpar + 1) ) / 
                                (modnobs - modnpar - 1 )

          tsctab[j, 1:3] <- c(
                           as.character(model_namer(names(models[[i]])[j])),
                           as.character(models[[i]][[j]]$dist),
                           round(modAICc, 3))

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


#' Create the figures for the carcass persistence models
#' 
#' @param models Carcass persistence models fit for each size class.
#' @param data Full carcass persistence data set.
#' @param predictors Predictor variable names for the carcass persistence 
#'   models.
#' @param theta Theta array.
#' @param time_unit Unit of time for plotting.
#' @param last_time_present_column Column name of the last time present data.
#' @param first_time_absent_column Column name of the first time absent data.
#' @param size_class_column Column header for the size class column.
#' @param r Size class index.
#' @param model_complexity Model index.
#' @param distribution_choice Which distribution to emphasize.
#' @return NA
#' @examples
#' NA
#' @export 

  create_cp_figure <- function(models, data, predictors, theta, 
                               time_unit, size_class_column, 
                               last_time_present_column, 
                               first_time_absent_column, r, 
                               model_complexity, distribution_choice){
    
    # prep the models
  
      # set up the response (surv object)

        t1 <- data[ , which(colnames(data) == last_time_present_column)]
        t2 <- data[ , which(colnames(data) == first_time_absent_column)]

        event <- rep(3, length(t1))
        event[which(is.na(t2))] <- 0
        event[which(t1 == t2)] <- 1

        t1[which(t1 == 0)] <- 0.0001

        CPobvs_survobj <- survival::Surv(time = t1, time2 = t2, event = event, 
                                type = "interval")

      # size classes

        sccol <- which(colnames(data) == size_class_column)
        sizeclasses <- as.character(unique(data[ , sccol]))
        sizeclasses[length(sizeclasses) == 0] <- 1
        Nsizeclasses <- length(sizeclasses)
        sizes <- as.character(data[ , sccol])
        sizes[rep(length(sizes) == 0, nrow(data))] <- "1"

      # models

        Nmodels <- length(models[[1]])
        CPmodnames1 <- names(models[[1]])
        nCPmodnames1 <- length(CPmodnames1)
        CPmodcomps <- CPmodnames1[seq(1, nCPmodnames1, 4)]

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

      # the four distribution models associated with model complexity

        distchoicenumber <- which(c("exponential", "weibull", 
                              "loglogistic", "lognormal") == 
                               distribution_choice)
        modelcomplexitynumber <- which(CPmodcomps == model_complexity)
        MODS <- 4 * (modelcomplexitynumber - 1) + 1:4


    # plotting

      # set up the predictors

        maxx <- max(na.omit(t2))
        maxx <- ceiling(maxx * 1.1)
        predxs <- seq(0, maxx, length.out = 1000)

      # plot window

        # dummy plot and axis labels
     
          par(fig = c(0, 1, 0, 0.95))
          par(mar = c(1, 1, 1, 1))
          plot(1,1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', 
                    xlab = "", ylab = "")
          mtext(side = 1, time_unit, line = -0.5, cex = 1.75)
          mtext(side = 2, "Carcass Persistence", line = -0.5, cex = 1.75)

        # divvy up the bottom of the image for the cell matrix

          figxspace <- 0.975 / nlev2
          figyspace <- 0.925 / nlev1

          x1 <- rep(figxspace * ((1:nlev2) - 1), each = nlev1) + 0.025
          x2 <- rep(figxspace * ((1:nlev2)), each = nlev1) + 0.025

          y1 <- rep(figyspace * ((nlev1:1) - 1), nlev2) + 0.025
          y2 <- rep(figyspace * ((nlev1:1)), nlev2) + 0.025

        # set margins

          par(mar = c(3, 3, 2, 1))

        # fill in each cell

          for(i in 1:Ncells){

            CPobvs_survobj_ri <- CPobvs_survobj[sizes == sizeclasses[r] & 
                                     combnames == fct[i, "CellNames"]]
            CPdata_ri <- data[sizes == sizeclasses[r] & 
                                 combnames == fct[i, "CellNames"], ]
            mform <- formula("CPobvs_survobj_ri ~ 1")

            par(fig = c(x1[i], x2[i], y1[i], y2[i]), new = T)

            plot(survfit(mform, data = CPdata_ri ), ylim = c(0, 1), 
                   xlim = c(0, maxx), main = fct[i, "CellNames"], 
                   xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "L", 
                   col = rgb(0.2, 0.2, 0.2), lwd = c(2,1, 1))


            axis(1, las = 1, cex.axis = 1.)
            axis(2, las = 1, cex.axis = 1., at = seq(0, 1, .2))



            thick <- rep(1, 4)
            thick[distchoicenumber] <- 4
            cols <- matrix(c(0.80, 0.38, 0.56, 1,
                             1.00, 0.76, 0.15, 1,
                             0.00, 1.00, 1.00, 1,
                             0.00, 0.41, 0.55, 1), byrow = T, nrow = 4)
            cols[distchoicenumber, 4] <- 0.8           

            for(j in 1:4){

              thetarji <- theta[, , i, MODS[j], r]
              meanpar <- apply(thetarji, 2, mean)
              lqpar <- apply(thetarji, 2, quantile, probs = 0.025)
              uqpar <- apply(thetarji, 2, quantile, probs = 0.975)
              distrj <- models[[r]][[MODS[j]]]$dist

              pts <- predxs[2:length(predxs)]
              pta0 <- rep(0, length(pts)) 
              pta1 <- rep(0.000001, length(pts))
              predys <- ppersist(
                               distr = distrj,
                               t_arrive0 = pta0, 
                               t_arrive1 = pta1, t_search = pts, 
                               pda = meanpar[1], pdb = meanpar[2]) 

              points(pts, predys, type = 'l', 
                      col = rgb(cols[j,1], cols[j,2], cols[j,3]), 
                      lwd = thick[j])
 
            }

            # emphasize the distribution choice

              j <- distchoicenumber
              j2 <- j
              j2[length(j2) == 0] <- 1
              thetarji <- theta[, , i, MODS[j], r]
              meanpar <- apply(thetarji, 2, mean)
              lqpar <- apply(thetarji, 2, quantile, probs = 0.025)
              uqpar <- apply(thetarji, 2, quantile, probs = 0.975)
              distrj <- models[[r]][[MODS[j2]]]$dist

              pts <- predxs[2:length(predxs)]
              pta0 <- rep(0, length(pts)) 
              pta1 <- rep(0.000001, length(pts))
              predys <- ppersist(
                               distr = distrj,
                               t_arrive0 = pta0,
                               t_arrive1 = pta1, t_search = pts, 
                               pda = meanpar[1], pdb = meanpar[2]) 
              cols[distchoicenumber, 4] <- 1.0
              points(pts, predys, type = 'l', 
                        col = rgb(cols[j,1], cols[j,2], cols[j,3]),
                        lwd = thick[j])

            initcarc <- nrow(CPdata_ri)
            text(x = maxx / 20, y = 1.1, paste("N = ", initcarc, sep = ""), 
                 cex = 0.75, xpd = T)
 
          }

        # legend at the top

          par(fig = c(0, 1, 0.95, 1), new = T)
          par(mar = c(0, 0, 0, 0))
          plot(1,1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', 
                xlab = "", ylab = "", ylim = c(0, 1), xlim = c(0, 1))

          rect(0.1, 0.15, 0.15, 0.35, lwd = 2, 
                   col = rgb(0.80, 0.38, 0.56, 1), 
                   border = NA)
          text(x = 0.16, y = 0.3, "= Exponential", adj = 0)
          rect(0.3, 0.15, 0.35, 0.35, lwd = 2, 
                   col = rgb(1.00, 0.76, 0.15, 1), 
                   border = NA)
          text(x = 0.36, y = 0.3, "= Weibull", adj = 0)
          rect(0.5, 0.15, 0.55, 0.35, lwd = 2,
                   col = rgb(0.00, 1.00, 1.00, 1), 
                   border = NA)
          text(x = 0.56, y = 0.3, "= Log-Logistic", adj = 0)
          rect(0.7, 0.15, 0.75, 0.35, lwd = 2, 
                   col = rgb(0.00, 0.41, 0.55, 1), 
                   border = NA)
          text(x = 0.76, y = 0.3, "= Log-Normal", adj = 0)

}


#' Calculate the probability of persistence to detection 
#' 
#' @param distr Distribution used.
#' @param t_arrive0 Beginning of arrival window.
#' @param t_arrive1 End of arrival window.
#' @param t_search Search time.
#' @param pdb parameter b.
#' @param pda parameter a.
#' @return Probability of persistence of detection to at t_search, 
#'          given arrival between t_arrive0 and t_arrive1
#' @examples
#' NA
#' @export 

  ppersist <- function(distr, t_arrive0, t_arrive1, t_search, pdb, pda = NULL){

    if(distr %in% c("Weibull", "weibull")){

      return(t((pgamma(outer(1 / pdb, t_search - t_arrive0)^pda, 1 / pda) -
              pgamma(outer(1 / pdb, t_search - t_arrive1)^pda, 1 / pda)) * 
              gamma(1 + 1 / pda) * outer(pdb, 1 / (t_arrive1 - t_arrive0))))

    }

    if(distr %in% c("Exponential", "exponential")){

     return((exp(outer(t_arrive1 - t_search, 1 / pdb)) - exp(outer(t_arrive0 - 
              t_search, 1 / pdb))) / (outer(t_arrive1 - t_arrive0, 1 / pdb)))

    }

    if(distr %in% c("Lognormal", "lognormal")){

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

    if(distr %in% c("Log-Logistic", "loglogistic")) {

      return(Vectorize(function(t_arrive0, t_arrive1, t_search, pda, pdb){
        t1 <- t_search-t_arrive1 
        t0 <- t_search-t_arrive0
        part1 <- ifelse(t1 == 0, 0, 
                  t1 / (1 + (t1 / pdb)^pda) *  
                  gsl::hyperg_2F1(1, 1, 1 + 1 / pda, 
                                  1 / (1 + (t1 / pdb)^(-pda))))
        part0 <- t0 / (1 + (t0 / pdb)^pda) * 
                 gsl::hyperg_2F1(1, 1, 1 + 1 / pda, 
                                  1 / (1 + (t0 / pdb)^(-pda)))

        -(part1 - part0)/(t_arrive1 - t_arrive0)
        },
        vectorize.args = c('pdb', 'pda'))(t_arrive0, t_arrive1,
           t_search, pda, pdb))
    }
  }
