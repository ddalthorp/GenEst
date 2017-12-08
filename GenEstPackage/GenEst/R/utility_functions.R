#' Compute the logit.
#' 
#' @param x A probability (between 0 and 1, inclusive).
#' @return The logit of \code{x}.
#' @examples
#' logit(0.5)
#' @export 

  logit <- function(x) {
    log(x / (1 - x))
  }


#' Compute the anti-logit.
#' 
#' @param x A number.
#' @return The anti-logit of \code{x}.
#' @examples
#' alogit(0)
#' @export 

  alogit <- function(x) {
    1 / (1 + exp(-x))
  }
  
#' Create the factor combination table for a searcher efficiency or carcass
#'  persistence analysis.
#' 
#' @param predictors Names of predictor variables to include.
#' @param data Searcher efficiency or carcass persistence data.
#' @return Factor combination table.
#' @examples
#' NA
#' @export 

combine_factors <- function(predictors, data) {
# generalized to accommodate any number of predictors
  npredictors <- length(predictors)
  if (npredictors == 0) {
    return(data.frame(group="all",CellNames="all"))
  } else {
      if(any(is.na(match(predictors,names(data))))) {
        stop("At least one predictor missing from data.")
      }
      varNames <- predictors
      varLabels <- list()
      varNlevels <- list()
      for (i in 1:npredictors) {
        varLabels[[i]] <- levels(as.factor(data[[varNames[i]]]))
        varNlevels[[i]] <- length(varLabels[[i]])
      }
      reps <- cumprod(varNlevels)[npredictors:1] # Reverse cumulative product
      egDat <- data.frame(var1=gl(varNlevels[[1]],1,length=reps[1],labels=varLabels[[1]]))
      if (npredictors > 1) {
        for (j in 2:npredictors) {
          egDat[[paste("var",j,collapse=NULL)]] <- gl(varNlevels[[j]],reps[j],length=reps[1],labels=varLabels[[j]])
        }
      }
    }
  names(egDat) <- varNames
  egDat$CellNames <- apply(egDat,1,paste0,collapse=".")
  return(egDat)
}

#' Create the factor combination table for a set of searcher efficiency 
#'  and carcass persistence analyses.
#' @param cp_data Carcass persistence data.
#' @param se_data Seacher efficiency data.
#' @param cp_predictors Carcass persistence predictor variables.
#' @param se_predictors Seacher efficiency predictor variables.
#' @return Factor combination table.
#' @examples
#' NA
#' @export 

  combine_factors_across_models <- function(cp_predictors, se_predictors, 
	                                        cp_data, se_data){

      lCPv <- length(cp_predictors)
      clCPv <- as.character(length(cp_predictors))

      pv1 <- NULL
      pv2 <- NULL
      pv3 <- NULL
      pv4 <- NULL

      pv1 <- cp_predictors[1][length(cp_predictors) > 0]
      pv2 <- cp_predictors[2][length(cp_predictors) > 1]
      pv3 <- se_predictors[1][length(se_predictors) > 0]
      pv4 <- se_predictors[2][length(se_predictors) > 1]

      lev1 <- as.character(unique(cp_data[, pv1]))
      lev2 <- as.character(unique(cp_data[, pv2]))
      lev3 <- as.character(unique(se_data[, pv3]))
      lev4 <- as.character(unique(se_data[, pv4]))

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

      combvarsCP1[length(cp_predictors) > 0] <- cp_predictors[1]
      combvarsCP2[length(cp_predictors) > 1] <- cp_predictors[2]
      combvarsSE1[length(se_predictors) > 0] <- se_predictors[1]
      combvarsSE2[length(se_predictors) > 1] <- se_predictors[2]

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


  
  
#' Create a reader-friendly model name from an equation.
#' 
#' @param model_equation Model in equation format.
#' @return Model in reader-friendly format.
#' @examples
#' NA
#' @export 

  model_namer <- function(model_equation){

    xx <- model_equation
    xx <- gsub("~ 1", "Intercept Only", xx)
    xx <- gsub("~fixed_at_", "Fixed at ", xx)
    xx <- gsub("~ ", "", xx)
    xx <- gsub("\\*", "crossed with", xx)
    xx <- gsub("\\+", "and", xx)
    return(xx)
  }


#' Create an equation from a reader-friendly model name.
#' 
#' @param model_name Model in reader-friendly format.
#' @return Model in equation format.
#' @examples
#' NA
#' @export 

  model_name_reverser <- function(model_name){

    xx <- model_name
    xx <- gsub("Intercept Only", "1", xx)
    xx <- gsub(" crossed with ", " \\* ", xx)
    xx <- gsub(" and ", " \\+ ", xx)
    xx <- paste("~ ", xx, sep = "")
    return(xx)
  }

  
#' Create a vector of concatenated search schedules.
#' 
#' @param data Search schedule data.
#' @return Vector of search schedules.
#' @examples
#' NA
#' @export 


  create_ss_vec <- function(data){

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

    for(i in 1:Nuss){
      uniquepastecombo[i] <- gsub("_NA", "", uniquepastecombo[i])
    }

    return(uniquepastecombo)
  }
