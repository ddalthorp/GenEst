#' Create search schedule table for presentation.
#' 
#' @param data Search schedule data.
#' @return Search schedule table.
#' @examples
#' NA

  create_ss_table <- function(data){

    SSv <- create_ss_vec(data = data)
 
    SStab <- matrix(NA, nrow = length(SSv), ncol = 2)
    SStab[ , 1] <- 1:length(SSv)
    SStab[ , 2] <- gsub("_", ", ", SSv)   
    SStab <- data.frame(SStab)
    colnames(SStab) <- c("Search Schedule", "Search Days")
    return(SStab)
  }


#' Create the model selection order list for searcher efficiency models.
#' 
#' @param models Searcher efficiency models.
#' @return Model order list.
#' @examples
#' NA


  order_se_models <- function(models){

    # determine number of size classes

      nsc <- length(models)

    # determine number of models w/in size classes

      nmods <- length(models[[1]])

    # set up list of tables for output

      AICcOrderList <- vector("list", nsc)

    # fill in

      for(i in 1:nsc){

        specificAICc <- rep(NA, nmods)

        for(j in 1:nmods){

          specificAICc[j] <- round(unlist(models[[i]][[j]]$AICc), 3)

        }

        AICcOrderList[[i]] <- order(specificAICc)

      }

    # add size col names

      names(AICcOrderList) <- names(models)

    # return

      return(AICcOrderList)

  }
  
#' Create the model selection order list for carcass persistence models.
#' 
#' @param models Carcass persistence models.
#' @return Model order list.
#' @examples
#' NA


  order_cp_models <- function(models){

    # determine number of size classes

      nsc <- length(models)

    # determine number of models w/in size classes

      nmods <- length(models[[1]])

    # set up list of tables for output

      AICcOrderList <- vector("list", nsc)

    # fill in

      for(i in 1:nsc){

        specificAICc <- rep(NA, nmods)

        for(j in 1:nmods){

          modAIC <- AIC(models[[i]][[j]]) 
          modnpar <- (models[[i]][[j]])$df
          modnobs <- length((models[[i]][[j]])$linear.predictors)
          specificAICc[j] <- modAIC + (2 * modnpar * (modnpar + 1) ) / 
                                (modnobs - modnpar - 1 )

        }

        AICcOrderList[[i]] <- order(specificAICc)
   
      }

    # add size col names

      names(AICcOrderList) <- names(models)

    # return

      return(AICcOrderList)

  }


#' Launches GenEst app.
#' 
#' @export

  runGenEst <- function(){

    appDir <- system.file("application", "GenEst", package = "GenEst")
	if(appDir == ""){
	  return("Could not find directory.")
	}
	shiny::runApp(appDir, display.mode = "normal")
  }

