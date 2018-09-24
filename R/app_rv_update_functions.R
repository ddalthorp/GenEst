#' @title Update the reactive value list when SE data are read in
#'
#' @description Update the rv list when the SE data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_SE <- function(rv, input){
  rv$data_SE <- readCSVs(input$file_SE$datapath)
  rv$filename_SE <- input$file_SE$name
  rv$colNames_SE <- colnames(rv$data_SE)
  rv$colNames_SE_preds <- predsCols(rv$data_SE)
  rv$colNames_SE_preds0 <- predsCols(rv$data_SE)
  rv$colNames_SE_obs <- obsCols_SE(rv$data_SE)
  rv$colNames_SE_obs0 <- obsCols_SE(rv$data_SE)
  rv$colNames_all <- updateColNames_all(rv)
  rv$colNames_size <- updateColNames_size(rv)
  rv$sizeclassCol <- updateSizeclassCol(input$sizeclassCol, rv$colNames_size)
  rv$colNames_SE_obs <- removeCols(rv$colNames_SE_obs, rv$sizeclassCol)
  rv$colNames_SE_preds <- removeCols(rv$colNames_SE_preds, rv$sizeclassCol)
  return(rv)
}

#' @title Update the reactive value list when CP data are read in
#'
#' @description Update the rv list when the CP data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_CP <- function(rv, input){
  rv$data_CP <- readCSVs(input$file_CP$datapath)
  rv$filename_CP <- input$file_CP$name
  rv$colNames_CP <- colnames(rv$data_CP)
  rv$colNames_CP_preds <- predsCols(rv$data_CP)
  rv$colNames_CP_preds0 <- predsCols(rv$data_CP)
  rv$colNames_fta <- obsCols_fta(rv$data_CP)
  rv$colNames_ltp <- obsCols_ltp(rv$data_CP)
  rv$colNames_fta0 <- obsCols_fta(rv$data_CP)
  rv$colNames_ltp0 <- obsCols_ltp(rv$data_CP)
  rv$colNames_all <- updateColNames_all(rv)
  rv$colNames_size <- updateColNames_size(rv)
  rv$sizeclassCol <- updateSizeclassCol(input$sizeclassCol, rv$colNames_size)
  rv$colNames_CP_fta <- removeCols(rv$colNames_fta, rv$sizeclassCol)
  rv$colNames_CP_ltp <- removeCols(rv$colNames_ltp, rv$sizeclassCol)
  rv$colNames_CP_preds <- removeCols(rv$colNames_CP_preds, rv$sizeclassCol)
  return(rv)
}

#' @title Update the reactive value list when SS data are read in
#'
#' @description Update the rv list when the SS data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_SS <- function(rv, input){
  rv$data_SS <- readCSVs(input$file_SS$datapath)
  rv$colNames_SS <- colnames(rv$data_SS)
  return(rv)
}

#' @title Update the reactive value list when DWP data are read in
#'
#' @description Update the rv list when the DWP data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_DWP <- function(rv, input){
  rv$data_DWP <- readCSVs(input$file_DWP$datapath)
  rv$colNames_DWP <- DWPCols(rv$data_DWP)
  return(rv)
}

#' @title Update the reactive value list when CO data are read in
#'
#' @description Update the rv list when the CO data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_CO <- function(rv, input){
  rv$data_CO <- readCSVs(input$file_CO$datapath)
  rv$colNames_CO <- colnames(rv$data_CO)
  rv$colNames_COdates <- dateCols(rv$data_CO)
  rv$colNames_all <- updateColNames_all(rv)
  rv$colNames_size <- updateColNames_size(rv)
  rv$sizeclassCol <- updateSizeclassCol(input$sizeclassCol, rv$colNames_size)
  return(rv)
}

#' @title Update the reactive value list when a size class column is selected
#'
#' @description Update the rv list when the size class column is selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_sizeClassCol <- function(rv, input){
  rv$sizeClassCol <- input$sizeclassCol
  rv$colNames_SE_obs <- removeCols(rv$colNames_SE_obs, rv$sizeClassCol)
  rv$colNames_SE_preds <- removeCols(rv$colNames_SE_preds, rv$sizeClassCol)
  rv$colNames_fta <- removeCols(rv$colNames_fta, rv$sizeClassCol)
  rv$colNames_ltp <- removeCols(rv$colNames_ltp, rv$sizeClassCol)
  rv$colNames_CP_preds <- removeCols(rv$colNames_CP_preds, rv$sizeClassCol)
  scCol <- input$sizeclassCol
  sizeclasses <- unique(c(rv$data_SE[ , scCol], rv$data_CP[ , scCol]))
  rv$nsizeclasses <- length(sizeclasses)
  if (rv$nsizeclasses > 1 & is.null(rv$DWPCol)){
    rv$DWPCol <- sizeclasses[1]
  } 
  if (rv$nsizeclasses == 0){
    rv$DWPCol <- NULL
  }
  rv
}

#' @title Update the reactive value list when an SE observation column is 
#'   selected
#'
#' @description Update the rv list when an SE observation column is selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_cols_SE_obs <- function(rv, input){
  rv$obsCols_SE <- input$obsCols_SE
  rv$preds_SE <- input$preds_SE
  rv$toRemove_SE_preds <- c(rv$obsCols_SE)
  rv$colNames_SE_preds <- removeCols(rv$colNames_SE_preds0, 
                            rv$toRemove_SE_preds)
  return(rv)
}

#' @title Update the reactive value list when an SE predictor column is 
#'   selected
#'
#' @description Update the rv list when an SE predictor column is selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_cols_SE_preds <- function(rv, input){
  rv$obsCols_SE <- input$obsCols_SE
  rv$preds_SE <- input$preds_SE
  rv$toRemove_SE_obs <- c(rv$preds_SE)
  rv$colNames_SE_obs <- removeCols(rv$colNames_SE_obs0, rv$toRemove_SE_obs)
  return(rv)
}

#' @title Update the reactive value list when a CP Last Time Present column is 
#'   selected
#'
#' @description Update the rv list when a CP Last Time Present column is 
#'   selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_cols_ltp <- function(rv, input){
  rv$ltp <- input$ltp
  rv$fta <- input$fta 
  rv$preds_CP <- input$preds_CP
  rv$toRemove_fta <- c(rv$preds_CP, rv$ltp)
  rv$toRemove_CP_preds <- c(rv$ltp, rv$fta)
  rv$colNames_fta <- removeCols(rv$colNames_fta0, rv$toRemove_fta)
  rv$colNames_CP_preds <- removeCols(rv$colNames_CP_preds0, 
                            rv$toRemove_CP_preds)
  return(rv)
}

#' @title Update the reactive value list when a CP First Time Absent column is 
#'   selected
#'
#' @description Update the rv list when a CP First Time Absent column is 
#'   selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_cols_fta <- function(rv, input){
  rv$ltp <- input$ltp
  rv$fta <- input$fta 
  rv$preds_CP <- input$preds_CP
  rv$toRemove_ltp <- c(rv$preds_CP, rv$fta)
  rv$toRemove_CP_preds <- c(rv$ltp, rv$fta) 
  rv$colNames_ltp <- removeCols(rv$colNames_ltp0, rv$toRemove_ltp)
  rv$colNames_CP_preds <- removeCols(rv$colNames_CP_preds0, 
                            rv$toRemove_CP_preds)
  return(rv)
}

#' @title Update the reactive value list when a CP predictor column is 
#'   selected
#'
#' @description Update the rv list when a CP predictor column is selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_cols_CP_preds <- function(rv, input){
  rv$ltp <- input$ltp
  rv$fta <- input$fta 
  rv$preds_CP <- input$preds_CP
  rv$toRemove_ltp <- c(rv$preds_CP, rv$fta)
  rv$colNames_ltp <- removeCols(rv$colNames_ltp0, rv$toRemove_ltp)
  rv$toRemove_fta <- c(rv$preds_CP, rv$ltp)
  rv$colNames_fta <- removeCols(rv$colNames_fta0, rv$toRemove_fta)
  return(rv)
}

#' @title Run the SE Models
#'
#' @description Use the inputs to run the SE models requested by the UI
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_run_SE <- function(rv, input){
  rv$obsCols_SE <- input$obsCols_SE
  rv$preds_SE <- input$preds_SE
  rv$predictors_SE <- prepPredictors(rv$preds_SE)
  rv$formula_p <- formula(paste0("p~", rv$predictors_SE))
  rv$formula_k <- formula(paste0("k~", rv$predictors_SE)) 
  rv$kFixedChoice <- input$kFixedChoice
  rv$kFixed <- setkFix(input$kFixedChoice, input$kFixed)
  rv$CL <- input$CL
  rv$sizeclassCol <- input$sizeclassCol
  rv$mods_SE <- suppressWarnings(
                  pkmSetSize(formula_p = rv$formula_p,
                    formula_k = rv$formula_k, data = rv$data_SE, 
                    obsCol = rv$obsCols_SE, sizeclassCol = rv$sizeclassCol,
                    kFixed = rv$kFixed, kInit = 0.7, CL = rv$CL, 
                    quiet = TRUE
                  ) 
                ) 
  rv$mods_SE_og <- rv$mods_SE
  rv$mods_SE <- pkmSetSizeFailRemove(rv$mods_SE)
  if (!all(unlist(pkmSetSizeFail(rv$mods_SE))) &&
      !any(unlist(lapply(rv$mods_SE_og, pkmSetAllFail)))){
    rv$sizeclasses <- updateSizeclasses(rv$data_SE, rv$sizeclassCol)
    rv$sizeclasses_SE <- rv$sizeclasses
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$outsizeclassSE)
    rv$sizeclass_SE <- rv$sizeclass
    rv$AICcTab_SE <- pkmSetAICcTab(rv$mods_SE[[rv$sizeclass_SE]], TRUE, TRUE)
    rv$modOrder_SE <- as.numeric(row.names(rv$AICcTab_SE))
    rv$modNames_SE <- names(rv$mods_SE[[rv$sizeclass_SE]])[rv$modOrder_SE]
    rv$modNames_SEp <- modNameSplit(rv$modNames_SE, 1)
    rv$modNames_SEk <- modNameSplit(rv$modNames_SE, 2)
    rv$modSet_SE <- rv$mods_SE[[rv$sizeclass_SE]]
    rv$best_SE <- (names(rv$modSet_SE)[rv$modOrder_SE])[1]
    rv$modTab_SE <- rv$mods_SE[[rv$sizeclass_SE]][[rv$best_SE]]$cellwiseTable
    rv$modTabPretty_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
    rv$modTabDL_SE <- dlModTabSE(rv$modTab_SE, rv$CL)
    rv$figH_SE <- setFigH(rv$modSet_SE)
    rv$figW_SE <- setFigW(rv$modSet_SE)
  }
  return(rv)
}

#' @title Update the SE reactive values when the size class is chosen
#'
#' @description Update the SE reactive values when the size class is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outsc_SE <- function(rv, input){
  if (length(rv$mods_SE) > 0){
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$outsizeclassSE)
    rv$sizeclass_SE <- rv$sizeclass
    rv$AICcTab_SE <- pkmSetAICcTab(rv$mods_SE[[rv$sizeclass_SE]], TRUE, TRUE)
    rv$modOrder_SE <- as.numeric(row.names(rv$AICcTab_SE))
    rv$modNames_SE <- names(rv$mods_SE[[rv$sizeclass_SE]])[rv$modOrder_SE]
    rv$modNames_SEp <- modNameSplit(rv$modNames_SE, 1)
    rv$modNames_SEk <- modNameSplit(rv$modNames_SE, 2)
    rv$modSet_SE <- rv$mods_SE[[rv$sizeclass_SE]]
    rv$best_SE <- (names(rv$modSet_SE)[rv$modOrder_SE])[1]
    rv$modTab_SE <- rv$mods_SE[[rv$sizeclass_SE]][[rv$best_SE]]$cellwiseTable
    rv$modTabPretty_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
    rv$modTabDL_SE <- dlModTabSE(rv$modTab_SE, rv$CL)
    rv$figH_SE <- setFigH(rv$modSet_SE)
    rv$figW_SE <- setFigW(rv$modSet_SE)
  }
  return(rv)
}

#' @title Update the SE reactive values when a p or k model is chosen
#'
#' @description Update the SE reactive values when a p or k model is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outpk_SE <- function(rv, input){
  if (length(rv$mods_SE) > 0){
    rv$outSEpk <- modNamePaste(c(input$outSEp, input$outSEk))
    rv$modSet_SE <- rv$mods_SE[[rv$sizeclass]]

    if (rv$outSEpk %in% names(rv$modSet_SE)){
      rv$modTab_SE <- rv$modSet_SE[[rv$outSEpk]]$cellwiseTable
      rv$modTabPretty_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
      rv$modTabDL_SE <- dlModTabSE(rv$modTab_SE, rv$CL)
    } else {
      rv$modTab_SE <- NULL
      holder <- data.frame(msg = "Selected model was not successfully fit.")
      rv$modTabPretty_SE <- holder
      rv$modTabDL_SE <- holder
    }
  }
  return(rv)
}

#' @title Run the CP Models
#'
#' @description Use the inputs to run the CP models requested by the UI
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_run_CP <- function(rv, input){

  rv$ltp <- input$ltp
  rv$fta <- input$fta
  rv$preds_CP <- input$preds_CP
  rv$dists <- input$dists  
  rv$nsim <- input$nsim
  rv$CL <- input$CL
  rv$sizeclassCol <- input$sizeclassCol
  rv$predictors_CP <- prepPredictors(rv$preds_CP)
  rv$formula_l <- formula(paste("l~", rv$predictors_CP, sep = ""))
  rv$formula_s <- formula(paste("s~", rv$predictors_CP, sep = "")) 

  rv$mods_CP <- suppressWarnings(
                  cpmSetSize(formula_l = rv$formula_l,
                    formula_s = rv$formula_s, data = rv$data_CP, 
                    left = rv$ltp, right = rv$fta, dists = rv$dists,
                    sizeclassCol = rv$sizeclassCol, CL = rv$CL, quiet = TRUE
                  )
                )
  rv$mods_CP_og <- rv$mods_CP
  rv$mods_CP <- cpmSetSizeFailRemove(rv$mods_CP)

  if (!all(unlist(cpmSetSizeFail(rv$mods_CP)))){
    rv$sizeclasses <- updateSizeclasses(rv$data_CP, rv$sizeclassCol)
    rv$sizeclasses_CP <- rv$sizeclasses
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$outsizeclassCP)
    rv$sizeclass_CP <- rv$sizeclass
    rv$AICcTab_CP <- cpmSetAICcTab(rv$mods_CP[[rv$sizeclass_CP]], TRUE, TRUE)
    rv$AICcTab_CP[ , "Scale Formula"] <- gsub("NULL", "", 
                                           rv$AICcTab_CP[ , "Scale Formula"]
                                         )
    rv$modOrder_CP <- as.numeric(row.names(rv$AICcTab_CP))
    rv$modNames_CP <- names(rv$mods_CP[[rv$sizeclass_CP]])[rv$modOrder_CP]
    rv$modNames_CPdist <- modNameSplit(rv$modNames_CP, 1)
    rv$modNames_CPl <- modNameSplit(rv$modNames_CP, 2)
    rv$modNames_CPs <- modNameSplit(rv$modNames_CP, 3)
    rv$modSet_CP <- rv$mods_CP[[rv$sizeclass_CP]]
    rv$best_CP <- (names(rv$modSet_CP)[rv$modOrder_CP])[1]
    rv$modTab_CP <- 
                  rv$mods_CP[[rv$sizeclass_CP]][[rv$best_CP]]$cellwiseTable_ls
    rv$modTabPretty_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
    rv$modTabDL_CP <- dlModTabCP(rv$modTab_CP, rv$CL)
    rv$best_CP <- gsub("NULL", "s ~ 1", rv$best_CP)
    rv$figH_CP <- setFigH(rv$modSet_CP, "CP")
    rv$figW_CP <- setFigW(rv$modSet_CP)
  }
  return(rv)
}

#' @title Update the SE reactive values when the size class is chosen
#'
#' @description Update the SE reactive values when the size class is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outsc_CP <- function(rv, input){
  if (length(rv$mods_CP) > 0){
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$outsizeclassCP)
    rv$sizeclass_CP <- rv$sizeclass
    rv$AICcTab_CP <- cpmSetAICcTab(rv$mods_CP[[rv$sizeclass_CP]], TRUE, TRUE)
    rv$modOrder_CP <- as.numeric(row.names(rv$AICcTab_CP))
    rv$modNames_CP <- names(rv$mods_CP[[rv$sizeclass_CP]])[rv$modOrder_CP]
    rv$modNames_CPdist <- modNameSplit(rv$modNames_CP, 1)
    rv$modNames_CPl <- modNameSplit(rv$modNames_CP, 2)
    rv$modNames_CPs <- modNameSplit(rv$modNames_CP, 3)
    rv$modSet_CP <- rv$mods_CP[[rv$sizeclass_CP]]
    rv$best_CP <- (names(rv$modSet_CP)[rv$modOrder_CP])[1]
    rv$modTab_CP <- 
                  rv$mods_CP[[rv$sizeclass_CP]][[rv$best_CP]]$cellwiseTable_ls
    rv$modTabPretty_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
    rv$modTabDL_CP <- dlModTabCP(rv$modTab_CP, rv$CL)
    rv$figH_CP <- setFigH(rv$modSet_CP, "CP")
    rv$figW_CP <- setFigW(rv$modSet_CP)
    rv$best_CP <- gsub("NULL", "s ~ 1", rv$best_CP)
  }
  return(rv)
}

#' @title Update the CP reactive values when a distribution or l or s model 
#'   is chosen
#'
#' @description Update the CP reactive values when a distribution or l or s 
#'   model is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outdls_CP <- function(rv, input){
  if (length(rv$mods_CP) > 0){
    rv$CPdls <- c(input$outCPdist, input$outCPl, input$outCPs)
    rv$outCPdlsfig <- modNamePaste(rv$CPdls, "CP")
    rv$outCPdlstab <- modNamePaste(rv$CPdls, "CP", tab = TRUE)
    rv$modSet_CP <- rv$mods_CP[[rv$sizeclass]]

    if (rv$outCPdlstab %in% names(rv$modSet_CP)){
      rv$modTab_CP <- rv$modSet_CP[[rv$outCPdlstab]]$cellwiseTable_ls
      rv$modTabPretty_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
      rv$modTabDL_CP <- dlModTabCP(rv$modTab_CP, rv$CL)
    } else {
      rv$modTab_CP <- NULL
      holder <- data.frame(msg = "Selected model was not successfully fit.")
      rv$modTabPretty_CP <- holder
      rv$modTabDL_CP <- holder
    }
  }
  return(rv)
}

#' @title Update the SS reactive values when the SS are chosen
#'
#' @description Update the SS reactive values when the SS are chosen
#'
#' @param rv the reactive values list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_useSSdata <- function(rv){
  rv$SS <- NULL
  rv$SStemp <- tryCatch(averageSS(rv$data_SS), error = function(x){NA})
  if (!is.na(rv$SStemp[1])){
    rv$SS <- rv$SStemp
    rv$avgSI <-  mean(diff(rv$SS[-length(rv$SS)]))
    rv$SStext <- paste(rv$SS, collapse = ", ")
  }
  rv
}

#' @title Update the SS reactive values when the average SS is chosen
#'
#' @description Update the SS reactive values when the average SS is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_useSSinputs <- function(rv, input){
  rv$SStemp <- NA
  rv$gSearchInterval <- input$gSearchInterval
  rv$gSearchMax <- input$gSearchMax
  if (rv$gSearchInterval > 0){
    rv$SStemp <- seq(0, rv$gSearchMax, by = rv$gSearchInterval)
  }
  if (any(is.na(rv$SStemp)) || any(rv$SStemp < 0) | any(rv$SStemp %% 1 != 0)){
    rv$SStemp <- NA
  }
  if (!is.na(rv$SStemp[1])){
    rv$SS <- seq(0, rv$gSearchMax, by = rv$gSearchInterval)
    if (max(rv$SS) != rv$gSearchMax){
      rv$SS <- c(rv$SS, rv$gSearchMax)
    }
    rv$SStext <- paste(rv$SS, collapse = ", ")
  }
  rv
}

#' @title Run the g Model
#'
#' @description Use the inputs to run the g model requested by the UI
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_run_g <- function(rv, input){
  rv$CL <- input$CL
  rv$kFill_g <- NA
  if (length(rv$obsCols_SE) == 1 & length(rv$kFixed) == 0){
    rv$kFill_g <- input$kFill_g
  }
  rv$sizeclasses_g <- rv$sizeclasses
  rv$nsizeclasses_g <- length(rv$sizeclasses_g)
  if (length(rv$nsizeclasses_g) == 1){
    if (is.null(rv$sizeclasses_g)){
      rv$sizeclasses_g <- "all"
      rv$nsizeclasses_g <- 1
    }
  }

  rv$nsim <- input$nsim
  rv$gGeneric <- vector("list", length = rv$nsizeclasses_g)
  for (sci in 1:rv$nsizeclasses_g){

    rv$SEmodToUse_g <- input[[sprintf("modelChoices_SE%d", sci)]]
    rv$CPmodToUse_g <- input[[sprintf("modelChoices_CP%d", sci)]]
    rv$SEmodToUse_g <- gsub("~ constant", "~ 1", rv$SEmodToUse_g)
    rv$CPmodToUse_g <- gsub("~ constant", "~ 1", rv$CPmodToUse_g)

    if (!grepl("s ~", rv$CPmodToUse_g)){
      rv$CPmodToUse_g <- paste(rv$CPmodToUse_g, "; NULL", sep = "")
    }
    rv$CPmodToUse_g <- paste("dist: ", rv$CPmodToUse_g, sep = "")

    rv$gGeneric[[sci]] <- tryCatch(
                            estgGeneric(nsim = rv$nsim, days = rv$SS,
                              model_SE = rv$mods_SE[[sci]][[rv$SEmodToUse_g]],
                              model_CP = rv$mods_CP[[sci]][[rv$CPmodToUse_g]],
                              kFill = rv$kFill_g
                            ), 
                            error = function(x){NULL}
                          )
  }
  names(rv$gGeneric) <- rv$sizeclasses_g
  rv$sizeclass_g <- rv$sizeclasses_g[1]
  rv
}

#' @title Update the g reactive values when the size class is chosen
#'
#' @description Update the g reactive values when the size class is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outsc_g <- function(rv, input){
  rv$sizeclass_g <- pickSizeclass(rv$sizeclasses_g, input$outsizeclassg)
  rv$CL <- input$CL
  rv
}

#' @title Run the M Model
#'
#' @description Use the inputs to run the M model requested by the UI
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_run_M <- function(rv, input){

  rv$M <- NULL
  rv$kFill <- NULL
  if (length(rv$obsCols_SE) == 1 & length(rv$kFixed) == 0){
    rv$kFill <- input$kFill
  }
  rv$nsizeclasses <- length(rv$sizeclasses)
  if (length(rv$nsizeclasses) == 1){
    if (is.null(rv$sizeclasses)){
      rv$sizeclasses <- "all"
     }
  }
  rv$dateFoundCol <- input$dateFoundCol
  rv$nsim <- input$nsim
  rv$frac <- input$frac
  rv$fracNote <- NULL
  if (rv$frac < 0.01){
    rv$frac <- 0.01
    rv$fracNote <- "Fraction facility surveyed set to minimum value (0.01)"
  }
  if (rv$frac > 1){
    rv$frac <- 1
    rv$fracNote <- "Fraction facility surveyed set to maximum value (1.0)"
  }

  rv$SEmodToUse <- rep(NA, rv$nsizeclasses)
  rv$CPmodToUse <- rep(NA, rv$nsizeclasses)

  if (length(rv$SEmodToUse) != length(rv$CPmodToUse)){
    return(rv)
  }
  for (sci in 1:rv$nsizeclasses){
    rv$SEmodToUse[sci] <- input[[sprintf("modelChoices_SE%d", sci)]]
    rv$CPmodToUse[sci] <- input[[sprintf("modelChoices_CP%d", sci)]]
    if (!grepl("s ~", rv$CPmodToUse[sci])){
      rv$CPmodToUse[sci] <- paste(rv$CPmodToUse[sci], "; NULL", sep = "")
    }
    rv$CPmodToUse[sci] <- paste("dist: ", rv$CPmodToUse[sci], sep = "")
  }
  rv$SEmodToUse <- gsub("~ constant", "~ 1", rv$SEmodToUse)
  rv$CPmodToUse <- gsub("~ constant", "~ 1", rv$CPmodToUse)
  names(rv$SEmodToUse) <- rv$sizeclasses
  names(rv$CPmodToUse) <- rv$sizeclasses

  rv$models_SE <- tryCatch(
                    trimSetSize(rv$mods_SE, rv$SEmodToUse), 
                    error = function(x){NULL}
                  )
  rv$models_CP <- tryCatch(
                    trimSetSize(rv$mods_CP, rv$CPmodToUse), 
                    error = function(x){NULL}
                  )
  if(any(c(is.null(rv$models_SE), is.null(rv$models_CP)))){
    rv$M <- NULL
    return(rv)
  }

  if (rv$nsizeclasses > 1){
    rv$DWPCol <- NULL
    rv$sizeclassCol_M <- rv$sizeclassCol
  } else{
    rv$DWPCol <- input$DWPCol  
    rv$sizeclassCol_M <- NULL
    rv$models_SE <- rv$models_SE[[1]]
    rv$models_CP <- rv$models_CP[[1]]
  }
  rv$CL <- input$CL

  rv$M <- tryCatch(
            estM(data_CO = rv$data_CO, data_SS = rv$data_SS, rv$data_DWP,
              frac = rv$frac, model_SE = rv$models_SE, 
              model_CP = rv$models_CP, kFill = rv$kFill, 
              dateFoundCol = rv$dateFoundCol, DWPCol = rv$DWPCol,
              sizeclassCol = rv$sizeclassCol_M, nsim = rv$nsim, 
              max_intervals = 8
            ), error = function(x){NULL}, warning = function(x){NULL}
          )

  if (!is.null(rv$M)){
    rv$Msplit <- tryCatch(
                   calcSplits(M = rv$M$Mhat, Aj = rv$M$Aj,
                     split_SS = NULL, split_CO = NULL,
                     data_SS = rv$data_SS, data_CO = rv$data_CO
                   ), error = function(x){NULL}, warning = function(x){NULL}
                 )
    rv$unitCol <- intersect(rv$colNames_CO, rv$colNames_DWP)  
    rv$colNames_SS_sel <- colnames(rv$data_SS) %in% rv$data_CO[ , rv$unitCol]
    rv$colNames_SS_nosel <- rv$colNames_SS[rv$colNames_SS_sel == FALSE]  
  }

  rv
}


#' @title Update the M reactive values when M is split
#'
#' @description Update the M reactive values when M is split
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_split_M <- function(rv, input){
  rv$Msplit <- NULL
  rv$split_CO <- input$split_CO
  rv$split_SS <- input$split_SS
  rv$nsplit_CO <- length(rv$split_CO)
  rv$nsplit_SS <- length(rv$split_SS)
  rv$dateFoundCol <- input$dateFoundCol

  rv$Msplit <- tryCatch(
                 calcSplits(M = rv$M$Mhat, Aj = rv$M$Aj,
                   split_SS = rv$split_SS, split_CO = rv$split_CO,
                   data_SS = rv$data_SS, data_CO = rv$data_CO
                 ), error = function(x){NULL}, warning = function(x){NULL}
               )
  if (!is.null(rv$Msplit)){
    rv$figH_M <- 600
    if (length(attr(rv$Msplit, "vars")) > 1){
      rv$figH_M <- max(600, 300 * length(rv$Msplit))
    }
  }
  rv
}


#' @title Update the M reactive values when M split is transposed
#'
#' @description Update the M reactive values when M split is transposed
#'
#' @param rv the reactive values list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_transpose_split <- function(rv){
  if (rv$nsplit_CO + rv$nsplit_SS == 2){
    rv$Msplit <- transposeSplits(rv$Msplit)
  } 
  rv
}
