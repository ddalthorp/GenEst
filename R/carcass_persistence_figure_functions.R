#' Plot cell-specific decay curve for carcass persistence
#'
#' @param model model of class cpm
#' @param specificCell name of the specific cell to plot
#' @param n number of draws to use to characterize the distributions
#' @param seed random number seed to use
#' @param col color to use
#' @param lwd line width to use
#' @param axis_x logical of whether or not to plot the x axis
#' @param axis_y logical of whether or not to plot the y axis
#'
#' @export
#'
cpmCPCellPlot <- function(model, specificCell, col, lwd, n, seed,
                          axis_y = TRUE, axis_x = TRUE){

  dist <- model$dist
  CL <- model$CL
  cellwise <- model$cellwiseTable_ab
  cellNames <- model$cells[ , "CellNames"]

  whichCarcs <- which(model$carcCell == specificCell)
  observations <- model$observations[whichCarcs, ]
  ncarc <- nrow(observations)
  
  whichSpecificCell <- which(cellNames == specificCell)
  a <- cellwise[whichSpecificCell, "pda_median"]
  b <- cellwise[whichSpecificCell, "pdb_median"]
  abs <- rcp(n, model, seed, type = "ppersist")
  as <- abs[[whichSpecificCell]][ , "pda"]
  bs <- abs[[whichSpecificCell]][ , "pdb"]

  max_x <- max(model$observations, na.rm = TRUE)
  max_x <- ceiling(max_x / 10) * 10
  pred_x <- seq(0, max_x, length.out = max_x * 10)
  pts <- pred_x[2:length(pred_x)]
  pta0 <- rep(0, length(pts)) 
  pta1 <- rep(0.000001, length(pts))
  CP <- ppersist(as, bs, dist, pta0, pta1, pts)
  pred_y <- apply(CP, 1, median)
  pred_yl <- apply(CP, 1, quantile, probs = (1 - CL) / 2)
  pred_yu <- apply(CP, 1, quantile, probs = 1 - (1 - CL) / 2)

  t1 <- observations[ , 1]
  t2 <- observations[ , 2]
  event <- rep(3, length(t1))
  event[which(is.na(t2))] <- 0
  event[which(t1 == t2)] <- 1
  t1[which(t1 == 0)] <- 0.0001
  survobj <- survival::Surv(t1, t2, event, "interval")
  form <- formula("survobj ~ 1")
  survfit <- survival::survfit(form, data = observations)

  plot(survfit, ylim = c(0, 1), xlim = c(0, max_x),  
    xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "L", lwd = c(2, 1, 1)
  )
  axis(1, las = 1, cex.axis = 0.9, at = seq(0, max_x, by = 10), 
    label = axis_x
  )
  axis(2, las = 1, cex.axis = 0.9, at = seq(0, 1, 0.2), labels = axis_y)
  points(pts, pred_y, type = "l", col = col, lwd = lwd)
  points(pts, pred_yl, type = "l", col = col, lwd = lwd, lty = 3)
  points(pts, pred_yu, type = "l", col = col, lwd = lwd, lty = 3)
  countText <- paste("N = ", ncarc, sep = "")
  text(x = max_x, y = 0.95, countText, cex = 0.75, xpd = TRUE, adj = 1)
  text(max_x, 1.02, specificCell, adj = 1, cex = 0.75, font = 2)
}

#' Plot results of a single cp model
#'
#' @param model model of class cpm
#' @param n number of draws to use to characterize the distributions
#' @param seed random number seed to use
#' @param col color to use
#'
#' @export
#'
plot.cpm <- function(model, n = 500, seed = 1, col = "black"){

  name_l <- format(model$formula_l)
  name_s <- format(model$formula_s)
  modelName <- paste(name_l, "; ", name_s, sep = "")
  if (model$dist == "exponential"){
    modelName <- name_l 
  }

  par(mar = c(0, 0, 0, 0), fig = c(0, 1, 0.95, 1))
  plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", 
    ylab = "", ylim = c(0, 1), xlim = c(0, 1)
  )

  points(c(0.05, 0.1), c(0.25, 0.25), type = 'l', lwd = 2, col = col)
  text(x = 0.11, y = 0.3, "= Median", adj = 0)
  points(c(0.2, 0.25), c(0.25, 0.25), type = 'l', lwd = 2, lty = 3, col = col)
  text(x = 0.26, y = 0.3, "= Confidence Bounds", adj = 0)

  labelsText <- paste(model$predictors, collapse = ".")
  text_label <- paste("Labels: ", labelsText, sep = "")
  text_model <- paste("Model: ", modelName, sep = "")
  text(x = 0.58, y = 0.3, text_label, adj = 0, cex = 0.75)
  text(x = 0.58, y = 0.7, text_model, adj = 0, cex = 0.75)
  par(fig = c(0, 1, 0, 1), mar = c(1, 1, 1, 1), new = TRUE)
  plot(1,1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", 
    ylab = ""
  )
  mtext(side = 1, "Time", line = 0, cex = 1.5)
  mtext(side = 2, "Carcass Persistence", line = -0.25, cex = 1.5)

  ncell <- model$ncell
  cellNames <- model$cells[ , "CellNames"]

  nmatrix_col <- min(3, ncell)
  nmatrix_row <- ceiling(ncell / nmatrix_col)
  figxspace <- 0.925 / nmatrix_col
  figyspace <- 0.875 / nmatrix_row
  x1 <- rep(figxspace * ((1:nmatrix_col) - 1), nmatrix_row) + 0.05
  x2 <- rep(figxspace * ((1:nmatrix_col)), nmatrix_row) + 0.05
  y1 <- rep(figyspace * ((nmatrix_row:1) - 1), each = nmatrix_col) + 0.04
  y2 <- rep(figyspace * ((nmatrix_row:1)), each = nmatrix_col) + 0.04
  bottomCells <- seq(ncell - (nmatrix_col - 1), ncell, 1)
  leftCells <- which(1:ncell %% nmatrix_col == 1)

  for (celli in 1:ncell){
    par(mar = c(2, 1, 0, 1))
    par(fig = c(x1[celli], x2[celli], y1[celli], y2[celli]), new = T)
    specificCell <- cellNames[celli]
    axis_x <- FALSE
    axis_y <- FALSE
    if (celli %in% bottomCells){
      axis_x <- TRUE
    }
    if (celli %in% leftCells){
      axis_y <- TRUE
    }
    cpmCPCellPlot(model, specificCell, col, lwd = 3, n, seed, axis_y, axis_x)
  }
}

#' Plot results of a cp model set
#'
#' @param modelSet pk model set of class pkmSet
#' @param specificModel the name of a specific model to restrict the plot
#' @param n number of draws to use to characterize the distributions
#' @param seed random number seed to use for the models
#' @param col color to use for the specific model
#'
#' @export
#'
plot.cpmSet <- function(modelSet, specificModel = NULL, n = 500, seed = 1,
                        col = c(exponential = rgb(0.80, 0.38, 0.56), 
                                weibull = rgb(1.00, 0.76, 0.15),
                                loglogistic = rgb(0.00, 1.00, 1.00),
                                lognormal = rgb(0.00, 0.41, 0.55))){

  modelSetNames <- names(modelSet)
  nmodelsInSet <- length(modelSetNames)

  modelSetNames_components <- unlist(strsplit(modelSetNames, "; "))
  modelSetNames_matrix <- matrix(modelSetNames_components, ncol = 3, 
                            byrow = TRUE
                          )
  colnames(modelSetNames_matrix) <- c("dist", "form_l", "form_s")
  whichExp <- which(modelSetNames_matrix[ , "dist"] == "dist: exponential")
  modelSetNames_matrix[whichExp, "form_s"] <- "s ~ 1"
  if (length(whichExp) > 0){
    uniqueForm_s <- unique(modelSetNames_matrix[ , "form_s"])
    names(modelSet)[whichExp] <- gsub("NULL", "s ~ 1", 
                                   names(modelSet)[whichExp]
                                 )
    if (length(uniqueForm_s) > 1){
      uniqueForm_s <- uniqueForm_s[-which(uniqueForm_s == "s ~ 1")]
      nuniqueForm_s <- length(uniqueForm_s)
      for (uniqueForm_si in nuniqueForm_s){
        replacement <- uniqueForm_s[uniqueForm_si]
        newEntries <- modelSetNames_matrix[whichExp, ]
        newEntries[ , "form_s"] <- replacement
        modelSetNames_matrix <- rbind(modelSetNames_matrix, newEntries)
        newEntries <- modelSet[whichExp]
        names(newEntries) <- gsub("s ~ 1", replacement, names(newEntries))
        modelSet <- c(modelSet, newEntries)
      }
    }
  }

  modelSetNames <- names(modelSet)
  nmodelsInSet <- length(modelSetNames)

  distsIncluded <- gsub("dist: ", "", unique(modelSetNames_matrix[ , "dist"]))

  if (length(specificModel) == 0){
    devAskNewPage(TRUE)
    nmod <- nmodelsInSet
    modNames_spec <- modelSetNames
    modNames <- modelSetNames
  }else{
    specificModel <- gsub("NULL", "s ~ 1", specificModel )
    if ((specificModel %in% modelSetNames) == FALSE){
      stop("Selected model not in set. To see options use names(modelSet).")
    }
    devAskNewPage(FALSE)
    nmod <- 1
    modNames_spec <- specificModel
    modNames <- modelSetNames 
  }

  modelMatches <- vector("list", length = nmodelsInSet)
  for (modelsInSeti in 1:nmodelsInSet){
    mod_l <- gsub(" ", "", modelSetNames_matrix[modelsInSeti, 2])
    mod_s <- gsub(" ", "", modelSetNames_matrix[modelsInSeti, 3])
    modComp_l <- gsub(" ", "", modelSetNames_matrix[ , 2])
    modComp_s <- gsub(" ", "", modelSetNames_matrix[ , 3])
    matches <- which(modComp_l == mod_l & modComp_s == mod_s)
    modelMatches[[modelsInSeti]] <- matches
  } 

  nmodelsInSet <- length(modelSet)
  cellsInModel <- vector("list", length = nmodelsInSet)
  ncellsInModel <- rep(NA, nmodelsInSet)
  distOfModel <- rep(NA, nmodelsInSet)
  for (modelInSeti in 1:nmodelsInSet){
    modelSpecific <- modelSet[[modelInSeti]]
    cellsInModel[[modelInSeti]] <- modelSpecific$cells[ , "CellNames"]
    ncellsInModel[modelInSeti] <- length(modelSpecific$cells[ , "CellNames"])
    distOfModel[modelInSeti] <- modelSpecific$dist
  }  

  for (modi in 1:nmod){

    specificModel <- modNames_spec[modi]
    whichSpecificModel <- which(names(modelSet) == specificModel)
    model_spec <- modelSet[[whichSpecificModel]]
    dist_spec <- model_spec$dist
    maxcells <- max(ncellsInModel)
    whichFull <- which(ncellsInModel == maxcells & distOfModel == dist_spec) 
    if (length(whichFull) > 1){
      nfulls <- length(whichFull)
      order_l <- rep(NA, nfulls)
      order_s <- rep(NA, nfulls)
      for (fulli in 1:nfulls){
        form_l <- as.formula(modelSetNames_matrix[whichFull[fulli], "form_l"])
        order_l[fulli] <- sum(attr(terms(form_l), "order"))
        form_s <- as.formula(modelSetNames_matrix[whichFull[fulli], "form_s"])
        order_s[fulli] <- sum(attr(terms(form_s), "order"))
      }
      maxorder_l <- max(order_l)
      maxorder_s <- max(order_s)
      whichFull <- whichFull[order_l == maxorder_l & order_s == maxorder_s]
    }
    model_full <- modelSet[[whichFull]]
    fullModel <- modNames[whichFull]

    par(fig = c(0, 1, 0, 0.95), mar = c(1, 1, 1, 1))
    plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", 
      ylab = ""
    )
    mtext(side = 1, "Time", line = 0, cex = 1.5)
    mtext(side = 2, "Carcass Persistence", line = -0.25, cex = 1.5)

    par(fig = c(0, 1, 0.95, 1), mar = c(0, 0, 0, 0), new = TRUE)
    plot(1,1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", 
      ylab = "", ylim = c(0, 1), xlim = c(0, 1))

    if ("exponential" %in% distsIncluded){
      rect(0.0, 0.15, 0.04, 0.35, col = col["exponential"], border = NA)
      text(x = 0.05, y = 0.3, "= Exponential", adj = 0)
    }
    if ("exponential" %in% distsIncluded){
      rect(0.16, 0.15, 0.2, 0.35, col = col["weibull"], border = NA)
      text(x = 0.21, y = 0.3, "= Weibull", adj = 0)
    }
    if ("loglogistic" %in% distsIncluded){
      rect(0.29, 0.15, 0.33, 0.35, col = col["loglogistic"], border = NA)
      text(x = 0.34, y = 0.3, "= Log-Logistic", adj = 0)
    }
    if ("lognormal" %in% distsIncluded){
      rect(0.45, 0.15, 0.49, 0.35, col = col["lognormal"], border = NA)
      text(x = 0.5, y = 0.3, "= Log-Normal", adj = 0) 
    }
    labelsText <- paste(model_full$predictors, collapse = ".")
    text_label <- paste("Labels: ", labelsText, sep = "")
    forms <- modelSetNames_matrix[whichSpecificModel, c("form_l", "form_s")]
    modelsText <- paste(forms, collapse = "; ")
    text_model <- paste("Model: ", modelsText, sep = "")
    text(x = 0.61, y = 0.3, text_label, adj = 0, cex = 0.75)
    text(x = 0.61, y = 0.7, text_model, adj = 0, cex = 0.75)

    ncell <- model_full$ncell
    cellNames <- model_full$cells[ , "CellNames"]

    nmatrix_col <- min(3, ncell)
    nmatrix_row <- ceiling(ncell / nmatrix_col)
    figxspace <- 0.925 / nmatrix_col
    figyspace <- 0.875 / nmatrix_row
    x1 <- rep(figxspace * ((1:nmatrix_col) - 1), nmatrix_row) + 0.05
    x2 <- rep(figxspace * ((1:nmatrix_col)), nmatrix_row) + 0.05
    y1 <- rep(figyspace * ((nmatrix_row:1) - 1), each = nmatrix_col) + 0.04
    y2 <- rep(figyspace * ((nmatrix_row:1)), each = nmatrix_col) + 0.04
    bottomCells <- seq(ncell - (nmatrix_col - 1), ncell, 1)
    leftCells <- which(1:ncell %% nmatrix_col == 1)

    for (celli in 1:ncell){
      par(mar = c(2, 1, 0, 1))
      par(fig = c(x1[celli], x2[celli], y1[celli], y2[celli]), new = T)
      specificCell <- cellNames[celli]
      axis_x <- FALSE
      axis_y <- FALSE
      if (celli %in% bottomCells){
        axis_x <- TRUE
      }  
      if (celli %in% leftCells){
        axis_y <- TRUE
      }

      cpmSetSpecCPCellPlot(modelSet, specificModel, fullModel, specificCell, 
        col, modelMatches, axis_x, axis_y, n, seed
      )
    }
  }
  devAskNewPage(FALSE)
}

#' Plot cell-specific decay curve for carcass persistence
#'
#' @param modelSet model set of class cpmSet
#' @param specificModel name of the specific model to plot and emphasize
#' @param fullModel name of the full model to create the cell structure
#' @param specificCell name of the specific cell to plot
#' @param modelMatches list of matching models for the modelSet
#' @param n number of draws to use to characterize the distributions
#' @param seed random number seed to use
#' @param col color to use
#' @param axis_x logical of whether or not to plot the x axis
#' @param axis_y logical of whether or not to plot the y axis
#'
#' @export
#'
cpmSetSpecCPCellPlot <- function(modelSet, specificModel, fullModel, 
                                 specificCell, col, modelMatches, axis_x, 
                                 axis_y, n, seed){ 
 
  model_spec <- modelSet[[specificModel]] 
  model_full <- modelSet[[fullModel]]

  dist <- model_spec$dist
  CL <- model_spec$CL
  cellwise_spec <- model_spec$cellwiseTable
  cellwise_full <- model_full$cellwiseTable
  cellNames_spec <- model_spec$cells[ , "CellNames"]
  cellNames_full <- model_full$cells[ , "CellNames"]

  whichCarcs <- which(model_full$carcCell == specificCell)
  observations <- model_full$observations[whichCarcs, ]
  ncarc <- nrow(observations)
  
  cellMatch <- matchCells(model_spec, model_full)
  reducedCell <- cellMatch[cellNames_full == specificCell]

  max_x <- max(model_full$observations, na.rm = TRUE)
  max_x <- ceiling(max_x / 10) * 10

  t1 <- observations[ , 1]
  t2 <- observations[ , 2]
  event <- rep(3, length(t1))
  event[which(is.na(t2))] <- 0
  event[which(t1 == t2)] <- 1
  t1[which(t1 == 0)] <- 0.0001
  survobj <- survival::Surv(t1, t2, event, "interval")
  form <- formula("survobj ~ 1")
  survfit <- survival::survfit(form, data = observations)

  plot(survfit, ylim = c(0, 1), xlim = c(0, max_x),  
    xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "L", lwd = c(2, 1, 1)
  )
  axis(1, las = 1, cex.axis = 0.9, at = seq(0, max_x, by = 10), 
    label = axis_x
  )
  axis(2, las = 1, cex.axis = 0.9, at = seq(0, 1, 0.2), labels = axis_y)
  countText <- paste("N = ", ncarc, sep = "")
  text(x = max_x, y = 0.95, countText, cex = 0.75, xpd = TRUE, adj = 1)
  text(max_x, 1.02, specificCell, adj = 1, cex = 0.75, font = 2)

  whichSpecificModel <- which(names(modelSet) == specificModel)
  modsToPlot <- modelMatches[[whichSpecificModel]]
  modsToPlot <- modsToPlot[-which(modsToPlot == whichSpecificModel)]
  modsToPlot <- c(modsToPlot, whichSpecificModel)
  nmodsToPlot <- length(modsToPlot)
  for (modsToPloti in 1:nmodsToPlot){
    model_i <- modelSet[[modsToPlot[modsToPloti]]]
    dist_i <- model_i$dist
    cellwise_i <- model_i$cellwiseTable_ab
    cellNames_i <- cellwise_i[ , "cell"]
    cellMatch_i <- matchCells(model_i, model_full)
    reducedCell_i <- cellMatch_i[cellNames_full == specificCell]
    whichSpecificCell <- which(cellNames_i == reducedCell_i)
    a <- cellwise_i[whichSpecificCell, "pda_median"]
    b <- cellwise_i[whichSpecificCell, "pdb_median"]
    pred_x <- seq(0, max_x, length.out = max_x * 10)
    pts <- pred_x[2:length(pred_x)]
    pta0 <- rep(0, length(pts)) 
    pta1 <- rep(0.000001, length(pts))
    pred_y <- t(ppersist(a, b, dist_i, pta0, pta1, pts))

    col_i <- col[dist_i]
    lwd <- 2
    if (modsToPloti == nmodsToPlot){
      lwd <- 5
    }
    points(pts, pred_y, type = "l", col = col_i, lwd = lwd)
  }
}
