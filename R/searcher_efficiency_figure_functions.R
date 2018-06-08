#' Plot results of a single pk model
#'
#' @param x model of class pkm
#' @param n number of draws to use to characterize the distributions
#' @param seed random number seed to use
#' @param col color to use
#' @param ... arguments to be passed to sub functions
#' @return a plot
#' @export
#'
plot.pkm <- function(x, n = 1000, seed = 1, col = "black", ...){

  model <- x
  name_p <- format(model$formula_p)
  name_k <- model$formula_k
  if (class(name_k) == "numeric"){
    name_k <- paste("k fixed at ", name_k, sep = "")
  }else if(class(name_k) == "character"){
    name_k <- "k not estimated"
  }else{
    name_k <- format(model$formula_k)
  }
  modelName <- paste(name_p, "; ", name_k, sep = "")

  par(mar = c(0, 0, 0, 0))
  par(fig = c(0, 1, 0.95, 1))
  plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", 
    ylab = "", ylim = c(0, 1), xlim = c(0, 1)
  )

  points(c(0.01, 0.06), c(0.25, 0.25), type = 'l', lwd = 2, col = col)
  text(x = 0.07, y = 0.3, "= Median", adj = 0, cex = 0.9)
  points(c(0.2, 0.25), c(0.25, 0.25), type = 'l', lwd = 2, lty = 3, col = col)
  text(x = 0.26, y = 0.3, "= Confidence Bounds", adj = 0, cex = 0.9)

  labelsText <- paste(model$predictors, collapse = ".")
  text_label <- paste("Labels: ", labelsText, sep = "")
  text_model <- paste("Model: ", modelName, sep = "")
  text(x = 0.58, y = 0.3, text_label, adj = 0, cex = 0.75)
  text(x = 0.58, y = 0.7, text_model, adj = 0, cex = 0.75)

  par(mar = c(2,4,2,1))
  par(fig = c(0, 0.5, 0.725, 0.975), new = TRUE)
  pkmParamPlot(model, "p", n, seed, col)

  par(fig = c(0.5, 1, 0.725, 0.975), new = TRUE)
  pkmParamPlot(model, "k", n, seed, col)

  par(fig = c(0, 1, 0, 0.75), new = TRUE)
  par(mar = c(1, 1, 1, 1))
  plot(1,1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", 
    ylab = ""
  )
  mtext(side = 1, "Search", line = -0.25, cex = 1.5)
  mtext(side = 2, "Searcher Efficiency", line = -0.25, cex = 1.5)

  ncell <- model$ncell
  cellNames <- model$cells[ , "CellNames"]

  nmatrix_col <- min(3, ncell)
  nmatrix_row <- ceiling(ncell / nmatrix_col)
  figxspace <- 0.95 / nmatrix_col
  figyspace <- 0.65 / nmatrix_row
  x1 <- rep(figxspace * ((1:nmatrix_col) - 1), nmatrix_row) + 0.05
  x2 <- rep(figxspace * ((1:nmatrix_col)), nmatrix_row) + 0.05
  y1 <- rep(figyspace * ((nmatrix_row:1) - 1), each = nmatrix_col) + 0.04
  y2 <- rep(figyspace * ((nmatrix_row:1)), each = nmatrix_col) + 0.04
  bottomCells <- seq(ncell - (nmatrix_col - 1), ncell, 1)
  leftCells <- which(1:ncell %% nmatrix_col == 1)
  if (length(leftCells) == 0){
    leftCells <- 1
  }

  for (celli in 1:ncell){
    par(mar = c(2.5, 2, 0, 0))
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
    pkmSECellPlot(model, specificCell, col, n, seed, axis_y, axis_x)
  }
}


#' Plot results of a pk model set
#'
#' @param x pk model set of class pkmSet
#' @param specificModel the name(s) of specific model(s) to restrict the plot
#' @param n number of draws to use to characterize the distributions
#' @param seed_spec random number seed to use for the specific model
#' @param seed_full random number seed to use for the full model
#' @param col_spec color to use for the specific model
#' @param col_full color to use for the specific model
#' @param sizeclassName name of the size class if it is to be added to the
#'   figure
#' @param ... to be sent to subfunctions
#' @return a set of plots
#' @export
#'
plot.pkmSet <- function(x, specificModel = NULL, n = 1000, 
                        seed_spec = 1, seed_full = 1, col_spec = "black", 
                        col_full = "grey", sizeclassName = NULL, ...){

  modelSet <- x
  if (length(specificModel) == 0){
    nmod <- length(modelSet)
    modNames <- names(modelSet)
  }else{
    if (any(specificModel %in% names(modelSet)) == FALSE){
      stop("Selected model not in set. To see options use names(modelSet).")
    }
    nmod <- length(specificModel)
    modNames <- specificModel
  }

  for (modi in 1:nmod){

    if (modi == 2){
      devAskNewPage(TRUE)
    }
    specificModel <- modNames[modi]
    model_spec <- modelSet[[specificModel]]
    model_full <- modelSet[[1]]

    if (model_spec[[1]] == "Failed model fit"){
      plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", 
        ylab = "", ylim = c(0, 1), xlim = c(0, 1)
      )
      failText <- paste("Model fit for ", specificModel, " failed", sep = "")
      text(0, 1, failText, adj = 0)
      next()
    }
    
    par(mar = c(0, 0, 0, 0))
    par(fig = c(0, 1, 0.935, 1))
    plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", 
      ylab = "", ylim = c(0, 1), xlim = c(0, 1)
    )

    rect(0.01, 0.2, 0.06, 0.45, lwd = 2, col = col_spec, border = NA)
    text(x = 0.07, y = 0.35, "= Selected Model", adj = 0, cex = 0.9)
    rect(0.3, 0.2, 0.35, 0.45, lwd = 2, col = col_full, border = NA)
    text(x = 0.36, y = 0.35, "= Full Model", adj = 0, cex = 0.9)

    labelsText <- paste(model_full$predictors, collapse = ".")
    text_label <- paste("Labels: ", labelsText, sep = "")
    text_model <- paste("Model: ", specificModel, sep = "")
    text(x = 0.52, y = 0.3, text_label, adj = 0, cex = 0.75)
    text(x = 0.52, y = 0.7, text_model, adj = 0, cex = 0.75)

    if (!is.null(sizeclassName)){
      text_sc <- paste("Size class: ", sizeclassName, sep = "")
      text(x = 0.01, y = 0.8, text_sc, adj = 0, cex = 1, font = 1)
    }

    par(mar = c(2,4,2,1))
    par(fig = c(0, 0.45, 0.715, 0.965), new = TRUE)
    pkmSetSpecParamPlot(modelSet, specificModel, "p", n, seed_spec, seed_full,
      col_spec, col_full)
    par(fig = c(0.45, 0.9, 0.715, 0.965), new = TRUE)
    pkmSetSpecParamPlot(modelSet, specificModel, "k", n, seed_spec, seed_full,
      col_spec, col_full)

    par(mar = c(2,0,2,0))
    par(fig = c(0.92, 1, 0.715, 0.965), new = TRUE)
    plot(1,1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", 
      ylab = "", ylim = c(0, 1), xlim = c(0, 1)
    )
    x_s <- 0.1
    CL_split <- (1 - model_spec$CL) / 2
    probs_y <- c(0, CL_split, 0.25, 0.5, 0.75, 1 - CL_split, 1)
    set.seed(12)
    y_s <- quantile(rnorm(1000, 0.5, 0.15), probs = probs_y)
    med <- c(-0.1, 0.1)
    tb <- c(-0.07, 0.07)
    rect(x_s - 0.1, y_s[3], x_s + 0.1, y_s[5], lwd = 2, border = col_spec) 
    points(x_s + med, rep(y_s[4], 2), type = 'l', lwd = 2, col = col_spec)
    points(x_s + tb, rep(y_s[2], 2), type = 'l', lwd = 2, col = col_spec)
    points(x_s + tb, rep(y_s[6], 2), type = 'l', lwd = 2, col = col_spec)
    points(rep(x_s, 3), y_s[1:3], type = 'l', lwd = 2, col = col_spec)
    points(rep(x_s, 3), y_s[5:7], type = 'l', lwd = 2, col = col_spec)
    num_CL <- c(CL_split, 1 - CL_split) * 100
    text_CL <- paste(num_CL, "%", sep = "")
    text_ex <- c("min", text_CL[1], "25%", "50%", "75%", text_CL[2], "max")
    text(x_s + 0.2, y_s, text_ex, cex = 0.65, adj = 0)

    par(fig = c(0, 1, 0, 0.715), new = TRUE)
    par(mar = c(1, 1, 1, 1))
    plot(1,1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", 
      ylab = ""
    )
    mtext(side = 1, "Search", line = -0.25, cex = 1.5)
    mtext(side = 2, "Searcher Efficiency", line = -0.25, cex = 1.5)

    ncell <- model_full$ncell
    cellNames <- model_full$cells[ , "CellNames"]

    nmatrix_col <- min(3, ncell)
    nmatrix_row <- ceiling(ncell / nmatrix_col)
    figxspace <- 0.95 / nmatrix_col
    figyspace <- 0.65 / nmatrix_row
    x1 <- rep(figxspace * ((1:nmatrix_col) - 1), nmatrix_row) + 0.035
    x2 <- rep(figxspace * ((1:nmatrix_col)), nmatrix_row) + 0.035
    y1 <- rep(figyspace * ((nmatrix_row:1) - 1), each = nmatrix_col) + 0.03
    y2 <- rep(figyspace * ((nmatrix_row:1)), each = nmatrix_col) + 0.03
    bottomCells <- seq(ncell - (nmatrix_col - 1), ncell, 1)
    leftCells <- which(1:ncell %% nmatrix_col == 1)
    if (length(leftCells) == 0){
      leftCells <- 1
    }
    for (celli in 1:ncell){
      par(mar = c(2.5, 2, 0, 0))
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
      pkmSetSpecSECellPlot(modelSet, specificModel, specificCell, 
        col_spec, col_full, axis_y, axis_x, n, seed_spec, seed_full)
    }
  }
  devAskNewPage(FALSE)
}

#' Plot parameter box plots for each cell for either p or k
#'
#' @param model model of class pkm
#' @param pk character of "p" or "k" to delineate between parameter graphed
#' @param n number of draws to use to characterize the distributions
#' @param seed random number seed to use
#' @param col color to use
#' @return a parameter plot panel
#' @export
#'
pkmParamPlot <- function(model, pk = "p", n, seed, col){

  ncell <- model$ncell
  cellNames <- model$cells[ , "CellNames"]
  predictors <- model$predictors
  CL <- model$CL
  probs <- c(0, (1 - CL) / 2, 0.25, 0.5, 0.75, 1 - (1 - CL) / 2, 1)
  pks <- rpk(n, model, seed)
  pks_full <- rpk(n, model, seed) 

  if (pk == "p"){
    maxy <- 1
  } else if (pk == "k"){
    maxy <- 1
    for (celli in 1:ncell){
      maxcell <- max(pks[[celli]][ , "k"]) * 1.01
      maxy <- max(maxy, maxcell)
    }
  }
  maxy[is.na(maxy)] <- 1

  plot(1, type = "n", xlab = "", ylab = "", bty = "L", xaxt = 'n', yaxt = 'n',
    ylim = c(0, maxy), xlim = c(0.5, ncell + 0.5)
  )

  for (celli in 1:ncell){
    x <- celli
    y <- quantile(pks[[celli]][ , pk], probs, na.rm = TRUE)

    med <- c(-0.1, 0.1)
    tb <- c(-0.07, 0.07)

    rect(x - 0.1, y[3], x + 0.1, y[5], lwd = 2, border = col) 
    points(x + med, rep(y[4], 2), type = 'l', lwd = 2, col = col)
    points(x + tb, rep(y[2], 2), type = 'l', lwd = 2, col = col)
    points(x + tb, rep(y[6], 2), type = 'l', lwd = 2, col = col)
    points(rep(x, 3), y[1:3], type = 'l', lwd = 2, col = col)
    points(rep(x, 3), y[5:7], type = 'l', lwd = 2, col = col)

  }

  axis(1, at = 1:ncell, cellNames, las = 1, cex.axis = 0.75)
  axis(2, at = seq(0, 1, 0.5), las = 1, cex.axis = 0.75)
  axis(2, at = seq(0, 1, 0.1), labels = FALSE, tck = -0.05)
  mtext(side = 2, pk, line = 2.75, cex = 1.1)

}


#' Plot cell-specific decay curve for searcher efficiency
#'
#' @param model model of class pkm
#' @param specificCell name of the specific cell to plot
#' @param n number of draws to use to characterize the distributions
#' @param seed random number seed to use
#' @param col color to use
#' @param axis_x logical of whether or not to plot the x axis
#' @param axis_y logical of whether or not to plot the y axis
#' @return a cell plot panel
#' @export
#'
pkmSECellPlot <- function(model, specificCell, col, n, seed, 
                          axis_y = TRUE, axis_x = TRUE){

  CL <- model$CL
  cellwise <- model$cellwiseTable
  cellNames <- model$cells[ , "CellNames"]

  whichCarcs <- which(model$carcCell == specificCell)
  observations <- as.matrix(model$observations[whichCarcs, ], 
                    nrow = length(whichCarcs), ncol = ncol(model$observations)
                  )
  nobs <- ncol(observations)
  ncarc <- nrow(observations)
  carcFound <- apply(observations, 2, sum, na.rm = TRUE)
  carcUnavail <- apply(apply(observations, 2, is.na), 2, sum)
  carcAvail <- ncarc - carcUnavail

  whichSpecificCell <- which(cellNames == specificCell)
  p <- cellwise[whichSpecificCell, "p_median"]
  k <- cellwise[whichSpecificCell, "k_median"]
  pks <- rpk(n, model, seed)
  ps <- pks[[whichSpecificCell]][ , "p"]
  ks <- pks[[whichSpecificCell]][ , "k"]
  searchTab <- matrix(1:nobs, nrow = n, ncol = nobs, byrow = TRUE)
  ktab <- ks^(searchTab - 1)
  SE <- ps * ktab
  y <- apply(SE, 2, median) 
  y_l <- apply(SE, 2, quantile, probs = (1 - CL) / 2)
  y_u <- apply(SE, 2, quantile, probs = 1 - (1 - CL) / 2)

  x_pts <- 1:nobs
  y_pts <- carcFound / carcAvail
  plot(x_pts, y_pts, ylim = c(0, 1), xlim = c(0.5, nobs + 0.5), xlab = "", 
    ylab = "", xaxt = "n", yaxt = "n", bty = "L", col = rgb(0.02, 0.02, 0.02),
    lwd = 2, pch = 1, cex = 1.5
  )

  points(x_pts, y, type = 'l', lwd = 3, col = col)
  points(x_pts, y_l, type = 'l', lwd = 2, lty = 3, col = col)
  points(x_pts, y_u, type = 'l', lwd = 2, lty = 3, col = col)

  for (obi in 1:nobs){
    x1 <- x_pts[obi] - 0.25
    y1 <- y_pts[obi] + 0.06
    x2 <- x_pts[obi] + 0.35
    y2 <- y_pts[obi] + 0.15
    rect(x1, y1, x2, y2, border = NA, col = "white")
  }
  obsLabels <- paste(carcFound, carcAvail, sep = "/") 
  text(x_pts + 0.05, y_pts + 0.1, obsLabels, cex = 0.65)

  axis(1, at = x_pts, las = 1, cex.axis = 0.75, labels = axis_x)
  axis(2, at = seq(0, 1, 0.2), las = 1, cex.axis = 0.75, labels = axis_y)

  text(0.5, 0.95, specificCell, adj = 0, cex = 0.75, font = 2)

}


#' Plot parameter box plots for each cell within a model for either p or k 
#'   with comparison to the cellwise model
#'
#' @param modelSet modelSet of class pkmSet
#' @param specificModel name of the specific submodel to plot
#' @param pk character of "p" or "k" to delineate between parameter graphed
#' @param n number of draws to use to characterize the distributions
#' @param seed_spec random number seed to use for the specific model
#' @param seed_full random number seed to use for the full model
#' @param col_spec color to use for the specific model
#' @param col_full color to use for the specific model
#' @return a specific parameter plot panel
#' @export
#' 
pkmSetSpecParamPlot <- function(modelSet, specificModel, pk = "p", n, 
                                seed_spec, seed_full, col_spec, col_full){

  model_spec <- modelSet[[specificModel]]
  model_full <- modelSet[[1]]

  CL <- model_full$CL
  probs <- c(0, (1 - CL) / 2, 0.25, 0.5, 0.75, 1 - (1 - CL) / 2, 1)
  observations_spec <- model_spec$observations
  observations_full <- model_full$observations
  ncell_spec <- model_spec$ncell
  ncell_full <- model_full$ncell
  cellNames_full <- model_full$cells[ , "CellNames"]
  predictors_spec <- model_spec$predictors
  predictors_full <- model_full$predictors

  pks_spec <- rpk(n, model_spec, seed_spec)
  pks_full <- rpk(n, model_full, seed_full) 
  cellMatch <- matchCells(model_spec, model_full)

  if (pk == "p"){
    maxy <- 1
  } else if (pk == "k"){
    maxy <- 1
    for (celli in 1:ncell_full){
      maxcell <- max(pks_full[[celli]][ , "k"]) * 1.01
      maxy <- max(maxy, maxcell)
    }
  }
  maxy[is.na(maxy)] <- 1

  par(mar = c(2,4,2,1))
  plot(1, type = "n", xlab = "", ylab = "", bty = "L", xaxt = 'n', yaxt = 'n',
    ylim = c(0, maxy), xlim = c(0.5, ncell_full + 0.5)
  )

  for (celli in 1:ncell_full){
    cMi <- cellMatch[celli]
    x_s <- celli - 0.2
    y_s <- quantile(pks_spec[[cMi]][ , pk], probs, na.rm = TRUE)
    x_f <- celli + 0.2
    y_f <- quantile(pks_full[[celli]][ , pk], probs, na.rm = TRUE)
    
    med <- c(-0.1, 0.1)
    tb <- c(-0.07, 0.07)

    rect(x_s - 0.1, y_s[3], x_s + 0.1, y_s[5], lwd = 2, border = col_spec) 
    points(x_s + med, rep(y_s[4], 2), type = 'l', lwd = 2, col = col_spec)
    points(x_s + tb, rep(y_s[2], 2), type = 'l', lwd = 2, col = col_spec)
    points(x_s + tb, rep(y_s[6], 2), type = 'l', lwd = 2, col = col_spec)
    points(rep(x_s, 3), y_s[1:3], type = 'l', lwd = 2, col = col_spec)
    points(rep(x_s, 3), y_s[5:7], type = 'l', lwd = 2, col = col_spec)

    rect(x_f - 0.1, y_f[3], x_f + 0.1, y_f[5], lwd = 2, border = col_full) 
    points(x_f + med, rep(y_f[4], 2), type = 'l', lwd = 2, col = col_full)
    points(x_f + tb, rep(y_f[2], 2), type = 'l', lwd = 2, col = col_full)
    points(x_f + tb, rep(y_f[6], 2), type = 'l', lwd = 2, col = col_full)
    points(rep(x_f, 3), y_f[1:3], type = 'l', lwd = 2, col = col_full)
    points(rep(x_f, 3), y_f[5:7], type = 'l', lwd = 2, col = col_full)
  }

  axis(1, at = 1:ncell_full, labels = FALSE)
  ang <- 0
  offx <- NULL
  if (ncell_full > 3){
    ang <- 35 
    offx <- 1
  }
  text(1:ncell_full, -0.11, srt = ang, adj = offx, labels = cellNames_full, 
    xpd = TRUE, cex = 0.75
  )

  axis(2, at = seq(0, 1, 0.5), las = 1, cex.axis = 0.75)
  axis(2, at = seq(0, 1, 0.1), labels = FALSE, tck = -0.02)
  mtext(side = 2, pk, line = 2.75, cex = 1.25)

}

#' Plot cell-specific decay curve for searcher efficiency for a specific model
#'   with comparison to the cellwise model
#'
#' @param modelSet modelSet of class pkmSet
#' @param specificModel name of the specific submodel to plot
#' @param specificCell name of the specific cell to plot
#' @param n number of draws to use to characterize the distributions
#' @param seed_spec random number seed to use for the specific model
#' @param seed_full random number seed to use for the full model
#' @param col_spec color to use for the specific model
#' @param col_full color to use for the specific model
#' @param axis_x logical of whether or not to plot the x axis
#' @param axis_y logical of whether or not to plot the y axis
#' @return a specific cell plot panel
#' @export
#' 
pkmSetSpecSECellPlot <- function(modelSet, specificModel, specificCell, 
                                 col_spec, col_full, axis_y = TRUE,
                                 axis_x = TRUE, n, seed_spec, seed_full){

  model_spec <- modelSet[[specificModel]]
  model_full <- modelSet[[1]]

  cellwise_spec <- model_spec$cellwiseTable
  cellwise_full <- model_full$cellwiseTable
  cellNames_spec <- model_spec$cells[ , "CellNames"]
  cellNames_full <- model_full$cells[ , "CellNames"]

  whichCarcs <- which(model_full$carcCell == specificCell)

  observations <- as.matrix(model_full$observations[whichCarcs, ], 
                    nrow = length(whichCarcs), 
                    ncol = ncol(model_full$observations)
                  )
  nobs <- ncol(observations)
  ncarc <- nrow(observations)
  carcFound <- apply(observations, 2, sum, na.rm = TRUE)
  carcUnavail <- apply(apply(observations, 2, is.na), 2, sum)
  carcAvail <- ncarc - carcUnavail

  cellMatch <- matchCells(model_spec, model_full)
  reducedCell <- cellMatch[cellNames_full == specificCell]
  
  whichSpecificCell_spec <- which(cellNames_spec == reducedCell)
  whichSpecificCell_full <- which(cellNames_full == specificCell)

  pks_spec <- rpk(n, model_spec, seed_spec)
  pks_full <- rpk(n, model_full, seed_full) 

  ps_spec <- pks_spec[[whichSpecificCell_spec]][ , "p"]
  ks_spec <- pks_spec[[whichSpecificCell_spec]][ , "k"]
  ps_full <- pks_full[[whichSpecificCell_full]][ , "p"]
  ks_full <- pks_full[[whichSpecificCell_full]][ , "k"]
  searchTab <- matrix(1:nobs, nrow = n, ncol = nobs, byrow = TRUE)
  ktab_spec <- ks_spec^(searchTab - 1)
  ktab_full <- ks_full^(searchTab - 1)
  SE_spec <- ps_spec * ktab_spec
  SE_full <- ps_full * ktab_full 
  y_spec <- apply(SE_spec, 2, median) 
  y_full <- apply(SE_full, 2, median)


  x_pts <- 1:nobs
  y_pts <- carcFound / carcAvail
  plot(x_pts, y_pts, ylim = c(0, 1), xlim = c(0.5, nobs + 0.5), xlab = "", 
    ylab = "", xaxt = "n", yaxt = "n", bty = "L", col = rgb(0.02, 0.02, 0.02),
    lwd = 2, pch = 1, cex = 1.5
  )

  points(x_pts, y_full, type = 'l', lwd = 3, col = col_full)
  points(x_pts, y_spec, type = 'l', lwd = 3, col = col_spec)

  for (obi in 1:nobs){
    x1 <- x_pts[obi] - 0.25
    y1 <- y_pts[obi] + 0.06
    x2 <- x_pts[obi] + 0.35
    y2 <- y_pts[obi] + 0.15
    rect(x1, y1, x2, y2, border = NA, col = "white")
  }
  obsLabels <- paste(carcFound, carcAvail, sep = "/") 
  text(x_pts + 0.05, y_pts + 0.1, obsLabels, cex = 0.65)

  axis(1, at = x_pts, las = 1, cex.axis = 0.75, labels = axis_x)
  axis(2, at = seq(0, 1, 0.2), las = 1, cex.axis = 0.75, labels = axis_y)
  text(0.5, 0.95, specificCell, adj = 0, cex = 0.75, font = 2)

}
 