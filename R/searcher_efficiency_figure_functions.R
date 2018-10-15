#' @title Plot results of a single pk model
#'
#' @description Plot a single \code{\link{pkm}} model
#'
#' @param x model of class pkm
#'
#' @param col color to use
#'
#' @param ... arguments to be passed to sub functions
#'
#' @return a plot
#'
#' @examples
#'   data(wind_RP)
#'   mod <- pkm(formula_p = p ~ Season, formula_k = k ~ 1, data = wind_RP$SE)
#'   plot(mod)
#'
#' @export
#'
plot.pkm <- function(x, col = "black", ...){
  model <- x
  if (anyNA(model$varbeta) || sum(diag(model$varbeta) < 0) > 0){
    stop("Variance in pkm not well-defined. Cannot plot.")
  }
  name_p <- format(model$formula_p)
  name_k <- model$formula_k
  if (!is.null(model$pOnly) && model$pOnly){
    stop("k missing from pk model. Cannot plot.")
  }
  if (class(name_k) == "numeric"){
    name_k <- paste("k fixed at ", name_k, sep = "")
  } else if (class(name_k) == "character"){
    name_k <- "k not estimated"
   }else {
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
  pkmParamPlot(model = model, pk = "p", col = col)

  par(fig = c(0.5, 1, 0.725, 0.975), new = TRUE)
  pkmParamPlot(model = model, pk = "k", col = col)

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
    pkmSECellPlot(model = model, specificCell = specificCell, col = col,
      axis_y = axis_y, axis_x = axis_x)
  }
}

#' Plot parameter box plots for each cell for either p or k
#'
#' @param model model of class pkm
#'
#' @param pk character of "p" or "k" to delineate between parameter graphed
#'
#' @param col color to use
#'
#' @return a parameter plot panel
#'
#' @export
#'
pkmParamPlot <- function(model, pk = "p", col){

  ncell <- model$ncell
  cellNames <- model$cells[ , "CellNames"]
  predictors <- model$predictors
  CL <- model$CL
  probs <- c(0, (1 - CL) / 2, 0.25, 0.5, 0.75, 1 - (1 - CL) / 2, 1)
  pks <- rpk(n = 1000, model = model)
  pks_full <- rpk(n = 1000, model = model)

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
#'
#' @param specificCell name of the specific cell to plot
#'
#' @param col color to use
#'
#' @param axis_x logical of whether or not to plot the x axis
#'
#' @param axis_y logical of whether or not to plot the y axis
#'
#' @return a cell plot panel
#'
#' @export
#'
pkmSECellPlot <- function(model, specificCell, col, axis_y = TRUE,
                          axis_x = TRUE){

  CL <- model$CL
  cellwise <- model$cell_pk
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
  pks <- rpk(n = 1000, model = model)
  ps <- pks[[whichSpecificCell]][ , "p"]
  ks <- pks[[whichSpecificCell]][ , "k"]
  searchTab <- matrix(1:nobs, nrow = length(ps), ncol = nobs, byrow = TRUE)
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

#' @title Plot results of a set of SE models
#'
#' @description Produce a set of figures for a set of SE models, as fit by
#'   \code{\link{pkmSet}}
#'
#' @param x pk model set of class pkmSet
#'
#' @param specificModel the name(s) or index number(s) of specific model(s)
#'   to restrict the plot
#'
#' @param app logical indicating if the plot is for the app
#'
#' @param cols named vector of colors to use for the specific and reference
#'   models
#'
#' @param ... to be sent to subfunctions
#'
#' @return a set of plots
#'
#' @examples
#'   data(wind_RP)
#'   mod <- pkmSet(formula_p = p ~ Season, formula_k = k ~ Season,
#'            data = wind_RP$SE
#'          )
#'   plot(mod)
#'
#' @export
#'
plot.pkmSet <- function(x, specificModel = NULL, app = FALSE, cols = SEcols(),
                        ...){

  modelSet <- x
  specMods <- checkSpecificModelSE(modelSet, specificModel)
  modelSet <- tidyModelSetSE(modelSet)
  nmod <- length(specMods)
  for (modi in 1:nmod){
    if (modi == 2){
      devAskNewPage(TRUE)
    }
    if (!is.null(modelSet[[modi]]$pOnly) && modelSet[[modi]]$pOnly){
      plot(0, 0, type = 'n', axes = F, xlab = '', ylab = '')
      text(0, .5, "k missing from pk model. Cannot plot.", cex = 2, col = 2)
    } else {
      plotSEFigure(modelSet, specMods[modi], app, cols)
    }
  }
  devAskNewPage(FALSE)
}


#' @title Plot results of a single SE model in a set
#'
#' @description Produce a figures for a specific SE model, as fit by
#'   \code{\link{pkmSet}}
#'
#' @param modelSet pk model set of class \code{pkmSet}
#'
#' @param specificModel the name of the specific model for the plot
#'
#' @param app logical indicating if the plot is for the app
#'
#' @param cols named vector of colors to use for the specific and reference
#'   models
#'
#' @return a plot
#'
#' @export
#'
plotSEFigure <- function(modelSet, specificModel, app, cols){
  plotSEHeader(modelSet, specificModel, app, cols)
  plotSEBoxPlots(modelSet, specificModel, cols)
  plotSEBoxTemplate(modelSet, specificModel, cols)
  plotSECells(modelSet, specificModel, cols)
}

#' @title The SE plot header
#'
#' @description Produce the header for an SE plot
#'
#' @param modelSet pk model set of class pkmSet
#'
#' @param specificModel the name of the specific model for the plot
#'
#' @param app logical indicating if the plot is for the app
#'
#' @param cols named vector of colors to use for the specific and reference
#'   models
#'
#' @return a plot
#'
#' @export
#'
plotSEHeader <- function(modelSet, specificModel, app = FALSE,
                         cols = SEcols()){
  par(mar = c(0, 0, 0, 0))
  par(fig = c(0, 1, 0.935, 1))
  plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "",
    ylab = "", ylim = c(0, 1), xlim = c(0, 1)
  )

  LL <- sapply(modelSet, "[[", "loglik")
  referenceMod <- names(modelSet)[which(LL == max(LL))]

  if (app){
    specificModel <- gsub("~ 1", "~ constant", specificModel)
    referenceMod <- gsub("~ 1", "~ constant", referenceMod)
  }

  rect(0.01, 0.725, 0.06, 0.925, lwd = 2, col = cols["spec"], border = NA)
  text_model <- paste("= Selected Model: ", specificModel, sep = "")
  text(x = 0.07, y = 0.85, text_model, adj = 0, cex = 0.9)

  rect(0.01, 0.325, 0.06, 0.525, lwd = 2, col = cols["ref"], border = NA)
  text_model <- paste("= Reference Model: ", referenceMod, sep = "")
  text(x = 0.07, y = 0.45, text_model, adj = 0, cex = 0.9)

  labelsText <- paste(modelSetPredictors(modelSet), collapse = ".")
  labelsText[labelsText == ""] <- "all"
  text_label <- paste("Labels: ", labelsText, sep = "")
  text(x = 0.9, y = 0.8, text_label, adj = 1, cex = 0.75)

}

#' @title p and k box plots for an SE model set
#'
#' @description Plot parameter box plots for each cell within a model for
#'   both p and k with comparison to the cellwise model
#'
#' @param modelSet modelSet of class pkmSet
#'
#' @param specificModel name of the specific submodel to plot
#'
#' @param cols named vector of colors to use for the specific and reference
#'   models
#'
#' @return a set of parameter plot panels
#'
#' @export
#'
plotSEBoxPlots <- function(modelSet, specificModel, cols){
  par(mar = c(0,0,0,0))
  par(fig = c(0, 0.45, 0.7, 0.965), new = TRUE)
  pkmSetSpecParamPlot(modelSet, specificModel, "p", cols)
  par(fig = c(0.45, 0.9, 0.7, 0.965), new = TRUE)
  if (!grepl("k not estimated", specificModel)){
    pkmSetSpecParamPlot(modelSet, specificModel, "k", cols)
  }
}

#' @title p or k box plots for an SE model set
#'
#' @description Plot parameter box plots for each cell within a model for
#'   either p or k with comparison to the cellwise model
#'
#' @param modelSet modelSet of class pkmSet
#'
#' @param specificModel name of the specific submodel to plot
#'
#' @param pk character of "p" or "k" to delineate between parameter graphed
#'
#' @param cols named vector of colors to use for the specific and reference
#'   models
#'
#' @return a specific parameter plot panel
#'
#' @export
#'
pkmSetSpecParamPlot <- function(modelSet, specificModel, pk = "p", cols){

  model_spec <- modelSet[[specificModel]]
  model_ref <- refMod(modelSet)

  CL <- model_ref$CL
  probs <- c(0, (1 - CL) / 2, 0.25, 0.5, 0.75, 1 - (1 - CL) / 2, 1)
  observations_spec <- model_spec$observations
  observations_ref <- model_ref$observations
  ncell_spec <- model_spec$ncell
  ncell_ref <- model_ref$ncell
  cellNames_ref <- model_ref$cells[ , "CellNames"]
  predictors_spec <- model_spec$predictors
  predictors_ref <- model_ref$predictors

  if (any(grepl("k not estimated", specificModel))){
    return(1)
  }
  pks_spec <- rpk(n = 1000, model = model_spec)
  pks_ref <- rpk(n = 1000, model = model_ref)

#  kIncluded <- !any(grepl("k not estimated", specificModel))
#  if (kIncluded){
#    pks_spec <- rpk(n = 1000, model = model_spec)
#    pks_ref <- rpk(n = 1000, model = model_ref)
#  } else{
#    pks_spec <- rpk(n = 1000, model = model_spec)
#    pks_ref <- rpk(n = 1000, model = model_ref)
#  }
  cellMatch_spec <- matchCells(model_spec, modelSet)
  cellMatch_ref <- matchCells(model_ref, modelSet)

  cells_set <- modelSetCells(modelSet)
  cellNames_set <- cells_set$CellNames
  ncell_set <- nrow(cells_set)

  if (pk == "p"){
    maxy <- 1
  } else if (pk == "k"){
    maxy <- 1
    for (celli in 1:ncell_set){
      maxcell_spec <- max(pks_spec[[cellMatch_spec[celli]]][ , "k"]) * 1.01
      maxcell_ref <- max(pks_ref[[cellMatch_ref[celli]]][ , "k"]) * 1.01
      maxy <- max(c(maxy, maxcell_spec, maxcell_ref))
    }
  }
  maxy[is.na(maxy)] <- 1

  par(mar = c(4,3,2,1))
  plot(1, type = "n", xlab = "", ylab = "", bty = "L", xaxt = 'n', yaxt = 'n',
    ylim = c(0, maxy), xlim = c(0.5, ncell_set + 0.5)
  )

  for (celli in 1:ncell_set){
    cMi_s <- cellMatch_spec[celli]
    cMi_f <- cellMatch_ref[celli]
    x_s <- celli - 0.2
    y_s <- quantile(pks_spec[[cMi_s]][ , pk], probs, na.rm = TRUE)
    x_f <- celli + 0.2
    y_f <- quantile(pks_ref[[cMi_f]][ , pk], probs, na.rm = TRUE)

    med <- c(-0.1, 0.1)
    tb <- c(-0.07, 0.07)

    rect(x_s - 0.1, y_s[3], x_s + 0.1, y_s[5], lwd = 1, border = cols["spec"])
    points(x_s + med, rep(y_s[4], 2), type = 'l', lwd = 1, col = cols["spec"])
    points(x_s + tb, rep(y_s[2], 2), type = 'l', lwd = 1, col = cols["spec"])
    points(x_s + tb, rep(y_s[6], 2), type = 'l', lwd = 1, col = cols["spec"])
    points(rep(x_s, 3), y_s[1:3], type = 'l', lwd = 1, col = cols["spec"])
    points(rep(x_s, 3), y_s[5:7], type = 'l', lwd = 1, col = cols["spec"])

    rect(x_f - 0.1, y_f[3], x_f + 0.1, y_f[5], lwd = 1, border = cols["ref"])
    points(x_f + med, rep(y_f[4], 2), type = 'l', lwd = 1, col = cols["ref"])
    points(x_f + tb, rep(y_f[2], 2), type = 'l', lwd = 1, col = cols["ref"])
    points(x_f + tb, rep(y_f[6], 2), type = 'l', lwd = 1, col = cols["ref"])
    points(rep(x_f, 3), y_f[1:3], type = 'l', lwd = 1, col = cols["ref"])
    points(rep(x_f, 3), y_f[5:7], type = 'l', lwd = 1, col = cols["ref"])
  }

  axis(1, at = 1:ncell_set, labels = FALSE, tck = -0.05)
  ang <- 0
  offy <- -0.25
  offx <- NULL
  if (ncell_set > 3){
    ang <- 35
    offx <- 1
  }
  xcex <- 0.75
  if (ncell_set > 6){
    xcex <- 0.5
    offy <- -0.125
  }
  text(1:ncell_set, offy, srt = ang, adj = offx, labels = cellNames_set,
    xpd = TRUE, cex = xcex
  )

  axis(2, at = seq(0, 1, 0.5), las = 1, cex.axis = 0.7)
  axis(2, at = seq(0, 1, 0.1), labels = FALSE, tck = -0.015)
  mtext(side = 2, pk, line = 2.2, cex = 1.125)

}

#' @title template box plot
#'
#' @description Plot template box plot
#'
#' @param modelSet modelSet of class pkmSet
#'
#' @param specificModel name of the specific submodel to plot
#'
#' @param cols named vector of colors to use for the specific and reference
#'   models
#'
#' @return a template box plot
#'
#' @export
#'
plotSEBoxTemplate <- function(modelSet, specificModel, cols){

  model_spec <- modelSet[[specificModel]]
  col_spec <- cols["spec"]
  par(mar = c(0,0,0,0))
  par(fig = c(0.92, 1, 0.8, 0.95), new = TRUE)
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
  rect(x_s - 0.1, y_s[3], x_s + 0.1, y_s[5], lwd = 1, border = col_spec)
  points(x_s + med, rep(y_s[4], 2), type = 'l', lwd = 1, col = col_spec)
  points(x_s + tb, rep(y_s[2], 2), type = 'l', lwd = 1, col = col_spec)
  points(x_s + tb, rep(y_s[6], 2), type = 'l', lwd = 1, col = col_spec)
  points(rep(x_s, 3), y_s[1:3], type = 'l', lwd = 1, col = col_spec)
  points(rep(x_s, 3), y_s[5:7], type = 'l', lwd = 1, col = col_spec)
  num_CL <- c(CL_split, 1 - CL_split) * 100
  text_CL <- paste(num_CL, "%", sep = "")
  text_ex <- c("min", text_CL[1], "25%", "50%", "75%", text_CL[2], "max")
  text(x_s + 0.2, y_s, text_ex, cex = 0.6, adj = 0)

}

#' @title Plot the cellwise results of a single model in a set of SE models
#'
#' @description Produce a set of cellwise figures for a specific SE model, as
#'   fit by \code{\link{pkmSet}}
#'
#' @param modelSet pk model set of class pkmSet
#'
#' @param specificModel the name of the specific model for the plot
#'
#' @param cols named vector of colors to use for the specific and reference
#'   models
#'
#' @return a plot
#'
#' @export
#'
plotSECells <- function(modelSet, specificModel, cols){

  model_ref <- refMod(modelSet)

  par(fig = c(0, 1, 0, 0.65), new = TRUE, mar = c(1, 1, 1, 1))
  plot(1,1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "",
    ylab = ""
  )
  mtext(side = 1, "Search", line = -0.25, cex = 1.5)
  mtext(side = 2, "Searcher Efficiency", line = -0.25, cex = 1.5)

  cells_set <- modelSetCells(modelSet)
  ncell <- nrow(cells_set)
  cellNames <- cells_set[ , "CellNames"]

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
    par(fig = c(x1[celli], x2[celli], y1[celli], y2[celli]), new = TRUE)
    specificCell <- cellNames[celli]
    axis_x <- FALSE
    axis_y <- FALSE
    if (celli %in% bottomCells){
      axis_x <- TRUE
    }
    if (celli %in% leftCells){
      axis_y <- TRUE
    }
    axes <- c("x" = axis_x, "y" = axis_y)
    pkmSetSpecSECellPlot(modelSet, specificModel, specificCell, cols, axes)
  }
}

#' Plot cell-specific decay curve for searcher efficiency for a specific model
#'   with comparison to the cellwise model
#'
#' @param modelSet modelSet of class pkmSet
#'
#' @param specificModel name of the specific submodel to plot
#'
#' @param specificCell name of the specific cell to plot
#'
#' @param cols named vector of colors to use for the specific and reference
#'   models
#'
#' @param axes named vector of logical values indicating whether or not to
#'   plot the x axis and the y axis
#'
#' @return a specific cell plot panel
#'
#' @export
#'
pkmSetSpecSECellPlot <- function(modelSet, specificModel, specificCell,
                                 cols, axes){

  model_spec <- modelSet[[specificModel]]
  model_ref <- refMod(modelSet)

  cellwise_spec <- model_spec$cell_pk
  cellwise_ref <- model_ref$cell_pk
  cellNames_spec <- model_spec$cells[ , "CellNames"]
  cellNames_ref <- model_ref$cells[ , "CellNames"]

  cells_set <- modelSetCells(modelSet)
  preds_set <- modelSetPredictors(modelSet)
  carcCells <- apply(data.frame(model_spec$data0[ , preds_set]),
                 1, paste, collapse = "."
               )
  whichCarcs <- which(carcCells == specificCell)
  if (specificCell == "all"){
    whichCarcs <- 1:length(carcCells)
  }

  observations <- as.matrix(model_spec$observations[whichCarcs, ],
                    nrow = length(whichCarcs),
                    ncol = ncol(model_spec$observations)
                  )
  nobs <- ncol(observations)
  ncarc <- nrow(observations)
  carcFound <- apply(observations, 2, sum, na.rm = TRUE)
  carcUnavail <- apply(apply(observations, 2, is.na), 2, sum)
  carcAvail <- ncarc - carcUnavail

  if (any(grepl("k not estimated", specificModel))){
    return(1)
  }
  pks_spec <- rpk(n = 1000, model = model_spec)
  pks_ref <- rpk(n = 1000, model = model_ref)

#  kIncluded <- !any(grepl("k not estimated", specificModel))
#  if (kIncluded){
#    pks_spec <- rpk(n = 1000, model = model_spec)
#    pks_ref <- rpk(n = 1000, model = model_ref)
#  } else{
#    pks_spec <- rpk(n = 1000, model = model_spec, kFill = 1)
#    pks_ref <- rpk(n = 1000, model = model_ref, kFill = 1)
#  }
  cellMatch_spec <- matchCells(model_spec, modelSet)
  cellMatch_ref <- matchCells(model_ref, modelSet)

  cells_set <- modelSetCells(modelSet)
  cellNames_set <- cells_set$CellNames

  whichSpecificCell_spec <- cellMatch_spec[cellNames_set == specificCell]
  whichSpecificCell_ref <- cellMatch_ref[cellNames_set == specificCell]

  ps_spec <- pks_spec[[whichSpecificCell_spec]][ , "p"]
  ks_spec <- pks_spec[[whichSpecificCell_spec]][ , "k"]
  ps_ref <- pks_ref[[whichSpecificCell_ref]][ , "p"]
  ks_ref <- pks_ref[[whichSpecificCell_ref]][ , "k"]
  searchTab <- matrix(1:nobs, nrow = 1000, ncol = nobs, byrow = TRUE)
  ktab_spec <- ks_spec^(searchTab - 1)
  ktab_ref <- ks_ref^(searchTab - 1)
  SE_spec <- ps_spec * ktab_spec
  SE_ref <- ps_ref * ktab_ref
  y_spec <- apply(SE_spec, 2, median)
  y_ref <- apply(SE_ref, 2, median)

  x_pts <- 1:nobs
  y_pts <- carcFound / carcAvail
  plot(x_pts, y_pts, ylim = c(0, 1.1), xlim = c(0.5, nobs + 0.5), xlab = "",
    ylab = "", xaxt = "n", yaxt = "n", bty = "L", col = rgb(0.02, 0.02, 0.02),
    lwd = 2, pch = 1, cex = 1.5
  )

  points(x_pts, y_ref, type = 'l', lwd = 3, col = cols["ref"])
  points(x_pts, y_spec, type = 'l', lwd = 3, col = cols["spec"])

  for (obi in 1:nobs){
    x1 <- x_pts[obi] - 0.25
    y1 <- y_pts[obi] + 0.035
    x2 <- x_pts[obi] + 0.35
    y2 <- y_pts[obi] + 0.11
    rect(x1, y1, x2, y2, border = NA, col = "white")
  }
  obsLabels <- paste(carcFound, carcAvail, sep = "/")
  text(x_pts + 0.05, y_pts + 0.075, obsLabels, cex = 0.65)

  axis(1, at = x_pts, las = 1, cex.axis = 0.75, labels = axes["x"])
  axis(2, at = seq(0, 1, 0.2), las = 1, cex.axis = 0.75, labels = axes["y"])
  text(0.5, 1.1, specificCell, adj = 0, cex = 0.75, font = 2, xpd = TRUE)

}


#' @title Produce a named vectory with standard SE plot colors
#'
#' @description Produce a named vectory with standard SE plot colors
#'
#' @export
#'
SEcols <- function(){
  c(spec = "black", ref = "grey")
}

#' @title Error check a specific model selection for an SE plot
#'
#' @description Make sure it's available and good, update the name for usage
#'
#' @param modelSet pk model set of class pkmSet
#'
#' @param specificModel the name of the specific model for the plot
#'
#' @return updated name of the model to use
#'
#' @export
#'
checkSpecificModelSE <- function(modelSet, specificModel){
  if (!is.null(specificModel) && anyNA(specificModel)){
    stop(
      "specificModel must be NULL or a vector of model names or positions.",
      "\nNAs not allowed."
    )
  }
  if (length(specificModel) > 0){
    if (is.numeric(specificModel)){
      if (anyNA(specificModel)){
        warning("specificModel cannot be NA. NA models removed.")
        specificModel <- specificModel[!is.na(specificModel)]
        if (length(specificModel) == 0){
          stop("No valid specificModel")
        }
      }
      if (any(specificModel > length(modelSet))){
        stop(paste0("there are only ", length(modelSet), " model choices."))
      }
      specificModel <- names(modelSet)[specificModel]
    }
    if (any(specificModel %in% names(modelSet)) == FALSE){
      stop("Selected model not in set. To see options use names(modelSet).")
    }
    modNames <- specificModel
    for (modi in modNames){
      if (pkmFail(modelSet[[modi]])){
        stop("specificModel ", modi, " is not a well-fit pk model")
      }
    }
  } else{
    specificModel <- names(pkmSetFailRemove(modelSet))
  }
  return(specificModel)
}

#' @title Tidy an SE model set
#'
#' @description Remove bad fit models
#'
#' @param modelSet pk model set of class pkmSet
#'
#' @return a trimmed model set
#'
#' @export
#'
tidyModelSetSE <- function(modelSet){
  modelSet <- pkmSetFailRemove(modelSet)
  modelSet <- modelSet[order(sapply(modelSet, "[[", "AICc"))]
  class(modelSet) <- c("pkmSet", "list")
  return(modelSet)
}
