#' Plot cell-specific decay curve for carcass persistence
#'
#' @param model model of class pkm
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

  plot(survfit, ylim = c(0, 1), xlim = c(0, max_x), main = specificCell, 
    xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "L", lwd = c(2, 1, 1)
  )
  if (axis_x == TRUE){
    axis(1, las = 1, cex.axis = 1.)
  }
  if (axis_y == TRUE){
    axis(2, las = 1, cex.axis = 1., at = seq(0, 1, 0.2))
  }
  points(pts, pred_y, type = "l", col = col, lwd = lwd)
  points(pts, pred_yl, type = "l", col = col, lwd = lwd, lty = 3)
  points(pts, pred_yu, type = "l", col = col, lwd = lwd, lty = 3)
  countText <- paste("N = ", ncarc, sep = "")
  text(x = 1, y = 1.07, countText, cex = 1, xpd = TRUE, adj = 0)
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
plot.cpm <- function(model, n = 1000, seed = 1, col = "black"){

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
  mtext(side = 1, "Time", line = -0.5, cex = 1.75)
  mtext(side = 2, "Carcass Persistence", line = -0.5, cex = 1.75)

  ncell <- model$ncell
  cellNames <- model$cells[ , "CellNames"]

  nmatrix_col <- min(3, ncell)
  nmatrix_row <- ceiling(ncell / nmatrix_col)
  figxspace <- 0.95 / nmatrix_col
  figyspace <- 0.925 / nmatrix_row
  x1 <- rep(figxspace * ((1:nmatrix_col) - 1), nmatrix_row) + 0.05
  x2 <- rep(figxspace * ((1:nmatrix_col)), nmatrix_row) + 0.05
  y1 <- rep(figyspace * ((nmatrix_row:1) - 1), each = nmatrix_col) + 0.04
  y2 <- rep(figyspace * ((nmatrix_row:1)), each = nmatrix_col) + 0.04
  bottomCells <- seq(ncell - (nmatrix_col - 1), ncell, 1)
  leftCells <- which(1:ncell %% nmatrix_col == 1)

  for (celli in 1:ncell){
    par(mar = c(2.5, 1, 2.5, 1))
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

