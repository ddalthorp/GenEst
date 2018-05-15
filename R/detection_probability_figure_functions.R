#' Plot results of a single pk model
#'
#' @param x ghatGeneric output
#' @param ... to be passed down
#' @param CL confidence level to use
#' @return a plot
#' @export
#'
plot.ghatGeneric <- function(x, ..., CL = 0.9){

  ghats <- x$ghat
  cells <- names(ghats)
  ncell <- length(cells)
  predsByCell <- strsplit(cells, "\\.")
  npred <- length(predsByCell[[1]])
  colNames <- colnames(x$predictors)
  predNames <- colNames[-grep("CellNames", colNames)]
  labelText <- paste("Labels = ", paste(predNames, collapse = "."))

  par(mar = c(5, 4, 2, 1))  
  plot (1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
    bty = "L", xlim = c(0.5, ncell + 0.5), ylim = c(0, 1)
  )
  axis(2, at = seq(0, 1, 0.2), las = 1, cex.axis = 1)
  mtext(side = 2, line = 2.75, "Detection Probability", cex = 1.25)

  axis(1, at = 1:ncell, labels = FALSE)
  ang <- 0
  offx <- NULL
  if (ncell > 3){
    ang <- 45 
    offx <- 1
  }

  text(1:ncell, -0.08, srt = ang, adj = offx, labels = cells, xpd = TRUE, 
    cex = 0.75
  )
  if (predNames[1] == "group" & length(predNames) == 1 & cells[1] == "all"){
    labelText <- NULL
  }

  text(0.5, 1.09, labelText, xpd = TRUE, cex = 0.75, adj = 0)

  probs <- c(0, (1 - CL) / 2, 0.25, 0.5, 0.75, 1 - (1 - CL) / 2, 1)

  for (celli in 1:ncell){
    x <- celli
    y <- quantile(ghats[[celli]], probs)

    med <- c(-0.1, 0.1)
    tb <- c(-0.07, 0.07)

    rect(x - 0.1, y[3], x + 0.1, y[5], lwd = 2, border = 1) 
    points(x + med, rep(y[4], 2), type = 'l', lwd = 2, col = 1)
    points(x + tb, rep(y[2], 2), type = 'l', lwd = 2, col = 1)
    points(x + tb, rep(y[6], 2), type = 'l', lwd = 2, col = 1)
    points(rep(x, 3), y[1:3], type = 'l', lwd = 2, col = 1)
    points(rep(x, 3), y[5:7], type = 'l', lwd = 2, col = 1)

  }
}