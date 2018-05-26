#' Plot results of a single generic ghat estimation
#'
#' @param x ghatGeneric output
#' @param CL confidence level to use
#' @param sizeclassName name of the size class if it is to be added to the
#'   figure
#' @param ... to be passed down
#' @return a plot
#' @export
#'
plot.gGeneric <- function(x, sizeclassName = NULL, CL = 0.9, ...){

  ghats <- x$ghat
  cells <- names(ghats)
  ncell <- length(cells)
  predsByCell <- strsplit(cells, "\\.")
  npred <- length(predsByCell[[1]])
  colNames <- colnames(x$predictors)
  predNames <- colNames[-grep("CellNames", colNames)]
  labelText <- paste("Labels = ", paste(predNames, collapse = "."))

  par(mar = c(5, 4, 4, 1))  
  par(fig = c(0, 0.9, 0, 1))
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

  text(0.5, 1.15, labelText, xpd = TRUE, cex = 0.75, adj = 0)

  if (!is.null(sizeclassName)){
    text_sc <- paste("Size class: ", sizeclassName, sep = "")
    text(x = 0.5, y = 1.25, xpd = TRUE, text_sc, adj = 0, cex = 1, font = 1)
  }

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

  par(mar = c(2,0,2,0))
  par(fig = c(0.9, 1, 0.25, 0.75), new = TRUE)
  plot(1,1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", 
    ylab = "", ylim = c(0, 1), xlim = c(0, 1)
  )
  x_s <- 0.1
  CL_split <- (1 - CL) / 2
  probs_y <- c(0, CL_split, 0.25, 0.5, 0.75, 1 - CL_split, 1)
  set.seed(12)
  y_s <- quantile(rnorm(1000, 0.5, 0.1), probs = probs_y)
  med <- c(-0.1, 0.1)
  tb <- c(-0.07, 0.07)
  rect(x_s - 0.1, y_s[3], x_s + 0.1, y_s[5], lwd = 2, border = 1) 
  points(x_s + med, rep(y_s[4], 2), type = 'l', lwd = 2, col = 1)
  points(x_s + tb, rep(y_s[2], 2), type = 'l', lwd = 2, col = 1)
  points(x_s + tb, rep(y_s[6], 2), type = 'l', lwd = 2, col = 1)
  points(rep(x_s, 3), y_s[1:3], type = 'l', lwd = 2, col = 1)
  points(rep(x_s, 3), y_s[5:7], type = 'l', lwd = 2, col = 1)
  num_CL <- c(CL_split, 1 - CL_split) * 100
  text_CL <- paste(num_CL, "%", sep = "")
  text_ex <- c("min", text_CL[1], "25%", "50%", "75%", text_CL[2], "max")
  text(x_s + 0.2, y_s, text_ex, cex = 0.65, adj = 0)
}

#' Plot results of a set of size-based generic g estimations
#'
#' @param x gGenericSize output
#' @param CL confidence level to use
#' @param ... to be passed down
#' @return a plot
#' @export
#'
plot.gGenericSize <- function(x, CL = 0.9, ...){

  nsizeclass <- length(x)

  if (nsizeclass == 1){
    return(plot(x[[1]], CL = CL, ...))
  }

  sizeclassNames <- names(x)

  increm <- 1 / nsizeclass
  y1 <- seq(0, 1, by = increm)
  y1 <- y1[-length(y1)]
  y2 <- seq(0, 1, by = increm)
  y2 <- y2[-1]
  par(fig = c(0, 1, 0, 1))
  par(mar = c(3, 3, 1, 1))  
  plot(1, 1, type = "n", ylab= "", xlab = "", xaxt = "n", yaxt = "n", 
    bty = "n"
  )  
  mtext(side = 2, line = 2, "Detection Probability", cex = 1)

  for (sci in 1:nsizeclass){

    par(fig = c(0, 0.9, y1[sci], y2[sci]), new = TRUE)
    ghats <- x[[sci]]$ghat
    cells <- names(ghats)
    ncell <- length(cells)
    predsByCell <- strsplit(cells, "\\.")
    npred <- length(predsByCell[[1]])
    colNames <- colnames(x[[sci]]$predictors)
    predNames <- colNames[-grep("CellNames", colNames)]
    labelText <- paste("Labels = ", paste(predNames, collapse = "."))

    par(mar = c(3, 4, 3, 1))  
    plot (1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
      bty = "L", xlim = c(0.5, ncell + 0.5), ylim = c(0, 1)
    )
    axis(2, at = seq(0, 1, 0.25), las = 1, cex.axis = 0.5)
    axis(1, at = 1:ncell, labels = FALSE, tck = -0.05)
    ang <- 0
    offx <- NULL
    if (ncell > 3){
      ang <- 45 
      offx <- 1
    }

    text(1:ncell, -0.2, srt = ang, adj = offx, labels = cells, xpd = TRUE, 
      cex = 0.5
    )
    if (predNames[1] == "group" & length(predNames) == 1 & cells[1] == "all"){
      labelText <- NULL
    }

    text(0.5, 1.15, labelText, xpd = TRUE, cex = 0.5, adj = 0)

    if (!is.null(sizeclassNames[sci])){
      text_sc <- paste("Size class: ", sizeclassNames[sci], sep = "")
      text(x = 0.5, y = 1.25, xpd = TRUE, text_sc, adj = 0, cex = 0.5)
    }

    probs <- c(0, (1 - CL) / 2, 0.25, 0.5, 0.75, 1 - (1 - CL) / 2, 1)

    for (celli in 1:ncell){
      xx <- celli
      yy <- quantile(ghats[[celli]], probs)

      med <- c(-0.1, 0.1)
      tb <- c(-0.07, 0.07)

      rect(xx - 0.1, yy[3], xx + 0.1, yy[5], lwd = 2, border = 1) 
      points(xx + med, rep(yy[4], 2), type = 'l', lwd = 2, col = 1)
      points(xx + tb, rep(yy[2], 2), type = 'l', lwd = 2, col = 1)
      points(xx + tb, rep(yy[6], 2), type = 'l', lwd = 2, col = 1)
      points(rep(xx, 3), yy[1:3], type = 'l', lwd = 2, col = 1)
      points(rep(xx, 3), yy[5:7], type = 'l', lwd = 2, col = 1)

    }
  }

  par(mar = c(2,0,2,0))
  par(fig = c(0.9, 1, 0, 1), new = TRUE)
  plot(1,1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", 
    ylab = "", ylim = c(0, 1), xlim = c(0, 1)
  )
  x_s <- 0.1
  CL_split <- (1 - CL) / 2
  probs_y <- c(0, CL_split, 0.25, 0.5, 0.75, 1 - CL_split, 1)
  set.seed(12)
  y_s <- quantile(rnorm(1000, 0.5, 0.1), probs = probs_y)
  med <- c(-0.1, 0.1)
  tb <- c(-0.07, 0.07)
  rect(x_s - 0.1, y_s[3], x_s + 0.1, y_s[5], lwd = 2, border = 1) 
  points(x_s + med, rep(y_s[4], 2), type = 'l', lwd = 2, col = 1)
  points(x_s + tb, rep(y_s[2], 2), type = 'l', lwd = 2, col = 1)
  points(x_s + tb, rep(y_s[6], 2), type = 'l', lwd = 2, col = 1)
  points(rep(x_s, 3), y_s[1:3], type = 'l', lwd = 2, col = 1)
  points(rep(x_s, 3), y_s[5:7], type = 'l', lwd = 2, col = 1)
  num_CL <- c(CL_split, 1 - CL_split) * 100
  text_CL <- paste(num_CL, "%", sep = "")
  text_ex <- c("min", text_CL[1], "25%", "50%", "75%", text_CL[2], "max")
  text(x_s + 0.2, y_s, text_ex, cex = 0.65, adj = 0)

}