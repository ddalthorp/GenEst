#' @title Plot total mortality estimation
#' @description \code{plot} defined for class \code{estM} objects
#' @param x \code{estM} object
#' @param ... arguments to pass down
#' @param CL confidence level
#' @export
#'
plot.estM <- function(x, ..., CL = 0.9){
  simpleMplot(x$M, ..., CL = CL)
}

#' @title Plot total mortality estimation for a simple situation
#' @description Hisogram and empirical PDF on top
#' @param M Mortality object
#' @param ... arguments to pass down
#' @param CL confidence level
#' @export
#'
simpleMplot <- function(M, ..., CL = 0.9){
  
  if ("splitFull" %in% class(M)){
    Mtot <- as.vector(M)
  } else {
    Mtot <- apply(M, 2, sum)
  }
  minMtot <- min(Mtot)
  maxMtot <- max(Mtot)
  pl <- (1 - CL)/2
  ph <- 1 - (1 - CL)/2
  MCLlow <- round(quantile(Mtot, pl), 2)
  MCLhi <- round(quantile(Mtot, ph), 2) 
  Mmed <- round(median(Mtot), 2)

  Mtext <- paste0("Median: ", Mmed, "; ", CL * 100, "% CI: [",
           MCLlow, ", ", MCLhi, "]"
         )

  Mtot <- apply(M, 2, sum)
  minMtot <- min(Mtot)
  maxMtot <- max(Mtot)
  
  MCLlow <- quantile(Mtot, (1 - CL)/2)
  MCLhi <- quantile(Mtot, 1 - (1 - CL)/2) 
  medM <- median(Mtot)
  meanM <- mean(Mtot)
  minM <- min(Mtot)
  maxM <- max(Mtot)
  stepMin <- floor(minM / 10) * 10 
  stepMax <- ceiling(maxM / 10) * 10
  steps <- seq(stepMin, stepMax, length.out = 10)
  stepSize <- steps[2] - steps[1]
  if (min(steps) > minM){
    steps <- c(min(steps) - stepSize, steps)
  }
  if (max(steps) < maxM){
    steps <- c(steps, max(steps) + stepSize)
  }
  nstep <- length(steps)
  nrect <- nstep - 1
  rectL <- rep(NA, nrect)
  rectR <- rep(NA, nrect)
  rectY <- rep(NA, nrect)

  for (recti in 1:nrect){
    rectL[recti] <- steps[recti]
    rectR[recti] <- steps[recti + 1]
    rectY[recti] <- sum(Mtot >= steps[recti] & Mtot < steps[recti + 1])
  }
  rectY <- rectY / sum(rectY)
  df <- approxfun(density(Mtot)) 
  xnew <- seq(minMtot, maxMtot, 1)
  ynew <- df(xnew)
  maxynew <- max(ynew)
  ynew <- ynew / maxynew
  ynew <- ynew * max(rectY)
  xxnew <- seq(MCLlow, MCLhi, 1)
  yynew <- df(xxnew)
  maxyynew <- max(yynew)
  yynew <- yynew / maxyynew
  yynew <- yynew * max(rectY)
  xx <- c(xxnew, xxnew[length(xxnew):1], xxnew[1])
  yy <- c(yynew, rep(0, length(xxnew)), yynew[1])
  Mmedy <- df(Mmed)
  Mmedy <- Mmedy / maxyynew
  Mmedy <- Mmedy * max(rectY)

  par(mar = c(4, 5, 2, 2), fig = c(0, 1, 0, 1))
  plot(rectL, rectY, type = "n", bty = "L", xlab = "", ylab = "", las = 1,
    xlim = c(rectL[1], rectR[nrect]))
  points(xnew, ynew, type = "l", lwd = 2)
  polygon(xx, yy, border = NA, col = rgb(0.9, 0.9, 0.9))
  points(xnew, ynew, type = "l", lwd = 2)
  for (recti in 1:nrect){
    rect(rectL[recti], 0, rectR[recti], rectY[recti])
  }
  points(Mmed, Mmedy, pch = 16, cex = 2, col = "white")
  points(Mmed, Mmedy, pch = 1, cex = 2, lwd = 2)
  mtext(side = 1, "Mortalities", line = 2.75, cex = 1.5)
  mtext(side = 2, "Probability", line = 3.5, cex = 1.5)

  text(minMtot, max(rectY) * 1.075, Mtext, adj = 0, xpd = T, cex = 0.85)

}