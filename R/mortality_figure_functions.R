#' @title Plot total mortality estimation
#' @description \code{plot} defined for class \code{estM} objects
#' @param x \code{estM} object
#' @param ... arguments to pass down
#' @param CL confidence level
#' @export
#'
plot.estM <- function(x, ..., CL = 0.9){
 
  M <- x$M
  Mtot <- apply(M, 2, sum)
  minMtot <- min(Mtot)
  maxMtot <- max(Mtot)
  
  MCLlow <- quantile(Mtot, (1 - CL)/2)
  MCLhi <- quantile(Mtot, 1 - (1 - CL)/2) 
  Mmed <- median(Mtot)
  Mmean <- mean(Mtot)

  df <- approxfun(density(Mtot)) 
  xnew <- seq(minMtot, maxMtot, 1)
  ynew <- df(xnew)
  maxynew <- max(ynew)
  par(mar = c(4, 6, 2, 1))
  plot(xnew, ynew, type = "l", bty = "L", xlab = "", ylab = "", las = 1)
  mtext(side = 1, "Fatatalities", line = 2.75, cex = 1.5)
  mtext(side = 2, "Probability Density", line = 4.5, cex = 1.5)
  Mtext <- paste0(names(summary(x)), ": ", summary(x))
  text(minMtot, maxynew * 1.075, Mtext, adj = 0, xpd = T, cex = 0.85)

  xxnew <- seq(MCLlow, MCLhi, 1)
  yynew <- df(xxnew)
  xx <- c(xxnew, xxnew[length(xxnew):1], xxnew[1])
  yy <- c(yynew, rep(0, length(xxnew)), yynew[1])
  polygon(xx, yy, border = NA, col = rgb(0.9, 0.9, 0.9))
  points(rep(MCLlow, 2), c(0, df(MCLlow)), type = "l")
  points(rep(MCLhi, 2), c(0, df(MCLhi)), type = "l")

  points(xnew, ynew, type = "l", lwd = 2)
  Mmedy <- df(Mmed)
  points(Mmed, Mmedy, pch = 16, cex = 1.75, col = "white")
  points(Mmed, Mmedy, pch = 1, cex = 1.75, lwd = 2)
 
}