#' @title Plot results of a single generic ghat estimation
#'
#' @description Plot method for a single generic \code{ghat} estimation
#'
#' @param x \code{\link{estgGeneric}} output
#'
#' @param CL confidence level to use
#'
#' @param ... to be passed down
#'
#' @return generic detection probability plot
#'
#' @examples 
#'   data(mock)
#'   model_SE <- pkm(formula_p = p ~ HabitatType, formula_k = k ~ 1,
#'                 data = mock$SE)
#'   model_CP <- cpm(formula_l = l ~ Visibility, formula_s = s ~ Visibility, 
#'                 data = mock$CP, left = "LastPresentDecimalDays", 
#'                 right = "FirstAbsentDecimalDays")
#'   avgSS <- averageSS(mock$SS)
#'   ghatsGeneric <- estgGeneric(nsim = 1000, avgSS, model_SE, model_CP)
#'   plot(ghatsGeneric)
#'
#' @export
#'
plot.gGeneric <- function(x, CL = 0.90, ...){

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

  text(1:ncell, -0.1, srt = ang, adj = offx, labels = cells, xpd = TRUE, 
    cex = 0.75
  )
  if (predNames[1] == "group" & length(predNames) == 1 & cells[1] == "all"){
    labelText <- NULL
  }

  text(0.5, 1.15, labelText, xpd = TRUE, cex = 0.75, adj = 0)

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

#' @title Plot results of a set of size-based generic ghat estimations
#'
#' @description Plot method for a size-based generic \code{ghat} estimation
#'
#' @param x \code{\link{estgGenericSize}} output
#'
#' @param CL confidence level to use
#'
#' @param ... to be passed down
#'
#' @return size-based detection probability plot
#'
#' @examples
#'   data(mock)
#'   pkmModsSize <- pkm(formula_p = p ~ HabitatType,
#'                    formula_k = k ~ HabitatType, data = mock$SE,
#'                    obsCol = c("Search1", "Search2", "Search3", "Search4"),
#'                    sizeCol = "Size", allCombos = TRUE)
#'   cpmModsSize <- cpm(formula_l = l ~ Visibility,
#'                    formula_s = s ~ Visibility, data = mock$CP,
#'                    left = "LastPresentDecimalDays",
#'                    right = "FirstAbsentDecimalDays",
#'                    dist = c("exponential", "lognormal"),
#'                    sizeCol = "Size", allCombos = TRUE)
#'   pkMods <- c("S" = "p ~ 1; k ~ 1", "L" = "p ~ 1; k ~ 1",
#'              "M" = "p ~ 1; k ~ 1", "XL" = "p ~ 1; k ~ 1"
#'             )
#'   cpMods <- c("S" = "dist: exponential; l ~ 1; NULL", 
#'               "L" = "dist: exponential; l ~ 1; NULL",
#'               "M" = "dist: exponential; l ~ 1; NULL",
#'               "XL" = "dist: exponential; l ~ 1; NULL"
#'             )
#'   avgSS <- averageSS(mock$SS)
#'   gsGeneric <- estgGenericSize(nsim = 1000, days = avgSS,
#'                  modelSetSize_SE = pkmModsSize,
#'                  modelSetSize_CP = cpmModsSize,
#'                  modelSizeSelections_SE = pkMods,
#'                  modelSizeSelections_CP = cpMods
#'                )
#'  plot(gsGeneric)
#'
#' @export
#'
plot.gGenericSize <- function(x, CL = 0.90, ...){

  nsizeclass <- length(x)

  if (nsizeclass == 1){
    return(plot(x[[1]], CL = CL, ...))
  }

  sizeclasses <- names(x)

  increm <- 1 / nsizeclass
  y1 <- seq(0, 1, by = increm)
  y1 <- y1[-length(y1)]
  y2 <- seq(0, 1, by = increm)
  y2 <- y2[-1]
  names(y1) <- sizeclasses
  names(y2) <- sizeclasses
  par(fig = c(0, 1, 0, 1))
  par(mar = c(3, 3, 1, 1))  
  plot(1, 1, type = "n", ylab= "", xlab = "", xaxt = "n", yaxt = "n", 
    bty = "n"
  )  
  mtext(side = 2, line = 2, "Detection Probability", cex = 1)

  for (sci in sizeclasses){
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

    if (!is.null(sci)){
      text_sc <- paste("Carcass class: ", sci, sep = "")
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