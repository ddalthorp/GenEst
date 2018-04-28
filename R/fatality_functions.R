#' @useDynLib GenEst
#' @importFrom Rcpp sourceCpp
#' @export
calcRate <- function(Mtilde, Aj, days, searches){
  if (is.vector(Mtilde)){
    Mtilde <- matrix(Mtilde, nrow = 1)
    Aj <- matrix(Aj, nrow = 1)
    searches <- matrix(searches, nrow = 1)
  }
  calcRateC(Mtilde, Aj, days, searches)
}
#' @export
calcTsplit <- function(rate, days, tsplit){
  if (is.vector(rate)){
    rate <- matrix(rate, nrow = 1)
  }
  calcTsplitC(rate, days, tsplit)
}
#' @export
calcSplits <- function(Mest, Aj = NULL,
  split_CO = NULL, COdat = NULL,
  split_SS = NULL, SSdat = NULL,
  split_time = NULL,
  CI_level = 0.95, ...){
  ##### read data and check for errors
  if ((!is.null(split_SS) || !is.null(split_time)) & is.null(SSdat)){
    stop("SSdat must be provided if ",
      ifelse(is.null(split_SS), "split_time ", "split_SS "), "is")
  }
  if (!is.null(split_CO) + !is.null(split_SS) + !is.null(split_time) > 0){
    if (is.null(COdat)) stop("COdat must be provided to perform non-null splits")
  }
  ### declare traffic directing variables (updated after parsing inputs)
  # number of valid split variables:
  nvar <- 0
  # characterization of horizontal and vertical split variables as "SS" or "CO":
  vartype <- NULL
  split_h <- NULL
  split_v <- NULL
  ### error-checking the split variables and interpreting inputs:
  if (!is.null(split_time)){
    if (!is.numeric(split_time) || !is.vector(split_time)){
      stop("split_time must be NULL or a numeric vector")
    }
    minspl <- min(split_time)
    if (minspl != 0){
      if (minspl < 0){
        stop(paste0("min(split_time) = ", minspl, " but must not be < 0" ))
      }
      if (minspl > 0){
        warning(paste0(
          "min(split_time) = ", minspl,
          ". Appending 0 to split_time so first split is [0, ", minspl, "]."
        ))
      }
    }
    maxspl <- max(split_time)
    maxdays <- max(SSdat$days)
    if (maxspl != maxdays){
      if (maxspl > maxdays){
        stop("max(split_time) = ", maxspl,
          " but must not be > max(SSdat$days) = ", maxdays)
      }
      if (maxspl > maxdays){
        warning(paste0(
          "max(split_time) = ", maxspl, " < max(SSdat$days) = ", maxdays,
          ". Appending max(days) to split_time so last split is [",
          maxspl, ", ", max(SS$days),"]."
        ))
      }
    }
    if (sum(diff(split_time) <= 0) > 0){
      stop("split_time must be strictly increasing")
    }
    # split_time has passed the initial error-checking
    split_h <- list()
    split_h[["name"]] <- "time"
    split_h[["vals"]] <- split_time
    if (minspl > min(SSdat$days)) split_h$vals <- c(0, split_h$vals)
    if (maxspl < maxdays) split_h$vals <- c(split_h$vals, maxdays)
    split_h$vals[length(split_h$vals)] <- SSdat$days[length(SSdat$days)]
    split_h[["level"]] <- unique(split_h$vals[-1])
    split_h[["nlev"]] <- length(split_h$level) - 1
    split_h[["type"]] <- "time"
  }
  if (!is.null(split_SS)){ # there is a split_SS variable
    if (!is.null(split_h)){
      stop("Only one temporal split allowed. ",
        "Either split_SS or split_time must be NULL")
    }
    if (!is.character(split_SS)){
      stop("split_SS must be NULL or the name of an element in SSdat")
    }
    if (length(split_SS) > 1){
      stop("At most 1 split_SS variable is allowed")
    }
    if (!(split_SS %in% names(SSdat))){
      stop(split_SS, " not found in ", SSdat)
    }
    split_h <- list()
    split_h[["name"]] <- split_SS
    tmp <- cumsum(table(SSdat[[split_h$name]])[order(unique(SSdat[[split_h$name]]))])
    split_h[["vals"]] <- c(0, days[tmp + 1])
    split_h[["level"]] <- unique(SSdat[[split_h$name]])
    split_h[["nlev"]] <- length(split_h$level)
    split_h[["type"]] <- "SS"
  }
  if (!is.null(split_CO)){
    if (length(split_CO) > 2 | (length(split_CO) > 1 & !is.null(split_h))){
      stop("At most two split variables are allowed in total, i.e. ",
        "length(split_CO) + length(split_SS) + length(split_time) must be <= 2"
      )
    }
    if (!is.character(split_CO)){
      stop("split_CO must be a name of a selected column in COdat")
    }
    if (sum(split_CO %in% names(COdat)) != length(split_CO)){
      stop(paste(split_CO[which(!(split_CO %in% names(COdat)))], "not in SSdat"))
    }
    if (length(split_CO) == 2 | !is.null(split_h)){
      split_v <- list()
      split_v[["name"]] <- ifelse(!is.null(split_h), split_CO[1], split_CO[2])
      split_v[["vals"]] <- COdat[[split_v$name]]
      split_v[["level"]] <- unique(split_v$vals)
      split_v[["nlev"]] <- length(split_v$level)
      split_v[["type"]] <- "CO"
    }
    if (is.null(split_h)){
      split_h <- list()
      split_h[["name"]] <- split_CO[1]
      split_h[["vals"]] <- COdat[[split_h$name]]
      split_h[["level"]] <- unique(split_h$vals)
      split_h[["nlev"]] <- length(split_h$level)
      split_h[["type"]] <- "CO"
    }
  }
  nvar <- sum(c(!is.null(split_h), !is.null(split_v)))

  # additional preprocessing
  x <- dim(COdat)[1] # total observed carcasses (assumes COdat is error-checked)
  if (is.vector(Mest)) Mest <- matrix(Mest, nrow = 1)
  nsim <- dim(Mest)[2] # number of simulation draws (columns in Mest)
  if (!is.null(split_h) && (split_h$type %in% c("SS", "time"))){
    days <- SSdat$days
    searches <- array(0, dim = c(x, length(days)))
    for (xi in 1:x){
      searches[xi, ] <- SSdat$searches[COdat$unit[xi], ]
    }
  }

  # calculate splits
  if (nvar == 0){ # no splits...just calculate total M
    splits <- colSums(Mest)
  } else if (nvar == 1){ # just one split variable: split_h
    if (split_h$type == "CO"){
      splits <- array(dim = c(split_h$nlev, nsim))
      for (li in 1:split_h$nlev) {
        lind <- which(COdat[, split_h$name] == split_h$level[li])
        if (length(lind) == 1){
          splits[li, ] <- Mest[lind, ]
        } else {
          splits[li, ] <- colSums(Mest[lind, ])
        }
      }
    } else if (split_h$type %in% c("time", "SS")){
      searches <- array(0, dim = c(x, length(SSdat$days)))
      for (xi in 1:x){
        searches[xi, ] <- SSdat$searches[COdat$unit[xi],]
      }
      rate <- calcRate(Mest, Aj, SSdat$days, searches)
      splits <- calcTsplit(rate, SSdat$days, c(split_h$vals))
    }
  } else if (nvar == 2){ # two split variable: split_h and split_v
    splits <- list()
    if (split_h$type == "CO"){
      for (vi in 1:split_v$nlev){
        splits[[vi]] <- array(0, dim = c(split_h$nlev, nsim))
        for (li in 1:split_h$nlev) {
          lind <- which(
            split_h$vals == split_h$level[li] &
            split_v$vals == split_v$level[vi]
          )
          if (length(lind) > 1){
            splits[[vi]][li, ] <- colSums(Mest[lind, ])
          } else if (length(lind) == 1){
            splits[[vi]][li, ] <- Mest[lind, ]
          }
        }
      }
    } else if (split_h$type %in% c("SS", "time")){
      for (vi in 1:split_v$nlev){
        lind <- which(split_v$vals == split_v$level[vi])
        rate <- calcRate(Mest[lind, ], Aj[lind, ], days, searches[lind,])
        splits[[vi]] <- calcTsplit(rate, days, split_h$vals)
      }
    }
  }
  splits <- sticky::sticky(splits)
  attr(splits, "vars") <- c(split_h$name, split_v$name)
  attr(splits, "type") <- c(split_h$type, split_v$type)
  if (split_h$type %in% c("time", "SS")) attr(splits, "times") <- split_h$vals
  if (nvar == 1){
    rownames(splits) <- split_h$level
  }
  if (nvar == 2){
    names(splits) <- split_v$level
    for (i in 1:length(splits)){
      rownames(splits[[i]]) <- split_h$level
    }
  }
  class(splits) <- "splitFull"
  return(splits)
}
#' @export
print.splitFull <- function(splits){
  print(unclass(splits))
}
#' @export
summary.splitFull <- function(splits, CI_level = 0.95){
  alpha <- 1 - CI_level
  probs <- c(alpha/2, 0.25, 0.5, 0.75, 1 - alpha/2)
  if (is.null(attr(splits, "vars"))){
    sumry <- c(mean = mean(splits), quantile(splits, probs = probs))
  } else if (length(attr(splits, "vars")) == 1){
    if (is.vector(splits)) splits <- matrix(splits, nrow = 1)
    sumry <- cbind(mean = rowMeans(splits),
      matrixStats::rowQuantiles(splits, p = probs))
  } else if (length(attr(splits, "vars")) == 2){
    if (is.vector(splits[[1]])){
      splits <- lapply(splits, function(x){
        matrix(x, nrow = 1)
      })
    }
    sumry <- lapply(splits, function(x){
      cbind(mean = rowMeans(x), matrixStats::rowQuantiles(x, p = probs))
    })
  } else {
    stop(paste("length(attr(splits, 'vars')) > 2. ",
               "At most two split variables are allowed."))
  }
  sumry <- sticky::sticky(sumry)
  attr(sumry, "CI_level") <- CI_level
  attr(sumry, "vars") <- attr(splits, "vars")
  attr(sumry, "type") <- attr(splits, "type")
  attr(sumry, "times") <- attr(splits, "times")
  class(sumry) <- "splitSummary"
  return(sumry)
}

#' @export
plot.splitFull <- function(splits, CI_level = 0.95, rate = FALSE, ...){
  plot(summary(splits, CI_level), rate)
}
#' @export
plot.splitSummary <- function(splits, rate = FALSE, ...){
  nvar <- length(attr(splits, "vars"))
  vartype <- attr(splits, "type")
  if (vartype[1] == "CO") rate <- FALSE
  vars <- attr(splits, "vars")
  alpha <- 1 - attr(splits, "CI_level")
  times <- attr(splits, "times")
  probs <- c(alpha/2, 0.25, 0.5, 0.75, 1 - alpha/2)
  deltaT <- diff(times)
  if (nvar == 0){
    # split is for total...what kind of figure? Need to show Mtilde & Mhat
  } else if (nvar == 1 & !is.list(splits)){
    splits <- list(splits)
    vnames <- NULL
  } else {
    vnames <- names(splits)
  }
  hnames <- rownames(splits[[1]])
  par(mar = c(0, 0, 0.5, 0.5), oma = c(6, 5.5, 4, 4))
  nlevel_h <- nrow(splits[[1]])
  nlevel_v <- length(splits)
  par(mfrow = c(nlevel_v, 1))
  cex.axis <- 1*(nlevel_v == 1) + (nlevel_v == 2)/0.83 + (nlevel_v > 2)/0.66
  for (vi in 1:nlevel_v){
    if ((vartype[1] %in% c("time", "SS")) & rate) {
      hwid <- deltaT/2
      xlim <- range(times)
      ylim <- range(
        matrixStats::rowQuantiles(splits[[vi]], probs = c(alpha/2, 1 - alpha/2))/deltaT
      )
    } else {
      hwid <- rep(0.45, nlevel_h) # half-width of boxes
      xlim <- c(1, nlevel_h) + hwid[1] * c(-1, 1)
      ylim <- range(matrixStats::rowQuantiles(splits[[vi]], probs = c(alpha/2, 1 - alpha/2)))
    }
    plot(0, xlim = xlim, ylim = ylim, type = 'n', axes = F, xlab = '', ylab = '')
    if (vartype[1] == "CO" | !rate) xx <- 1:nlevel_h else xx <- times[-1] - hwid
    for (hi in 1:nlevel_h){
      deno <- ifelse(vartype[1] == "CO" || !rate, 1, deltaT[hi])
      qtls <- quantile(splits[[vi]][hi, ], prob = probs)/deno
      polygon(xx[hi] + hwid[hi] * c(1, 1, -1, -1), qtls[c(2, 4, 4, 2)])
      lines(xx[hi] + hwid[hi] * c(1, -1), rep(qtls[3], 2), lwd = 3)
      if (alpha >= 0.5) yst <- c(3, 3) else yst <- c(2, 4)
      lines(rep(xx[hi], 2), qtls[c(1, yst[1])])
      lines(rep(xx[hi], 2), qtls[c(yst[2], 5)])
      lines(xx[hi] + hwid[hi]/2 * c(1, -1), rep(qtls[1], 2))
      lines(xx[hi] + hwid[hi]/2 * c(1, -1), rep(qtls[5], 2))
    }
    axis(2, las = 1)
    box()
    ymid <- mean(par('usr')[3:4])
    if (!rate | vartype[1] == "CO"){
      if (vartype[1] == "time"){
        at <- 0.5 + 0:nlevel_h
        lab <- c(0, rownames(splits[[vi]]))
      } else {
        at <- 1:nlevel_h
        lab <- rownames(splits[[vi]])
      }
    } else {
      at <- times
      lab <- times
    }
    if (vi == nlevel_v){
      axis(1, at = at, lab = lab, cex.axis = cex.axis)
    }
    if (nlevel_v > 1){
      axis(4, at = mean(par('usr')[c(3, 4)]), lab = vnames[vi],
        tck = 0, mgp = c(3, 0.5, 0), cex.axis = cex.axis)
    }
  }
  mtext(side = 1, vars[1], line = 4.6, cex = 1.2)
  if (vartype[1] == "SS" & rate) {
    mtext(side = 1, line = 3, text = hnames, at = times[-1] - diff(times)/2)
  }
  ylab <- ifelse(vartype[1] == "CO" | !rate,
    paste0("Estimated M"),
    paste0("Estimated M/day")
  )
  mtext(side = 2, text = ylab, outer = T, line = 3.5, cex = 1.2)

  title <- ifelse(vartype[1] == "CO" | !rate,
    paste0("Estimated mortality by ", vars[1]),
    paste0("Estimated daily mortality rate")
  )
  mtext(text = title, side = 3, line = 2, cex = 1.3, outer = T)
  mtext(side = 3, line = 0.5, cex = 0.9, outer = T,
    text = paste0("Median, IQR, and ", 100*(1 - alpha), "% confidence intervals"))
  mtext(side = 4, text = vars[2], outer = T, line = 2.5, cex = 1.2)
}
