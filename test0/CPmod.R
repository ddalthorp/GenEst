# CPmod -- function to model carcass persistence for a number of covariates
# Arguments
#   data frame with columns CPmin, CPmax, covar1, covar2 [covariates needn't be provided]
### preliminary:
#   formatting the data from the given .csv...
#   just need a dataframe or list with columns for CPmin, CPmax, and 0-2 covariates
#   the little function is expecting
CPdatFull <- read.csv('BatCP.csv', head = T, as.is = T)
CP <- CPdatFull[(CPdatFull$Species %in% c("LABO", "LANO", "PESU")), c("Species", "Visibility", "Left", "Right", "Event")]
names(CP) <- c('spec', 'vis', 'CPmin', 'CPmax', 'Event')
CP$vis[CP$vis == 'Difficult'] <- 'D'
CP$vis[CP$vis == 'Moderate'] <- 'M'
CP$vis[CP$vis == 'Easy'] <- 'E'
CP$CPmax[CP$Event == 0] <- Inf
CP$CPmin[CP$Event == 2] <- 0
CP <- CP[, c(3, 4, 1, 2)]


# NOTE: 'survival' package uses different parameterizations for the Weibull, lognormal, and
# exponential than does the R base package and uses a different parameterization from the
# package 'actuar' for modeling loglogistic. Translation guide (where 'mod' is a survival model
# as specified in survreg output => R parameter => pda or pdb in EoA):
# exponential: exp(mod$coef) = 1/rate = pdb
# Weibull: 1/mod$scale = shape = pda, exp(mod$coef) = scale = pdb
# lognormal: sdlog = mod$scale = sqrt(pda), mod$coef = meanlog = pdb
# loglogistic: 1/mod$scale = shape = pda, exp(mod$coef) = scale = pdb
# NOTE: base R does not include loglogistic; shape & scale here are for package 'actuar'
# NOTE: Wikipedia parameterization for loglogistic is identical to that of 'actuar' BUT with
#   the names of the parameters reversed!

# create 'survival' data from CPmin (last seen) and CPmax (first gone; = Inf if not scavenged):
event<-ifelse(CP$CPmin == CP$CPmax, 1, ifelse(CP$CPmin == 0, 2, ifelse(CP$CPmax == Inf, 0, 3)))
left<-CP$CPmin
left[event==2]<-CP$CPmax[event==2]
right<-CP$CPmax
right[event==0]<-CP$CPmin[event==0]
surv<- survival::Surv(time=left, time2=right, event=event, type=c('interval'))

covars <- names(CP)[-(1:2)]
modNames <- "NULL"
if (length(covars) > 0)
  modNames <- c(modNames, covars)
if (length(covars) > 1)
  modNames <- c(modNames, paste(covars[1], "+", covars[2]), paste(covars[1], "*", covars[2]))

nmod <- length(modNames)
modelSumry <- data.frame(array(dim = c(nmod * 2, 5)))
names(modelSumry) <- c("Family", "Model", "edf", "AIC", "deltaAIC")
modelSumry[1, 1:2] <- c("exponential", modNames[1])
modelSumry[1, 3:4] <- extractAIC(survival::survreg(surv ~ 1, dist = "exponential"))
modelSumry[2, 1:2] <- c("weibull", modNames[1])
modelSumry[2, 3:4] <- extractAIC(survival::survreg(surv ~ 1, dist = "weibull"))
if (length(covars) > 0){
  modelSumry[3, 1:2] <- c("exponential", modNames[2])
  modelSumry[3, 3:4] <- extractAIC(
    survival::survreg(surv ~ get(covars[1]), dist = "exponential", data = CP)
  )
  modelSumry[4, 1:2] <- c("weibull", modNames[2])
  modelSumry[4, 3:4] <- extractAIC(
    survival::survreg(surv ~ get(covars[1]), dist = "weibull", data = CP)
  )
}
if (length(covars) > 1){
  modelSumry[5, 1:2] <- c("exponential", modNames[3])
  modelSumry[5, 3:4] <- extractAIC(
    survival::survreg(surv ~ get(covars[2]), dist = "exponential", data = CP)
  )
  modelSumry[6, 1:2] <- c("weibull", modNames[3])
  modelSumry[6, 3:4] <- extractAIC(
    survival::survreg(surv ~ get(covars[2]), dist = "weibull", data = CP)
  )
  modelSumry[7, 1:2] <- c("exponential", modNames[4])
  modelSumry[7, 3:4] <- extractAIC(
      survival::survreg(surv ~ get(covars[1]) + get(covars[2]), dist = "exponential", data = CP)
    )
  modelSumry[8, 1:2] <- c("weibull", modNames[4])
  modelSumry[8, 3:4] <- extractAIC(
      survival::survreg(surv ~ get(covars[1]) + get(covars[2]), dist = "weibull", data = CP)
    )
  modelSumry[9, 1:2] <- c("exponential", modNames[5])
  modelSumry[9, 3:4] <-  extractAIC(
      survival::survreg(surv ~ get(covars[1]) * get(covars[2]), dist = "exponential", data = CP)
    )
  modelSumry[10, 1:2] <- c("weibull", modNames[5])
  modelSumry[10, 3:4] <- extractAIC(
      survival::survreg(surv ~ get(covars[1]) * get(covars[2]), dist = "weibull", data = CP)
    )
}
modelSumry <- modelSumry[order(modelSumry$AIC), ]
modelSumry$deltaAIC <- modelSumry$AIC - min(modelSumry$AIC)

# function from EoA for reading CP data from a file and doing error-checking
# this will need modification to deal with covariates and other, more complicated data sets
getCPdata<-function(){
  fileName<-tclvalue(tkgetOpenFile(initialdir=.Rvar$csvpath))
  if (!nchar(fileName)) {
    return (FALSE)
  }
  tmp <- unlist(strsplit(fileName,'/'))
  .Rvar$csvpath <- paste(tmp[-length(tmp)], collapse='/')
  # stores the name of the directory for default save/open directory next time
  CP<-try(read.csv(fileName, sep=",", as.is = T))
  # CP data file has two columns: CPmin and CPmax, which bracket the time when the carcass disappeared
  # NOTE: This assumes just two columns (and no covariates)
  #   if carcass disappears before first check, CPmin = 0
  #   if carcass scavenging event was observed, CPmin = CPmax = time since carcass placement
  #   if carcass does not disappear before end of study, then
  #     CPmin = time of last check, CPmax = Inf (not inf or "Inf" or Infinity or 100000 or ...)
  if (class(CP) != "try-error") {
    CP<-na.omit(CP)
  } else {
    tkmessageBox(icon = 'error',
      message=paste0("Error in data (", fileName,
      "\nRequired: two columns with data for CPmin and CPmax.\nCheck file."))
      return(F)
  }
  if (dim(CP)[2] < 2){
    tkmessageBox(icon='error', message = paste0("Error in data (", fileName,
    "\nRequired: two columns with data for CPmin and CPmax.\nCheck file."))
    return(F)
  } else {
    CP<-CP[,1:2]
    names(CP)<-c("CPmin", "CPmax")
  }
  if (!is.numeric(CP[,1]) || !is.numeric(CP[,2]) || sum(CP[,1] < 0) > 0 || sum(CP[,1] > CP[,2]) > 0) {
    tkmessageBox(message = "Error in CP data. Cannot calculate.")
    return(F)
  }
  xind <- which(CP$CPmin == 0 & CP$CPmax == Inf)
  if (length(xind) > 0){
    CP$CPmin <- CP$CPmin[-xind]
    CP$CPmax <- CP$CPmax[-xind]
  }
  event <- ifelse(CP$CPmin == CP$CPmax, 1, ifelse(CP$CPmin == 0, 2, ifelse(CP$CPmax == Inf, 0, 3)))
  left <- CP$CPmin
  left[event == 2] <- CP$CPmax[event == 2]
  right <- CP$CPmax
  right[event == 0] <- CP$CPmin[event == 0]
  # if no carcasses are scavenged during the field trials, it is still possible to model a
  # persistence distribution, but it takes extra care:
  if (sum(CP$CPmax == Inf) == length(CP$CPmax)){
    n <- length(CP$CPmax)
    trial.period <- mean(CP$CPmin)
    deno <- gsl::hyperg_2F1(1/2, 1/2 - n, 3/2, 1)
    rbnd <- suppressWarnings(c(
      uniroot(function(p) gsl::hyperg_2F1(1/2, 1/2 - n, 3/2, 1-p)*sqrt(1-p)/deno - (1-pexp(1,1)), interval=c(0,1))$root,
      uniroot(function(p) gsl::hyperg_2F1(1/2, 1/2 - n, 3/2, 1-p)*sqrt(1-p)/deno - 0.975, interval=c(0,1))$root,
      uniroot(function(p) gsl::hyperg_2F1(1/2, 1/2 - n, 3/2, 1-p)*sqrt(1-p)/deno - 0.025, interval=c(0,1))$root))
    sclbnd <<- signif(trial.period/-log(rbnd), 3)
    # NOTE: the "<<-" assignment operator makes sense w/in an R6 class, but not here
    tkmessageBox(message=paste0("No carcasses removed in persistence trials. Use exponential persistence distribution and enter parameters manually.\n",
      "scale = ", signif(sclbnd[1], 3),
      "\nlwr = ", signif(sclbnd[2], 3),
      "\nupr = ", signif(sclbnd[3], 3)))
    return (F)
  }
}



# here's a reasonably nice-looking graph (for one class only):
expoColor<-colors()[370]
weibullColor<-colors()[148]
par(mar=c(10, 4, 2, 1), mgp=c(2,.7,0), tck = -.015, family = 'sans')
plot(survival::survfit(surv~1,data=CP),xlab='Days', ylab = 'Fraction of carcasses persisting', xaxs='i', yaxs='i')
xx<-seq(par('usr')[1],par('usr')[2],length=1000)

# a generic model:
mod.e <- survival::survreg(surv ~ 1, dist = "exponential")
mod.w <- survival::survreg(surv ~ 1, dist = "weibull")
lines(xx,1-pexp(xx, rate = 1/exp(mod.e$coef)),col = expoColor, lwd = 2)
lines(xx,1-pweibull(xx, 1/mod.w$scale, exp(mod.w$coef[1])), col = weibullColor, lwd = 2)
# NOTE: 'survival' package has its own, unique parameterizations of persistence distributions
# exponential: exp(survival "coef") = 1/rate in dexp = b
# Weibull: exp(survival "coef") = scale in dweibull; 1/(survival "scale") = shape in dweibull
# in EoA, for Weibull, pdb = R's "scale" and pda = R's "shape"
box()
legend(x='topright', legend=c("Exponential", "Weibull"),
  lty = 1, lwd = 2, col = c(expoColor, weibullColor))
title("Persistence Distribution")
