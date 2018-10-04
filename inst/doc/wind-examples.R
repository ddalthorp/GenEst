## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE------------------------------------------------------
library(GenEst)
vers <- packageVersion("GenEst")
today <- Sys.Date()

## ------------------------------------------------------------------------
data(wind_RPbat)
names(wind_RPbat)

## ------------------------------------------------------------------------
data_SE <- wind_RPbat$SE
data_CP <- wind_RPbat$CP
data_SS <- wind_RPbat$SS
data_DWP <- wind_RPbat$DWP
data_CO <- wind_RPbat$CO

## ---- include = FALSE----------------------------------------------------
daterange <- range(data_SS$SearchDate)
seasons <- paste(unique(data_SS$Season), collapse = ', ')

## ------------------------------------------------------------------------
head(data_SE)

## ------------------------------------------------------------------------
head(data_CP)

## ------------------------------------------------------------------------
head(data_SS[, 1:10])

## ------------------------------------------------------------------------
head(data_DWP)

## ------------------------------------------------------------------------
head(data_CO)

## ---- eval = FALSE-------------------------------------------------------
#  ?pkm

## ------------------------------------------------------------------------
model_SE <- pkm(p ~ Season, k ~ 1, data = data_SE)
model_SE

## ---- eval = FALSE-------------------------------------------------------
#  ?cpm

## ------------------------------------------------------------------------
model_CP <- cpm(l ~ Season, s ~ Season, data = data_CP, dist = "weibull",
  left = "LastPresent", right = "FirstAbsent")
model_CP

## ---- eval = FALSE-------------------------------------------------------
#  ?estM

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'----------------
Mhat <- estM(nsim = 1000, data_CO = data_CO, data_SS = data_SS,
  data_DWP = data_DWP, model_SE = model_SE, model_CP = model_CP,
  unitCol = "Turbine", dateFoundCol = "DateFound")

summary(Mhat)
plot(Mhat)

## ---- eval = FALSE-------------------------------------------------------
#  ?calcSplits

## ---- fig.width = 5, fig.height = 5, fig.align = 'center'----------------
M_species <- calcSplits(M = Mhat$Mhat, Aj = Mhat$Aj, split_CO = "Species",
  data_CO = data_CO)
summary(M_species)
plot(M_species)

## ------------------------------------------------------------------------
SSdat <- prepSS(data_SS)

## ----Season Split, fig.width = 4, fig.height = 4, fig.align = 'center'----
M_season <- calcSplits(M = Mhat$Mhat, Aj = Mhat$Aj,
  split_SS = "Season", data_SS = SSdat, split_CO = NULL,  data_CO = data_CO)
summary(M_season)
plot(M_season)

## ----Temporal Split, fig.width = 7, fig.height = 5, fig.align = 'center'----
M_month <- calcSplits(M = Mhat$Mhat, Aj = Mhat$Aj,
  split_time = seq(0, max(SSdat$days), by = 28),
  data_SS = SSdat, data_CO = data_CO)
summary(M_month)
plot(M_month)

## ----Time unit Split, fig.width = 7, fig.height = 5, fig.align = 'center'----
M_various_times <- calcSplits(M = Mhat$Mhat, Aj = Mhat$Aj,
  split_time = c(seq(0, 90, by = 15), 120, 150, seq(155, 200, by = 5)),
  data_SS = SSdat, data_CO = data_CO)
plot(M_various_times)
plot(M_various_times, rate = TRUE)

## ----Species and Season, fig.width = 4, fig.height = 6, fig.align = 'center'----
M_species_by_season <- calcSplits(M = Mhat$Mhat, Aj = Mhat$Aj,
  split_CO = "Species", data_CO = data_CO,
  split_SS = "Season", data_SS = SSdat)
plot(M_species_by_season)

## ------------------------------------------------------------------------
data(wind_cleared)
names(wind_cleared)

## ------------------------------------------------------------------------
data_SE <- wind_cleared$SE
data_CP <- wind_cleared$CP
data_SS <- wind_cleared$SS
data_DWP <- wind_cleared$DWP
data_CO <- wind_cleared$CO

## ------------------------------------------------------------------------
head(data_SE)
head(data_CP)

## ---- eval = FALSE-------------------------------------------------------
#  ?pkm
#  ?pkmSet

## ------------------------------------------------------------------------
SEsml <- data_SE[data_SE$Size == "sml", ]
pkSet_sml <- pkmSet(p ~ Visibility * Season, k ~ Visibility * Season,
  data = SEsml)
names(pkSet_sml)

## ------------------------------------------------------------------------
pkmSetAICcTab(pkSet_sml)

## ---- eval = F, fig.show = "hold", fig.width = 7, fig.height = 7---------
#  plot(pkSet_sml, specificModel = "p ~ Visibility; k ~ 1")

## ---- eval = F, fig.show = "hold", fig.width = 7, fig.height = 7---------
#  plot(pkSet_sml, specificModel = "p ~ Visibility; k ~ 1")

## ---- eval = F, fig.show = "hold", fig.width = 7, fig.height = 7---------
#  plot(pkSet_sml, specificModel = "p ~ Season; k ~ Season")

## ------------------------------------------------------------------------
pk_med <- pkm(p ~ Visibility, k ~ 1, data = data_SE[data_SE$Size == "med", ])
pk_lrg <- pkm(p ~ Visibility, k ~ 1, data = data_SE[data_SE$Size == "lrg", ])
pk_bat <- pkm(p ~ Visibility, k ~ 1, data = data_SE[data_SE$Size == "bat", ])

pkMods <- list(
  sml = pkSet_sml[["p ~ Visibility; k ~ 1"]],
  med = pk_med,
  lrg = pk_lrg,
  bat = pk_bat
)

## ---- eval = F-----------------------------------------------------------
#  ? pkmSetSize
#  vignette("command-line-example", package = "GenEst")

## ---- eval = F-----------------------------------------------------------
#  ? cpm
#  ? cpmSet

## ------------------------------------------------------------------------
cpSet_sml <- cpmSet(
  l ~ Visibility * Season, s ~ Visibility * Season,
  data = data_CP[data_CP$Size == "sml", ], left = "LastPresent", right = "FirstAbsent",
  dists = c( "weibull", "lognormal", "loglogistic", "exponential")
)

## ------------------------------------------------------------------------
cpmSetAICcTab(cpSet_sml)

## ------------------------------------------------------------------------
cp_smlCandidates <- names(cpSet_sml)[c(24, 19, 14, 9, 25, 17, 20, 22, 15, 44)]
cp_smlCandidates

## ---- eval = F-----------------------------------------------------------
#  plot(cpSet_sml, specificModel = cp_smlCandidates)

## ------------------------------------------------------------------------
cp_sml <- cpSet_sml[[cp_smlCandidates[1]]]

## ------------------------------------------------------------------------
cp_med <- cpm(l ~ Visibility, s ~ Season,
  data = data_CP[data_CP$Size == "med", ], left = "LastPresent", right = "FirstAbsent",
  dist = "weibull")
cp_lrg <- cpm(l ~ Visibility + Season, data = data_CP[data_CP$Size == "lrg", ],
  left = "LastPresent", right = "FirstAbsent", dist = "exponential")
cp_bat <- cpm(l ~ Visibility + Season, s ~ 1,
  data = data_CP[data_CP$Size == "med", ], left = "LastPresent", right = "FirstAbsent",
  dist = "weibull")

## ------------------------------------------------------------------------
cpMods <- list(
  sml = cp_sml,
  med = cp_med,
  lrg = cp_lrg,
  bat = cp_bat
)

## ----Mhat plot, fig.height = 4, fig.width = 7, fig.align = 'center'------
Mhat <- estM(nsim = 1000, data_CO = data_CO, data_SS = data_SS, frac = 0.23,
  data_DWP = data_DWP, model_SE = pkMods, model_CP = cpMods,
  sizeclassCol = "Size", unitCol = "Turbine", dateFoundCol = "DateFound")

summary(Mhat)
plot(Mhat)

## ----Species Group Plot, , fig.height = 5, fig.width = 5, fig.align = 'center'----
M_speciesGroup <- calcSplits(M = Mhat$Mhat, Aj = Mhat$Aj,
  split_CO = "SpeciesGroup",  data_CO = data_CO)
summary(M_speciesGroup)
plot(M_speciesGroup)

## ----Split Species and Season, fig.height = 12, fig.width = 4, fig.align = 'center'----
M_speciesseason <- calcSplits(M = Mhat$Mhat, Aj = Mhat$Aj,
  split_CO = "Species",  data_CO = data_CO, split_SS = "Season", data_SS = data_SS)
summary(M_speciesseason)
plot(M_speciesseason)

## ----Transposed Species Season, fig.height = 7, fig.width = 7, fig.align = 'center'----
plot(transposeSplits(M_speciesseason))

## ---- fig.width = 4, fig.height = 6, fig.align = 'center'----------------
M_distance <- calcSplits(M = Mhat$Mhat, Aj = Mhat$Aj,
  split_CO = c("Distance", "Size"),  data_CO = data_CO)
plot(M_distance)

