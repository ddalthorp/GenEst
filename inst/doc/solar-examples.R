## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

rm(list = ls())

library(GenEst)
vers <- packageVersion("GenEst")
today <- Sys.Date()
set.seed(951)

## ------------------------------------------------------------------------
data(solar_PV) # (Note: fix pv to PV in folder structure.)
names(solar_PV)

## ----pk data-------------------------------------------------------------
SE_data <- solar_PV$SE
head(SE_data, 3)

## ----pk one model--------------------------------------------------------
SE_model <- pkm(p ~ 1, k ~ 1, data = SE_data)
  SE_model$AICc
  SE_model$cellwiseTable


## ----pk two models-------------------------------------------------------
SE_model_set <- pkmSet(p~Season, k~1, data = SE_data)
class(SE_model_set)
length(SE_model_set)
names(SE_model_set)
class(SE_model_set[[1]])

## ----pk set AICc---------------------------------------------------------
AIC(SE_model_set)

## ----pk size set---------------------------------------------------------
SE_size_model_set <- pkmSetSize(p ~ Season, 
                           k ~ 1, 
                           sizeclassCol = "Size", 
                           data = SE_data)
class(SE_size_model_set)
names(SE_size_model_set)  # A list is created with a model set per size class.
class(SE_size_model_set$small)
names(SE_size_model_set$small) # Each model set contains one model in this case.



## ----pk size Small-------------------------------------------------------
AIC(SE_size_model_set$small)
SE_models <- list(small = SE_size_model_set$med[[2]])

## ----pk size Medium------------------------------------------------------
AIC(SE_size_model_set$med)
SE_models$med <- SE_size_model_set$med[[2]]

## ----pk Size Large-------------------------------------------------------
AIC(SE_size_model_set$lrg)
SE_models$lrg <- SE_size_model_set$lrg[[1]]

## ----cp data-------------------------------------------------------------
CP_data <- solar_PV$CP
head(CP_data, 3)

## ----cp------------------------------------------------------------------
CP_model <- cpm(l ~ Season, s ~ 1, data = CP_data, 
                left = "LastPresent",
                right = "FirstAbsent",
                dist = "weibull")

## ----cp set--------------------------------------------------------------
  CP_weibull_set <- cpmSet(l ~ Season, s ~ 1, data = CP_data, 
                  left = "LastPresent",
                  right = "FirstAbsent",
                  dist = "weibull")
class(CP_weibull_set)
length(CP_weibull_set)
names(CP_weibull_set)

## ----cp Set Table--------------------------------------------------------
  AIC(CP_weibull_set)


## ----cp Size Set---------------------------------------------------------
CP_size_model_set <- cpmSetSize(formula_l = l ~ Season, 
                           formula_s = s ~ 1, 
                           left = "LastPresent",
                           right = "FirstAbsent",
                           dists = c("exponential", "weibull"),
                           sizeclassCol = "Size", 
                           data = CP_data)
class(CP_size_model_set)
length(CP_size_model_set)
names(CP_size_model_set)
class(CP_size_model_set$small)
length(CP_size_model_set$small)
names(CP_size_model_set$small)

## ----cp Size Small-------------------------------------------------------
AIC(CP_size_model_set$small)
CP_models <- list(small = CP_size_model_set$med[[3]])

## ----cp size Medium------------------------------------------------------
AIC(CP_size_model_set$med)
CP_models$med <- CP_size_model_set$med[[4]]

## ----Size Large----------------------------------------------------------
AIC(CP_size_model_set$lrg)
CP_models$lrg <- CP_size_model_set$lrg[[2]]

## ----Load CO SS and DWP--------------------------------------------------
CO_data <- solar_PV$CO
head(CO_data, 3)

## ----SS Data-------------------------------------------------------------
SS_data <- solar_PV$SS
SS_data[1:3 , 1:10]

## ----DWP data------------------------------------------------------------
DWP_data <- solar_PV$DWP
head(DWP_data, 3)
names(DWP_data) <- c("Unit", "small", "med", "lrg")

## ----Arrival Times, options----------------------------------------------
  Mest <- estM(
    nsim = 100, frac = 1, 
    data_CO = CO_data, data_SS = SS_data, data_DWP = DWP_data, 
    model_SE = SE_models, model_CP = CP_models,
    unitCol = "Unit", sizeclassCol = "Size",
    dateFoundCol = "DateFound", datesSearchedCol = "DateSearched"
  )

## ---- fig.show = "hold", fig.height = 4, fig.width = 6, fig.align = 'center'----
  plot(Mest)

## ----Summary - Season----------------------------------------------------
SS_data[1, 2] <- "winter"

M_season <- calcSplits(
  M = Mest$Mhat, Aj = Mest$Aj, 
  split_SS = "Season", data_SS = SS_data,
  split_CO = NULL,  data_CO = CO_data
)

## ----splitFull plot, fig.height = 4, fig.width = 4, fig.align = 'center'----
plot(M_season)

## ----SplitFull Summary---------------------------------------------------
  summary(M_season, CL = 0.95)

## ----Summary - Weekly----------------------------------------------------

SSdat <- prepSS(SS_data) # Creates an object of type prepSS.
schedule <- seq(from = 0, to = max(SSdat$days), by = 7)
tail(schedule)

## ----Summary - Weekly Part 2, fig.height = 4, fig.width = 7, fig.align = 'center'----
M_week <- calcSplits(
  M = Mest$M, Aj = Mest$Aj,
  split_time = schedule, 
  data_SS = SSdat,
  data_CO = CO_data
)
plot(x = M_week, rate = T)


## ----Summary - Unit, fig.height = 4, fig.width = 7, fig.align = 'center'----

M_unit <- calcSplits(
  M = Mest$M, Aj = Mest$Aj,
  split_CO = "Unit", 
  data_CO = CO_data, 
  data_SS = SS_data
)
plot(M_unit, rate = F)

## ----individual unit summary---------------------------------------------
dim(summary(M_unit))  # only 163 arrays had observations.

# A list of the arrays without observations:
which(!c(1:300) %in% sub(x = CO_data$Unit,
                         pattern = "Unit([0-9]+)",
                         replacement = "\\1"))

# Only create summaries for Arrays Unit8 and Unit100.
whichRow <- rownames(summary(M_unit))  %in% c("Unit12", "Unit100")
summary(M_unit)[whichRow, ]


## ----Summary - season and species, fig.height = 5, fig.width = 3, fig.align = 'center'----

M_unit_and_species <- calcSplits(
  M = Mest$M, Aj = Mest$Aj,
  split_SS = c("Season"),
  split_CO = c("Size"),
  data_CO = CO_data,
  data_SS = SS_data
)
plot(M_unit_and_species, rate = F)


