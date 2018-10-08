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
data(solar_PV)
names(solar_PV)

## ----pk data-------------------------------------------------------------
data_SE <- solar_PV$SE
head(data_SE)

## ----pk one model--------------------------------------------------------
SE_model <- pkm(p ~ 1, k ~ 1, data = data_SE)
SE_model


## ----pk two models-------------------------------------------------------
SE_model_set <- pkm(p~Season, k~1, data = data_SE, allCombos = TRUE)
class(SE_model_set)
length(SE_model_set)
names(SE_model_set)
class(SE_model_set[[1]])

## ----pk set AICc---------------------------------------------------------
aicc(SE_model_set)

## ----pk size set---------------------------------------------------------
SE_size_model <- pkm(p ~ Season,
                     k ~ 1,
                     sizeCol = "Size",
                     data = data_SE)
class(SE_size_model)
names(SE_size_model)  # A list is created with a model set per size class.
class(SE_size_model$small)
names(SE_size_model$small) # Each model set contains one model in this case.


## ------------------------------------------------------------------------
SE_size_model_set <- pkm(p ~ Season,
                     k ~ 1,
                     sizeCol = "Size",
                     data = data_SE, allCombos = TRUE)
aicc(SE_size_model_set)
SE_models <- list()

## ------------------------------------------------------------------------
SE_models$small <- SE_size_model_set$small[[2]]

## ----pk size Medium------------------------------------------------------
SE_models$med <- SE_size_model_set$med[[2]]

## ----pk Size Large-------------------------------------------------------
SE_models$lrg <- SE_size_model_set$lrg[[1]]

## ----cp data-------------------------------------------------------------
data_CP <- solar_PV$CP
head(data_CP)

## ----cp------------------------------------------------------------------
cpm(l ~ Season, s ~ 1, data = data_CP,
                left = "LastPresent",
                right = "FirstAbsent",
                dist = "weibull")

## ----cp set--------------------------------------------------------------
  CP_weibull_set <- cpm(l ~ Season, s ~ 1, data = data_CP,
                  left = "LastPresent",
                  right = "FirstAbsent",
                  dist = "weibull", allCombos = TRUE)
class(CP_weibull_set)
aicc(CP_weibull_set)

## ----cp Size Set---------------------------------------------------------
CP_size_model_set <- cpm(formula_l = l ~ Season,
                           formula_s = s ~ 1, 
                           left = "LastPresent",
                           right = "FirstAbsent",
                           dist = c("exponential", "weibull"),
                           sizeCol = "Size",
                           data = data_CP, allCombos = T)
class(CP_size_model_set)
names(CP_size_model_set)
class(CP_size_model_set$small)
length(CP_size_model_set$small)
names(CP_size_model_set$small)

## ----cp Size Small-------------------------------------------------------
aicc(CP_size_model_set)
CP_models <- list()

## ----cp size Medium------------------------------------------------------
CP_models$med <- CP_size_model_set$med[[4]]

## ----Size Large----------------------------------------------------------
CP_models$lrg <- CP_size_model_set$lrg[[2]]

## ----Load CO SS and DWP--------------------------------------------------
data_CO <- solar_PV$CO
head(data_CO)

## ----SS Data-------------------------------------------------------------
data_SS <- solar_PV$SS
data_SS[1:5 , 1:10]

## ----DWP data------------------------------------------------------------
data_DWP <- solar_PV$DWP
head(data_DWP)

