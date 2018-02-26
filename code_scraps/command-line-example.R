## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE----------------------------------------------------
library(devtools)
library(matrixStats)
library(survival)
library(mvtnorm)
library(matrixStats)
library(gsl)
library(GenEst)

## ---- results = "hide"---------------------------------------------------
SEdataIn <- read.csv("data/ExampleSearcherEfficiency.csv")
CPdataIn <- read.csv("data/ExampleCarcassPersistence.csv")
SSdataIn <- read.csv("data/ExampleSearchSchedule.csv")
COdataIn <- read.csv("data/ExampleCarcassObservations.csv") 

## ---- results = "hide"---------------------------------------------------
Niterations <- 1000

## ---- results = "hide"---------------------------------------------------
SEvars <- c("Season", "HabitatType")
SEobscols <- 8:11
SEsizeclasscol <- "Size"

## ---- results = "hide"---------------------------------------------------
fixKchoice <- FALSE
fixKvalchoice <- NULL

## ---- results = "hide"---------------------------------------------------
SEmods <- se_model_set_across_sizes_fit(data = SEdataIn, predictors = SEvars, 
                                        observation_columns = SEobscols, 
                                        size_class_column = SEsizeclasscol,
                                        init_k_value = 0.7, fix_k = fixKchoice, 
                                        fix_k_value = fixKvalchoice)

## ---- results = "hide"---------------------------------------------------
thetaSE <- se_theta_create(data = SEdataIn, predictors = SEvars,
                           size_class_column = SEsizeclasscol,
                           model_fits = SEmods, replicates = Niterations,
                           fix_k = fixKchoice, fix_k_value = fixKvalchoice)

## ---- result = "hide"----------------------------------------------------
SEtable <- se_aicc_table_create(models = SEmods)

## ---- result = "asis"----------------------------------------------------
knitr::kable(SEtable$S)

## ---- fig.width = 8, fig.height = 8--------------------------------------
create_se_figure(data = SEdataIn, predictors = SEvars, theta = thetaSE, 
                 observation_columns = SEobscols, replicates = Niterations, 
                 size_class_column = SEsizeclasscol, r = 4, j = 1, 
				 cellwise = 25)

## ---- results = "hide"---------------------------------------------------
CPvars <- NULL
CPsizeclasscol <- "Size"
CPltp <- "LastPresentDecimalDays"
CPfta <- "FirstAbsentDecimalDays"

## ---- results = "hide"---------------------------------------------------
CPmods <- cp_model_set_across_sizes_fit(data = CPdataIn, predictors = CPvars,
                                        size_class_column = CPsizeclasscol, 
                                        last_time_present_column = CPltp, 
                                        first_time_absent_column = CPfta)

## ---- results = "hide"---------------------------------------------------
thetaCP <- cp_theta_create(data = CPdataIn, predictors = CPvars,
                           size_class_column = CPsizeclasscol,
                           model_fits = CPmods, replicates = Niterations)

## ---- result = "hide"----------------------------------------------------
CPtable <- cp_aicc_table_create(models = CPmods)

## ---- result = "asis"----------------------------------------------------
knitr::kable(CPtable$S)

## ---- fig.width = 8, fig.height = 8--------------------------------------
create_cp_figure(models = CPmods, data = CPdataIn, predictors = CPvars,
                 theta = thetaCP, time_unit = "days",
                 size_class_column = CPsizeclasscol,
                 last_time_present_column = CPltp, 
                 first_time_absent_column = CPfta, r = 1, 
                 model_complexity = "~ 1", 
                 distribution_choice = "exponential")

## ---- results = "hide"---------------------------------------------------
CPmodstouse <- c(1, 1, 1, 1)
SEmodstouse <- c(1, 1, 1, 1)

## ---- results = "hide"---------------------------------------------------
garray <- estimate_g_across_sizes(cp_data = CPdataIn, se_data = SEdataIn, 
                                  ss_data = SSdataIn, replicates = Niterations, 
                                  cp_predictors = CPvars, cp_theta = thetaCP,
								  se_predictors = SEvars, se_theta = thetaSE,  
								  cp_models = CPmods,
                                  se_models_to_use = SEmodstouse, 
                                  cp_models_to_use = CPmodstouse)

## ---- results = "hide"---------------------------------------------------
Mhatarray <- estimate_mhat(co_data = COdataIn, ss_data = SSdataIn, 
                           size_class_column = "Size", split_column = "Split", 
                           unit_column = "Unit", df_column = "DateFound",
                           replicates = Niterations, cp_predictors = CPvars, 
                           se_predictors = SEvars, cp_data = CPdataIn, 
                           se_data = SEdataIn, garray = garray) 

## ---- results = "hide"---------------------------------------------------
Mhatsc <- condense_mhat(Mhatarray)

## ---- results = "hide"---------------------------------------------------
Mhattab <- create_mhat_table(condensed_mhat = Mhatsc, 
                             fraction_area_sampled = 0.85, 
                             confidence_level = 0.9)

## ---- result = "asis"----------------------------------------------------
knitr::kable(Mhattab)

## ---- fig.width = 8, fig.height = 8--------------------------------------
par(mfrow = c(1, 2))
l <- 1
create_mhat_figure(condensed_mhat_split = Mhatsc[,l], 
                   split_category_name = colnames(Mhatsc)[l], 
                   fraction_area_sampled = 0.85)
l <- 2
create_mhat_figure(condensed_mhat_split = Mhatsc[,l], 
                   split_category_name = colnames(Mhatsc)[l], 
                   fraction_area_sampled = 0.85)

