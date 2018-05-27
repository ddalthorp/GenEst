devtools::load_all()
data(mock)

data_SE <- mock$SE
data_CP <- mock$CP
data_CO <- mock$CO
data_SS <- mock$SS
data_DWP <- mock$DWP

model_SE <- pkm(formula_p = p ~ Visibility , 
             formula_k = k ~ Visibility + HabitatType,
             data = data_SE
            )
model_CP <- cpm(formula_l = l ~ Visibility + Season, formula_s = s ~ 1, 
             data = data_CP,
             left = "LastPresentDecimalDays", 
             right = "FirstAbsentDecimalDays", dist = "weibull"
            )
avgSS <- averageSS(data_SS)
ghatsGeneric <- estgGeneric(n = 1000, avgSS, model_SE, model_CP, 
                  seed_SE = 1, seed_CP = 1, kFill = 0.5
                )

eM <- estM(nsim = 1000, data_CO, data_SS, data_DWP, frac = 1,  
                 model_SE = model_SE, model_CP = model_CP, 
                 seed_SE = NULL, seed_CP = NULL, seed_g = NULL, 
                 seed_M = NULL, kFill = NA,  
                 unitCol = "Unit", dateFoundCol = "DateFound", 
                 datesSearchedCol = "DateSearched", DWPCol = "S",
                 sizeclassCol = NULL)

M <- eM$M
Aj <- eM$Aj

M_season <- calcSplits(M = M, Aj = Aj, split_SS = NULL, 
                 split_CO = NULL,
                 data_SS = data_SS, data_CO = data_CO, unitCol = "Unit", 
                 dateFoundCol = "DateFound", dateSearchedCol = "DateSearched"
               )




modelSetSize_SE <- pkmSetSize(formula_p = p ~ Visibility*HabitatType, 
                     formula_k = k ~ HabitatType, data = data_SE,
                     obsCol = c("Search1", "Search2", "Search3", "Search4"),
                     sizeclassCol = "Size"
                   )
modelSetSize_CP <- cpmSetSize(formula_l = l ~ Visibility*Season, 
                     formula_s = s ~ Visibility, data = data_CP,
                     left = "LastPresentDecimalDays", 
                     right = "FirstAbsentDecimalDays", 
                     dist = c("exponential", "lognormal"),
                     sizeclassCol = "Size"
                   )

modelSizeSelections_SE <- c("p ~ Visibility; k ~ HabitatType", 
                            "p ~ Visibility * HabitatType; k ~ HabitatType",
                            "p ~ HabitatType; k ~ HabitatType",
                            "p ~ 1; k ~ HabitatType")
names(modelSizeSelections_SE) <- c("S", "M", "L", "XL")
modelSizeSelections_CP <- c("dist: exponential; l ~ 1; NULL",
                            "dist: lognormal; l ~ Visibility; s ~ Visibility",
                            "dist: exponential; l ~ 1; NULL",
                            "dist: exponential; l ~ Visibility; NULL")
names(modelSizeSelections_CP) <- c("S", "M", "L", "XL")

ghatsGenericSize <- estgGenericSize(n = 1000, avgSS, modelSetSize_SE, 
                      modelSetSize_CP, modelSizeSelections_SE,
                      modelSizeSelections_CP, seed_SE = 1, seed_CP = 1, 
                      kFill = NULL
                    )

sizeclassCol <- "Size"
sizeclasses <- as.character(unique(data_CO[ , sizeclassCol]))
nsizeclass <- length(sizeclasses)
models_SE <- vector("list", length = nsizeclass)
names(models_SE) <- names(modelSetSize_SE)
models_CP <- vector("list", length = nsizeclass)
names(models_CP) <- names(modelSetSize_CP)

for (sci in 1:nsizeclass){
  sizeclass <- names(models_SE[sizeclasses[sci]])
  classMatch <- which(names(models_SE) == sizeclass)
  modSE <- modelSizeSelections_SE[[sizeclass]]
  modCP <- modelSizeSelections_CP[[sizeclass]]
  models_SE[[sizeclass]] <- modelSetSize_SE[[sizeclass]][[modSE]]
  models_CP[[sizeclass]] <- modelSetSize_CP[[sizeclass]][[modCP]]
}




eM <- estM(nsim = 1000, data_CO, data_SS, data_DWP, frac = 1,  
                 model_SE = models_SE, model_CP = models_CP, 
                 seed_SE = NULL, seed_CP = NULL, seed_g = NULL, 
                 seed_M = NULL, kFill = NULL,  
                 unitCol = "Unit", dateFoundCol = "DateFound", 
                 datesSearchedCol = "DateSearched", DWPCol = sizeclasses,
                 sizeclassCol = "Size")

M <- eM$M
Aj <- eM$Aj

M_season <- calcSplits(M = M, Aj = Aj, split_SS = "Season", 
                 split_CO = "Unit",
                 data_SS = data_SS, data_CO = data_CO, unitCol = "Unit", 
                 dateFoundCol = "DateFound", dateSearchedCol = "DateSearched"
               )

summary(M_season)
plot(M_season)

splitSummary <- summary(M_season)

x<-(summary(M_season))


length(M_season)
length(summary(M_season))
class(summary(M_season))

attr(M_season, "vars")

(length(attr(M_season, "vars")) > 1)
length(M_season)