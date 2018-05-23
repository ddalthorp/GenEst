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
ghatsGeneric <- estghatGeneric(n = 1000, avgSS, model_SE, model_CP, 
                  seed_SE = 1, seed_CP = 1, kFill = NULL
                )



DWP <- DWPbyCarcass(data_DWP, data_CO, data_SS, 
                         dateFoundCol = "DateFound",
                         dateSearchedCol = "DateSearched",
                         DWPCol = "S", unitCol = "Unit")



ghatsAjs <- estghat(n = 1000, data_CO, data_SS, model_SE, model_CP, 
             seed_SE = 1, seed_CP = 1, unitCol = "Unit", 
             dateFoundCol = "DateFound", dateSearchedCol = "DateSearched"
           )
ghat <- ghatsAjs$ghat
Aj <- ghatsAjs$Aj

Mhat <- rMhat(n = 1, ghat = ghat, DWP, seed = 12)

Mhat_season <- calcSplits(M = Mhat, Aj = Aj, split_SS = "Season", 
                 split_CO = "Split",
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

modelSizeSelections_SE <- rep("p ~ Visibility; k ~ HabitatType", 4)
modelSizeSelections_CP <- rep("dist: exponential; l ~ 1; NULL", 4)

ghatsGenericSize <- rghatGenericSize(n = 1000, avgSS, modelSetSize_SE, 
                      modelSetSize_CP, modelSizeSelections_SE,
                      modelSizeSelections_CP, seed_SE = 1, seed_CP = 1, 
                      kFill = NULL
                    )






