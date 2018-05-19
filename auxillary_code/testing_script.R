devtools::load_all()
data(mockData)

data_SE <- mockData$SearcherEfficiencyData
data_CP <- mockData$CarcassPersistenceData
data_CO <- mockData$CarcassObservationData
data_SS <- mockData$SearchScheduleData


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
ghatsGeneric <- rghatGeneric(n = 1000, avgSS, model_SE, model_CP, seed_SE = 1,
                  seed_CP = 1, kFill = NULL
                )


data_DWP <- mockData$DensityWeightedProportionData
DWP <- DWPbyCarcass(data_DWP, data_CO, unitCol = "Unit",
         sizeclassCol = NULL, data_SS, dateFoundCol = "DateFound",
         dateSearchedCol = "DateSearched"
       )



ghatsAjs <- rghat(n = 10, data_CO, data_SS, model_SE, model_CP, 
             seed_SE = 1, seed_CP = 1, unitCol = "Unit", 
             dateFoundCol = "DateFound", dateSearchedCol = "DateSearched",
removeCleanout = TRUE
           )
ghat <- ghatsAjs$ghat








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






