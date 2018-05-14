devtools::load_all()
data(mockData)

data_SE <- mockData$SearcherEfficiencyData
data_CP <- mockData$CarcassPersistenceData
data_CO <- mockData$CarcassObservationData
data_SS <- mockData$SearchScheduleData


model_SE <- pkm(formula_p = p ~ Visibility * HabitatType, 
              formula_k = k ~ 1, data = data_SE)
model_CP <- cpm(formula_l = l ~ Season, formula_s = s ~ 1, 
             data = data_CP,
             left = "LastPresentDecimalDays", 
             right = "FirstAbsentDecimalDays", dist = "weibull")
avgSS <- averageSS(data_SS)
ghatsGeneric <- rghatGeneric(n = 1000, avgSS, model_SE, model_CP, seed_SE = 1,
                  seed_CP = 1, kFill = NULL
                )

ghatsAjs <- rghat(n = 1000, data_CO, data_SS, model_SE, model_CP, 
             seed_SE = 1, seed_CP = 1, unitCol = "Unit", 
             dateFoundCol = "DateFound", dateSearchedCol = "DateSearched",
             removeCleanout = TRUE
           )
ghat <- ghatsAjs$ghat


# need to create an rghatGenericSize so it works seamlessly in app
# need to add basic table and plotting stuff
