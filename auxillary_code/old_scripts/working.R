#devtools::install_github("ddalthorp/GenEst")
devtools::load_all()
data(mockData)

dataSE <- mockData$SearcherEfficiencyData
dataCP <- mockData$CarcassPersistenceData
data_carc <- mockData$CarcassObservationData
data_ss <- mockData$SearchScheduleData
data_dwp <- mockData$DensityWeightedProportionData

pkModel <- GenEst::pkm(formula_p = p ~ Visibility,
                       formula_k = k ~ 1, data = dataSE)

cpModel <- GenEst::cpm(formula_l = l ~ Season, formula_s = s ~ 1, 
             data = dataCP, left = "LastPresentDecimalDays", 
             right = "FirstAbsentDecimalDays", dist = "weibull")

gandA <- rghat(n = 1000, data_carc, data_ss, model_SE = pkModel, 
               model_CP = cpModel, seed_SE = 1, seed_CP = 1, 
               kFill = NULL, unitCol = "Unit",
               dateFoundCol = "DateFound", 
               dateSearchedCol = "DateSearched")

ghat <- gandA$ghat

MT <- rMtilde(length(ghat), ghat, 1)
DWP <- DWPbyCarcass(data_DWP = data_dwp, data_carc, data_ss, unitCol = "Unit", 
         sizeclassCol = "Size")
MH <- calcMhat(MT, DWP)

MH2 <- rMhat(n = 1, ghat, DWP, seed = 1)


pkmModSetSize <- pkmSetSize(formula_p = p ~ Visibility*HabitatType, 
                   formula_k = k ~ HabitatType, data = dataSE,
                   obsCol = c("Search1", "Search2", "Search3", "Search4"),
                   sizeclassCol = "Size"
                 )
cpmModSetSize <- cpmSetSize(formula_l = l ~ Visibility*Season, 
                   formula_s = s ~ Visibility, data = dataCP,
                   left = "LastPresentDecimalDays", 
                   right = "FirstAbsentDecimalDays", 
                   dist = c("exponential", "lognormal"),
                   sizeclassCol = "Size"
                 )






