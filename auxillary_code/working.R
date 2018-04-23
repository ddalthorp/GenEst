#devtools::install_github("ddalthorp/GenEst")
devtools::load_all()
data(mockData)

dataSE <- mockData$SearcherEfficiencyData
pkModel <- GenEst::pkm(formula_p = p ~ Visibility,
                       formula_k = k ~ 1, data = dataSE)

dataCP <- mockData$CarcassPersistenceData
cpModel <- GenEst::cpm(formula_l = l ~ Season, formula_s = s ~ 1, data = dataCP,
             left = "LastPresentDecimalDays", 
             right = "FirstAbsentDecimalDays", dist = "weibull")


ds <- data_ss[,"DateSearched"]
ds <- format(ds, format = "%m/%d/%Y")
data_ss[,"DateSearched"] <- ds

data_carc <- mockData$CarcassObservationData
data_ss <- mockData$SearchScheduleData
SEmod <- pkModel
CPmod <- cpModel


rghat(n = 10, data_carc, data_ss, model_SE = pkModel, model_CP = cpModel, 
seed_SE = 1, seed_CP = 1, kFill = NULL, unitCol = "Unit",
                  dateFoundCol = "DateFound", 
dateSearchedCol = "DateSearched")



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






