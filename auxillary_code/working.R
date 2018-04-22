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


data_carc <- mockData$CarcassObservationData
data_ss <- mockData$SearchScheduleData
SEmod <- pkModel
CPmod <- cpModel


rghat(n = 1000, data_carc, data_ss, model_SE = pkModel, model_CP = cpModel, 
seed_SE = 1, seed_CP = 1, kFill = NULL, unitCol = "Unit",
                  dateFoundCol = "DateFound", 
dateSearchedCol = "DateSearched")