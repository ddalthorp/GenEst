SE_data <- read.csv("inst/extdata/ExampleSearcherEfficiency.csv")

SE_data <- SE_data[SE_data$Size == "S", ]
model <- pkm(formula_p = p~Visibility*Season, kFix = 0.5, data = SE_data)
mo<-pkmSet(data = SE_data, formula_p = p ~ HabitatType*Visibility, kFix = 0.5)
pkmSetAICcTab(mo)
SE_data <- read.csv("inst/extdata/ExampleSearcherEfficiency.csv")

pkmSS <- pkmSetSize(formula_p = p~Visibility, formula_k = k~HabitatType * Visibility, sizeclassCol = "Size", data = SE_data)
pkmCheck(pkmSS)
pkmCheck("HI")

pkmSECellPlot(model, "M")
model_spec <- pkm(formula_p = p~Season, formula_k = k~1, data = SE_data)
model_full <- pkm(formula_p = p~Visibility*Season, formula_k = k~Visibility*Season, data = SE_data)
matchCells(model_spec, model_full)




model <- pkm(formula_p = p~Visibility, formula_k = k~HabitatType, data = SE_data)
plot(model, col = "blue", n = 10, seed = 2)

pkmSetSECellPlot(mo, "p ~ Visibility; k fixed at 1", "HT1.H")
par(mfrow=c(1,2))
pkmParamPlot(model, "p", 1000, NULL, "black")
pkmParamPlot(model, "k", 1000, NULL, "black")


par(mfrow=c(1,2))
mo<-pkmSet(data = SE_data, formula_p = p ~ HabitatType*Visibility)
pkmSetParamPlot(modelSet = mo, pk = "p", n = 1000, 
  specificModel = "p ~ 1; k fixed at 1")
pkmSetParamPlot(modelSet = mo, pk = "k", n = 1000, 
  specificModel = "p ~ 1; k fixed at 1")
