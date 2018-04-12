
devtools::install_github("ddalthorp/GenEst")
data(mockData)

data_SE <- mockData$SearcherEfficiencyData




xx <- GenEst::pkm(formula_p = p ~ 1, formula_k = k~1, data = data_SE, 
        obsCol = c("Search1"), kFixed = 0.2, kInit = 0.7, CL = 0.9)


devtools::load_all()

xx <- GenEst::pkmSet(formula_p = p ~ Visibility, formula_k = k~Visibility, 
        data = data_SE, 
        obsCol = c("Search1"), kFixed = 0.5, kInit = 0.7, CL = 0.9)

xx <- GenEst::pkmSetSize(formula_p = p ~ Visibility, formula_k = k~1, 
        data = data_SE, sizeclassCol = NULL, 
        obsCol = c("Search1"), kFixed = NULL, kInit = 0.7, CL = 0.9)




