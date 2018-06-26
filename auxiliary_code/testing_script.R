devtools::load_all()
data(mock)

data_SE <- mock$SE
data_CP <- mock$CP
data_CO <- mock$CO
data_SS <- mock$SS
data_DWP <- mock$DWP

model_SE <- pkmSet(formula_p = p ~ Visibility*HabitatType, 
             formula_k = k ~ 1,
             data = data_SE
            )
plot(model_SE)
model_CP <- cpm(formula_l = l ~ Visibility, formula_s = s ~ Visibility, 
             data = data_CP,
             left = "LastPresentDecimalDays", 
             right = "FirstAbsentDecimalDays"
            )


avgSS <- averageSS(data_SS)
ghatsGeneric <- estgGeneric(n = 1000, avgSS, model_SE, model_CP, 
                  seed_SE = 1, seed_CP = 1, kFill = NULL
                )


eM <- estM(nsim = 1000, data_CO, data_SS, data_DWP, frac = 1,  
                 model_SE = model_SE, model_CP = model_CP, 
                 seed_SE = NULL, seed_CP = NULL, seed_g = NULL, 
                 seed_M = NULL, kFill = NULL,  
                 dateFoundCol = "DateFound", 
                 DWPCol = "S",
                 sizeclassCol = NULL)

M <- eM$M
Aj <- eM$Aj

M_ <- calcSplits(M = M, Aj = Aj, split_SS = NULL, 
                 split_CO = NULL,
                 data_SS = data_SS, data_CO = data_CO
               )
plot(M_)


pkmModSetSize <- pkmSetSize(formula_p = p ~ Visibility*HabitatType,
                   formula_k = k ~ HabitatType, data = data_SE,
                   obsCol = c("Search1", "Search2", "Search3", "Search4"),
                   sizeclassCol = "Size"
                 )

cpmModSetSize <- cpmSetSize(formula_l = l ~ Visibility*Season,
                   formula_s = s ~ Visibility, data = data_CP,
                   left = "LastPresentDecimalDays",
                   right = "FirstAbsentDecimalDays",
                   dist = c("exponential", "lognormal"),
                   sizeclassCol = "Size"
                 )

pkMods <- c("S" = "p ~ 1; k ~ 1", "L" = "p ~ 1; k ~ 1",
            "M" = "p ~ 1; k ~ 1", "XL" = "p ~ 1; k ~ 1"
          )
cpMods <- c("S" = "dist: exponential; l ~ 1; NULL", 
            "L" = "dist: exponential; l ~ 1; NULL",
            "M" = "dist: exponential; l ~ 1; NULL",
            "XL" = "dist: exponential; l ~ 1; NULL"
          )

pkmModSize <- trimSetSize(pkmModSetSize, pkMods)
cpmModSize <- trimSetSize(cpmModSetSize, cpMods)


eM <- estM(data_CO = data_CO, data_SS = data_SS, data_DWP = data_DWP, 
        frac = 1,
        model_SE = pkmModSize, model_CP = cpmModSize, seed_SE = NULL, 
        seed_CP = NULL, seed_g = NULL, seed_M = NULL, kFill = NULL,  
        unitCol = "Unit", dateFoundCol = "DateFound", 
        sizeclassCol = "Size", nsim = 1000)

gsGeneric <- estgGenericSize(nsim = 1000, days = avgSS,
               modelSetSize_SE = pkmModSetSize,
               modelSetSize_CP = cpmModSetSize,
               modelSizeSelections_SE = pkMods,
               modelSizeSelections_CP = cpMods
             )

