data(wind_cleared)

data_SE <- wind_cleared$SE
data_CP <- wind_cleared$CP
data_CO <- wind_cleared$CO
data_SS <- wind_cleared$SS
data_DWP <- wind_cleared$DWP

pkmModSetSize <- pkmSetSize(formula_p = p ~ Season*Visibility,
                   formula_k = k ~ Season*Visibility, data = data_SE,
                   sizeclassCol = "Size"
                 )
cpmModSetSize <- cpmSetSize(formula_l = l ~ Season * Visibility,
                   formula_s = s ~ Season * Visibility, data = data_CP,
                   left = "Left",
                   right = "Right",
                   sizeclassCol = "Size"
                 )

avgSS <- averageSS(data_SS)

pkMods <- c("bat" = "p ~ Visibility; k ~ Season", 
            "lrg" = "p ~ Visibility; k ~ Season + Visibility",
            "med" = "p ~ Visibility; k ~ 1", 
            "sml" = "p ~ Visibility; k ~ Season + Visibility"
          )
cpMods <- c("bat" = "dist: weibull; l ~ Season + Visibility; s ~ 1", 
            "lrg" = "dist: exponential; l ~ Season + Visibility; NULL",
            "med" = "dist: weibull; l ~ Visibility; s ~ Season",
            "sml" = "dist: weibull; l ~ Visibility; s ~ 1"
          )

gsGeneric <- estgGenericSize(nsim = 1000, days = avgSS,
               modelSetSize_SE = pkmModSetSize,
               modelSetSize_CP = cpmModSetSize,
               modelSizeSelections_SE = pkMods,
               modelSizeSelections_CP = cpMods
             )