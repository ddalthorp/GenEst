SE_data <- read.csv("inst/extdata/ExampleSearcherEfficiency.csv")

SE_data <- SE_data[SE_data$Size == "S", ]
model <- pkm(formula_p = p~Visibility, kFix = 0.5, data = SE_data)
mo<-pkmSet(data = SE_data, formula_p = p ~ HabitatType*Visibility)
pkmSetAICcTab(mo)
SE_data <- read.csv("inst/extdata/ExampleSearcherEfficiency.csv")

pkmSS <- pkmSetSize(formula_p = p~Visibility, formula_k = k~HabitatType * Visibility, sizeclassCol = "Size", data = SE_data)
pkmCheck(pkmSS)
pkmCheck("HI")



sizeclass_col <- "Size"


SE_data <- SEdataIn[SEdataIn$Size == "S", ]

pkm_set_s <- pkm_set(data = SE_data, pformula = p ~ HabitatType*Visibility)
SE_mods <- pkm_set_size(data = SE_data, pformula = p ~ Visibility*HabitatType,
kformula = k~1, sizeclass_col = "Size")

pkm_check(SE_mods)

pk1 <- pkm(data = SE_data, pformula = p ~ Visibility*Size*HabitatType)


pkm_set_aicc_tab(SE_mods[[1]])
names(SE_mods)[as.numeric(row.names(pkm_set_aicc_tab(SE_mods)))]
length(SE_mods)
SE_mods[[1]][[25]]

pkm_set_s[[1]][[1]]

sizeclasses <- unique(SE_data[ , sizeclass_col])
sizeclass_chosen <- which(sizeclasses == "XL")
pkm_set_aicc_tab(NULL)

rv$SE_data <- SEdataIn
rv$fixed_k



      rv$SE_mod <- pkm(pformula = p ~ 1, kformula = k ~ 1,
                       data = rv$SE_data, fixed_k = rv$fixed_k)