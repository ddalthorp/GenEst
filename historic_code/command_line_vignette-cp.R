data_CP <- read.csv("inst/extdata/ExampleCarcassPersistence.csv")

head(data_CP)

left <- "LastPresentDecimalDays"
right <- "FirstAbsentDecimalDays"

data <- data_CP

formula_l <- l ~ Visibility*Season
formula_s <- s ~ Visibility
dist <- "exponential"

x<-cpm(formula_l, data = data, left = left, right = right, dist = dist)
print(x)
rcp(n = 1, model = x, seed = 1, type = "ppersist")

dists <- c("weibull", "exponential", "lognormal", "loglogistic")
xx <- cpmSet(formula_l, formula_s, data, left, right, dists)
cpmSetAICcTab(xx)

dists <- c("weibull", "exponential")
scc <- "Size"
xxx <- cpmSetSize(formula_l, formula_s, data, left, right, dists, scc)
xxx

cpmCheck(x)
cpmCheck(xx)
cpmCheck(xxx)