data_CP <- read.csv("inst/extdata/ExampleCarcassPersistence.csv")

head(data_CP)

left <- "LastPresentDecimalDays"
right <- "FirstAbsentDecimalDays"

data <- data_CP

formula_l <- l ~ Visibility*Season
formula_s <- s ~ Season
dist <- "exponential"

x<-cpm(formula_l, formula_s, data, left, right, dist)
print(x)