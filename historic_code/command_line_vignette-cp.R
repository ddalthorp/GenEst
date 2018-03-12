data_CP <- read.csv("inst/extdata/ExampleCarcassPersistence.csv")

head(data_CP)

left <- "LastPresentDecimalDays"
right <- "FirstAbsentDecimalDays"

data <- data_CP

formula <- cp~Visibility*Season
dist <- "lognormal"

cpm(formula, data, left, right, "exponential")

cpLogLik(dataMM, thetaStart, t1, t2, dist) 