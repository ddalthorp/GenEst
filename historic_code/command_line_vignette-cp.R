data_CP <- read.csv("inst/extdata/ExampleCarcassPersistence.csv")

head(data_CP)

left <- "LastPresentDecimalDays"
right <- "FirstAbsentDecimalDays"

data <- data_CP

formula <- cp~Visibility*Season
dist <- "lognormal"

x<-cpm(formula, data, left, right, "exponential")
print(x)

cpLogLik(dataMM, thetaStart, t1, t2, dist) 