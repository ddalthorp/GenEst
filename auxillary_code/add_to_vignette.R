this is what dan adds but then he comments it all out because it doesnt work

-By setting the seed here, we can arrive at the same randomized values from 
-each procedure (`rMhat` or `rMtilde` and `calcMhat`).
+Mortality estimates can be split into subcategories, such as species,
+visibility class, season, search interval, or other splitting covariate.
+
+For example,
+```{r}
+SSdat <- SS(data_SS)
+M_by_season <- calcSplits(M = Mhat, Aj = Aj, split_SS = "Season", SSdat = SSdat)
+plot(M_by_season)
+M_by_week <- calcSplits(M = Mhat, Aj = Aj,
+  split_time = seq(0, max(SSdat$days), by = 7), SSdat = SSdat)
+plot(M_by_week, rate = T)
+M_by_Unit <- calcSplits(M = Mhat, Aj = Aj, split_CO = "Unit", COdat = data_CO)
+plot(M_by_week, rate = T)
+summary(M_by_week)