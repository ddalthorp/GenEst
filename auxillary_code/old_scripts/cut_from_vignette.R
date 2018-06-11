### Fatalities

We use the continuous binomial distribution to estimate the number of
fatalities ($\hat{M}$) associated with each estimate of detection probability
for each carcass and weight the estimate by the Density Weighted Proportion
(DWP) of the relevant area (e.g., around the turbine) where the carcass was
found. If DWP is not provided, `rMhat` makes a simple assumption that DWP is
1 for all carcasses (i.e., that all of the possibe area where the carcasses
could have landed was searched).

```{r}
Mhat <- rMhat(n = 1, ghat = ghat)
```

If DWP data are available in the standard condensed-table, format, they can be
incorporated into the ($\hat{M}$) calculation, but need to first be expanded
to be carcass-by-carcass:

```{r}
data_DWP <- mock$DWP
DWP <- DWPbyCarcass(data_DWP, data_CO, unitCol = "Unit",
         sizeclassCol = "Size", data_SS, dateFoundCol = "DateFound",
         dateSearchedCol = "DateSearched"
       )
Mhat <- rMhat(n = 1, ghat = ghat, DWP, seed = 12)
```

If one desires $\tilde{M}$ values, they can be obtained using `rMtilde`:

```{r}
Mtilde <- rMtilde(length(ghat), ghat, seed = 12)
```

which can then be used to calculate ($\hat{M}$):

```{r}
Mhat_viaMtilde <- calcMhat(Mtilde, DWP)
```

It is possible to split the resulting fatality estimation into components
that are denoted according to covariates in either the search schedule or
carcass observation data sets. 

### First, a temporal split:  

```{r, fig.show = "hold", fig.width = 7, fig.height = 7}
Mhat_season <- calcSplits(M = Mhat, Aj = Aj, split_SS = "Season", 
                 split_CO = NULL, data_SS = data_SS, data_CO = data_CO, 
                 unitCol = "Unit", dateFoundCol = "DateFound", 
                 dateSearchedCol = "DateSearched"
               )
summary(Mhat_season)
plot(Mhat_season)
```

### Next, a carcass split:  

```{r, fig.show = "hold", fig.width = 7, fig.height = 7}
Mhat_class <- calcSplits(M = Mhat, Aj = Aj, split_SS = NULL, 
                split_CO = "Split", data_SS = data_SS, data_CO = data_CO, 
                unitCol = "Unit", dateFoundCol = "DateFound", 
                dateSearchedCol = "DateSearched"
              )
summary(Mhat_class)
plot(Mhat_class)
```

### And finally, if two splits are included, the mortality estimation is 
expanded fully factorially:

```{r, fig.show = "hold", fig.width = 7, fig.height = 7}
Mhat_SbyC <- calcSplits(M = Mhat, Aj = Aj, split_SS = "Season", 
               split_CO = "Split", data_SS = data_SS, data_CO = data_CO, 
               unitCol = "Unit", dateFoundCol = "DateFound", 
               dateSearchedCol = "DateSearched"
             )
summary(Mhat_SbyC)
plot(Mhat_SbyC)
```