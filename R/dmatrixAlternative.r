library(plyr)

dat = data

dat$cell = paste0(data$size, data$tastiness)
aggregate(s1 ~ cell, mean, data = dat, na.rm = T)


agg = ddply(.data = dat[,6:12], .variables = ~dat$cell, colMeans, na.rm = T)



layout(1:4)
x = 1:7
for(i in 1:nrow(agg)){
    plot(as.numeric(agg[i, 2:8]) ~x, type = 'l', ylim = c(0,1))
}


all.equal(model.matrix(formula_p, data), model.matrix(formulaRHS_p, data))


dmatrix = function(m1, m2){
    out = cbind(
        rbind(m1, matrix(0,  ncol = ncol(m1), nrow = nrow(m2))),
        rbind(matrix(0, ncol = ncol(m2), nrow = nrow(m1)), m2))
    rownames(out) = c(rownames(m1), rownames(m2))
    colnames(out) = c(colnames(m1), colnames(m2))
    return(out)
}
