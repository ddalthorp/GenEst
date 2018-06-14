########Formulae and matrices
  preds_p <- all.vars(formula_p[[3]])
  formulaRHS_p <- formula(delete.response(terms(formula_p)))
  levels_p <- .getXlevels(terms(formulaRHS_p), data)

  preds_k <- character(0)
  if (length(formula_k) > 0){
    preds_k <- all.vars(formula_k[[3]])
    formulaRHS_k <- formula(delete.response(terms(formula_k)))
    levels_k <- .getXlevels(terms(formulaRHS_k), data)
  }
  if (length(kFixed) == 1){
    preds_k <- character(0)
    formulaRHS_k <- formula(~1)  
    formula_k <- c(fixedk = kFixed)
    levels_k <- .getXlevels(terms(formulaRHS_k), data)
  }

  preds <- unique(c(preds_p, preds_k))
  cells <- combinePreds(preds, data)
  ncell <- nrow(cells)
  cellNames <- cells$CellNames

  dataMM_p <- model.matrix(formulaRHS_p, data)
  dataMM_k <- model.matrix(formulaRHS_k, data)
  dataMM <- t(cbind(dataMM_p, dataMM_k))
   

  cellMM_p <- model.matrix(formulaRHS_p, cells)
  cellMM_k <- model.matrix(formulaRHS_k, cells)
  cellMM <- cbind(cellMM_p, cellMM_k)
 
  nbeta_k <- ncol(dataMM_k)
  nbeta_p <- ncol(dataMM_p)
  nbeta <- nbeta_p + nbeta_k

#################playing
x <- cells[-c(2,3), ]
for(i in names(x)){
    if(is.factor(x[, i])){
      x[, i] <- droplevels(x[, i])
    } 
}

x; str(x)

model.matrix(formulaRHS_k, x)
model.matrix(formula('~ size * tastiness'), x)
cellMM_k

resp = runif(10)
size = runif(10)
tastiness = runif(10)