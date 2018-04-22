

rv$kFill <- NA
if (rv$kFixed == TRUE){
  rv$kFill <- rv$kFixedChoice 
}

rghat(rv$niterations, rv$dataCO, rv$dataSS, <semodel>, <cpmodel>,
 kFill = rv$kFill, unitCol = rv$unitCol, dateFoundCol = rv$dateFoundCol,
 dateSearchedCol = rv$dateSearchedCol)