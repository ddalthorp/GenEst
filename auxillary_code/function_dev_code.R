

splitMhat <- function(Mhat, A, data_CO, data_ unitCol = "Unit",
                      sizeclassCol = NULL, removeCleanout = FALSE,
                      splitCol_CO = NULL, splitCol_SS = NULL, 
                      split_time = NULL
                      data_SS = NULL, dateFoundCol = "DateFound",
                      dateSearchedCol = "DateSearched", CL = 0.9){

  if ((!is.null(splitCol_SS) || !is.null(split_time)) & is.null(data_SS)){
    stop("Search schedule data must be provided if ",
      ifelse(is.null(splitCol_SS), "split_time ", "splitCol_SS "), "is")
  }
  if (!is.null(splitCol_CO) + !is.null(splitCol_SS) + !is.null(split_time) > 0){
    if (is.null(data_carc)){
      stop("Carcass data must be provided to split data")
    }
  }


}