#' Compute the logit.
#' 
#' @param x A probability (between 0 and 1, inclusive).
#' @return The logit of \code{x}.
#' @examples
#' logit(0.5)
#' @export 
#'
  logit <- function(x) {
    log(x / (1 - x))
  }

#' Compute the anti-logit.
#' 
#' @param x A number.
#' @return The anti-logit of \code{x}.
#' @examples
#' alogit(0)
#' @export 
#'
  alogit <- function(x) {
    1 / (1 + exp(-x))
  }
  
#' Create the predictor combination table for a searcher efficiency or carcass
#'  persistence analysis.
#' 
#' @param preds Names of predictor variables to include.
#' @param data Searcher efficiency or carcass persistence data.
#' @return Predictor combination table.
#' @examples
#' NA
#' @export 
#'
combine_preds <- function(preds, data){

  n_preds <- length(preds)
  if(n_preds == 0){
    return(data.frame(group = "all", CellNames = "all"))
  }else{
    if(any(is.na(match(preds, names(data))))) {
        stop("At least one predictor missing from data.")
    }
    var_names <- preds
    var_labels <- list()
    var_n_levels <- list()
    for(i in 1:n_preds){
      var_labels[[i]] <- levels(as.factor(data[[var_names[i]]]))
      var_n_levels[[i]] <- length(var_labels[[i]])
    }
    reps <- cumprod(var_n_levels)[n_preds:1]
    var_n_levels_1 <- var_n_levels[[1]]
    var_labels_1 <- var_labels[[1]]
    var_1 <- gl(var_n_levels_1, 1, length = reps[1], labels = var_labels_1)
    output <- data.frame(var_1)
    if(n_preds > 1){
      for(j in 2:n_preds){
        var_n_levels_j <- var_n_levels[[j]]
        var_labels_j <- var_labels[[j]]
        var_j <- gl(var_n_levels_j, reps[j], labels = var_labels_j) 
        output[[j]] <- var_j
      }
    }
  }
  names(output) <- var_names
  output$CellNames <- apply(output, 1, paste0, collapse = ".")
  return(output)
}


#' Check if all component terms and interactions are included in a formula
#'
#'
#' @param formula_in A formula object
#'
#' Terms are automatically alphabatized within interactions.
#'
#' @export 
#'
check_component_terms_included <- function(formula_in){
 
  term_orders <- attr(terms(formula_in), "order")
  term_names <- attr(terms(formula_in), "term.labels")
  n_terms <- length(term_names)

  if(n_terms == 0){
    return(TRUE)
  }
  if(max(term_orders) == 1){
    return(TRUE)
  }

  term_names_alph <- rep(NA, n_terms)
  for(i in 1:n_terms){
    temp <- strsplit(term_names[i], ":")[[1]]
    temp <- sort(temp)
    term_names_alph[i] <- paste(temp, collapse = ":")
  }
  term_orders_sort <- term_orders[order(term_orders, term_names_alph)]
  term_names_sort <- term_names_alph[order(term_orders, term_names_alph)]

  ixns <- which(term_orders_sort > 1)
  n_ixns <- length(ixns)
  ixn_orders <- term_orders_sort[ixns]
  all_there <- rep(FALSE, n_ixns)

  for(i in 1:n_ixns){
    ixn_name <- term_names_sort[ixns[i]]
    ixn_parts <- strsplit(ixn_name, ":")[[1]]
    ixn_sub_order <- ixn_orders[i] - 1
    to_check <- NULL
   
    for(j in 1:ixn_sub_order){

      comb_parts_options <- combn(ixn_parts, j)
      add_to_check <- apply(comb_parts_options, 2, paste, collapse = ":")
      to_check <- c(to_check, add_to_check)

    }

    all_there[i] <- all(to_check %in% term_names_alph)
  }
  output <- all(all_there)
  return(output)
}


