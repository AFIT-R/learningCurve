#' Aggregate Learning Curve
#'
#' @description Computes the approximate aggregate cumulative learning curve formula by calculating the sum of all contributing hours from all departments for all production units 1 through n.
#' 
#' @param t vector of hours (or costs) for the first unit from departments 1 through m
#' @param n total units to be produced across all departments
#' @param r vector of historical learning rates for departments 1 through m
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export

agg_curve <- function(t, r, n, na.rm = FALSE){
  
  if(!is.numeric(t) | !is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         't: ', class(t), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if(na.rm == TRUE) {
    t <- t[!is.na(t)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    warning('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  H <- sum(t)
  
  b <- log(r)/log(2)
  c <- 1 + b
  hours_i <- t * n^c
  hours_all <- sum(hours_i)
  
  B <- (log(hours_all) - log(H)) / log(n)
  
  y <- H*n^B
  
  return(y)
}