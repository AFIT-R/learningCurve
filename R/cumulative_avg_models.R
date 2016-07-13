#' Wright's Cumulative Average Unit Learning Curve Function
#'
#' @description Computes the time (or cost) required for a specific unit using Wright's cumulative average model.
#' 
#' @param t time (or cost) required for the mth unit of production
#' @param n nth unit you wish to predict the time (or cost) for
#' @param m mth unit for which you have time (or cost) information (default is m = 1)
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export

ca_unit <- function(t, n, r, m = 1, na.rm = FALSE){
  
  if(!is.numeric(t) | !is.numeric(m) |!is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         't: ', class(t), '\n',
         'n: ', class(n), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if(na.rm == TRUE) {
    t <- t[!is.na(t)]
    m <- m[!is.na(m)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    warning('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  b <- log(r)/log(2)
  c <- 1 + b
  
  y <- t * ((n^c - (n-1)^c)) / (m^c - (m-1)^c)
  
  return(y)
}


#' Wright's Cumulative Average Learning Curve Function
#'
#' @description Computes cumulative time or cost for units m through n in a production block using Wright's cumulative average model. Assumes the block begins at unit m and ends at unit n.
#' 
#' @param t time (or cost) required for the mth unit of production
#' @param n last unit of the production block of concern
#' @param m first unit of the production block of concern (default: m = 1)
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export

ca_block <- function(t, n, r, m = 1, na.rm = FALSE){
  
  if(!is.numeric(t) | !is.numeric(m) | !is.numeric(n) | !is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         't: ', class(t), '\n',
         'm: ', class(m), '\n',
         'n: ', class(n), '\n',
         'r: ', class(r))
  }
  
  if( m > n ){
    stop('This function computes the total hours/costs for a production block for \n',
         'units m through n; consequently, n must be larger than m.')
  }
  
  if(na.rm == TRUE) {
    t <- t[!is.na(t)]
    m <- m[!is.na(m)]
    n <- n[!is.na(n)]
    r <- r[!is.na(r)]
    
    warning('Any strings with NA were filtered. This may result in \n',
            'unexpected recycling.')
  }
  
  b <- log(r)/log(2)
  c <- 1 + b
  y <- t * (n^c - (m-1)^c)
  
  return(y)
}
