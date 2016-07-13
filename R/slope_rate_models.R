#' Natural Slope Rate Converter
#'
#' @description Computes the natural slope rate for given learning rates
#' 
#' @param r learning curve rate
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export

natural_slope <- function(r, na.rm = FALSE){
  
  if(!is.numeric(r)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided a rate in the form of a ',class(r))
  }
  
  if(na.rm == TRUE) {
    r <- r[!is.na(r)]
  }
  
  b <- log(r)/log(2)
  
  return(b)
  
} 


#' Learning Rate Converter
#'
#' @description Computes the learning rate for given natural slopes
#' 
#' @param b natural slope
#' @param na.rm Should \code{NA} values be removed?
#'
#' @export

lc_rate <- function(b, na.rm = FALSE){
  
  if(!is.numeric(b)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided a natural slope in the form of a ',class(b))
  }
  
  if(na.rm == TRUE) {
    b <- b[!is.na(b)]
  }
  
  y = 10^(b * log10(2) + 2)/100
  
  return(y)
}


#' Natural Slope Estimate
#'
#' @description Computes the natural slope based on total time (cost) to produce the first n units, time (cost) required for the first unit and total units produced.
#' 
#' @param T total time (or cost) required to produce the first n units
#' @param t time (or cost) required to produce the first unit
#' @param n total n units produced
#'
#' @export

natural_slope_est <- function(T, t, n){
  
  if(!is.numeric(T) | !is.numeric(t) | !is.numeric(n)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'T: ', class(T), '\n',
         't: ', class(t), '\n',
         'n: ', class(n))
  }
  
  b <- (log(T) - log(t)) / log(n) - 1
  
  return(b)
  
}  


#' Learning Rate Estimate
#'
#' @description Computes the learning rate based on total time (cost) to produce the first n units, time (cost) required for the first unit and total units produced.
#' 
#' @param T total time (or cost) required to produce the first n units
#' @param t time (or cost) required to produce the first unit
#' @param n total n units produced
#'
#' @export

lc_rate_est <- function(T, t, n){
  
  if(!is.numeric(T) | !is.numeric(t) | !is.numeric(n)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'T: ', class(T), '\n',
         't: ', class(t), '\n',
         'n: ', class(n))
  }
  
  b <- (log(T) - log(t)) / log(n) - 1
  y = 10^(b * log10(2) + 2)/100
  
  return(y)
  
} 